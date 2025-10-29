#include <wayfire/per-output-plugin.hpp>
#include <memory>
#include <wayfire/plugin.hpp>
#include <wayfire/opengl.hpp>
#include <wayfire/output.hpp>
#include <wayfire/core.hpp>
#include <wayfire/workspace-stream.hpp>
#include <wayfire/render-manager.hpp>
#include <wayfire/workspace-set.hpp>
#include <wayfire/scene-operations.hpp>
#include <wayfire/plugins/common/input-grab.hpp>
#include "wayfire/plugins/ipc/ipc-activator.hpp"
#include <linux/input-event-codes.h>
#include "wayfire/plugins/wobbly/wobbly-signal.hpp"

#include <glm/gtc/matrix_transform.hpp>
#include <limits>
#include <glm/gtc/matrix_inverse.hpp>
#include <wayfire/img.hpp>

#include "cube.hpp"
#include "simple-background.hpp"
#include "skydome.hpp"
#include "cubemap.hpp"
#include "cube-control-signal.hpp"
#include "wayfire/region.hpp"
#include "wayfire/scene-render.hpp"
#include "wayfire/scene.hpp"
#include "wayfire/signal-definitions.hpp"
#include <chrono>

#define Z_OFFSET_NEAR 0.89567f
#define Z_OFFSET_FAR  2.00000f

#define ZOOM_MAX 10.0f
#define ZOOM_MIN 0.1f


#define CUBE_VERTICAL_SPACING -1.2f
#define CUBE_SPACING 1.2f

#ifdef USE_GLES32
    #include <GLES3/gl32.h>
#endif

#include "shaders.tpp"
#include "shaders-3-2.tpp"

// Vertex shader - updated to pass world position
static const char *cube_cap_vertex = R"(
#version 100
attribute mediump vec2 position;
attribute mediump vec2 uvPosition;

uniform mat4 VP;
uniform mat4 model;

varying mediump vec2 uvpos;
varying mediump vec3 worldPos;

void main() {
    uvpos = uvPosition;
    vec4 worldPosition = model * vec4(position.x, 0.0, position.y, 1.0);
    worldPos = worldPosition.xyz;
    gl_Position = VP * worldPosition;
}
)";

static const char *cube_cap_fragment = R"(
#version 100
precision mediump float;
varying mediump vec2 uvpos;
varying mediump vec3 worldPos;
uniform sampler2D smp;
uniform float cap_alpha;
uniform float time;
uniform float u_num_sides;

// Simple rounded corner mask for square UV space
float roundedCornerMask(vec2 uv, float radius) {
    vec2 centered_uv = uv - 0.5;
    vec2 corner_dist = abs(centered_uv) - (0.5 - radius);
    
    if (corner_dist.x <= 0.0 && corner_dist.y <= 0.0) {
        return 1.0;
    }
    
    if (corner_dist.x > 0.0 && corner_dist.y > 0.0) {
        float dist_to_corner = length(corner_dist);
        return smoothstep(radius + 0.01, radius - 0.01, dist_to_corner);
    }
    
    return 1.0;
}

// Edge anti-aliasing
float edgeAntiAliasMask(vec2 uv) {
    float min_edge_dist = min(min(uv.x, 1.0 - uv.x), min(uv.y, 1.0 - uv.y));
    float aa_width = 0.01;
    return smoothstep(0.0, aa_width, min_edge_dist);
}

void main() {
    // Calculate distance from center using UV coordinates
    vec2 centerUV = uvpos - vec2(0.5, 0.5);
    float dist = length(centerUV) * 2.0;
    
    // Wave parameters
    float frequency = 40.0;
    float speed = 1.8;
    float amplitude = 0.1;
    
    // Calculate wave height
    float height = sin(dist * frequency - time * speed) * amplitude;
    
    // Calculate gradients for normal mapping
    float delta = 0.01;
    
    // X gradient
    vec2 uvX1 = uvpos + vec2(delta, 0.0);
    vec2 uvX2 = uvpos - vec2(delta, 0.0);
    float distX1 = length((uvX1 - vec2(0.5, 0.5)) * 2.0);
    float distX2 = length((uvX2 - vec2(0.5, 0.5)) * 2.0);
    float hX1 = sin(distX1 * frequency - time * speed) * amplitude;
    float hX2 = sin(distX2 * frequency - time * speed) * amplitude;
    float dx = (hX1 - hX2) / (2.0 * delta);
    
    // Y gradient
    vec2 uvY1 = uvpos + vec2(0.0, delta);
    vec2 uvY2 = uvpos - vec2(0.0, delta);
    float distY1 = length((uvY1 - vec2(0.5, 0.5)) * 2.0);
    float distY2 = length((uvY2 - vec2(0.5, 0.5)) * 2.0);
    float hY1 = sin(distY1 * frequency - time * speed) * amplitude;
    float hY2 = sin(distY2 * frequency - time * speed) * amplitude;
    float dy = (hY1 - hY2) / (2.0 * delta);
    
    // Calculate normal from gradients
    vec3 normal = normalize(vec3(-dx, -dy, 1.0));
    
    // Animated light direction
    vec3 lightDir = normalize(vec3(0.3, sin(time * 0.2), 0.5));
    
    // Calculate lighting
    float brightness = clamp(exp(dot(normal, lightDir)) * 0.5, 0.0, 1.0);
    
    // Get base color from texture
    vec4 texColor = texture2D(smp, uvpos);
    
    // Apply lighting
    vec3 finalColor = texColor.rgb * brightness;
    
    // Apply simple rounded corners (works in UV space)
    float corner_radius = 0.6;
    float corner_mask = roundedCornerMask(uvpos, corner_radius);
    float edge_mask = edgeAntiAliasMask(uvpos);
    float final_mask = min(corner_mask, edge_mask);
    
    // Combine texture alpha with corner mask
    float finalAlpha = texColor.a * final_mask;
    
    gl_FragColor = vec4(finalColor, finalAlpha);
}
)";

// Background Vertex Shader - Simple fullscreen quad
static const char *background_vertex_shader = R"(
#version 100
attribute vec2 position;
varying vec2 v_uv;

void main() {
    gl_Position = vec4(position, 0.0, 1.0);
    v_uv = position * 0.5 + 0.5;
}
)";




static const char *background_fragment_shader = R"(
#version 100
precision mediump float;
uniform float u_time;
uniform vec2 u_resolution;
varying vec2 v_uv;

// Star Nest by Pablo Roman Andrioli
// License: MIT
#define iterations 10
#define formuparam 0.28
#define volsteps 20
#define stepsize 0.1
#define zoom 0.800
#define tile 0.850
#define speed 0.01
#define brightness 0.0005
#define darkmatter 0.900
#define distfading 0.630
#define saturation 1.90

// TRON-STYLE EDGE CONFIGURATION
#define LINE_WIDTH 0.008
#define LINE_LENGTH 0.12
#define GLOW_WIDTH 0.55
#define ANIMATION_SPEED 0.2
#define BASE_EDGE_GLOW 0.5
#define NUM_LINES 12
#define EDGE_MARGIN 0.0

// LINE COLOR - Blue
#define LINE_COLOR vec3(0.0, 0.5, 1.0)

// NEW TRON ELEMENTS
#define GRID_SIZE 0.05
#define GRID_WIDTH 0.001
#define GRID_GLOW 0.15
#define SCANLINE_SPEED 0.3
#define HEX_GLOW 0.2
#define PI 3.14159265359

float distanceToEdge(vec2 uv, vec2 resolution) {
    vec2 pixel_pos = uv * resolution;
    float dist_left = pixel_pos.x - EDGE_MARGIN;
    float dist_right = resolution.x - EDGE_MARGIN - pixel_pos.x;
    float dist_bottom = pixel_pos.y - EDGE_MARGIN;
    float dist_top = resolution.y - EDGE_MARGIN - pixel_pos.y;
    return min(min(dist_left, dist_right), min(dist_bottom, dist_top)) / resolution.x;
}



float uvToPerimeter(vec2 uv, vec2 resolution) {
    vec2 pixel_pos = uv * resolution;
    vec2 margin = vec2(EDGE_MARGIN);
    vec2 inner_size = resolution - 2.0 * margin;
    vec2 p = (pixel_pos - margin) / inner_size.x;
    
    float dist_left = p.x;
    float dist_right = (inner_size.x / inner_size.x) - p.x;
    float dist_bottom = p.y * (inner_size.y / inner_size.x);
    float dist_top = (inner_size.y / inner_size.x) - p.y * (inner_size.y / inner_size.x);
    
    float edge_dist = min(min(dist_left, dist_right), min(dist_bottom, dist_top));
    float perimeter_length = 2.0 * (1.0 + inner_size.y / inner_size.x);
    
    if (edge_dist == dist_bottom) {
        return p.x / perimeter_length;
    } else if (edge_dist == dist_right) {
        return (1.0 + p.y * (inner_size.y / inner_size.x)) / perimeter_length;
    } else if (edge_dist == dist_top) {
        return (1.0 + (inner_size.y / inner_size.x) + (1.0 - p.x)) / perimeter_length;
    } else {
        return (2.0 + (inner_size.y / inner_size.x) + ((inner_size.y / inner_size.x) - p.y * (inner_size.y / inner_size.x))) / perimeter_length;
    }
}

// Rotation
vec2 rotate2D(vec2 p, float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return vec2(p.x * c - p.y * s, p.x * s + p.y * c);
}

// Grid pattern
float grid(vec2 uv) {
    vec2 grid_uv = fract(uv / GRID_SIZE);
    vec2 grid_line = smoothstep(GRID_WIDTH, 0.0, abs(grid_uv - 0.5) - 0.5 + GRID_WIDTH);
    return max(grid_line.x, grid_line.y);
}

// Hexagon shape
float hexagon(vec2 p, float r) {
    vec3 k = vec3(-0.866025404, 0.5, 0.577350269);
    p = abs(p);
    p -= 2.0 * min(dot(k.xy, p), 0.0) * k.xy;
    p -= vec2(clamp(p.x, -k.z * r, k.z * r), r);
    return length(p) * sign(p.y);
}

// Spinning hexagon with glow
vec3 spinningHexagon(vec2 uv, vec2 center, float size, float atime, float aspeed) {
    vec2 p = uv - center;
    p = rotate2D(p, atime * aspeed);
    
    float d = hexagon(p, size);
    float outline = smoothstep(0.003, 0.0, abs(d));
    float glow = smoothstep(0.04, 0.0, abs(d)) * 0.3;
    
    return LINE_COLOR * (outline + glow);
}
// Pinwheel/Turbine shape - matches the uploaded logo with dual-lip blades
// Pinwheel/Turbine shape - matches the uploaded logo with dual-lip blades
vec3 pinwheel(vec2 uv, vec2 center, float size, float atime, float aspeed) {
    vec2 p = uv - center;
    p = rotate2D(p, atime * aspeed);
    
    float angle = atan(p.y, p.x);
    float dist = length(p);
    
    vec3 color = vec3(0.0);
    
    // Define colors for each blade
    vec3 red = vec3(0.9, 0.15, 0.1);
    vec3 yellow = vec3(1.0, 0.95, 0.2);
    vec3 orange = vec3(1.0, 0.45, 0.0);
    
    // Create 6 curved blades with alternating colors
    for (int i = 0; i < 6; i++) {
        float blade_angle = float(i) * PI / 3.0;
        float angle_diff = angle - blade_angle;
        
        // Normalize angle difference to -PI to PI
        angle_diff = mod(angle_diff + PI, 2.0 * PI) - PI;
        
        // Create smooth S-curve
        float normalized_dist = dist / size;
        
        // Primary curve
        float curve = -1.2 * sin(normalized_dist * 2.8 + 0.5) * normalized_dist;
        
        // Base blade width that WIDENS toward the tip
        float width_curve = sin(normalized_dist * PI);
        
        // Blade gets wider as it extends outward (from 0.6 to tip)
        float widening = 1.0;
        if (normalized_dist > 0.6) {
            widening = 1.0 + ((normalized_dist - 0.6) / 0.4) * 0.8; // Gets up to 1.8x wider
        }
        
        // Add edge dips - creates narrowing at start
        float edge_dip_start = smoothstep(0.0, 0.1, normalized_dist);
        
        // Create the center notch at the outer edge (creating two lips)
       // Create the dual-lip notch at the outer edge
        float notch = 0.0;
        if (normalized_dist > 0.75) {
            // Position along the outer portion of the blade
            float outer_progress = (normalized_dist - 0.75) / 0.25;
            
            // Distance from center line of blade
            float lateral_dist = abs(angle_diff - curve);
            
            // Create two "lips" - one on each side of center
            // The notch is deepest slightly off-center on each side
            float lip_left = smoothstep(0.01, 0.12, lateral_dist) * smoothstep(0.20, 0.15, lateral_dist);
            float lip_right = smoothstep(0.01, 0.12, lateral_dist) * smoothstep(0.20, 0.15, lateral_dist);
            
            // Combine lips to create the dual-notch effect
            // Stronger near the tip
            notch = (lip_left + lip_right) * outer_progress * 0.35;
        }
        
        // Combine base width with widening, edge dips, and center notch
        float blade_width = (0.25 + width_curve * 0.25) * widening * (0.5 + edge_dip_start * 0.5) * (1.0 - notch);
        
        float blade_dist = abs(angle_diff - curve);
        
        // Sharper blade edges
        float blade_mask = 1.0 - smoothstep(blade_width * 0.5, blade_width, blade_dist);
        
        // Better radial falloff
        float inner_radius = 0.05;
        float outer_radius = 1.05;
        
        float outer_fade = smoothstep(outer_radius, outer_radius - 0.15, normalized_dist);
        float inner_fade = 1.0 - smoothstep(inner_radius + 0.08, inner_radius, normalized_dist);
        
        float radial_mask = outer_fade * inner_fade;
        
        // Assign colors: red, orange, yellow pattern
        vec3 blade_color;
        int color_index = int(mod(float(i), 3.0));
        if (color_index == 0) {
            blade_color = red;
        } else if (color_index == 1) {
            blade_color = orange;
        } else {
            blade_color = yellow;
        }
        
        // Apply blade color with intensity boost
        color += blade_color * blade_mask * radial_mask * 1.1;
    }
    
    // Add blue hexagonal outer border
    float hex_d = hexagon(p, size * 1.15);
    float hex_outline = smoothstep(0.005, 0.0, abs(hex_d));
    float hex_glow = smoothstep(0.01, 0.0, abs(hex_d)) * 0.3;
    color += LINE_COLOR * (hex_outline * 2.0 + hex_glow);
    
    return color;
}
// Triangle shape
float triangle(vec2 p, float r) {
    float k = sqrt(3.0);
    p.x = abs(p.x) - r;
    p.y = p.y + r / k;
    if (p.x + k * p.y > 0.0) {
        p = vec2(p.x - k * p.y, -k * p.x - p.y) / 2.0;
    }
    p.x -= clamp(p.x, -2.0 * r, 0.0);
    return -length(p) * sign(p.y);
}

// Rotating triangle
vec3 rotatingTriangle(vec2 uv, vec2 center, float size, float atime, float aspeed) {
    vec2 p = uv - center;
    p = rotate2D(p, atime * aspeed);
    
    float d = triangle(p, size);
    float outline = smoothstep(0.003, 0.0, abs(d));
    float glow = smoothstep(0.04, 0.0, abs(d)) * 0.3;
    
    return LINE_COLOR * (outline + glow);
}

// Pulsing hexagon
vec3 pulsingHexagon(vec2 uv, vec2 center, float baseSize, float atime, float aspeed) {
    vec2 p = uv - center;
    float pulse = sin(atime * aspeed) * 0.5 + 0.5;
    float size = baseSize * (0.7 + pulse * 0.3);
    
    float d = hexagon(p, size);
    float outline = smoothstep(0.003, 0.0, abs(d));
    float glow = smoothstep(0.04, 0.0, abs(d)) * 0.3 * pulse;
    
    return LINE_COLOR * (outline + glow * 2.0);
}

// Orbiting shapes - unrolled
vec3 orbitingShapes(vec2 uv, vec2 center, float atime) {
    vec3 color = vec3(0.0);
    float orbitRadius = 0.15;
    
    float angle0 = atime * 0.5 + 0.0 * PI * 2.0 / 3.0;
    vec2 orbitPos0 = center + vec2(cos(angle0), sin(angle0)) * orbitRadius;
    vec2 p0 = uv - orbitPos0;
    float d0 = length(p0) - 0.015;
    float outline0 = smoothstep(0.002, 0.0, abs(d0));
    float glow0 = smoothstep(0.02, 0.0, abs(d0)) * 0.5;
    color += LINE_COLOR * (outline0 + glow0);
    
    float angle1 = atime * 0.5 + 1.0 * PI * 2.0 / 3.0;
    vec2 orbitPos1 = center + vec2(cos(angle1), sin(angle1)) * orbitRadius;
    vec2 p1 = uv - orbitPos1;
    float d1 = length(p1) - 0.015;
    float outline1 = smoothstep(0.002, 0.0, abs(d1));
    float glow1 = smoothstep(0.02, 0.0, abs(d1)) * 0.5;
    color += LINE_COLOR * (outline1 + glow1);
    
    float angle2 = atime * 0.5 + 2.0 * PI * 2.0 / 3.0;
    vec2 orbitPos2 = center + vec2(cos(angle2), sin(angle2)) * orbitRadius;
    vec2 p2 = uv - orbitPos2;
    float d2 = length(p2) - 0.015;
    float outline2 = smoothstep(0.002, 0.0, abs(d2));
    float glow2 = smoothstep(0.02, 0.0, abs(d2)) * 0.5;
    color += LINE_COLOR * (outline2 + glow2);
    
    return color;
}

// Scanlines
float scanlines(vec2 uv, float atime) {
    float line = sin((uv.y + atime * SCANLINE_SPEED) * 200.0);
    return smoothstep(0.9, 1.0, line);
}

// Helper function: signed distance to line segment
float sdSegment(vec2 p, vec2 a, vec2 b) {
    vec2 pa = p - a;
    vec2 ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h);
}


// W symbol with glowing blue effect
vec3 letter_W(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the W shape using line segments
    // W has 4 diagonal strokes forming the letter
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Left outer stroke (top-left to bottom-middle-left)
    vec2 p1_start = vec2(-0.5, 0.5);
    vec2 p1_end = vec2(-0.25, -0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Left inner stroke (bottom-middle-left to top-middle)
    vec2 p2_start = vec2(-0.25, -0.5);
    vec2 p2_end = vec2(0.0, 0.2);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Right inner stroke (top-middle to bottom-middle-right)
    vec2 p3_start = vec2(0.0, 0.2);
    vec2 p3_end = vec2(0.25, -0.5);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Right outer stroke (bottom-middle-right to top-right)
    vec2 p4_start = vec2(0.25, -0.5);
    vec2 p4_end = vec2(0.5, 0.5);
    float d4 = sdSegment(p, p4_start, p4_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), min(d3, d4));
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// A symbol with glowing blue effect
vec3 letter_A(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the A shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Left stroke (bottom-left to top-center)
    vec2 p1_start = vec2(-0.4, -0.5);
    vec2 p1_end = vec2(0.0, 0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Right stroke (top-center to bottom-right)
    vec2 p2_start = vec2(0.0, 0.5);
    vec2 p2_end = vec2(0.4, -0.5);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Horizontal crossbar (middle of both strokes)
    vec2 p3_start = vec2(-0.2, 0.0);
    vec2 p3_end = vec2(0.2, 0.0);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), d3);
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// Y symbol with glowing blue effect
vec3 letter_Y(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the Y shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Left stroke (top-left to center)
    vec2 p1_start = vec2(-0.4, 0.5);
    vec2 p1_end = vec2(0.0, 0.0);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Right stroke (top-right to center)
    vec2 p2_start = vec2(0.4, 0.5);
    vec2 p2_end = vec2(0.0, 0.0);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Vertical stem (center to bottom)
    vec2 p3_start = vec2(0.0, 0.0);
    vec2 p3_end = vec2(0.0, -0.5);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), d3);
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// F symbol with glowing blue effect
// F symbol with glowing blue effect (flipped)
vec3 letter_F(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the F shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Vertical stroke (bottom to top)
    vec2 p1_start = vec2(0.3, -0.5);
    vec2 p1_end = vec2(0.3, 0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Top horizontal stroke (right to left)
    vec2 p2_start = vec2(0.3, 0.5);
    vec2 p2_end = vec2(-0.3, 0.5);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Middle horizontal stroke (right to left, shorter)
    vec2 p3_start = vec2(0.3, 0.05);
    vec2 p3_end = vec2(-0.2, 0.05);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), d3);
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// I symbol with glowing blue effect
vec3 letter_I(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the I shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Vertical stroke (bottom to top)
    vec2 p1_start = vec2(0.0, -0.5);
    vec2 p1_end = vec2(0.0, 0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Top horizontal stroke (serif)
    vec2 p2_start = vec2(-0.25, 0.5);
    vec2 p2_end = vec2(0.25, 0.5);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Bottom horizontal stroke (serif)
    vec2 p3_start = vec2(-0.25, -0.5);
    vec2 p3_end = vec2(0.25, -0.5);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), d3);
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// R symbol with glowing blue effect (flipped)
vec3 letter_R(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the R shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Vertical stroke (bottom to top)
    vec2 p1_start = vec2(0.3, -0.5);
    vec2 p1_end = vec2(0.3, 0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Top horizontal stroke
    vec2 p2_start = vec2(0.3, 0.5);
    vec2 p2_end = vec2(-0.25, 0.5);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Left vertical stroke (top bowl)
    vec2 p3_start = vec2(-0.25, 0.5);
    vec2 p3_end = vec2(-0.25, 0.05);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Middle horizontal stroke (bottom of bowl)
    vec2 p4_start = vec2(-0.25, 0.05);
    vec2 p4_end = vec2(0.3, 0.05);
    float d4 = sdSegment(p, p4_start, p4_end);
    
    // Diagonal leg (middle to bottom-left)
    vec2 p5_start = vec2(0.05, 0.05);
    vec2 p5_end = vec2(-0.3, -0.5);
    float d5 = sdSegment(p, p5_start, p5_end);
    
    // Combine all strokes
    float d = min(min(min(d1, d2), min(d3, d4)), d5);
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

// E symbol with glowing blue effect
vec3 letter_E(vec2 uv, vec2 center, float size) {
    vec2 p = (uv - center) / size;
    
    vec3 color = vec3(0.0);
    
    // Define the E shape using line segments
    float line_width = 0.08;
    float glow_width = 0.15;
    
    // Vertical stroke (bottom to top)
    vec2 p1_start = vec2(0.3, -0.5);
    vec2 p1_end = vec2(0.3, 0.5);
    float d1 = sdSegment(p, p1_start, p1_end);
    
    // Top horizontal stroke (right to left)
    vec2 p2_start = vec2(0.3, 0.5);
    vec2 p2_end = vec2(-0.3, 0.5);
    float d2 = sdSegment(p, p2_start, p2_end);
    
    // Middle horizontal stroke (right to left)
    vec2 p3_start = vec2(0.3, 0.0);
    vec2 p3_end = vec2(-0.2, 0.0);
    float d3 = sdSegment(p, p3_start, p3_end);
    
    // Bottom horizontal stroke (right to left)
    vec2 p4_start = vec2(0.3, -0.5);
    vec2 p4_end = vec2(-0.3, -0.5);
    float d4 = sdSegment(p, p4_start, p4_end);
    
    // Combine all strokes
    float d = min(min(d1, d2), min(d3, d4));
    
    // Create the main line
    float line = smoothstep(line_width, line_width * 0.5, d);
    
    // Create the glow
    float glow = smoothstep(glow_width, 0.0, d) * 0.5;
    
    // Apply blue color
    color = LINE_COLOR * (line * 2.0 + glow);
    
    return color;
}

void main() {
    // STAR NEST BACKGROUND
    vec2 uv = v_uv - 0.5;
    uv.y *= u_resolution.y / u_resolution.x;
    vec3 dir = vec3(uv * zoom, 1.0);
    
    float time = u_time * speed + 0.25;
    
    float a1 = 0.5 + sin(u_time * 0.001) * 0.5;
    float a2 = 0.8 + cos(u_time * 0.0015) * 0.5;
    mat2 rot1 = mat2(cos(a1), sin(a1), -sin(a1), cos(a1));
    mat2 rot2 = mat2(cos(a2), sin(a2), -sin(a2), cos(a2));
    dir.xz *= rot1;
    dir.xy *= rot2;
    
    vec3 from = vec3(1.0, 0.5, 0.5);
    from += vec3(time * 2.0, time, -2.0);
    from.xz *= rot1;
    from.xy *= rot2;
    
    float s = 0.1;
    float fade = 1.0;
    vec3 v = vec3(0.0);
    
    for (int r = 0; r < volsteps; r++) {
        vec3 p = from + s * dir * 10.5;
        p = abs(vec3(tile) - mod(p, vec3(tile * 2.0)));
        
        float pa = 0.0;
        float a = 0.0;
        for (int i = 0; i < iterations; i++) { 
            p = abs(p) / dot(p, p) - formuparam;
            a += abs(length(p) - pa);
            pa = length(p);
        }
        
        float dm = max(0.0, darkmatter - a * a * 0.1);
        a *= a * a;
        
        fade *= 1.0 - dm;
        v += fade;
        v += vec3(s, s * s, s * s * s) * a * brightness * fade;
        fade *= distfading;
        s += stepsize;
    }
    
    v = mix(vec3(length(v)), v, saturation);
    vec3 finalColor = v * 0.01;
    
    // NEW TRON ELEMENTS
    vec2 centered_uv = v_uv - 0.5;
    centered_uv.y *= u_resolution.y / u_resolution.x;
    
    // Add subtle grid
    float gridPattern = grid(centered_uv * 5.0);
    finalColor += LINE_COLOR * gridPattern * GRID_GLOW * 0.3;
    
    // Add horizontal scanlines
    float scan = scanlines(v_uv, u_time);
    finalColor += vec3(0.0, 0.3, 0.6) * scan * 0.15;
    
    // Add vertical accent lines (sparse)
    float verticalLines = smoothstep(0.002, 0.0, abs(fract(v_uv.x * 8.0) - 0.5));
    finalColor += LINE_COLOR * verticalLines * 0.2 * sin(u_time * 0.5 + v_uv.x * 10.0) * 0.5;
    
    // ANIMATED SHAPES - Multiple spinning hexagons
   /* finalColor += spinningHexagon(centered_uv, vec2(-0.3, 0.2), 0.08, u_time, 1.0);
    finalColor += spinningHexagon(centered_uv, vec2(0.35, -0.15), 0.06, u_time, -1.5);
    finalColor += spinningHexagon(centered_uv, vec2(-0.25, -0.25), 0.05, u_time, 0.8);
    finalColor += spinningHexagon(centered_uv, vec2(0.2, 0.25), 0.07, u_time, -1.2);
    */
    // CENTER PINWHEEL (replaces rotating ring)
 /*   finalColor += pinwheel(centered_uv, vec2(0.0, 0.0), 0.04, u_time, 0.6);
 
vec3 w_symbol = letter_W(uv, vec2(-0.2, 0.0), -0.05); // position at (0.5, 0.3), size 0.15
finalColor += w_symbol;

vec3 a_symbol = letter_A(uv, vec2(-0.15, 0.0), -0.05);
finalColor += a_symbol;
 
// Add this where you want to render the Y symbol
vec3 y_symbol = letter_Y(uv, vec2(-0.1,  0.0), -0.05);// position at (0.9, 0.3), size 0.15
finalColor += y_symbol;

vec3 f_symbol = letter_F(uv,  vec2(0.1,  0.0), -0.05);
finalColor += f_symbol;

vec3 i_symbol = letter_I(uv,  vec2(0.15,  0.0), -0.05);
finalColor += i_symbol;

vec3 r_symbol = letter_R(uv, vec2(0.2,  0.0), -0.05);
finalColor += r_symbol;

vec3 e_symbol = letter_E(uv, vec2(0.25,  0.0), -0.05);
finalColor += e_symbol;
    */

    // Rotating triangles
//    finalColor += rotatingTriangle(centered_uv, vec2(-0.35, -0.05), 0.06, u_time, -1.0);
//    finalColor += rotatingTriangle(centered_uv, vec2(0.15, -0.3), 0.05, u_time, 1.3);
    
    // Pulsing hexagons
//    finalColor += pulsingHexagon(centered_uv, vec2(-0.15, 0.15), 0.055, u_time, 2.0);
  //  finalColor += pulsingHexagon(centered_uv, vec2(0.3, 0.0), 0.045, u_time, 2.5);
    
    // Orbiting small shapes around center
   // finalColor += orbitingShapes(centered_uv, vec2(0.0, 0.0), u_time);
    
    // TRON TRAVELING LINES OVERLAY (Original)
    float edge_dist = distanceToEdge(v_uv, u_resolution);
    
    if (edge_dist < 0.05 && edge_dist > 0.0) {
        float perimeter_pos = uvToPerimeter(v_uv, u_resolution);
        float perimeter_length = 2.0 * (1.0 + (u_resolution.y - 2.0 * EDGE_MARGIN) / (u_resolution.x - 2.0 * EDGE_MARGIN));
        float spacing = perimeter_length / float(NUM_LINES);
        
        float edge_falloff = smoothstep(0.05, 0.0, edge_dist);
        float sharp_line = smoothstep(LINE_WIDTH * 2.0, 0.0, edge_dist);
        float soft_glow = smoothstep(GLOW_WIDTH, 0.0, edge_dist);
        
        for (int i = 0; i < NUM_LINES; i++) {
            float line_pos = mod(u_time * ANIMATION_SPEED + float(i) * spacing, perimeter_length);
            float dist_to_line = abs(perimeter_pos * perimeter_length - line_pos);
            dist_to_line = min(dist_to_line, abs(dist_to_line - perimeter_length));
            
            float line_intensity = smoothstep(LINE_LENGTH, 0.0, dist_to_line);
            float trail_intensity = smoothstep(LINE_LENGTH * 2.0, LINE_LENGTH, dist_to_line) * 0.3;
            line_intensity = max(line_intensity, trail_intensity);
            
            vec3 glow_color = LINE_COLOR * 0.6;
            
            finalColor += LINE_COLOR * sharp_line * line_intensity * edge_falloff * 2.0;
            finalColor += glow_color * soft_glow * line_intensity * edge_falloff;
        }
        
        finalColor += vec3(0.3, 0.4, 0.5) * edge_falloff * BASE_EDGE_GLOW * 0.3;
    }
    
    gl_FragColor = vec4(finalColor, 1.0);
}
)";


static const char *cursor_vertex_shader = R"(
#version 100
attribute vec2 position;
uniform mat4 mvp;
varying vec2 v_pos;

void main() {
    v_pos = position;
    gl_Position = mvp * vec4(position.x, position.y, 0.001, 1.0);
}
)";

static const char *cursor_fragment_shader = R"(
#version 100
precision mediump float;
varying vec2 v_pos;
uniform float u_time;
uniform vec3 u_color;

void main() {
    float dist = length(v_pos);
    
    // Create a ring with animated pulse
    float ring_outer = 0.08;
    float ring_inner = 0.06;
    float pulse = sin(u_time * 4.0) * 0.01 + 0.07;
    
    // Ring alpha
    float ring_alpha = smoothstep(ring_outer, ring_inner, dist) * 
                      smoothstep(pulse - 0.02, pulse, dist);
    
    // Center dot
    float dot_alpha = smoothstep(0.02, 0.01, dist);
    
    float alpha = max(ring_alpha, dot_alpha);
    
    // Bright color with glow
    vec3 color = u_color * (1.0 + sin(u_time * 3.0) * 0.3);
    
    gl_FragColor = vec4(color, alpha * 0.8);
}
)";
class wayfire_cube : public wf::per_output_plugin_instance_t, public wf::pointer_interaction_t
{
        // Window dragging state
        bool is_dragging_window = false;
        wayfire_toplevel_view dragged_view = nullptr;
        wf::pointf_t drag_start_cursor;
        wf::point_t drag_start_workspace;
        wf::pointf_t last_cursor_pos;
        bool cursor_hidden_by_cube = false;
       
        // Flag to track if workspace was already set (e.g., by right-click)
        bool workspace_already_set = false;
wf::geometry_t dragged_window_original_geometry;
bool is_moving_within_desktop = false;
wf::pointf_t drag_offset; // Offset from window position to cursor when drag started
glm::vec3 drag_start_3d_pos;
int dragged_window_face_index = -1;
struct CursorIndicator {
    bool active = false;
    glm::vec3 world_position;
    glm::vec2 uv_position;
    int workspace_x = -1;
    int workspace_y = -1;
    float timestamp = 0.0f;
} cursor_indicator;
GLuint cursor_vbo = 0; // VBO for cursor circle
GLuint trail_vbo = 0;
glm::vec3 virtual_ray_hit_pos = {0.0f, 0.0f, 0.0f};
bool has_virtual_hit = false;
int hit_workspace_x = -1; // Which workspace (horizontal index) is at the raycast point
int hit_workspace_y = -1; // Which workspace row is at the raycast point
float plane_z = 0.0f;
// Helper function to check if a workspace has any visible windows or window parts on it
bool workspace_has_windows(int ws_x, int ws_y) {
    auto ws_set = output->wset();
    auto views = ws_set->get_views();
    auto og = output->get_layout_geometry();
   
    // Calculate this workspace's bounding box
    wf::geometry_t ws_geometry = {
        ws_x * og.width,
        ws_y * og.height,
        og.width,
        og.height
    };
   
    for (auto& view : views) {
        if (!view->is_mapped()) {
            continue;
        }
       
        auto tview = wf::toplevel_cast(view);
        if (!tview) {
            continue;
        }
       
        // Get the window's geometry
        auto win_geom = tview->get_geometry();
       
        // Check if the window overlaps with this workspace at all
        if (win_geom.x < ws_geometry.x + ws_geometry.width &&
            win_geom.x + win_geom.width > ws_geometry.x &&
            win_geom.y < ws_geometry.y + ws_geometry.height &&
            win_geom.y + win_geom.height > ws_geometry.y) {
            return true; // Window overlaps with this workspace
        }
    }
   
    return false; // No windows overlap this workspace
}
      wf::animation::simple_animation_t popout_scale_animation{wf::create_option<int>(300)}; // 0.3 second
    class cube_render_node_t : public wf::scene::node_t
    {
// Custom node that filters to show only windows (no background/desktop)
class windows_only_workspace_node_t : public wf::scene::node_t
{
    wf::output_t *output;
    wf::point_t workspace;
   
  public:
  
    windows_only_workspace_node_t(wf::output_t *output, wf::point_t ws) : node_t(false)
    {
        this->output = output;
        this->workspace = ws;
    }
   
    void gen_render_instances(std::vector<wf::scene::render_instance_uptr>& instances,
        wf::scene::damage_callback push_damage, wf::output_t *shown_on) override
    {
        if (shown_on != output)
        {
            return;
        }
       
        // Get all views and filter by workspace
        auto views = output->wset()->get_views();
        auto og = output->get_layout_geometry();
       
        // Calculate this workspace's screen position
        wf::geometry_t ws_geom{
            og.x + workspace.x * og.width,
            og.y + workspace.y * og.height,
            og.width,
            og.height
        };
       
        int view_count = 0;
        for (auto& view : views)
        {
            if (!view->is_mapped())
            {
                continue;
            }
           
            // Check if view is on our workspace OR overlaps into it
            auto view_ws = output->wset()->get_view_main_workspace(view);
            auto view_geom = view->get_geometry();
           
            // Check for rectangle intersection
            bool overlaps_workspace = !(view_geom.x + view_geom.width <= ws_geom.x ||
                                       view_geom.x >= ws_geom.x + ws_geom.width ||
                                       view_geom.y + view_geom.height <= ws_geom.y ||
                                       view_geom.y >= ws_geom.y + ws_geom.height);
           
            bool is_on_workspace = (view_ws == workspace);
           
            // Include if: window is assigned to this workspace OR window geometry overlaps this workspace
            if (!is_on_workspace && !overlaps_workspace)
            {
                continue;
            }
           
            view_count++;
        // LOGI("Generating render instances for view on workspace ", workspace.x, ",", workspace.y);
           
            // Use root node which includes decorations
            auto view_node = view->get_root_node();
            if (view_node)
            {
                size_t before = instances.size();
                view_node->gen_render_instances(instances, push_damage, shown_on);
      // LOGI("Generated ", instances.size() - before, " render instances");
            }
        }
       
    // LOGI("Total views on workspace ", workspace.x, ",", workspace.y, ": ", view_count);
    }
   
    wf::geometry_t get_bounding_box() override
    {
        return output->get_layout_geometry();
    }
};
// Custom node that shows only desktop/background (no windows)
class desktop_only_workspace_node_t : public wf::scene::node_t
{
    wf::output_t *output;
    wf::point_t workspace;
   
  public:
    desktop_only_workspace_node_t(wf::output_t *output, wf::point_t ws) : node_t(false)
    {
        this->output = output;
        this->workspace = ws;
    }
   
    void gen_render_instances(std::vector<wf::scene::render_instance_uptr>& instances,
        wf::scene::damage_callback push_damage, wf::output_t *shown_on) override
    {
        if (shown_on != output)
        {
            return;
        }
       
        // Get the workspace scene graph
        auto wset = output->wset();
        auto views = wset->get_views();
       
        // We want to render everything EXCEPT window views
        // This means rendering background layers only
        auto root = output->node_for_layer(wf::scene::layer::BACKGROUND);
        if (root)
        {
            root->gen_render_instances(instances, push_damage, shown_on);
        }
       
        auto bottom = output->node_for_layer(wf::scene::layer::BOTTOM);
        if (bottom)
        {
            bottom->gen_render_instances(instances, push_damage, shown_on);
        }
    }
   
    wf::geometry_t get_bounding_box() override
    {
        return output->get_layout_geometry();
    }
};
class cube_render_instance_t : public wf::scene::render_instance_t
{
    std::shared_ptr<cube_render_node_t> self;
    wf::scene::damage_callback push_damage;
    std::vector<std::vector<wf::scene::render_instance_uptr>> ws_instances;
    std::vector<wf::region_t> ws_damage;
    std::vector<wf::auxilliary_buffer_t> framebuffers;
    // NEW: Framebuffers for window-only cubes
    std::vector<wf::auxilliary_buffer_t> framebuffers_windows;
    std::vector<std::vector<wf::auxilliary_buffer_t>> framebuffers_windows_rows;
   
// std::vector<std::vector<wf::scene::render_instance_uptr>> ws_instances_windows;
 // std::vector<std::vector<std::vector<wf::scene::render_instance_uptr>>> ws_instances_windows_rows;
// std::vector<wf::region_t> ws_damage_windows;
// std::vector<std::vector<wf::region_t>> ws_damage_windows_rows;
// continuous window updates
std::vector<std::unique_ptr<wf::scene::render_instance_manager_t>> ws_instance_managers_windows;
std::vector<std::vector<std::unique_ptr<wf::scene::render_instance_manager_t>>> ws_instance_managers_windows_rows;
    // Multiple cube workspaces for all rows
    std::vector<std::vector<std::vector<wf::scene::render_instance_uptr>>> ws_instances_rows;
    std::vector<std::vector<wf::region_t>> ws_damage_rows;
    std::vector<std::vector<wf::auxilliary_buffer_t>> framebuffers_rows;
    std::vector<wf::region_t> ws_damage_windows;
    std::vector<std::vector<wf::region_t>> ws_damage_windows_rows;
// FIXED: Dirty flags for conditional realloc
        std::vector<bool> window_dirty;
        std::vector<std::vector<bool>> window_rows_dirty;
        // FIXED: Pending allocation flags to defer alloc until valid geometry
        std::vector<bool> pending_alloc_desktop;
        std::vector<bool> pending_alloc_windows;
        std::vector<std::vector<bool>> pending_alloc_windows_rows;
        std::vector<std::vector<bool>> pending_alloc_desktop_rows;
    wf::signal::connection_t<wf::scene::node_damage_signal> on_cube_damage =
        [=] (wf::scene::node_damage_signal *ev)
    {
        push_damage(ev->region);
    };
public:
        // FIXED: Constructor - No allocations here; just resize vectors and set pending flags
        // This avoids segfault on invalid geometry during init
        cube_render_instance_t(cube_render_node_t *self, wf::scene::damage_callback push_damage)
        {
            this->self = std::dynamic_pointer_cast<cube_render_node_t>(self->shared_from_this());
            this->push_damage = push_damage;
            self->connect(&on_cube_damage);
           
            ws_damage.resize(self->workspaces.size());
            framebuffers.resize(self->workspaces.size());
            ws_instances.resize(self->workspaces.size());
           
            // Initialize storage for all rows
            int num_rows = self->workspaces_all_rows.size();
            ws_damage_rows.resize(num_rows);
            framebuffers_rows.resize(num_rows);
            ws_instances_rows.resize(num_rows);
           
            // IMPORTANT: Resize window storage BEFORE creating managers
            ws_damage_windows.resize(self->workspaces_windows.size());
            framebuffers_windows.resize(self->workspaces_windows.size());
            ws_instance_managers_windows.resize(self->workspaces_windows.size());
           
            ws_damage_windows_rows.resize(num_rows);
            framebuffers_windows_rows.resize(num_rows);
            ws_instance_managers_windows_rows.resize(num_rows);
           
            for (int row = 0; row < num_rows; row++)
            {
                ws_damage_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
                framebuffers_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
                ws_instance_managers_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
            }
           
            // FIXED: No allocations - set pending flags to true for all
            auto bbox = self->cube->output->get_layout_geometry();
            const float scale = self->cube->output->handle->scale;
           
            // Top row desktops - pending
            pending_alloc_desktop.assign(self->workspaces.size(), bbox.width > 0 && bbox.height > 0);
           
            // All rows desktops - pending
            pending_alloc_desktop_rows.resize(num_rows);
            for (int row = 0; row < num_rows; ++row) {
                pending_alloc_desktop_rows[row].assign(self->workspaces_all_rows[row].size(), bbox.width > 0 && bbox.height > 0);
            }
           
            // Window buffers - top row pending
            pending_alloc_windows.assign(self->workspaces_windows.size(), bbox.width > 0 && bbox.height > 0);
           
            // Window buffers - all rows pending
            pending_alloc_windows_rows.resize(num_rows);
            for (int row = 0; row < num_rows; ++row) {
                pending_alloc_windows_rows[row].assign(self->workspaces_windows_rows[row].size(), bbox.width > 0 && bbox.height > 0);
            }
           
            // FIXED: Dirty flags init
            window_dirty.assign(self->workspaces_windows.size(), false);
            window_rows_dirty.resize(num_rows);
            for (int row = 0; row < num_rows; ++row) {
                window_rows_dirty[row].assign(self->workspaces_windows_rows[row].size(), false);
            }
   
    for (int row = 0; row < num_rows; row++)
    {
        ws_damage_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
        framebuffers_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
        ws_instance_managers_windows_rows[row].resize(self->workspaces_windows_rows[row].size());
    }
   
    // Initialize top cube workspaces (current row)
    for (int i = 0; i < (int)self->workspaces.size(); i++)
    {
        auto push_damage_child = [=] (const wf::region_t& damage)
        {
            ws_damage[i] |= damage;
            push_damage(self->get_bounding_box());
        };
       
        self->workspaces[i]->gen_render_instances(ws_instances[i],
            push_damage_child, self->cube->output);
       
        ws_damage[i] |= self->workspaces[i]->get_bounding_box();
    }
   
    // NOW create window managers after everything is resized
    for (int i = 0; i < (int)self->workspaces_windows.size(); i++)
    {
        auto push_damage_child = [this, i] (const wf::region_t& damage)
        {
            this->ws_damage_windows[i] |= damage;
            this->push_damage(this->self->get_bounding_box());
        };
       
        std::vector<wf::scene::node_ptr> nodes;
        nodes.push_back(self->workspaces_windows[i]);
       
        ws_instance_managers_windows[i] = std::make_unique<wf::scene::render_instance_manager_t>(
            nodes, push_damage_child, self->cube->output);
       
        const int BIG_NUMBER = 1e5;
        wf::region_t big_region = wf::geometry_t{-BIG_NUMBER, -BIG_NUMBER, 2 * BIG_NUMBER, 2 * BIG_NUMBER};
        ws_instance_managers_windows[i]->set_visibility_region(big_region);
       
        ws_damage_windows[i] |= self->workspaces_windows[i]->get_bounding_box();
    }
   
    // Initialize all other row workspaces
    for (int row = 0; row < num_rows; row++)
    {
        ws_damage_rows[row].resize(self->workspaces_all_rows[row].size());
        framebuffers_rows[row].resize(self->workspaces_all_rows[row].size());
        ws_instances_rows[row].resize(self->workspaces_all_rows[row].size());
       
        for (int i = 0; i < (int)self->workspaces_all_rows[row].size(); i++)
        {
            auto push_damage_child = [=] (const wf::region_t& damage)
            {
                ws_damage_rows[row][i] |= damage;
                push_damage(self->get_bounding_box());
            };
           
            self->workspaces_all_rows[row][i]->gen_render_instances(ws_instances_rows[row][i],
                push_damage_child, self->cube->output);
           
            ws_damage_rows[row][i] |= self->workspaces_all_rows[row][i]->get_bounding_box();
        }
       
        // Create window managers for this row
        for (int i = 0; i < (int)self->workspaces_windows_rows[row].size(); i++)
        {
            auto push_damage_child = [this, row, i] (const wf::region_t& damage)
            {
                this->ws_damage_windows_rows[row][i] |= damage;
                this->push_damage(this->self->get_bounding_box());
            };
           
            std::vector<wf::scene::node_ptr> nodes;
            nodes.push_back(self->workspaces_windows_rows[row][i]);
           
            ws_instance_managers_windows_rows[row][i] =
                std::make_unique<wf::scene::render_instance_manager_t>(
                    nodes, push_damage_child, self->cube->output);
           
            const int BIG_NUMBER = 1e5;
            wf::region_t big_region = wf::geometry_t{-BIG_NUMBER, -BIG_NUMBER, 2 * BIG_NUMBER, 2 * BIG_NUMBER};
            ws_instance_managers_windows_rows[row][i]->set_visibility_region(big_region);
           
            ws_damage_windows_rows[row][i] |=
                self->workspaces_windows_rows[row][i]->get_bounding_box();
        }
    }
}
~cube_render_instance_t()
        {
            // Explicitly free buffers on destruction
            for (auto& fb : framebuffers) fb.free();
            for (auto& row : framebuffers_rows) {
                for (auto& fb : row) fb.free();
            }
            for (auto& fb : framebuffers_windows) fb.free();
            for (auto& row : framebuffers_windows_rows) {
                for (auto& fb : row) fb.free();
            }
        }
      
void schedule_instructions(
    std::vector<wf::scene::render_instruction_t>& instructions,
    const wf::render_target_t& target, wf::region_t& damage) override
{
 
auto bbox = self->get_bounding_box();
            damage ^= bbox;
            const float scale = self->cube->output->handle->scale;
            bool resize_dirty = false; // TODO: Set from output resize signal
    instructions.push_back(wf::scene::render_instruction_t{
        .instance = this,
        .target = target.translated(-wf::origin(self->get_bounding_box())),
        .damage = damage & self->get_bounding_box(),
    });
    // Render top cube workspaces (current row) - WITH BACKGROUND
    for (int i = 0; i < (int)ws_instances.size(); i++)
    {
        const float scale = self->cube->output->handle->scale;
        auto bbox = self->workspaces[i]->get_bounding_box();
        framebuffers[i].allocate(wf::dimensions(bbox), scale);
        wf::render_target_t target{framebuffers[i]};
        target.geometry = self->workspaces[i]->get_bounding_box();
        target.scale = self->cube->output->handle->scale;
        wf::render_pass_params_t params;
        params.instances = &ws_instances[i];
        params.damage = ws_damage[i];
        params.reference_output = self->cube->output;
        params.target = target;
        params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
        wf::render_pass_t::run(params);
        ws_damage[i].clear();
    }
   
    // Render window-only workspaces dynamically (top row)
   // In schedule_instructions, for the window rendering:
// In schedule_instructions, fix the render target geometry:
// In schedule_instructions, for window rendering:
for (int i = 0; i < (int)ws_instance_managers_windows.size(); i++)
{
    const float scale = self->cube->output->handle->scale;
    auto bbox = self->cube->output->get_layout_geometry();
    framebuffers_windows[i].allocate(wf::dimensions(bbox), scale);
    // Calculate which workspace this represents
    auto cws = self->cube->output->wset()->get_current_workspace();
    auto grid = self->cube->output->wset()->get_workspace_grid_size();
    wf::point_t target_ws = {(cws.x + i) % grid.width, cws.y};
    wf::render_target_t fb_target{framebuffers_windows[i]};
    fb_target.geometry = wf::geometry_t{
        bbox.x + target_ws.x * bbox.width,
        bbox.y + target_ws.y * bbox.height,
        bbox.width,
        bbox.height
    };
    fb_target.scale = scale;
    // CRITICAL FIX: Generate fresh instances during drag to capture window movement
    std::vector<wf::scene::render_instance_uptr> fresh_instances;
    std::vector<wf::scene::render_instance_uptr>* instances_ptr;
   
 // if (self->cube->is_dragging_window)
    {
        // Regenerate instances manually to capture current window positions
        auto dummy_damage = [](wf::region_t){};
        self->regenerate_workspace_instances(i, fresh_instances, dummy_damage);
        instances_ptr = &fresh_instances;
    }
 // else
 // {
        // Use cached instances when not dragging
 // instances_ptr = &ws_instance_managers_windows[i]->get_instances();
 // }
   
    // Force full damage to ensure complete redraw
    wf::region_t full_damage = fb_target.geometry;
   
    wf::render_pass_params_t params;
    params.instances = instances_ptr;
    params.damage = full_damage;
    params.reference_output = self->cube->output;
    params.target = fb_target;
    params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
    wf::render_pass_t::run(params);
    ws_damage_windows[i].clear();
}
    // Render all other row workspaces - WITH BACKGROUND
    for (int row = 0; row < (int)ws_instances_rows.size(); row++)
    {
        for (int i = 0; i < (int)ws_instances_rows[row].size(); i++)
        {
            const float scale = self->cube->output->handle->scale;
            auto bbox = self->workspaces_all_rows[row][i]->get_bounding_box();
            framebuffers_rows[row][i].allocate(wf::dimensions(bbox), scale);
            wf::render_target_t target{framebuffers_rows[row][i]};
            target.geometry = self->workspaces_all_rows[row][i]->get_bounding_box();
            target.scale = self->cube->output->handle->scale;
            wf::render_pass_params_t params;
            params.instances = &ws_instances_rows[row][i];
            params.damage = ws_damage_rows[row][i];
            params.reference_output = self->cube->output;
            params.target = target;
            params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
            wf::render_pass_t::run(params);
            ws_damage_rows[row][i].clear();
        }
    }
   
    // Render window-only workspaces dynamically (other rows)
for (int row = 0; row < (int)ws_instance_managers_windows_rows.size(); row++)
{
    for (int i = 0; i < (int)ws_instance_managers_windows_rows[row].size(); i++)
    {
        const float scale = self->cube->output->handle->scale;
        auto bbox = self->cube->output->get_layout_geometry();
        framebuffers_windows_rows[row][i].allocate(wf::dimensions(bbox), scale);
        auto cws = self->cube->output->wset()->get_current_workspace();
        auto grid = self->cube->output->wset()->get_workspace_grid_size();
        int target_y = (cws.y + row + 1) % grid.height;
        wf::point_t target_ws = {(cws.x + i) % grid.width, target_y};
        wf::render_target_t fb_target{framebuffers_windows_rows[row][i]};
        fb_target.geometry = wf::geometry_t{
            bbox.x + target_ws.x * bbox.width,
            bbox.y + target_ws.y * bbox.height,
            bbox.width,
            bbox.height
        };
        fb_target.scale = scale;
        // CRITICAL FIX: Generate fresh instances during drag to capture window movement
        std::vector<wf::scene::render_instance_uptr> fresh_instances;
        std::vector<wf::scene::render_instance_uptr>* instances_ptr;
       
       // if (self->cube->is_dragging_window)
        {
            // Regenerate instances manually to capture current window positions
            auto dummy_damage = [](wf::region_t){};
            self->regenerate_workspace_instances_row(row, i, fresh_instances, dummy_damage);
            instances_ptr = &fresh_instances;
        }
      // else
      // {
            // Use cached instances when not dragging
      // instances_ptr = &ws_instance_managers_windows_rows[row][i]->get_instances();
      // }
       
        // Force full damage to ensure complete redraw
        wf::region_t full_damage = fb_target.geometry;
       
        wf::render_pass_params_t params;
        params.instances = instances_ptr;
        params.damage = full_damage;
        params.reference_output = self->cube->output;
        params.target = fb_target;
        params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
        wf::render_pass_t::run(params);
        ws_damage_windows_rows[row][i].clear();
    }
}
}
// NEW: Helper to render a view to a buffer
void render_view_to_buffer(wayfire_view view, wf::auxilliary_buffer_t& buffer)
{
    auto toplevel = wf::toplevel_cast(view);
    if (!toplevel)
    {
        return; // Only works for toplevel views
    }
   
    auto vg = toplevel->get_geometry();
    buffer.allocate(wf::dimensions(vg), 1.0f);
   
    // Create render instance manager for this view
    std::vector<wf::scene::node_ptr> nodes;
    nodes.push_back(view->get_root_node());
   
    auto push_damage_dummy = [=] (wf::region_t) {};
   
    wf::scene::render_instance_manager_t instance_manager(
        nodes, push_damage_dummy, self->cube->output);
   
    instance_manager.set_visibility_region(vg);
   
    // Render the view
    wf::render_target_t target{buffer};
    target.geometry = vg;
    target.scale = 1.0f;
   
    std::vector<wf::scene::render_instance_uptr> instances;
    wf::region_t damage;
    damage |= vg; // Add geometry to damage region
   
    for (auto& node : nodes)
    {
        node->gen_render_instances(instances, push_damage_dummy, self->cube->output);
    }
   
    wf::render_pass_params_t params;
    params.instances = &instances;
    params.damage = damage;
    params.reference_output = self->cube->output;
    params.target = target;
    params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
   
    wf::render_pass_t::run(params);
}
void render(const wf::scene::render_instruction_t& data) override
{
    self->cube->render(data, framebuffers, framebuffers_rows,
                      framebuffers_windows, framebuffers_windows_rows);
}
    void compute_visibility(wf::output_t *output, wf::region_t& visible) override
    {
        for (int i = 0; i < (int)self->workspaces.size(); i++)
        {
            wf::region_t ws_region = self->workspaces[i]->get_bounding_box();
            for (auto& ch : this->ws_instances[i])
            {
                ch->compute_visibility(output, ws_region);
            }
        }
       
        // NEW: Compute visibility for window-only top row
for (int i = 0; i < (int)ws_instance_managers_windows.size(); i++)
{
    wf::region_t ws_region = self->workspaces_windows[i]->get_bounding_box();
    for (auto& ch : ws_instance_managers_windows[i]->get_instances())
    {
        ch->compute_visibility(output, ws_region);
    }
}
       
       for (int row = 0; row < (int)ws_instance_managers_windows_rows.size(); row++)
{
    for (int i = 0; i < (int)ws_instance_managers_windows_rows[row].size(); i++)
    {
        wf::region_t ws_region = self->workspaces_windows_rows[row][i]->get_bounding_box();
        for (auto& ch : ws_instance_managers_windows_rows[row][i]->get_instances())
        {
            ch->compute_visibility(output, ws_region);
        }
    }
}
       
        // NEW: Compute visibility for window-only other rows
        for (int row = 0; row < (int)ws_instance_managers_windows_rows.size(); row++)
{
    for (int i = 0; i < (int)ws_instance_managers_windows_rows[row].size(); i++)
    {
        const float scale = self->cube->output->handle->scale;
        auto bbox = self->cube->output->get_layout_geometry();
        framebuffers_windows_rows[row][i].allocate(wf::dimensions(bbox), scale);
        wf::render_target_t fb_target{framebuffers_windows_rows[row][i]};
        fb_target.geometry = bbox;
        fb_target.scale = scale;
        auto& instances = ws_instance_managers_windows_rows[row][i]->get_instances();
       
        wf::render_pass_params_t params;
        params.instances = &instances;
        params.damage = ws_damage_windows_rows[row][i];
        params.reference_output = self->cube->output;
        params.target = fb_target;
        params.flags = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;
        wf::render_pass_t::run(params);
        ws_damage_windows_rows[row][i].clear();
    }
}
    }
};
      public:
cube_render_node_t(wayfire_cube *cube) : node_t(false)
{
    this->cube = cube;
    auto w = cube->output->wset()->get_workspace_grid_size().width;
    auto h = cube->output->wset()->get_workspace_grid_size().height;
    auto y = cube->output->wset()->get_current_workspace().y;
   
    // Top cube - current row
    for (int i = 0; i < w; i++)
    {
        // CHANGED: Use desktop-only for regular cube
        auto node = std::make_shared<desktop_only_workspace_node_t>(cube->output, wf::point_t{i, y});
        workspaces.push_back(node);
       
        // Window-only for popout cube
        auto node_windows = std::make_shared<windows_only_workspace_node_t>(cube->output, wf::point_t{i, y});
        workspaces_windows.push_back(node_windows);
    }
   
    // All other rows
    for (int row_offset = 1; row_offset < h; row_offset++)
    {
        int target_y = (y + row_offset) % h;
        std::vector<std::shared_ptr<wf::scene::node_t>> row_workspaces;
        std::vector<std::shared_ptr<wf::scene::node_t>> row_workspaces_windows;
       
        for (int i = 0; i < w; i++)
        {
            // CHANGED: Desktop-only for regular cube
            auto node = std::make_shared<desktop_only_workspace_node_t>(cube->output, wf::point_t{i, target_y});
            row_workspaces.push_back(node);
           
            // Window-only for popout
            auto node_windows = std::make_shared<windows_only_workspace_node_t>(cube->output, wf::point_t{i, target_y});
            row_workspaces_windows.push_back(node_windows);
        }
       
        workspaces_all_rows.push_back(row_workspaces);
        workspaces_windows_rows.push_back(row_workspaces_windows);
    }
}
        virtual void gen_render_instances(
            std::vector<wf::scene::render_instance_uptr>& instances,
            wf::scene::damage_callback push_damage, wf::output_t *shown_on)
        {
            if (shown_on != this->cube->output)
            {
                return;
            }
            instances.push_back(std::make_unique<cube_render_instance_t>(
                this, push_damage));
        }
        wf::geometry_t get_bounding_box()
        {
            return cube->output->get_layout_geometry();
        }
            // ADD THE METHOD HERE, BEFORE "private:"
    void damage_all_workspace_windows()
    {
        for (auto& ws_node : workspaces_windows)
        {
            wf::scene::damage_node(ws_node, ws_node->get_bounding_box());
        }
       
        for (auto& row : workspaces_windows_rows)
        {
            for (auto& ws_node : row)
            {
                wf::scene::damage_node(ws_node, ws_node->get_bounding_box());
            }
        }
    }
        // Manually regenerate instances for a specific workspace
    void regenerate_workspace_instances(int workspace_index,
                                       std::vector<wf::scene::render_instance_uptr>& instances,
                                       wf::scene::damage_callback push_damage)
    {
        if (workspace_index >= 0 && workspace_index < workspaces_windows.size())
        {
            workspaces_windows[workspace_index]->gen_render_instances(
                instances, push_damage, cube->output);
        }
    }
   
    // Regenerate instances for workspace in a specific row
    void regenerate_workspace_instances_row(int row, int workspace_index,
                                           std::vector<wf::scene::render_instance_uptr>& instances,
                                           wf::scene::damage_callback push_damage)
    {
        if (row >= 0 && row < workspaces_windows_rows.size() &&
            workspace_index >= 0 && workspace_index < workspaces_windows_rows[row].size())
        {
            workspaces_windows_rows[row][workspace_index]->gen_render_instances(
                instances, push_damage, cube->output);
        }
    }
private:
    std::vector<std::shared_ptr<wf::scene::node_t>> workspaces; // Changed type
    std::vector<std::vector<std::shared_ptr<wf::scene::node_t>>> workspaces_all_rows; // Changed type
   
    std::vector<std::shared_ptr<wf::scene::node_t>> workspaces_windows;
    std::vector<std::vector<std::shared_ptr<wf::scene::node_t>>> workspaces_windows_rows;
   
    wayfire_cube *cube;
    };
    std::unique_ptr<wf::input_grab_t> input_grab;
    std::shared_ptr<cube_render_node_t> render_node;
        // Window dragging support
    wayfire_toplevel_view grabbed_view = nullptr;
    wf::point_t grab_position;
    wf::pointf_t relative_grab_position;
    wf::option_wrapper_t<double> XVelocity{"vertical_expo/speed_spin_horiz"},
    YVelocity{"vertical_expo/speed_spin_vert"}, ZVelocity{"vertical_expo/speed_zoom"};
    wf::option_wrapper_t<double> zoom_opt{"vertical_expo/zoom"};
// wf::option_wrapper_t<bool> enable_window_popout{"vertical_expo/enable_window_popout"};
 // wf::option_wrapper_t<double> popout_scale{"vertical_expo/popout_scale"}; // e.g., 1.15 = 15% larger
  // wf::option_wrapper_t<double> popout_opacity{"vertical_expo/popout_opacity"}; // 0.0 to 1.0
    OpenGL::program_t cap_program; // Separate program for caps
  // wf::option_wrapper_t<bool> enable_caps{"vertical_expo/enable_caps"};
   // wf::option_wrapper_t<double> cap_alpha{"vertical_expo/cap_alpha"};
  // wf::option_wrapper_t<wf::color_t> cap_color_top{"vertical_expo/cap_color_top"};
  // wf::option_wrapper_t<wf::color_t> cap_color_bottom{"vertical_expo/cap_color_bottom"};
  // wf::option_wrapper_t<std::string> cap_texture_top{"vertical_expo/cap_texture_top"};
  // wf::option_wrapper_t<std::string> cap_texture_bottom{"vertical_expo/cap_texture_bottom"};
    wf::option_wrapper_t<bool> tron{"vertical_expo/tron"};
    wf::option_wrapper_t<bool> star_background{"vertical_expo/star_background"};
    OpenGL::program_t background_program;
    GLuint background_vbo = 0;
    struct TrailPoint {
        glm::vec2 position;
        float timestamp;
        float alpha;
    };
   
    std::vector<TrailPoint> cursor_trail;
    const int MAX_TRAIL_POINTS = 20; // Number of trail points
    const float TRAIL_LIFETIME = 0.3f; // How long trail lasts (seconds)
    const float TRAIL_SPACING = 0.02f; // Minimum distance between points
    // Cap textures/buffers
    wf::auxilliary_buffer_t top_cap_buffer;
    wf::auxilliary_buffer_t bottom_cap_buffer;
   
    // Loaded cap texture images
    GLuint top_cap_texture_id = 0;
    GLuint bottom_cap_texture_id = 0;
    /* the Z camera distance so that (-1, 1) is mapped to the whole screen
     * for the given FOV */
    float identity_z_offset;
    // Camera vertical position for viewing different cube rows
    wf::animation::simple_animation_t camera_y_offset{wf::create_option<int>(300)};
    OpenGL::program_t program;
    OpenGL::program_t window_program;
    OpenGL::program_t cursor_program;
    OpenGL::program_t beam_program; // NEW: Beam shader program
    wf_cube_animation_attribs animation;
    wf::option_wrapper_t<bool> use_light{"vertical_expo/light"};
    wf::option_wrapper_t<int> use_deform{"vertical_expo/deform"};
    std::string last_background_mode;
    std::unique_ptr<wf_cube_background_base> background;
    wf::option_wrapper_t<std::string> background_mode{"vertical_expo/background_mode"};
    void reload_background()
    {
        if (last_background_mode == (std::string)background_mode)
        {
            return;
        }
        last_background_mode = background_mode;
        if (last_background_mode == "simple")
        {
            background = std::make_unique<wf_cube_simple_background>();
        } else if (last_background_mode == "skydome")
        {
            background = std::make_unique<wf_cube_background_skydome>(output);
        } else if (last_background_mode == "cubemap")
        {
            background = std::make_unique<wf_cube_background_cubemap>();
        } else
        {
            LOGE("cube: Unrecognized background mode %s. Using default \"simple\"",
                last_background_mode.c_str());
            background = std::make_unique<wf_cube_simple_background>();
        }
    }
    bool tessellation_support;
    int get_num_faces()
    {
        return output->wset()->get_workspace_grid_size().width;
    }
    wf::plugin_activation_data_t grab_interface{
        .name = "cube",
        .capabilities = wf::CAPABILITY_MANAGE_COMPOSITOR,
        .cancel = [=] () { deactivate(); },
    };
    // NEW: Beam shaders
    const char *beam_vertex_shader = R"(
#version 100
attribute vec3 position;
attribute float distance;  // Distance from beam center (0 to 1)
uniform mat4 mvp;
varying float v_distance;

void main() {
    v_distance = distance;
    gl_Position = mvp * vec4(position, 1.0);
}
)";
    const char *beam_fragment_shader = R"(
#version 100
#ifdef GL_ES
precision mediump float;
#endif

uniform vec3 color;
varying float v_distance;  // Distance from beam center

void main() {
    // Clamp distance to prevent artifacts
    float dist = clamp(v_distance, 0.0, 1.0);
    
    // Create radial falloff from center of beam
    float glow = 1.0 - dist;
    
    // Apply smooth falloff curve for softer glow
    glow = pow(glow, 2.5);
    
    // Add bright core in the center
    float core = 1.0 - smoothstep(0.0, 0.15, dist);
    core = pow(core, 3.0);
    
    // Combine glow with brighter core
    float intensity = glow + core * 2.0;
    
    // Make the color brighter in the center
    vec3 finalColor = color * (1.0 + core * 0.5);
    
    // Clamp final intensity to prevent overflow
    intensity = clamp(intensity, 0.0, 2.0);
    
    gl_FragColor = vec4(finalColor, intensity * 0.8);
}
)";
    // NEW: Get world centers of active workspaces (with windows)
    std::vector<glm::vec3> get_active_workspace_centers() {
        std::vector<glm::vec3> centers;
        auto grid = output->wset()->get_workspace_grid_size();
        auto cws = output->wset()->get_current_workspace();
        for (int y = 0; y < grid.height; ++y) {
            float v_offset = static_cast<float>(y - cws.y) * CUBE_VERTICAL_SPACING;
            for (int x = 0; x < grid.width; ++x) {
                if (workspace_has_windows(x, y)) {
                    auto model = calculate_model_matrix(x, v_offset, 1.0f, false, 0.0f);
                    glm::vec4 local(0.0f, 0.0f, 0.0f, 1.0f);
                    glm::vec4 world = model * local;
                    centers.emplace_back(world.x / world.w, world.y / world.w, world.z / world.w);
                }
            }
        }
        return centers;
    }
  public:
    void init() override
    {
        input_grab = std::make_unique<wf::input_grab_t>("cube", output, nullptr, this, nullptr);
        input_grab->set_wants_raw_input(true);
        animation.cube_animation.offset_y.set(0, 0);
        animation.cube_animation.offset_z.set(0, 0);
        animation.cube_animation.rotation.set(0, 0);
        animation.cube_animation.zoom.set(1, 1);
        animation.cube_animation.ease_deformation.set(0, 0);
        animation.cube_animation.max_tilt.set(0, 0); // Start with no tilt
        LOGI("########## CONSTRUCTOR: max_tilt initialized to 0° ##########");
        animation.cube_animation.start();
       
        camera_y_offset.set(0, 0);
        popout_scale_animation.set(1.0, 1.0);
        reload_background();
        output->connect(&on_cube_control);
        wf::gles::run_in_context([&]
        {
            load_program();
        });
    }
// Helper: Convert screen coordinates to world ray
glm::vec3 screen_to_world_ray(float screen_x, float screen_y, const wf::render_target_t& target)
{
    auto og = target.geometry;
   
    // Normalize screen coordinates to [-1, 1]
    float ndc_x = (2.0f * screen_x) / og.width - 1.0f;
    float ndc_y = 1.0f - (2.0f * screen_y) / og.height; // Flip Y
   
    // Create NDC point
    glm::vec4 ray_clip(ndc_x, ndc_y, -1.0f, 1.0f);
   
    // Get inverse matrices
    auto vp = calculate_vp_matrix(target);
    glm::mat4 inv_vp = glm::inverse(vp);
   
    // Transform to world space
    glm::vec4 ray_world = inv_vp * ray_clip;
    ray_world /= ray_world.w; // Perspective divide
   
    // Return as direction vector
    glm::vec3 ray_origin(0.0f, 0.0f, 0.0f); // Camera at origin
    glm::vec3 ray_dir = glm::normalize(glm::vec3(ray_world) - ray_origin);
   
    return ray_dir;
}
// Helper: Raycast to find which cube face was clicked
int raycast_to_cube_face(glm::vec3 ray_dir, glm::vec3 ray_origin)
{
    float closest_distance = FLT_MAX;
    int closest_face = -1;
   
    auto cws = output->wset()->get_current_workspace();
    int num_faces = get_num_faces();
   
    for (int i = 0; i < num_faces; i++)
    {
        // Get face model matrix
        auto model = calculate_model_matrix(i, 0.0f, 1.0f, false);
       
        // Face center in world space (before model transform)
        glm::vec4 face_center_local(0.0f, 0.0f, 0.0f, 1.0f);
        glm::vec4 face_center_world = model * face_center_local;
       
        // Face normal (pointing toward camera for flat layout)
        glm::vec4 face_normal_local(0.0f, 0.0f, 1.0f, 0.0f);
        glm::vec4 face_normal_world = model * face_normal_local;
        glm::vec3 normal = glm::normalize(glm::vec3(face_normal_world));
       
        // Plane equation: dot(normal, point - center) = 0
        // Ray: point = origin + t * direction
        // Solve: dot(normal, origin + t*dir - center) = 0
       
        float denom = glm::dot(normal, ray_dir);
        if (std::abs(denom) > 1e-6) // Not parallel
        {
            glm::vec3 p0_to_center = glm::vec3(face_center_world) - ray_origin;
            float t = glm::dot(p0_to_center, normal) / denom;
           
            if (t >= 0 && t < closest_distance) // In front of camera
            {
                // Check if intersection point is within face bounds
                glm::vec3 intersection = ray_origin + t * ray_dir;
               
                // Transform intersection to face local coordinates
                glm::mat4 inv_model = glm::inverse(model);
                glm::vec4 local_pos = inv_model * glm::vec4(intersection, 1.0f);
               
                // Face is 1x1 square from -0.5 to 0.5 in X and Y
                if (std::abs(local_pos.x) <= 0.5f && std::abs(local_pos.y) <= 0.5f)
                {
                    closest_distance = t;
                    closest_face = i;
                }
            }
        }
    }
   
    return closest_face;
}
struct Ray
{
    glm::vec3 origin;
    glm::vec3 dir;
};
struct HitInfo
{
    wf::point_t ws = {-1, -1};
    float t = std::numeric_limits<float>::max();
    glm::vec2 local_uv = {0.0f, 0.0f};
    int row_offset = 0; // Relative row offset from current
};
Ray screen_to_world_ray(float screen_x, float screen_y, wf::output_t *output)
{
    auto bbox = output->get_layout_geometry();
    float w = static_cast<float>(bbox.width);
    float h = static_cast<float>(bbox.height);
    float ndc_x = 2.0f * screen_x / w - 1.0f;
    float ndc_y = 1.0f - 2.0f * screen_y / h;
    glm::vec4 clip_near(ndc_x, ndc_y, -1.0f, 1.0f);
    // Current projection and view
    glm::mat4 proj = animation.projection;
    glm::mat4 inv_proj = glm::inverse(proj);
    glm::vec4 eye_near4 = inv_proj * clip_near;
    eye_near4 /= eye_near4.w;
    glm::vec3 eye_dir(eye_near4);
    // Compute centered_scale
    float zoom_factor = animation.cube_animation.zoom;
    auto scale_matrix = glm::scale(glm::mat4(1.0f), glm::vec3(1.0f / zoom_factor));
    auto to_row_center = glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, camera_y_offset, 0.0f));
    auto from_row_center = glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, -camera_y_offset, 0.0f));
    auto centered_scale = from_row_center * scale_matrix * to_row_center;
    // view_cs = animation.view * centered_scale
    glm::mat4 view_cs = animation.view * centered_scale;
    glm::mat4 inv_view_cs = glm::inverse(view_cs);
    // Camera origin in world space
    glm::vec4 eye_pos(0.0f, 0.0f, 0.0f, 1.0f);
    glm::vec4 cam4 = inv_view_cs * eye_pos;
    cam4 /= cam4.w;
    glm::vec3 origin = glm::vec3(cam4);
    // Direction in world space
    glm::vec4 eye_dir4(eye_dir.x, eye_dir.y, eye_dir.z, 0.0f);
    glm::vec4 dir4 = inv_view_cs * eye_dir4;
    glm::vec3 dir = glm::normalize(glm::vec3(dir4));
    // OFFSET RAY ORIGIN DOWN BY GRID HEIGHT
    auto grid = output->wset()->get_workspace_grid_size();
    float grid_height = (grid.height - 1) * std::abs(CUBE_VERTICAL_SPACING);
    origin.y -= grid_height; // Move ray origin down by grid height
    return {origin, dir};
}
// Get current window Z offset based on zoom level (interpolates during animation)
float get_window_z_offset()
{
    float zoom_factor = animation.cube_animation.zoom;
    zoom_factor = std::max(0.0f, std::min(1.0f, zoom_factor));
    // Interpolate: zoom=0 (cube view) -> 0.2, zoom=1 (normal view) -> 0.01
    return 0.2f * (1.0f - zoom_factor) + 0.01f * zoom_factor;
}
// Helper function: raycast at window layer depth (0.2 units forward)
HitInfo raycast_at_window_depth(const glm::vec3& ray_origin, const glm::vec3& ray_dir, wf::output_t *output)
{
    HitInfo hit;
    auto cws = output->wset()->get_current_workspace();
    auto grid = output->wset()->get_workspace_grid_size();
    int num_cols = grid.width;
    int num_rows = grid.height;
    // Test current row with window layer offset
    {
        float v_offset = 0.0f;
        for (int tx = 0; tx < num_cols; ++tx)
        {
            int face_i = tx;
            auto model = calculate_model_matrix(face_i, v_offset, 1.0f, true); // true = window layer
            glm::vec4 center_local(0.0f, 0.0f, 0.0f, 1.0f);
            glm::vec4 center_world4 = model * center_local;
            glm::vec3 center = glm::vec3(center_world4 / center_world4.w);
            glm::vec4 normal_local(0.0f, 0.0f, 1.0f, 0.0f);
            glm::vec4 normal_world4 = model * normal_local;
            glm::vec3 normal = glm::normalize(glm::vec3(normal_world4));
            float denom = glm::dot(normal, ray_dir);
            if (std::abs(denom) < 1e-6f)
                continue;
            float t = glm::dot(center - ray_origin, normal) / denom;
            if (t < 0.0f || t >= hit.t)
                continue;
            glm::vec3 intersection = ray_origin + t * ray_dir;
            glm::mat4 inv_model = glm::inverse(model);
            glm::vec4 local4 = inv_model * glm::vec4(intersection, 1.0f);
            glm::vec3 local = glm::vec3(local4 / local4.w);
            if (std::abs(local.x) > 0.5f || std::abs(local.y) > 0.5f || std::abs(local.z) > 1e-3f)
                continue;
            int abs_tx = (cws.x + tx) % num_cols;
            int abs_ty = cws.y;
            hit.ws = {abs_tx, abs_ty};
            hit.t = t;
            hit.local_uv.x = local.x + 0.5f;
            hit.local_uv.y = local.y + 0.5f;
            hit.row_offset = 0;
        }
    }
    // Test other rows
    for (int row_offset = 1; row_offset < num_rows; ++row_offset)
    {
        int abs_ty = (cws.y + row_offset) % num_rows;
        float v_offset = -static_cast<float>(row_offset) * CUBE_SPACING;
        for (int tx = 0; tx < num_cols; ++tx)
        {
            int face_i = tx;
            auto model = calculate_model_matrix(face_i, v_offset, 1.0f, true); // true = window layer
            glm::vec4 center_local(0.0f, 0.0f, 0.0f, 1.0f);
            glm::vec4 center_world4 = model * center_local;
            glm::vec3 center = glm::vec3(center_world4 / center_world4.w);
            glm::vec4 normal_local(0.0f, 0.0f, 1.0f, 0.0f);
            glm::vec4 normal_world4 = model * normal_local;
            glm::vec3 normal = glm::normalize(glm::vec3(normal_world4));
            float denom = glm::dot(normal, ray_dir);
            if (std::abs(denom) < 1e-6f)
                continue;
            float t = glm::dot(center - ray_origin, normal) / denom;
            if (t < 0.0f || t >= hit.t)
                continue;
            glm::vec3 intersection = ray_origin + t * ray_dir;
            glm::mat4 inv_model = glm::inverse(model);
            glm::vec4 local4 = inv_model * glm::vec4(intersection, 1.0f);
            glm::vec3 local = glm::vec3(local4 / local4.w);
            if (std::abs(local.x) > 0.5f || std::abs(local.y) > 0.5f || std::abs(local.z) > 1e-3f)
                continue;
            int abs_tx = (cws.x + tx) % num_cols;
            hit.ws = {abs_tx, abs_ty};
            hit.t = t;
            hit.local_uv.x = local.x + 0.5f;
            hit.local_uv.y = local.y + 0.5f;
            hit.row_offset = row_offset;
        }
    }
    return hit;
}
HitInfo raycast_to_workspace(const glm::vec3& ray_origin, const glm::vec3& ray_dir, wf::output_t *output)
{
    HitInfo hit;
    auto cws = output->wset()->get_current_workspace();
    auto grid = output->wset()->get_workspace_grid_size();
    int num_cols = grid.width;
    int num_rows = grid.height;
    // Test current row (v_offset = 0)
    {
        float v_offset = 0.0f;
        for (int tx = 0; tx < num_cols; ++tx)
        {
            int face_i = tx;
            auto model = calculate_model_matrix(face_i, v_offset, 1.0f, false);
            // Plane center
            glm::vec4 center_local(0.0f, 0.0f, 0.0f, 1.0f);
            glm::vec4 center_world4 = model * center_local;
            glm::vec3 center = glm::vec3(center_world4 / center_world4.w);
            // Plane normal
            glm::vec4 normal_local(0.0f, 0.0f, 1.0f, 0.0f);
            glm::vec4 normal_world4 = model * normal_local;
            glm::vec3 normal = glm::normalize(glm::vec3(normal_world4));
            float denom = glm::dot(normal, ray_dir);
            if (std::abs(denom) < 1e-6f)
            {
                continue;
            }
            float t = glm::dot(center - ray_origin, normal) / denom;
            if (t < 0.0f || t >= hit.t)
            {
                continue;
            }
            glm::vec3 intersection = ray_origin + t * ray_dir;
            glm::mat4 inv_model = glm::inverse(model);
            glm::vec4 local4 = inv_model * glm::vec4(intersection, 1.0f);
            glm::vec3 local = glm::vec3(local4 / local4.w);
            if (std::abs(local.x) > 0.5f || std::abs(local.y) > 0.5f || std::abs(local.z) > 1e-3f)
            {
                continue;
            }
            // Valid hit
            int abs_tx = (cws.x + tx) % num_cols;
            int abs_ty = cws.y;
            hit.ws = {abs_tx, abs_ty};
            hit.t = t;
            hit.local_uv.x = local.x + 0.5f;
            hit.local_uv.y = local.y + 0.5f;
            hit.row_offset = 0;
        }
    }
    // Test other rows (positive y direction only, matching rendered layout)
    for (int row_offset = 1; row_offset < num_rows; ++row_offset)
    {
        int abs_ty = (cws.y + row_offset) % num_rows;
        // Fixed: Use positive spacing with negative sign for downward stacking
        float v_offset = -static_cast<float>(row_offset) * CUBE_SPACING;
        for (int tx = 0; tx < num_cols; ++tx)
        {
            int face_i = tx;
            auto model = calculate_model_matrix(face_i, v_offset, 1.0f, false);
            // Plane center
            glm::vec4 center_local(0.0f, 0.0f, 0.0f, 1.0f);
            glm::vec4 center_world4 = model * center_local;
            glm::vec3 center = glm::vec3(center_world4 / center_world4.w);
            // Plane normal
            glm::vec4 normal_local(0.0f, 0.0f, 1.0f, 0.0f);
            glm::vec4 normal_world4 = model * normal_local;
            glm::vec3 normal = glm::normalize(glm::vec3(normal_world4));
            float denom = glm::dot(normal, ray_dir);
            if (std::abs(denom) < 1e-6f)
            {
                continue;
            }
            float t = glm::dot(center - ray_origin, normal) / denom;
            if (t < 0.0f || t >= hit.t)
            {
                continue;
            }
            glm::vec3 intersection = ray_origin + t * ray_dir;
            glm::mat4 inv_model = glm::inverse(model);
            glm::vec4 local4 = inv_model * glm::vec4(intersection, 1.0f);
            glm::vec3 local = glm::vec3(local4 / local4.w);
            if (std::abs(local.x) > 0.5f || std::abs(local.y) > 0.5f || std::abs(local.z) > 1e-3f)
            {
                continue;
            }
            // Valid hit
            int abs_tx = (cws.x + tx) % num_cols;
            hit.ws = {abs_tx, abs_ty};
            hit.t = t;
            hit.local_uv.x = local.x + 0.5f;
            hit.local_uv.y = local.y + 0.5f;
            hit.row_offset = -row_offset;
        }
    }
    return hit;
}
// Modified function to use virtual cursor
wayfire_toplevel_view find_window_at_cursor_on_face(wf::pointf_t virtual_cursor, const wf::point_t& target_ws)
{
    LOGI("=== find_window_at_cursor_on_face ===");
    LOGI("Target WS: ", target_ws.x, ",", target_ws.y);
    LOGI("Virtual cursor at: ", virtual_cursor.x, ",", virtual_cursor.y);
   
    auto ws_set = output->wset();
   
    // Get all views - check ALL windows, not just those on target workspace
    // This handles windows that span across multiple workspaces
    auto all_views = ws_set->get_views();
    LOGI("Total views: ", all_views.size());
   
    wayfire_toplevel_view best_match = nullptr;
   
    for (auto& view : all_views)
    {
        if (!view->is_mapped())
        {
            continue;
        }
       
        auto tview = wf::toplevel_cast(view);
        if (!tview)
        {
            continue;
        }
       
        // Get view geometry
        auto geom = tview->get_geometry();
       
        LOGI("View '", tview->get_title(), "' at geom(", geom.x, ",", geom.y, " ", geom.width, "x", geom.height, ")");
       
        // Check if virtual cursor is within bounds
        if (virtual_cursor.x >= geom.x && virtual_cursor.x <= geom.x + geom.width &&
            virtual_cursor.y >= geom.y && virtual_cursor.y <= geom.y + geom.height)
        {
            auto view_ws = ws_set->get_view_main_workspace(view);
            LOGI(" ✓ FOUND window! Main WS: (", view_ws.x, ",", view_ws.y, "), Click WS: (", target_ws.x, ",", target_ws.y, ")");
            return tview;
        }
    }
   
    LOGI("No window found on face");
    return nullptr;
}
// Updated handle_pointer_button
void handle_pointer_button(const wlr_pointer_button_event& event) override
{
    if (event.button == BTN_LEFT)
    {
        if (event.state == WL_POINTER_BUTTON_STATE_PRESSED)
        {
            LOGI("Mouse button pressed");
            auto cursor = wf::get_core().get_cursor_position();
            last_cursor_pos = cursor;
            drag_start_cursor = cursor;
           
            wf::get_core().unhide_cursor();
           
            // 3D raycasting
            auto bbox = output->get_layout_geometry();
            Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
           
            // Always compute virtual hit on the infinite plane
            if (std::abs(ray.dir.z) > 1e-6f) {
                float t = (plane_z - ray.origin.z) / ray.dir.z;
                if (t > 0.0f) {
                    virtual_ray_hit_pos = ray.origin + t * ray.dir;
                    has_virtual_hit = true;
                } else {
                    has_virtual_hit = false;
                }
            } else {
                has_virtual_hit = false;
            }
           
            HitInfo hit = raycast_at_window_depth(ray.origin, ray.dir, output);
           
            if (hit.ws.x >= 0)
            {
                LOGI("Hit workspace (", hit.ws.x, ",", hit.ws.y, ") at UV (", hit.local_uv.x, ",", hit.local_uv.y, ")");
                LOGI("Bbox dimensions: ", bbox.width, "x", bbox.height);
               
                // ============================================================
                // UPDATE CURSOR INDICATOR - Shows where raycasting hit
                // ============================================================
                cursor_indicator.active = true;
                cursor_indicator.workspace_x = hit.ws.x;
                cursor_indicator.workspace_y = hit.ws.y;
                cursor_indicator.uv_position = hit.local_uv;
                cursor_indicator.world_position = ray.origin + hit.t * ray.dir;
                // Sync virtual hit with actual hit when bounded
                virtual_ray_hit_pos = cursor_indicator.world_position;
                // ============================================================
               
                // Compute virtual cursor - add workspace offset to get absolute position
                // Since we're raycasting at window depth, we need to
                // apply perspective correction to match where windows are actually positioned
                float window_offset = get_window_z_offset();
                float perspective_scale = plane_z / (plane_z + window_offset);
               
                // Scale UV from center
                float centered_uv_x = hit.local_uv.x - 0.5f;
                float centered_uv_y = hit.local_uv.y - 0.5f;
                float corrected_uv_x = (centered_uv_x * perspective_scale) + 0.5f;
                float corrected_uv_y = (centered_uv_y * perspective_scale) + 0.5f;
               
                float virtual_x = (hit.ws.x + corrected_uv_x) * static_cast<float>(bbox.width);
                float virtual_y = (hit.ws.y + (1.0f - corrected_uv_y)) * static_cast<float>(bbox.height);
                wf::pointf_t virtual_cursor{virtual_x, virtual_y};
               
                LOGI("Virtual cursor (with WS offset): (", virtual_x, ",", virtual_y, ")");
                LOGI("Hit UV: (", hit.local_uv.x, ",", hit.local_uv.y, ")");
                LOGI("Corrected UV: (", corrected_uv_x, ",", corrected_uv_y, ")");
                LOGI("Window offset: ", window_offset, ", Perspective scale: ", perspective_scale);
                LOGI("Cursor indicator updated at workspace (", hit.ws.x, ",", hit.ws.y, ")");
               
                // Find window using virtual cursor
                dragged_view = find_window_at_cursor_on_face(virtual_cursor, hit.ws);
               
                dragged_window_face_index = hit.ws.x; // Keep for compatibility, now absolute
               
                is_dragging_window = true;
               
                if (dragged_view)
                {
                    auto ws = output->wset()->get_view_main_workspace(dragged_view);
                    drag_start_workspace = ws;
                   
                    // Store the offset from the window position to where the cursor clicked
                    auto geom = dragged_view->get_geometry();
                    drag_offset.x = virtual_cursor.x - geom.x;
                    drag_offset.y = virtual_cursor.y - geom.y;
                   
                    // ============================================================
                    // WOBBLY: Start wobbly effect when grabbing window
                    // ============================================================
                    start_wobbly(dragged_view, virtual_cursor.x, virtual_cursor.y);
                   
                    // Calculate relative grab position for wobbly
                    relative_grab_position.x = drag_offset.x / (double)geom.width;
                    relative_grab_position.y = drag_offset.y / (double)geom.height;
                    // ============================================================
                   
                    LOGI("✓ Dragging '", dragged_view->get_title(), "' on WS (",
                         hit.ws.x, ",", hit.ws.y, ")");
                    LOGI(" Window at (", geom.x, ",", geom.y, "), cursor at (",
                         virtual_cursor.x, ",", virtual_cursor.y, "), offset (",
                         drag_offset.x, ",", drag_offset.y, ")");
                }
                else
                {
                    LOGI("No window hit, but face hit - allowing cube drag");
                }
            }
            else
            {
                LOGI("✗ No face hit");
                is_dragging_window = false;
                cursor_indicator.active = false;
               
                // Still have virtual hit for smooth tilt
            }
        }
       else if (event.state == WL_POINTER_BUTTON_STATE_RELEASED)
        {
            LOGI("Mouse released");
           
            if (is_dragging_window)
            {
                // ============================================================
                // WOBBLY: End wobbly effect before moving window
                // ============================================================
                if (dragged_view)
                {
                    // End the wobbly effect
                    end_wobbly(dragged_view);
                   
                    // Get final position for wobbly rebuild
                    auto cursor = wf::get_core().get_cursor_position();
                    Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
                    HitInfo hit = raycast_at_window_depth(ray.origin, ray.dir, output);
                   
                    if (hit.ws.x >= 0)
                    {
                        auto bbox = output->get_layout_geometry();
                        float window_offset = get_window_z_offset();
                        float perspective_scale = plane_z / (plane_z + window_offset);
                       
                        float centered_uv_x = hit.local_uv.x - 0.5f;
                        float centered_uv_y = hit.local_uv.y - 0.5f;
                        float corrected_uv_x = (centered_uv_x * perspective_scale) + 0.5f;
                        float corrected_uv_y = (centered_uv_y * perspective_scale) + 0.5f;
                       
                        float virtual_x = (hit.ws.x + corrected_uv_x) * static_cast<float>(bbox.width);
                        float virtual_y = (hit.ws.y + (1.0f - corrected_uv_y)) * static_cast<float>(bbox.height);
                        wf::point_t final_position{(int)virtual_x, (int)virtual_y};
                       
                        // Rebuild wobbly with final position
                     // rebuild_wobbly(dragged_view, final_position, relative_grab_position);
                       
                        // Translate back to output-local coordinates
                        translate_wobbly(dragged_view,
                            -wf::origin(dragged_view->get_output()->get_layout_geometry()));
                    }
                    // ============================================================
                   
                    // Before clearing the drag state, move the window to the correct workspace
                    auto ws_set = output->wset();
                    auto current_ws = ws_set->get_view_main_workspace(dragged_view);
                    auto window_geom = dragged_view->get_geometry();
                    auto output_geom = output->get_layout_geometry();
                    auto grid = ws_set->get_workspace_grid_size();
                   
                    // Calculate which workspace the window center is on
                    int center_x = window_geom.x + window_geom.width / 2;
                    int center_y = window_geom.y + window_geom.height / 2;
                   
                    wf::point_t target_ws;
                    target_ws.x = center_x / output_geom.width;
                    target_ws.y = center_y / output_geom.height;
                   
                    // Clamp to grid bounds
                    target_ws.x = std::max(0, std::min(target_ws.x, grid.width - 1));
                    target_ws.y = std::max(0, std::min(target_ws.y, grid.height - 1));
                   
                    LOGI("Window center at (", center_x, ",", center_y, ") -> workspace (",
                         target_ws.x, ",", target_ws.y, ")");
                   
                    // Move window to target workspace if different
                    if (target_ws != current_ws)
                    {
                        ws_set->move_to_workspace(dragged_view, target_ws);
                       
                        // Adjust window geometry to be relative to the new workspace
                        wf::geometry_t adjusted_geom = window_geom;
                        adjusted_geom.x = window_geom.x - (target_ws.x * output_geom.width);
                        adjusted_geom.y = window_geom.y - (target_ws.y * output_geom.height);
                       
                        dragged_view->set_geometry(adjusted_geom);
                       
                        LOGI("Moved window from workspace (", current_ws.x, ",", current_ws.y,
                             ") to (", target_ws.x, ",", target_ws.y, ")");
                        LOGI("Adjusted geometry to (", adjusted_geom.x, ",", adjusted_geom.y, ")");
                    }
                }
               
                is_dragging_window = false;
                dragged_view = nullptr;
                dragged_window_face_index = -1;
               
                // Don't deactivate on left-click release anymore
                // User can now use right-click to exit
            }
        }
    }
    else if (event.button == BTN_RIGHT)
    {
        if (event.state == WL_POINTER_BUTTON_STATE_PRESSED)
        {
            LOGI("Right-click - detecting target workspace");
           
            // Get cursor position and raycast to find which workspace we're hovering
            auto cursor = wf::get_core().get_cursor_position();
            Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
           
            // Use desktop layer raycasting (not window layer) to detect workspace faces
            HitInfo hit = raycast_to_workspace(ray.origin, ray.dir, output);
           
            if (hit.ws.x >= 0)
            {
                LOGI("Right-click on workspace (", hit.ws.x, ",", hit.ws.y, ")");
               
                auto ws_set = output->wset();
                auto current_ws = ws_set->get_current_workspace();
               
                // Calculate workspace delta
                int dx = hit.ws.x - current_ws.x;
                int dy = hit.ws.y - current_ws.y;
               
                LOGI("Delta: dx=", dx, ", dy=", dy);
               
                // Switch to target workspace first
               // ws_set->set_workspace(wf::point_t{hit.ws.x, hit.ws.y});
               
                // Mark that workspace is already set, so deactivate() doesn't switch again
                workspace_already_set = true;
               
                // Now manually set up the exit animation
                animation.in_exit = true;
               
                // Calculate target rotation: we want to rotate TO the target workspace
                // Current rotation represents where we are, we need to add the delta
                float current_rotation = animation.cube_animation.rotation;
                float rotation_delta = -dx * animation.side_angle; // Negative because right is negative rotation
                float target_rotation = current_rotation + rotation_delta;
               
                // But input_ungrabbed expects rotation to already represent the workspace
                // So we need to set it to what it WOULD be for that workspace
                // Then input_ungrabbed will align it to 0
                target_rotation = -hit.ws.x * animation.side_angle;
               
                animation.cube_animation.rotation.set(current_rotation, target_rotation);
               
                LOGI("Rotation: current=", current_rotation, ", target=", target_rotation);
               
                // Handle vertical offset
                // CUBE_VERTICAL_SPACING is already negative (-1.2), so no need for extra minus
                float target_y = static_cast<float>(hit.ws.y) * CUBE_VERTICAL_SPACING;
                camera_y_offset.animate(target_y);
               
                // Reset other attributes
                reset_attribs();
               
                popout_scale_animation.animate(1.01);
                animation.cube_animation.start();
               
                update_view_matrix();
                output->render->schedule_redraw();
            }
            else
            {
                LOGI("Right-click in void - exiting to current workspace");
                input_ungrabbed();
            }
           
            // Disable cursor indicator when exiting
            cursor_indicator.active = false;
        }
    }
   
    // Force redraw to update cursor indicator
    output->render->schedule_redraw();
}
// Add this new helper function to move windows:
void move_window_to_target_workspace(float dx, float dy)
{
    if (!dragged_view)
    {
        LOGI("No dragged view!");
        return;
    }
   
    auto grid = output->wset()->get_workspace_grid_size();
   
    LOGI("=== WINDOW DRAG ANALYSIS ===");
    LOGI("Drag delta: dx=", dx, " dy=", dy);
    LOGI("Start workspace: (", drag_start_workspace.x, ",", drag_start_workspace.y, ")");
    LOGI("Grid size: ", grid.width, "x", grid.height);
   
    // Calculate workspace offset based on drag direction
    int workspace_offset_x = 0;
    int workspace_offset_y = 0;
   
    // Thresholds
    float horizontal_threshold = 100.0f;
    float vertical_threshold = 100.0f;
   
    // CRITICAL: The cube uses HORIZONTAL layout (workspaces side-by-side)
    // So horizontal drag (dx) should change HORIZONTAL workspace (x)
    // And vertical drag (dy) should change VERTICAL workspace (y)
   
    // HORIZONTAL DRAG (left/right) → Change workspace X
    if (std::abs(dx) > horizontal_threshold)
    {
        // Dragging RIGHT (+dx) should move to workspace on the RIGHT (+x)
        // Dragging LEFT (-dx) should move to workspace on the LEFT (-x)
        workspace_offset_x = (dx > 0) ? 1 : -1;
        LOGI("→ Horizontal drag: offset_x = ", workspace_offset_x);
    }
   
    // VERTICAL DRAG (up/down) → Change workspace Y
    if (std::abs(dy) > vertical_threshold)
    {
        // IMPORTANT: Check if your Y-axis is inverted
        // Screen coordinates: +Y = DOWN
        // Workspace grid: +Y might be UP or DOWN depending on your layout
       
        // Option 1: If workspace Y increases going DOWN (row below)
        workspace_offset_y = (dy > 0) ? 1 : -1; // +dy = move down = +y
       
        // Option 2: If workspace Y increases going UP (row above)
        // workspace_offset_y = (dy > 0) ? -1 : 1; // +dy = move down = -y
       
        LOGI("↕ Vertical drag: offset_y = ", workspace_offset_y);
    }
   
    // If dragged diagonally, choose the STRONGER direction
    if (workspace_offset_x != 0 && workspace_offset_y != 0)
    {
        if (std::abs(dx) > std::abs(dy))
        {
            // Horizontal is stronger - only move horizontally
            workspace_offset_y = 0;
            LOGI("Diagonal drag: choosing HORIZONTAL (stronger)");
        }
        else
        {
            // Vertical is stronger - only move vertically
            workspace_offset_x = 0;
            LOGI("Diagonal drag: choosing VERTICAL (stronger)");
        }
    }
   
    LOGI("Final offset: (", workspace_offset_x, ",", workspace_offset_y, ")");
   
    // Calculate new workspace coordinates
    int new_workspace_x = drag_start_workspace.x + workspace_offset_x;
    int new_workspace_y = drag_start_workspace.y + workspace_offset_y;
   
    // Clamp to valid range
    new_workspace_x = wf::clamp(new_workspace_x, 0, grid.width - 1);
    new_workspace_y = wf::clamp(new_workspace_y, 0, grid.height - 1);
   
    LOGI("Target workspace (before clamp): (",
         drag_start_workspace.x + workspace_offset_x, ",",
         drag_start_workspace.y + workspace_offset_y, ")");
    LOGI("Target workspace (after clamp): (", new_workspace_x, ",", new_workspace_y, ")");
   
    // Check if workspace actually changed
    if (new_workspace_x == drag_start_workspace.x &&
        new_workspace_y == drag_start_workspace.y)
    {
        LOGI("✗ No workspace change (same position or hit grid edge)");
        return;
    }
   
    wf::point_t new_ws = {new_workspace_x, new_workspace_y};
    LOGI("✓ Moving window '", dragged_view->get_title(), "' to workspace (",
         new_ws.x, ",", new_ws.y, ")");
   
    // Move the window
    output->wset()->move_to_workspace(dragged_view, new_ws);
   
    // Force redraw
    output->render->schedule_redraw();
   
    LOGI("=== MOVE COMPLETE ===");
}
wayfire_toplevel_view find_window_at_cursor(wf::pointf_t cursor)
{
    LOGI("=== find_window_at_cursor ===");
    LOGI("Cursor at: ", cursor.x, ",", cursor.y);
   
    auto ws_set = output->wset();
    auto current_ws = ws_set->get_current_workspace();
   
    LOGI("Current workspace: ", current_ws.x, ",", current_ws.y);
   
    // Get all views
    auto all_views = ws_set->get_views();
    LOGI("Total views: ", all_views.size());
   
    // Prioritize views on current workspace
    wayfire_toplevel_view best_match = nullptr;
   
    for (auto& view : all_views)
    {
        if (!view->is_mapped())
        {
            continue;
        }
       
        auto tview = wf::toplevel_cast(view);
        if (!tview)
        {
            continue;
        }
       
        // Get view workspace
        auto view_ws = ws_set->get_view_main_workspace(view);
       
        // Get view geometry
        auto geom = tview->get_geometry();
       
        LOGI("View '", tview->get_title(), "' at ws(", view_ws.x, ",", view_ws.y,
             ") geom(", geom.x, ",", geom.y, " ", geom.width, "x", geom.height, ")");
       
        // Check if cursor is within bounds
        if (cursor.x >= geom.x && cursor.x <= geom.x + geom.width &&
            cursor.y >= geom.y && cursor.y <= geom.y + geom.height)
        {
            // Prefer windows on current workspace
            if (view_ws.x == current_ws.x && view_ws.y == current_ws.y)
            {
                LOGI(" ✓ FOUND on current workspace!");
                return tview;
            }
           
            // Otherwise, save as potential match
            if (!best_match)
            {
                best_match = tview;
                LOGI(" ~ Potential match (different workspace)");
            }
        }
    }
   
    if (best_match)
    {
        LOGI("Returning best match: '", best_match->get_title(), "'");
    }
    else
    {
        LOGI("No window found at cursor");
    }
   
    return best_match;
}
void finalize_window_drag()
{
    if (!dragged_view || !is_dragging_window)
    {
        return;
    }
   
    auto cursor = wf::get_core().get_cursor_position();
    float dx = cursor.x - drag_start_cursor.x;
    float dy = cursor.y - drag_start_cursor.y;
   
    // Calculate workspace offset based on drag distance
    int workspace_offset_x = 0;
    int workspace_offset_y = 0;
   
    // Threshold for workspace change (adjust as needed)
    float horizontal_threshold = 150.0f;
    float vertical_threshold = 150.0f;
   
    if (std::abs(dx) > horizontal_threshold)
    {
        workspace_offset_x = (dx > 0) ? 1 : -1;
    }
   
    if (std::abs(dy) > vertical_threshold)
    {
        workspace_offset_y = (dy > 0) ? 1 : -1;
    }
   
    // Calculate new workspace
    auto grid = output->wset()->get_workspace_grid_size();
    int new_workspace_x = drag_start_workspace.x + workspace_offset_x;
    int new_workspace_y = drag_start_workspace.y + workspace_offset_y;
   
    // Clamp to valid workspace range
    new_workspace_x = wf::clamp(new_workspace_x, 0, grid.width - 1);
    new_workspace_y = wf::clamp(new_workspace_y, 0, grid.height - 1);
   
    // Only move if workspace changed
    if (new_workspace_x != drag_start_workspace.x ||
        new_workspace_y != drag_start_workspace.y)
    {
        wf::point_t new_ws = {new_workspace_x, new_workspace_y};
        LOGI("Moving window to workspace ", new_ws.x, ",", new_ws.y);
       
        output->wset()->move_to_workspace(dragged_view, new_ws);
       
        // Reload cube to show updated positions
        //reload_buffers();
        output->render->schedule_redraw();
    }
}
    void handle_pointer_axis(const wlr_pointer_axis_event& event) override
    {
        if (event.orientation == WL_POINTER_AXIS_VERTICAL_SCROLL)
        {
            pointer_scrolled(event.delta);
        }
    }
void load_program()
{
#ifdef USE_GLES32
    std::string ext_string(reinterpret_cast<const char*>(glGetString(GL_EXTENSIONS)));
    tessellation_support = ext_string.find(std::string("GL_EXT_tessellation_shader")) !=
        std::string::npos;
#else
    tessellation_support = false;
#endif
    if (!tessellation_support)
    {
        program.set_simple(OpenGL::compile_program(cube_vertex_2_0, cube_fragment_2_0));
        window_program.set_simple(OpenGL::compile_program(cube_vertex_2_0, cube_fragment_2_0)); // Same for windows
        cap_program.set_simple(OpenGL::compile_program(cube_cap_vertex, cube_cap_fragment));
        cursor_program.set_simple(OpenGL::compile_program(cursor_vertex_shader, cursor_fragment_shader));
        // NEW: Load beam program
        beam_program.set_simple(OpenGL::compile_program(beam_vertex_shader, beam_fragment_shader));
    } else
    {
#ifdef USE_GLES32
        // Desktop backgrounds - full tessellation shader
        auto id = GL_CALL(glCreateProgram());
        GLuint vss, fss, tcs, tes, gss;
        vss = OpenGL::compile_shader(cube_vertex_3_2, GL_VERTEX_SHADER);
if (!tron)
        {fss = OpenGL::compile_shader(cube_fragment_3_2, GL_FRAGMENT_SHADER);}
if (tron)
        {fss = OpenGL::compile_shader(cube_fragment_3_2_tron, GL_FRAGMENT_SHADER);}
       
        tcs = OpenGL::compile_shader(cube_tcs_3_2, GL_TESS_CONTROL_SHADER);
        tes = OpenGL::compile_shader(cube_tes_3_2, GL_TESS_EVALUATION_SHADER);
        gss = OpenGL::compile_shader(cube_geometry_3_2, GL_GEOMETRY_SHADER);
        GL_CALL(glAttachShader(id, vss));
        GL_CALL(glAttachShader(id, tcs));
        GL_CALL(glAttachShader(id, tes));
        GL_CALL(glAttachShader(id, gss));
        GL_CALL(glAttachShader(id, fss));
        GL_CALL(glLinkProgram(id));
        GL_CALL(glUseProgram(id));
        GL_CALL(glDeleteShader(vss));
        GL_CALL(glDeleteShader(fss));
        GL_CALL(glDeleteShader(tcs));
        GL_CALL(glDeleteShader(tes));
        GL_CALL(glDeleteShader(gss));
       
        program.set_simple(id);
       
 
       // Windows - simple shader (original fragment shader, no tessellation)
auto window_id = GL_CALL(glCreateProgram());
GLuint win_vss, win_fss;
win_vss = OpenGL::compile_shader(cube_vertex_3_2_simple, GL_VERTEX_SHADER); // Use simple vertex
win_fss = OpenGL::compile_shader(cube_fragment_3_2_orginal, GL_FRAGMENT_SHADER);
GL_CALL(glAttachShader(window_id, win_vss));
GL_CALL(glAttachShader(window_id, win_fss));
GL_CALL(glLinkProgram(window_id));
GL_CALL(glUseProgram(window_id));
GL_CALL(glDeleteShader(win_vss));
GL_CALL(glDeleteShader(win_fss));
window_program.set_simple(window_id);
       
        cap_program.set_simple(OpenGL::compile_program(cube_cap_vertex, cube_cap_fragment));
       
        // ========================================
        // FIX: Add cursor program here too!
        // ========================================
        cursor_program.set_simple(OpenGL::compile_program(cursor_vertex_shader, cursor_fragment_shader));
        // NEW: Load beam program
        beam_program.set_simple(OpenGL::compile_program(beam_vertex_shader, beam_fragment_shader));
#endif
    }
    // Load background shader program
    background_program.set_simple(OpenGL::compile_program(
        background_vertex_shader, background_fragment_shader));
   
// Create circle VBO for cursor
if (cursor_vbo == 0)
{
    const int segments = 32;
    std::vector<GLfloat> circle_vertices;
   
    // Center point
    circle_vertices.push_back(0.0f);
    circle_vertices.push_back(0.0f);
   
    // Circle points
    for (int i = 0; i <= segments; i++)
    {
        float angle = (float)i / segments * 2.0f * M_PI;
        circle_vertices.push_back(cos(angle));
        circle_vertices.push_back(sin(angle));
    }
   
    GL_CALL(glGenBuffers(1, &cursor_vbo));
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, cursor_vbo));
    GL_CALL(glBufferData(GL_ARRAY_BUFFER,
                         circle_vertices.size() * sizeof(GLfloat),
                         circle_vertices.data(),
                         GL_STATIC_DRAW));
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, 0));
}
    // Create fullscreen quad VBO for background
    if (background_vbo == 0)
    {
        static const GLfloat quad_vertices[] = {
            -1.0f, -1.0f,
             1.0f, -1.0f,
            -1.0f, 1.0f,
             1.0f, 1.0f
        };
       
        GL_CALL(glGenBuffers(1, &background_vbo));
        GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, background_vbo));
        GL_CALL(glBufferData(GL_ARRAY_BUFFER, sizeof(quad_vertices),
                             quad_vertices, GL_STATIC_DRAW));
        GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, 0));
    }
    animation.projection = glm::perspective(45.0f, 1.f, 0.1f, 100.f);
}
void render_shader_background(const wf::render_target_t& target)
{
    if (background_program.get_program_id(wf::TEXTURE_TYPE_RGBA) == 0)
    {
        return;
    }
   
    // Render with depth test, but write max depth
    GL_CALL(glEnable(GL_DEPTH_TEST));
    GL_CALL(glDepthFunc(GL_LEQUAL)); // Use LEQUAL
    GL_CALL(glDepthMask(GL_TRUE));
   
    background_program.use(wf::TEXTURE_TYPE_RGBA);
   
    static auto start_time = std::chrono::steady_clock::now();
    auto current_time = std::chrono::steady_clock::now();
    float elapsed = std::chrono::duration<float>(current_time - start_time).count();
    background_program.uniform1f("u_time", elapsed);
   
    auto geom = output->get_layout_geometry();
    background_program.uniform2f("u_resolution", (float)geom.width, (float)geom.height);
   
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, background_vbo));
    background_program.attrib_pointer("position", 2, 0, nullptr);
   
    GL_CALL(glDrawArrays(GL_TRIANGLE_STRIP, 0, 4));
   
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, 0));
    background_program.deactivate();
   
    // Restore GL_LESS for cube
    GL_CALL(glDepthFunc(GL_LESS));
}
    wf::signal::connection_t<cube_control_signal> on_cube_control = [=] (cube_control_signal *d)
    {
        rotate_and_zoom_cube(d->angle, d->zoom, d->ease, d->last_frame);
        d->carried_out = true;
    };
    void rotate_and_zoom_cube(double angle, double zoom, double ease,
        bool last_frame)
    {
        if (last_frame)
        {
            deactivate();
            return;
        }
        if (!activate())
        {
            return;
        }
        float offset_z = identity_z_offset + Z_OFFSET_NEAR;
       
        // Tilt stays at 180° during cube mode, regardless of zoom level
        float tilt_angle = glm::radians(180.0f);
       
        LOGI("########## rotate_and_zoom_cube: Setting max_tilt to 180° ##########");
        animation.cube_animation.rotation.set(angle, angle);
        animation.cube_animation.zoom.set(zoom, zoom);
        animation.cube_animation.ease_deformation.set(ease, ease);
        animation.cube_animation.offset_y.set(0, 0);
        animation.cube_animation.offset_z.set(offset_z, offset_z);
        animation.cube_animation.max_tilt.set(tilt_angle, tilt_angle); // Always 180° in cube mode
       
        LOGI("After set: max_tilt=", glm::degrees((float)animation.cube_animation.max_tilt), "°");
        animation.cube_animation.start();
        update_view_matrix();
        output->render->schedule_redraw();
    }
    /* Tries to initialize renderer, activate plugin, etc. */
    bool activate()
    {
        if (output->is_plugin_active(grab_interface.name))
        {
            return true;
        }
        if (!output->activate_plugin(&grab_interface))
        {
            return false;
        }
        wf::get_core().connect(&on_motion_event);
    if (trail_vbo == 0)
    {
        GL_CALL(glGenBuffers(1, &trail_vbo));
    }
   
    // Reset workspace flag when plugin activates
    workspace_already_set = false;
 output->wset()->set_workspace({0, 0});
        render_node = std::make_shared<cube_render_node_t>(this);
        wf::scene::add_front(wf::get_core().scene(), render_node);
        output->render->add_effect(&pre_hook, wf::OUTPUT_EFFECT_PRE);
        output->render->set_require_depth_buffer(true);
        output->wset()->set_workspace({0, 0});
      // wf::get_core().hide_cursor();
        input_grab->grab_input(wf::scene::layer::OVERLAY);
        auto wsize = output->wset()->get_workspace_grid_size();
        animation.side_angle = 2 * M_PI / float(wsize.width);
        identity_z_offset = 0.5 / std::tan(animation.side_angle / 2);
        if (wsize.width == 1)
        {
            // tan(M_PI) is 0, so identity_z_offset is invalid
            identity_z_offset = 0.0f;
        }
plane_z = identity_z_offset;
// Calculate optimal zoom to fit entire grid
auto g = output->wset()->get_workspace_grid_size();
// Calculate actual grid dimensions in world space
float grid_width = (g.width - 1) * CUBE_SPACING;
float grid_height = (g.height - 1) * CUBE_SPACING;
// Calculate required Z distance to see full grid
float fov = glm::radians(45.0f);
auto output_geometry = output->get_layout_geometry();
float aspect = (float)output_geometry.width / output_geometry.height;
float required_z_for_height = grid_height / (2.0f * std::tan(fov / 2.0f));
float required_z_for_width = grid_width / (2.0f * aspect * std::tan(fov / 2.0f));
float required_z = std::max(required_z_for_height, required_z_for_width) * CUBE_SPACING;
// Start zoomed out to show full grid
// When cube is active, tilt is always at 180° regardless of zoom level
// Tilt only interpolates to 0° during the exit animation
float initial_zoom = 0.1f; // Start mostly zoomed out
float initial_tilt = glm::radians(180.0f); // Always 180° while cube is active
LOGI("=================================================================");
LOGI("CUBE ACTIVATE: initial_zoom=", initial_zoom, " initial_tilt_deg=", glm::degrees(initial_tilt));
LOGI("=================================================================");
animation.cube_animation.zoom.set(initial_zoom, initial_zoom);
animation.cube_animation.rotation.set(0, 0);
animation.cube_animation.offset_y.set(0, 0);
animation.cube_animation.offset_z.set(required_z, required_z);
animation.cube_animation.ease_deformation.set(0, 0);
animation.cube_animation.max_tilt.set(initial_tilt, initial_tilt); // 180° while in cube mode
LOGI("After setting animations: zoom=", (float)animation.cube_animation.zoom,
     " max_tilt=", glm::degrees((float)animation.cube_animation.max_tilt), "°");
       
reload_background();
       
        popout_scale_animation.animate(1.0, 1.2);
        // Force a full redraw to clear any stale state
        output->render->damage_whole();
       
        return true;
    }
    int calculate_viewport_dx_from_rotation()
    {
        float dx = -animation.cube_animation.rotation / animation.side_angle;
        return std::floor(dx + 0.5);
    }
// Add this method to calculate which row we're currently viewing
int calculate_viewport_dy_from_camera()
{
    // Calculate which cube row the camera is focused on
    // Each row is offset by CUBE_VERTICAL_SPACING
    float dy = -camera_y_offset / (-CUBE_VERTICAL_SPACING);
    return std::floor(dy + 0.5);
}
// Modified deactivate() method
void deactivate()
{
    if (!output->is_plugin_active(grab_interface.name))
    {
        return;
    }
    // Animate popout scale back to 1.0
  // popout_scale_animation.animate(1.0);
   
    // Don't actually deactivate until animation finishes
   // animation.in_exit = true;
    is_dragging_window = false;
    dragged_view = nullptr;
    wf::scene::remove_child(render_node);
    output->render->damage_whole();
    render_node = nullptr;
    output->render->rem_effect(&pre_hook);
  // output->render->set_require_depth_buffer(false);
    input_grab->ungrab_input();
    output->deactivate_plugin(&grab_interface);
    wf::get_core().unhide_cursor();
    on_motion_event.disconnect();
    /* Figure out how much we have rotated and switch workspace */
    // Only calculate and switch if workspace wasn't already set (e.g., by right-click)
   // if (!workspace_already_set)
    {
        int size = get_num_faces();
        int dvx = calculate_viewport_dx_from_rotation();
       
        // NEW: Calculate vertical workspace change based on camera position
        int dvy = calculate_viewport_dy_from_camera();
        auto cws = output->wset()->get_current_workspace();
        auto grid = output->wset()->get_workspace_grid_size();
       
        int nvx = (cws.x + (dvx % size) + size) % size;
        int nvy = (cws.y + dvy) % grid.height;
       
        // Clamp to valid workspace range
        nvy = std::max(0, std::min(nvy, grid.height - 1));
       
        output->wset()->set_workspace({nvx, nvy});
    }
   
    // Reset the flag for next time
    workspace_already_set = false;
has_virtual_hit = false;
virtual_ray_hit_pos = {0.0f, 0.0f, 0.0f};
    /* We are finished with rotation, make sure the next time cube is used
     * it is properly reset */
  // animation.cube_animation.rotation.set(0, 0);
 // camera_y_offset.set(0, 0); // Reset camera position
}
bool move_vp_vertical(int dir)
{
    bool was_active = output->is_plugin_active(grab_interface.name);
   
    if (!was_active && !activate())
    {
        return false;
    }
   
    animation.in_exit = false;
   
    // Get grid dimensions
    auto grid = output->wset()->get_workspace_grid_size();
   
    // Calculate actual grid extents in world space
    float grid_width = (grid.width - 1) * CUBE_SPACING;
    float grid_height = (grid.height - 1) * CUBE_SPACING;
   
    // Calculate CENTER of the grid
    float center_x = grid_width / 2.0f;
    float center_y = grid_height / 2.0f;
   
    // Calculate required Z distance to see full grid
    float fov = glm::radians(45.0f);
    auto output_geometry = output->get_layout_geometry();
    float aspect = (float)output_geometry.width / output_geometry.height;
   
    float required_z_for_height = grid_height / (2.0f * std::tan(fov / 2.0f));
    float required_z_for_width = grid_width / (2.0f * aspect * std::tan(fov / 2.0f));
    float required_z = std::max(required_z_for_height, required_z_for_width) * 1.1f;
   
    // Center horizontally using rotation
    float center_rotation = -(center_x / CUBE_SPACING) * animation.side_angle;
   
    // Position camera BELOW the grid center so when looking straight, grid is centered
    // Since camera looks at Y=camera_y_offset on the z-plane, we want it at 0 to see center_y content
    camera_y_offset.animate(-center_y);
   
    animation.cube_animation.zoom.restart_with_end(1.0f);
    animation.cube_animation.rotation.restart_with_end(center_rotation);
    animation.cube_animation.offset_y.restart_with_end(0);
    animation.cube_animation.offset_z.restart_with_end(required_z);
    animation.cube_animation.ease_deformation.restart_with_end(0);
    animation.cube_animation.max_tilt.restart_with_end(glm::radians(45.0f)); // Set tilt to 180° in cube mode
   
    LOGI("########## move_vp_vertical: Setting max_tilt to 180° ##########");
   
    animation.cube_animation.start();
    update_view_matrix();
    output->render->schedule_redraw();
    return true;
}
// Modified reset_attribs to maintain camera position during transitions
void reset_attribs()
{
    float current_tilt = animation.cube_animation.max_tilt;
   
    LOGI("reset_attribs: Animating tilt from ", glm::degrees(current_tilt), "° to 0°");
   
    animation.cube_animation.zoom.restart_with_end(1.0);
    animation.cube_animation.offset_z.restart_with_end(
        identity_z_offset + Z_OFFSET_NEAR);
    animation.cube_animation.offset_y.restart_with_end(0);
    animation.cube_animation.ease_deformation.restart_with_end(0);
    animation.cube_animation.max_tilt.restart_with_end(0); // Interpolate tilt to 0 during exit
    // Don't reset camera_y_offset here - let it maintain position until deactivate
}
    /* Start moving to a workspace to the left/right using the keyboard */
    bool move_vp(int dir)
    {
        if (!activate())
        {
            return false;
        }
        /* After the rotation is done, we want to exit cube and focus the target
         * workspace */
        animation.in_exit = false;
        /* Set up rotation target to the next workspace in the given direction,
         * and reset other attribs */
      // reset_attribs();
        animation.cube_animation.rotation.restart_with_end(
            animation.cube_animation.rotation.end - dir * animation.side_angle);
        animation.cube_animation.start();
        update_view_matrix();
        output->render->schedule_redraw();
        return true;
    }
    /* Initiate with an button grab. */
    bool input_grabbed()
    {
        if (!activate())
        {
            return false;
        }
        /* Rotations, offset_y and zoom stay as they are now, as they have been
         * grabbed.
         * offset_z changes to the default one.
         *
         * We also need to make sure the cube gets deformed */
        animation.in_exit = false;
        float current_rotation = animation.cube_animation.rotation;
        float current_offset_y = animation.cube_animation.offset_y;
        float current_zoom = animation.cube_animation.zoom;
            wf::get_core().unhide_cursor();
    cursor_hidden_by_cube = false;
        animation.cube_animation.rotation.set(current_rotation, current_rotation);
        animation.cube_animation.offset_y.set(current_offset_y, current_offset_y);
        animation.cube_animation.offset_z.restart_with_end(
            zoom_opt + identity_z_offset + Z_OFFSET_NEAR);
        animation.cube_animation.zoom.set(current_zoom, current_zoom);
        animation.cube_animation.ease_deformation.restart_with_end(1);
        animation.cube_animation.start();
        update_view_matrix();
        output->render->schedule_redraw();
        // Let the button go to the input grab
        return false;
    }
    /* Mouse grab was released */
    void input_ungrabbed()
    {
        animation.in_exit = true;
        /* Rotate cube so that selected workspace aligns with the output */
        float current_rotation = animation.cube_animation.rotation;
        int dvx = calculate_viewport_dx_from_rotation();
        animation.cube_animation.rotation.set(current_rotation,
            -dvx * animation.side_angle);
        /* And reset other attributes, again to align the workspace with the output
         * */
        reset_attribs();
        popout_scale_animation.animate(1.01); //longer time to fix screen glitch
        animation.cube_animation.start();
        update_view_matrix();
        output->render->schedule_redraw();
    }
    /* Update the view matrix used in the next frame */
    void update_view_matrix()
    {
        auto zoom_translate = glm::translate(glm::mat4(1.f),
            glm::vec3(0.f, 0.f, -animation.cube_animation.offset_z));
        auto rotation = glm::rotate(glm::mat4(1.0),
            (float)animation.cube_animation.offset_y,
            glm::vec3(1., 0., 0.));
        // Apply camera vertical offset for viewing different cube rows
        auto camera_vertical = glm::translate(glm::mat4(1.0),
            glm::vec3(0.f, camera_y_offset, 0.f));
        auto view = glm::lookAt(glm::vec3(0., 0., 0.),
            glm::vec3(0., 0., -animation.cube_animation.offset_z),
            glm::vec3(0., 1., 0.));
        animation.view = zoom_translate * rotation * camera_vertical * view;
    }
    glm::mat4 output_transform(const wf::render_target_t& target)
    {
        auto scale = glm::scale(glm::mat4(1.0), {1, -1, 1});
        return wf::gles::render_target_gl_to_framebuffer(target) * scale;
    }
glm::mat4 calculate_vp_matrix(const wf::render_target_t& dest)
{
    float zoom_factor = animation.cube_animation.zoom;
   
    // NEW: Build scale with row-centered translation wrapper
    auto scale_matrix = glm::scale(glm::mat4(1.0),
        glm::vec3(1. / zoom_factor, 1. / zoom_factor, 1. / zoom_factor));
    // NEW: Translate scene so current row is at Y=0 for zoom, scale, then translate back
    auto to_row_center = glm::translate(glm::mat4(1.0), glm::vec3(0.0f, camera_y_offset, 0.0f));
    auto from_row_center = glm::translate(glm::mat4(1.0), glm::vec3(0.0f, -camera_y_offset, 0.0f));
    auto centered_scale = from_row_center * scale_matrix * to_row_center;
    // Compose: projection * view * centered_scale (applies row-focused zoom)
    return output_transform(dest) * animation.projection * animation.view * centered_scale;
}
// Helper function for smoothstep (add this as a member function or static inline in the class)
float smoothstep(float edge0, float edge1, float x) {
    float t = glm::clamp((x - edge0) / (edge1 - edge0), 0.0f, 1.0f);
    return t * t * (3.0f - 2.0f * t);
}
// Change the signature to take a float depth index
glm::mat4 calculate_model_matrix(int i, float vertical_offset = 0.0f, float scale = 1.0f, bool is_window_layer = false, float layer_depth_index = 0.0f)
{
    // *** NEW: LAYERED DEPTH SETUP ***
    // Set the total number of layers for the depth effect (including the main face at 0)
    const float NUM_DEPTH_LAYERS = 40.0f; // 0.0, 0.2, 0.4, 0.6, 0.8...
    const float MAX_RECESSED_Z = 1.0f; // The deepest plane will be at Z=-0.5f
   
    const float INITIAL_PANE_TILT = glm::radians(0.0f);
   
    // Determine the Z-shift based on the index.
    float z_shift = 0.0f;
   
    if (layer_depth_index > 0.0f) {
        // Calculate a Z-offset from the main plane (z=0)
        // Layer 1.0 -> Z=-0.1, Layer 5.0 -> Z=-0.5 (or similar, depending on how you index it)
        // NOTE: We invert the index so layer 1 is shallowest, layer 5 is deepest.
        float normalized_depth = (layer_depth_index / NUM_DEPTH_LAYERS);
        z_shift = -(normalized_depth * MAX_RECESSED_Z);
    }
   
    // The original window_z_offset logic only applies to the dedicated window layer.
    // The regular z_shift logic below handles the main desktop faces.
    // ... (FLAT GRID LAYOUT / X_OFFSET calculation remains the same) ...
    float horizontal_spacing = CUBE_SPACING;
    float x_offset = (i * horizontal_spacing) + (animation.cube_animation.rotation / animation.side_angle) * horizontal_spacing;
   
    // Initial rotation (unconditional tilt for ALL planes)
    auto rotation = glm::mat4(1.0);
    {
        float tilt_direction = glm::sign(x_offset) * -1.0f;
        rotation = glm::rotate(glm::mat4(1.0f), tilt_direction * INITIAL_PANE_TILT, glm::vec3(0.0f, 1.0f, 0.0f));
    }
   
    // Calculate standard Z offsets (original logic)
    double additional_z = 0.0;
    if (get_num_faces() == 2) { additional_z = 1e-3; }
   
   // Interpolate window Z offset (preserved original meaning for window content)
   double window_z_offset = 0.0;
    if (is_window_layer) // This remains for its original use: putting window content in front of its texture background
    {
        float zoom_factor = animation.cube_animation.zoom;
        zoom_factor = std::max(0.0f, std::min(1.0f, zoom_factor));
        window_z_offset = 0.2 * (1.0 - zoom_factor) + 0.01 * zoom_factor;
    }
   
    // Apply translation using the calculated z_shift (now based on depth index)
    auto translation = glm::translate(glm::mat4(1.0),
        glm::vec3(x_offset, 0, identity_z_offset + additional_z + window_z_offset + z_shift));
   
   
    // ... (scale_matrix and vertical_translation remain the same) ...
    auto scale_matrix = glm::scale(glm::mat4(1.0), glm::vec3(scale, scale, scale));
    auto vertical_translation = glm::translate(glm::mat4(1.0), glm::vec3(0, vertical_offset, 0));
   
    // Physics-based pivot/tilt logic (applies to ALL desktop planes, regardless of depth)
    glm::mat4 tilt = glm::mat4(1.0f);
    glm::mat4 pivot_rotation = glm::mat4(1.0f);
   
    // The tilt logic should apply if tilt is generally needed AND we are NOT drawing the window layer (since windows shouldn't tilt)
    bool is_desktop_layer = !is_window_layer;
   
    bool should_apply_tilt = (animation.in_exit || has_virtual_hit) && is_desktop_layer;
   
    if (should_apply_tilt) {
        // ... (Original tilt and pivot logic runs here, identical to the last working version) ...
        auto cws = output->wset()->get_current_workspace();
        int this_workspace_x = i;
       
        int row_offset = static_cast<int>(std::round(vertical_offset / std::abs(CUBE_VERTICAL_SPACING)));
        int this_workspace_y = cws.y + row_offset;
       
        bool has_windows = workspace_has_windows(this_workspace_x, this_workspace_y);
       
        if (!has_windows) {
            glm::vec3 hit_pos = animation.in_exit ? glm::vec3(0.0f, 0.0f, 0.0f) : virtual_ray_hit_pos;
           
            float dx = x_offset - hit_pos.x;
            float dist_x = std::abs(dx);
           
            // Pivot Rotation
            const float inner_radius = 0.35f * CUBE_SPACING;
            const float outer_radius = 2.8f * CUBE_SPACING;
           
            if (dist_x >= inner_radius && dist_x <= outer_radius) {
                float t = (dist_x - inner_radius) / (outer_radius - inner_radius);
                float rotation_strength = std::sin(t * M_PI);
                float pivot_angle = rotation_strength * glm::radians(40.0f) * (dx > 0 ? 1.0f : -1.0f);
               
                if (!animation.in_exit) {
                    pivot_rotation = glm::rotate(glm::mat4(1.0f), pivot_angle, glm::vec3(0.0f, 1.0f, 0.0f));
                }
            }
           
            // Tilt Effect
            const float max_ws_radius = 4.0f;
            const float ws_dist = dist_x / CUBE_SPACING;
           
            bool in_tilt_range = animation.in_exit || (ws_dist > 1.0f && ws_dist <= max_ws_radius);
           
            if (in_tilt_range) {
                float tilt_factor = smoothstep(1.0f, max_ws_radius, ws_dist);
                float max_tilt_rad = animation.cube_animation.max_tilt;
               
                if (dist_x > 1e-6f && tilt_factor > 0.0f && max_tilt_rad > 0.0f) {
                    float tilt_y = -(dx / dist_x) * max_tilt_rad * tilt_factor;
                   
                    auto rot_y = glm::rotate(glm::mat4(1.0f), tilt_y, glm::vec3(0.0f, 1.0f, 0.0f));
                    tilt = rot_y;
                }
            }
        }
    }
   
    return vertical_translation * translation * tilt * pivot_rotation * scale_matrix * rotation;
}
    /* Render the sides of the cube, using the given culling mode - cw or ccw */
void render_cube(GLuint front_face, std::vector<wf::auxilliary_buffer_t>& buffers,
                 const glm::mat4& vp,
                 float vertical_offset = 0.0f, float scale = 1.0f, bool is_window_layer = false, float layer_depth_index = 0.0f)
{
    GL_CALL(glEnable(GL_DEPTH_TEST));
    GL_CALL(glDepthFunc(GL_LESS));
    GL_CALL(glDepthMask(GL_TRUE));

    // Choose shader based on layer type
    auto& active_program = is_window_layer ? window_program : program;
    active_program.use(wf::TEXTURE_TYPE_RGBA);

    GL_CALL(glFrontFace(front_face));
    static const GLuint indexData[] = {0, 1, 2, 0, 2, 3};

    static GLfloat vertexData[] = {
        -0.5, 0.5,
        0.5, 0.5,
        0.5, -0.5,
        -0.5, -0.5
    };

    static GLfloat coordData[] = {
        0.0f, 1.0f,
        1.0f, 1.0f,
        1.0f, 0.0f,
        0.0f, 0.0f
    };

    // attrib_pointer handles enabling vertex attributes
    active_program.attrib_pointer("position", 2, 0, vertexData);
    active_program.attrib_pointer("uvPosition", 2, 0, coordData);
    active_program.uniformMatrix4f("VP", vp);

auto now = std::chrono::steady_clock::now();
auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
    now.time_since_epoch()).count();
float time_seconds = elapsed / 1000.0f;
GLuint prog_id = active_program.get_program_id(wf::TEXTURE_TYPE_RGBA);
GLint time_loc = glGetUniformLocation(prog_id, "u_time");
if (time_loc >= 0) {
    GL_CALL(glUniform1f(time_loc, time_seconds));
}
GLint brightness_loc = glGetUniformLocation(prog_id, "u_brightness");
if (brightness_loc >= 0 && !is_window_layer) {
    // Set default brightness (will be overridden per-face below)
    GL_CALL(glUniform1f(brightness_loc, 1.0f));
}
    // Set uniforms only for desktop shader (not window shader)
    if (!is_window_layer && tessellation_support)
    {
        active_program.uniform1i("deform", use_deform);
        active_program.uniform1i("light", use_light);
        active_program.uniform1f("ease", animation.cube_animation.ease_deformation);
      
        GLuint prog_id = active_program.get_program_id(wf::TEXTURE_TYPE_RGBA);
        GLint loc = glGetUniformLocation(prog_id, "cameraYOffset");
        if (loc >= 0)
        {
            GL_CALL(glUniform1f(loc, camera_y_offset));
        }
      
        loc = glGetUniformLocation(prog_id, "cubeVerticalOffset");
        if (loc >= 0)
        {
            GL_CALL(glUniform1f(loc, vertical_offset));
        }
    }

    auto cws = output->wset()->get_current_workspace();
    auto grid = output->wset()->get_workspace_grid_size();
    int num_faces = get_num_faces();
    int row_offset = static_cast<int>(std::round(vertical_offset / std::abs(CUBE_VERTICAL_SPACING)));
    int abs_row_y = (cws.y + row_offset + grid.height) % grid.height;  // Handle negative offsets
    float zoom_factor = animation.cube_animation.zoom;
    for (int i = 0; i < num_faces; i++)
    {
        int index = (cws.x + i) % num_faces;
        auto tex_id = wf::gles_texture_t::from_aux(buffers[index]).tex_id;
      
        GL_CALL(glBindTexture(GL_TEXTURE_2D, tex_id));
         auto model = calculate_model_matrix(i, vertical_offset, scale, is_window_layer, layer_depth_index);
        active_program.uniformMatrix4f("model", model);
      
        // NEW: Set per-face brightness for desktop layers only
      if (brightness_loc >= 0 && !is_window_layer) {
            int ws_x = (cws.x + i) % grid.width;
            int ws_y = abs_row_y;
            bool has_windows = workspace_has_windows(ws_x, ws_y);
            float base_brightness = has_windows ? 1.8f : 1.0f;
            float interpolated_brightness = 1.0f + (base_brightness - 1.0f) * (zoom_factor);
            GL_CALL(glUniform1f(brightness_loc, interpolated_brightness));
        }
      
        if (tessellation_support && !is_window_layer)
        {
#ifdef USE_GLES32
            GL_CALL(glDrawElements(GL_PATCHES, 6, GL_UNSIGNED_INT, &indexData));
#endif
        } else
        {
            GL_CALL(glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, &indexData));
        }
    }
}
void render_beam(const wf::scene::render_instruction_t& data, const glm::mat4& vp) {
    if (beam_program.get_program_id(wf::TEXTURE_TYPE_RGBA) == 0) {
        return;
    }
    auto active_centers = get_active_workspace_centers();
    
    // Need at least 2 active workspaces to connect
    if (active_centers.size() < 2) {
        return;
    }
    
    // Extract camera position from view-projection matrix
    glm::mat4 inv_vp = glm::inverse(vp);
    glm::vec4 cam_pos_4 = inv_vp * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f);
    glm::vec3 camera_pos = glm::vec3(cam_pos_4) / cam_pos_4.w;
    
    // Generate quad geometry for each beam segment
    std::vector<GLfloat> vertices;
    std::vector<GLfloat> distances;
    std::vector<GLuint> indices;
    
    float beam_width = 0.2f;
    GLuint vertex_count = 0;
    
    // Create beams connecting consecutive workspaces
    for (size_t i = 0; i < active_centers.size() - 1; i++) {
        glm::vec3 start = glm::vec3(active_centers[i].x, active_centers[i].y, active_centers[i].z);
        glm::vec3 end = glm::vec3(active_centers[i + 1].x, active_centers[i + 1].y, active_centers[i + 1].z);
        
        // Calculate beam direction
        glm::vec3 beam_dir = glm::normalize(end - start);
        
        // Calculate camera-facing perpendicular (billboard technique)
        glm::vec3 to_camera = glm::normalize(camera_pos - (start + end) * 0.5f);
        glm::vec3 right = glm::normalize(glm::cross(beam_dir, to_camera)) * beam_width;
        
        // Fallback if cross product fails (camera aligned with beam)
        if (glm::length(right) < 0.001f) {
            glm::vec3 arbitrary(0.0f, 1.0f, 0.0f);
            if (std::abs(glm::dot(beam_dir, arbitrary)) > 0.99f) {
                arbitrary = glm::vec3(1.0f, 0.0f, 0.0f);
            }
            right = glm::normalize(glm::cross(beam_dir, arbitrary)) * beam_width;
        }
        
        // Create quad vertices (4 corners) - with Y flip
        glm::vec3 v0 = start - right;
        glm::vec3 v1 = start + right;
        glm::vec3 v2 = end - right;
        glm::vec3 v3 = end + right;
        
        // Add vertices with Y flip
        vertices.insert(vertices.end(), {v0.x, -v0.y, v0.z});
        vertices.insert(vertices.end(), {v1.x, -v1.y, v1.z});
        vertices.insert(vertices.end(), {v2.x, -v2.y, v2.z});
        vertices.insert(vertices.end(), {v3.x, -v3.y, v3.z});
        
        // Distance from center: edges are 1.0, center is 0.0
        distances.insert(distances.end(), {1.0f, 1.0f, 1.0f, 1.0f});
        
        // Add center line vertices for the glow core
        vertices.insert(vertices.end(), {start.x, -start.y, start.z});
        vertices.insert(vertices.end(), {end.x, -end.y, end.z});
        distances.insert(distances.end(), {0.0f, 0.0f});
        
        // Create triangles for the quad
        indices.push_back(vertex_count + 0);
        indices.push_back(vertex_count + 1);
        indices.push_back(vertex_count + 2);
        
        indices.push_back(vertex_count + 1);
        indices.push_back(vertex_count + 3);
        indices.push_back(vertex_count + 2);
        
        // Center strip triangles for bright core
        indices.push_back(vertex_count + 0);
        indices.push_back(vertex_count + 4);
        indices.push_back(vertex_count + 2);
        
        indices.push_back(vertex_count + 2);
        indices.push_back(vertex_count + 4);
        indices.push_back(vertex_count + 5);
        
        indices.push_back(vertex_count + 1);
        indices.push_back(vertex_count + 5);
        indices.push_back(vertex_count + 3);
        
        indices.push_back(vertex_count + 1);
        indices.push_back(vertex_count + 4);
        indices.push_back(vertex_count + 5);
        
        vertex_count += 6;
    }
    
    if (vertices.empty()) {
        return;
    }
    
    // Render
    GL_CALL(glEnable(GL_DEPTH_TEST));
    GL_CALL(glDepthFunc(GL_LEQUAL));
    GL_CALL(glEnable(GL_BLEND));
    GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE)); // Additive blending
    
    beam_program.use(wf::TEXTURE_TYPE_RGBA);
    beam_program.uniformMatrix4f("mvp", vp);
    beam_program.uniform3f("color", 1.0f, 0.95f, 0.3f); // Bright yellow-orange
    
    beam_program.attrib_pointer("position", 3, 0, vertices.data());
    beam_program.attrib_pointer("distance", 1, 0, distances.data());
    
    GL_CALL(glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(indices.size()), 
                          GL_UNSIGNED_INT, indices.data()));
    
    beam_program.deactivate();
    GL_CALL(glDisable(GL_BLEND));
}
void render(const wf::scene::render_instruction_t& data,
            std::vector<wf::auxilliary_buffer_t>& buffers,
            std::vector<std::vector<wf::auxilliary_buffer_t>>& buffers_rows,
            std::vector<wf::auxilliary_buffer_t>& buffers_windows,
            std::vector<std::vector<wf::auxilliary_buffer_t>>& buffers_windows_rows)
{
    data.pass->custom_gles_subpass([&]
    {
        if (program.get_program_id(wf::TEXTURE_TYPE_RGBA) == 0)
        {
            load_program();
        }
        GL_CALL(glClearColor(0.0f, 0.0f, 0.0f, 1.0f));
        GL_CALL(glEnable(GL_DEPTH_TEST));
        GL_CALL(glDepthFunc(GL_LESS));
        GL_CALL(glDepthMask(GL_TRUE));
        GL_CALL(glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT));
        // RENDER SHADER BACKGROUND FIRST
       if(star_background)
        {render_shader_background(data.target);}
        // NEW: Render beam overlay after background but before desktops
        auto vp = calculate_vp_matrix(data.target);
        GL_CALL(glClear(GL_DEPTH_BUFFER_BIT));
        render_beam(data, vp);
        GL_CALL(glClear(GL_DEPTH_BUFFER_BIT));
        program.use(wf::TEXTURE_TYPE_RGBA);
        static GLfloat vertexData[] = {
            -0.5, 0.5,
            0.5, 0.5,
            0.5, -0.5,
            -0.5, -0.5
        };
        static GLfloat coordData[] = {
            0.0f, 1.0f,
            1.0f, 1.0f,
            1.0f, 0.0f,
            0.0f, 0.0f
        };
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
       
        if (tessellation_support)
        {
            program.uniform1i("deform", use_deform);
            program.uniform1i("light", use_light);
            program.uniform1f("ease", animation.cube_animation.ease_deformation);
           
            GLint loc = glGetUniformLocation(program.get_program_id(wf::TEXTURE_TYPE_RGBA), "cameraYOffset");
            if (loc >= 0)
            {
                GL_CALL(glUniform1f(loc, camera_y_offset));
            }
        }
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glEnable(GL_BLEND));
        GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA));
       
       
// RESTORE CUBE PROGRAM STATE
        program.use(wf::TEXTURE_TYPE_RGBA);
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glDepthMask(GL_TRUE));
       
        // ============================================
        // 1. NEW LAYERED DESKTOP BACKGROUND (RECESSED PLATES)
        // Loop from deepest layer (index 5) to the frontmost layer (index 1)
        // The layer at index 0 is the main desktop which is drawn with the windows on top.
        // ============================================
       
        const int NUM_RECESSED_PLATES = 5;
        for (int layer_idx = NUM_RECESSED_PLATES; layer_idx >= 1; --layer_idx)
        {
            float depth_index = (float)layer_idx;
           
            // Render BACK FACES for all rows
            for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                // is_window_layer=false: This is still desktop content, but the depth index handles the Z-shift.
                render_cube(GL_CCW, buffers_rows[row], vp, vertical_offset, 1.0f, false, depth_index);
            }
            // Render BACK FACES for current row
            render_cube(GL_CCW, buffers, vp, 0.0f, 1.0f, false, depth_index);
           
            // Render FRONT FACES for all rows
            for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                render_cube(GL_CW, buffers_rows[row], vp, vertical_offset, 1.0f, false, depth_index);
            }
            // Render FRONT FACES for current row
            render_cube(GL_CW, buffers, vp, 0.0f, 1.0f, false, depth_index);
        }
        // ============================================
        // 2. MAIN DESKTOP PLANE (LAYER 0 - The one the windows float over)
        // CONTENT: buffers (desktop content)
        // MATRIX: is_window_layer=false, layer_depth_index=0.0f (z_shift=0.0f, ALL Tilts)
        // ============================================
       
        // BACK FACES
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CCW, buffers_rows[row], vp, vertical_offset, 1.0f, false, 0.0f);
        }
        render_cube(GL_CCW, buffers, vp, 0.0f, 1.0f, false, 0.0f);
       
        // FRONT FACES
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CW, buffers_rows[row], vp, vertical_offset, 1.0f, false, 0.0f);
        }
        render_cube(GL_CW, buffers, vp, 0.0f, 1.0f, false, 0.0f);
       
        // RESTORE STATE for window popout cubes
        program.use(wf::TEXTURE_TYPE_RGBA);
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glDepthFunc(GL_LESS));
        GL_CALL(glDepthMask(GL_TRUE));
       
        // ============================================
        // 1. ORIGINAL: RENDER DESKTOP BACKGROUNDS (NEAR/ANGLED)
        // CONTENT: buffers (desktop content)
        // MATRIX: is_window_layer=false (z_shift=0.0f, INITIAL_PANE_TILT) -> KEEP AS IS
        // ============================================
       
        // BACK FACES - Desktop backgrounds for all rows
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CCW, buffers_rows[row], vp, vertical_offset, 1.0f, false, false); // <--- ADDED 'false'
        }
        render_cube(GL_CCW, buffers, vp, 0.0f, 1.0f, false, false); // <--- ADDED 'false'
       
        // FRONT FACES - Desktop backgrounds for all rows
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CW, buffers_rows[row], vp, vertical_offset, 1.0f, false, false); // <--- ADDED 'false'
        }
        render_cube(GL_CW, buffers, vp, 0.0f, 1.0f, false, false); // <--- ADDED 'false'
       
        // RESTORE STATE for window popout cubes
        program.use(wf::TEXTURE_TYPE_RGBA);
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glDepthFunc(GL_LESS));
        GL_CALL(glDepthMask(GL_TRUE));
       
        // ============================================
        // 2. ORIGINAL: RENDER WINDOWS (FAR/FLAT)
        // CONTENT: buffers_windows (window content)
        // MATRIX: is_window_layer=true (z_shift=0.5f, rotation=I) -> KEEP AS IS
        // ============================================
       
      // if (enable_window_popout)
        {
            float scale = popout_scale_animation;
           
            // BACK FACES - Windows for all rows
            for (int row = (int)buffers_windows_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                render_cube(GL_CCW, buffers_windows_rows[row], vp, vertical_offset, scale, true, false); // <--- ADDED 'false'
            }
            render_cube(GL_CCW, buffers_windows, vp, 0.0f, scale, true, false); // <--- ADDED 'false'
           
            // FRONT FACES - Windows for all rows
            for (int row = (int)buffers_windows_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                render_cube(GL_CW, buffers_windows_rows[row], vp, vertical_offset, scale, true, false); // <--- ADDED 'false'
            }
            render_cube(GL_CW, buffers_windows, vp, 0.0f, scale, true, false); // <--- ADDED 'false'
        }
        // ============================================
        // DEACTIVATE CUBE PROGRAM AND CLEANUP
        // ============================================
        program.deactivate();
       
        // DISABLE DEPTH TEST AND CULL FACE
        GL_CALL(glDisable(GL_CULL_FACE));
        GL_CALL(glDisable(GL_DEPTH_TEST));
       
        // ============================================
        // NOW RENDER CURSOR ON TOP (no depth test!)
        // ============================================
        render_cursor_indicator(vp);
       
        // Final cleanup
        GL_CALL(glDisable(GL_BLEND));
    });
}
wf::effect_hook_t pre_hook = [=] ()
{
    update_view_matrix();
    wf::scene::damage_node(render_node, render_node->get_bounding_box());
   
    if (animation.cube_animation.running() || camera_y_offset.running() || popout_scale_animation.running())
    {
        output->render->schedule_redraw();
    } else if (animation.in_exit)
    {
        deactivate();
    }
};
    wf::signal::connection_t<wf::input_event_signal<wlr_pointer_motion_event>> on_motion_event =
        [=] (wf::input_event_signal<wlr_pointer_motion_event> *ev)
    {
        pointer_moved(ev->event);
        ev->event->delta_x = 0;
        ev->event->delta_y = 0;
        ev->event->unaccel_dx = 0;
        ev->event->unaccel_dy = 0;
    };
void pointer_moved(wlr_pointer_motion_event *ev)
{
    if (animation.in_exit)
    {
        return;
    }
    auto cursor = wf::get_core().get_cursor_position();
    // UPDATE CURSOR INDICATOR AND VIRTUAL HIT ON EVERY MOUSE MOVE
    if (output->is_plugin_active(grab_interface.name))
    {
        Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
        // Always compute virtual hit on the infinite plane
        if (std::abs(ray.dir.z) > 1e-6f) {
            float t = (plane_z - ray.origin.z) / ray.dir.z;
            if (t > 0.0f) {
                virtual_ray_hit_pos = ray.origin + t * ray.dir;
                has_virtual_hit = true;
            } else {
                has_virtual_hit = false;
            }
        } else {
            has_virtual_hit = false;
        }
        // Compute bounded hit for cursor indicator
        HitInfo hit = raycast_to_workspace(ray.origin, ray.dir, output);
        static bool logged_hit = false;
        if (hit.ws.x >= 0)
        {
            // Update cursor indicator position
            cursor_indicator.active = true;
            cursor_indicator.workspace_x = hit.ws.x;
            cursor_indicator.workspace_y = hit.ws.y;
            cursor_indicator.uv_position = hit.local_uv;
            cursor_indicator.world_position = ray.origin + hit.t * ray.dir;
            // Sync virtual hit with actual hit when bounded
            virtual_ray_hit_pos = cursor_indicator.world_position;
            // STORE WHICH WORKSPACE IS AT THE HIT POINT
            hit_workspace_x = hit.ws.x;
            hit_workspace_y = hit.ws.y;
            if (!logged_hit)
            {
                LOGI("pointer_moved: Hit detected! Setting cursor_indicator.active=true");
                LOGI(" workspace=(", hit.ws.x, ",", hit.ws.y, ")");
                LOGI(" UV=(", hit.local_uv.x, ",", hit.local_uv.y, ")");
                logged_hit = true;
            }
        }
        else
        {
            // Not pointing at any workspace face
            cursor_indicator.active = false;
            logged_hit = false;
        }
    } else {
        // When not active, clear virtual hit
        has_virtual_hit = false;
    }
    // ============================================================
    // If dragging a window, move it and check workspace boundaries
// If dragging a window, move it and check workspace boundaries
if (is_dragging_window && dragged_view)
{
    static wf::pointf_t last_virtual_pos = {0, 0}; // NEW: Track prev virtual for delta
    // Get the actual mouse cursor position and convert to 3D ray
    Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
    // Use window layer depth for accurate window dragging
    HitInfo hit = raycast_at_window_depth(ray.origin, ray.dir, output);
    if (hit.ws.x >= 0)
    {
        // Compute virtual cursor position - add workspace offset to get absolute position
        auto bbox = output->get_layout_geometry();
       
        // Apply perspective correction since we're at window depth (NEW: Less aggressive for Y)
        float window_offset = get_window_z_offset();
        float perspective_scale_x = plane_z / (plane_z + window_offset); // Keep for X
        float perspective_scale_y = 1.0f; // NEW: No scale for Y (direct UV) to boost sensitivity
        float centered_uv_x = hit.local_uv.x - 0.5f;
        float centered_uv_y = hit.local_uv.y - 0.5f;
        float corrected_uv_x = (centered_uv_x * perspective_scale_x) + 0.5f;
        float corrected_uv_y = (centered_uv_y * perspective_scale_y) + 0.5f; // Direct UV for Y
       
        float virtual_x = (hit.ws.x + corrected_uv_x) * static_cast<float>(bbox.width);
        float virtual_y = (hit.ws.y + (1.0f - corrected_uv_y)) * static_cast<float>(bbox.height); // Flip Y for screen-down
       
        // NEW: Hybrid for intra-row Y: If same row, add direct mouse delta (smooth fallback)
        float delta_y = cursor.y - last_cursor_pos.y;
        if (hit.row_offset == 0) { // Same row: Boost with raw delta
            virtual_y += delta_y * 0.8f; // 80% mouse sensitivity (tune 0.8f)
            LOGI("INTRA-ROW Y BOOST: delta_y=", delta_y, " virtual_y=", virtual_y);
        }
       
        // FIXED: Update wobbly effect with dragged_view and VIRTUAL position
        move_wobbly(dragged_view, virtual_x, virtual_y);
       
        // Position the window so that the drag_offset point stays under the cursor
        wf::geometry_t new_geom = dragged_view->get_geometry();
        new_geom.x = virtual_x - drag_offset.x;
        new_geom.y = virtual_y - drag_offset.y;
       
        // CRITICAL: Just set the geometry, DON'T move to new workspace during drag
        dragged_view->set_geometry(new_geom);
       
        // DEBUG: Log changes (add UV delta for sensitivity check)
        static float last_uv_y = 0.5f;
        float uv_delta_y = hit.local_uv.y - last_uv_y;
        LOGI("DRAG: Mouse delta_y=", (cursor.y - last_cursor_pos.y), " UV_y=", hit.local_uv.y, " UV_delta_y=", uv_delta_y,
             " virtual_y=", virtual_y, " ws_y=", hit.ws.y);
        last_uv_y = hit.local_uv.y;
        last_virtual_pos = {virtual_x, virtual_y};
       
        // Damage workspace nodes to trigger re-rendering
        if (render_node)
        {
            auto cube_node = std::dynamic_pointer_cast<cube_render_node_t>(render_node);
            if (cube_node)
            {
                cube_node->damage_all_workspace_windows();
            }
        }
    }
    last_cursor_pos = cursor;
    output->render->schedule_redraw();
    return; // Don't rotate camera
}
    last_cursor_pos = cursor;
    output->render->schedule_redraw();
}
void render_cursor_indicator(const glm::mat4& vp)
{
    if (!cursor_indicator.active)
    {
        return;
    }
   
    // CHECK IF CURSOR PROGRAM IS INITIALIZED
    if (cursor_program.get_program_id(wf::TEXTURE_TYPE_RGBA) == 0)
    {
        return;
    }
   
    if (cursor_vbo == 0)
    {
        return;
    }
   
    // Update timestamp for animation
    static auto start_time = std::chrono::steady_clock::now();
    auto now = std::chrono::steady_clock::now();
    cursor_indicator.timestamp = std::chrono::duration<float>(now - start_time).count();
   
    // Calculate the model matrix for the workspace where cursor is pointing
    auto cws = output->wset()->get_current_workspace();
    auto grid = output->wset()->get_workspace_grid_size();
   
    int face_index = cursor_indicator.workspace_x;
    int row_offset = cursor_indicator.workspace_y - cws.y;
    float v_offset = -static_cast<float>(row_offset) * CUBE_VERTICAL_SPACING;
   
    auto model = calculate_model_matrix(face_index, v_offset, 1.0f, false);
   
    // Transform UV to local position on the quad (-0.5 to 0.5)
    float local_x = cursor_indicator.uv_position.x - 0.5f;
    float local_y = -(cursor_indicator.uv_position.y - 0.5f); // Flip Y
   
    // Create cursor model matrix
    glm::mat4 cursor_model = model;
    cursor_model = glm::translate(cursor_model, glm::vec3(local_x, local_y, 0.9f)); // Moved closer (was 0.01f)
    cursor_model = glm::scale(cursor_model, glm::vec3(0.3f, 0.3f, 1.0f)); // MUCH BIGGER (was 0.05f)
   
    glm::mat4 mvp = vp * cursor_model;
   
    // Render cursor
    GL_CALL(glEnable(GL_BLEND));
    GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA));
    GL_CALL(glDisable(GL_DEPTH_TEST)); // Always on top
   
    cursor_program.use(wf::TEXTURE_TYPE_RGBA);
    cursor_program.uniformMatrix4f("mvp", mvp);
    cursor_program.uniform1f("u_time", cursor_indicator.timestamp);
    cursor_program.uniform3f("u_color", 1.0f, 0.0f, 0.0f); // BRIGHT RED (was cyan 0.2, 1.0, 0.8)
   
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, cursor_vbo));
    cursor_program.attrib_pointer("position", 2, 0, nullptr);
   
    GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 34)); // 1 center + 33 points
   
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, 0));
    cursor_program.deactivate();
   
    GL_CALL(glEnable(GL_DEPTH_TEST));
}
void pointer_scrolled(double amount)
{
    LOGI("pointer_scrolled called: amount=", amount, " in_exit=", animation.in_exit);
   
    if (animation.in_exit)
    {
        return;
    }
    animation.cube_animation.ease_deformation.restart_with_end(
        animation.cube_animation.ease_deformation.end);
    float target_zoom = animation.cube_animation.zoom;
    float start_zoom = target_zoom;
   
    LOGI(" Current zoom: start_zoom=", start_zoom);
    target_zoom +=
        std::min(std::pow(target_zoom, 1.5f), ZOOM_MAX) * amount * ZVelocity;
    target_zoom = std::min(std::max(target_zoom, ZOOM_MIN), ZOOM_MAX);
    animation.cube_animation.zoom.set(start_zoom, target_zoom);
    // Get grid info
    auto ws_set = output->wset();
    auto grid = ws_set->get_workspace_grid_size();
   
    // Calculate actual grid dimensions in world space
    float grid_width = (grid.width - 1) * CUBE_SPACING;
    float grid_height = (grid.height - 1) * CUBE_SPACING;
   
    // Calculate required Z distance to see full grid
    float fov = glm::radians(45.0f);
    auto output_geometry = output->get_layout_geometry();
    float aspect = (float)output_geometry.width / output_geometry.height;
   
    float required_z_for_height = grid_height / (2.0f * std::tan(fov / 2.0f));
    float required_z_for_width = grid_width / (2.0f * aspect * std::tan(fov / 2.0f));
    float required_z = std::max(required_z_for_height, required_z_for_width) * CUBE_SPACING;
   
    // Interpolate Z based on zoom
    float zoom_factor = std::min(target_zoom, 1.0f);
    float base_z = identity_z_offset + Z_OFFSET_NEAR;
    float target_offset_z = required_z * (1.0f - zoom_factor) + base_z * zoom_factor;
   
    // DON'T change tilt during scroll zoom - keep it at 180° while in cube mode
    // Tilt only interpolates during exit animation (reset_attribs)
   
    animation.cube_animation.offset_y.restart_with_end(0);
    animation.cube_animation.offset_z.restart_with_end(target_offset_z);
    animation.cube_animation.rotation.restart_with_end(animation.cube_animation.rotation.end);
    // Don't touch max_tilt here - it stays at 180° during cube view
    animation.cube_animation.start();
    output->render->schedule_redraw();
}
    void fini() override
    {
        if (output->is_plugin_active(grab_interface.name))
        {
            deactivate();
        }
        if (cursor_vbo)
        {
            GL_CALL(glDeleteBuffers(1, &cursor_vbo));
        }
        cursor_program.free_resources();
        beam_program.free_resources(); // NEW
        wf::gles::run_in_context_if_gles([&]
        {
           program.free_resources();
window_program.free_resources();
            cap_program.free_resources();
            background_program.free_resources();
           
            if (background_vbo)
            {
                GL_CALL(glDeleteBuffers(1, &background_vbo));
            }
           
            if (top_cap_texture_id)
            {
                GL_CALL(glDeleteTextures(1, &top_cap_texture_id));
            }
            if (bottom_cap_texture_id)
            {
                GL_CALL(glDeleteTextures(1, &bottom_cap_texture_id));
            }
               
            top_cap_buffer.free();
            bottom_cap_buffer.free();
        });
    }
};
class WayfireVerticalExpo : public wf::plugin_interface_t,
    public wf::per_output_tracker_mixin_t<wayfire_cube>
{
    wf::ipc_activator_t rotate_left{"vertical_expo/rotate_left"};
    wf::ipc_activator_t rotate_right{"vertical_expo/rotate_right"};
    wf::ipc_activator_t rotate_up{"vertical_expo/rotate_up"};
    wf::ipc_activator_t rotate_down{"vertical_expo/rotate_down"};
    wf::ipc_activator_t activate{"vertical_expo/activate"};
  public:
    void init() override
    {
        if (!wf::get_core().is_gles2())
        {
            const char *render_type =
                wf::get_core().is_vulkan() ? "vulkan" : (wf::get_core().is_pixman() ? "pixman" : "unknown");
            LOGE("cube: requires GLES2 support, but current renderer is ", render_type);
            return;
        }
        this->init_output_tracking();
        rotate_left.set_handler(rotate_left_cb);
        rotate_right.set_handler(rotate_right_cb);
        rotate_up.set_handler(rotate_up_cb);
        rotate_down.set_handler(rotate_down_cb);
        activate.set_handler(activate_cb);
    }
    void fini() override
    {
        this->fini_output_tracking();
    }
    wf::ipc_activator_t::handler_t rotate_left_cb = [=] (wf::output_t *output, wayfire_view)
    {
        return this->output_instance[output]->move_vp(-1);
    };
    wf::ipc_activator_t::handler_t rotate_right_cb = [=] (wf::output_t *output, wayfire_view)
    {
        return this->output_instance[output]->move_vp(+1);
    };
    wf::ipc_activator_t::handler_t rotate_up_cb = [=] (wf::output_t *output, wayfire_view)
    {
        return this->output_instance[output]->move_vp_vertical(-1);
    };
    wf::ipc_activator_t::handler_t rotate_down_cb = [=] (wf::output_t *output, wayfire_view)
    {
        return this->output_instance[output]->move_vp_vertical(+1);
    };
    wf::ipc_activator_t::handler_t activate_cb = [=] (wf::output_t *output, wayfire_view)
    {
        return this->output_instance[output]->input_grabbed();
    };
};
DECLARE_WAYFIRE_PLUGIN(WayfireVerticalExpo);
//////////////////////////////////