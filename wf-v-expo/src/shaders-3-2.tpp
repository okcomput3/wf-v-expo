static const char *cube_vertex_3_2 =
R"(#version 320 es
in vec3 position;
in vec2 uvPosition;

out vec2 uvpos;
out vec3 vPos;

void main() {
    vPos = position;
    uvpos = uvPosition;
})";

static const char *cube_tcs_3_2 =
R"(#version 320 es
layout(vertices = 3) out;

in vec2 uvpos[];
in vec3 vPos[];

out vec3 tcPosition[];
out vec2 uv[];

#define ID gl_InvocationID

uniform int deform;
uniform int light;

void main() {
    tcPosition[ID] = vPos[ID];
    uv[ID] = uvpos[ID];

    if(ID == 0){
        /* deformation requires tessellation
           and lighting even higher degree to
           make lighting smoother */

        float tessLevel = 1.0f;
        if(deform > 0)
            tessLevel = 30.0f;
        if(light > 0)
            tessLevel = 50.0f;

        gl_TessLevelInner[0] = tessLevel;
        gl_TessLevelOuter[0] = tessLevel;
        gl_TessLevelOuter[1] = tessLevel;
        gl_TessLevelOuter[2] = tessLevel;
    }
})";

static const char *cube_tes_3_2 =
R"(#version 320 es
layout(triangles) in;

in vec3 tcPosition[];
in vec2 uv[];

out vec2 tesuv;
out vec3 tePosition;

uniform mat4 model;
uniform mat4 VP;
uniform int  deform;
uniform float ease;

vec2 interpolate2D(vec2 v0, vec2 v1, vec2 v2) {
    return vec2(gl_TessCoord.x) * v0
         + vec2(gl_TessCoord.y) * v1
         + vec2(gl_TessCoord.z) * v2;
}

vec3 interpolate3D(vec3 v0, vec3 v1, vec3 v2) {
    return vec3(gl_TessCoord.x) * v0
         + vec3(gl_TessCoord.y) * v1
         + vec3(gl_TessCoord.z) * v2;
}


vec3 tp;
void main() {
    tesuv = interpolate2D(uv[0], uv[1], uv[2]);

    tp = interpolate3D(tcPosition[0], tcPosition[1], tcPosition[2]);
    tp = (model * vec4(tp, 1.0)).xyz;

    if(deform > 0) {
        float r = 0.5;
        float d = distance(tp.xz, vec2(0, 0));
        float scale = 1.0;
        if(deform == 1)
            scale = r / d;
        else
            scale = d / r;

        scale = pow(scale, ease);
        tp = vec3(tp[0] * scale, tp[1], tp[2] * scale);
    }

    tePosition = tp;
    gl_Position = VP * vec4 (tp, 1.0);
})";

static const char* cube_geometry_3_2 =
R"(#version 320 es
layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;
in vec2 tesuv[3];
in vec3 tePosition[3];
uniform int  light;
uniform float cameraYOffset;
uniform float cubeVerticalOffset;  // Y position of the cube being rendered
out vec2 guv;
out vec3 colorFactor;
#define AL 0.3    // ambient lighting
#define DL (1.0-AL) // diffuse lighting
void main() {
    // Light position at the same Y as the cube being rendered
    vec3 lightSource = vec3(0, cubeVerticalOffset, 2);
    vec3 lightNormal = normalize(vec3(0, 0, 1));
    
    if(light == 1) {
        vec3 A = tePosition[2] - tePosition[0];
        vec3 B = tePosition[1] - tePosition[0];
        vec3 N = normalize(cross(A, B));
        vec3 center = (tePosition[0] + tePosition[1] + tePosition[2]) / 3.0;
        float d = distance(center, lightSource);
        float ambient_coeff = pow(clamp(2.0 / d, 0.0, 1.0), 10.0);
        float value = clamp(pow(abs(dot(N, lightNormal)), 1.5), 0.0, 1.0);
        float df = AL * ambient_coeff + DL * value;
        colorFactor = vec3(df, df, df);
    }
    else
        colorFactor = vec3(1.0, 1.0, 1.0);
    gl_Position = gl_in[0].gl_Position;
    guv = tesuv[0];
    EmitVertex();
    gl_Position = gl_in[1].gl_Position;
    guv = tesuv[1];
    EmitVertex();
    gl_Position = gl_in[2].gl_Position;
    guv = tesuv[2];
    EmitVertex();
})";


static const char *cube_vertex_3_2_simple = R"(
#version 320 es
layout(location = 0) in highp vec2 position;
layout(location = 1) in highp vec2 uvPosition;

uniform highp mat4 VP;
uniform highp mat4 model;

out highp vec2 guv;
out highp vec3 colorFactor;

void main() {
    guv = uvPosition;
    colorFactor = vec3(1.0, 1.0, 1.0);
    gl_Position = VP * model * vec4(position, 0.0, 1.0);
}
)";


static const char *cube_fragment_3_2_orginal =
R"(#version 320 es
precision highp float;
precision highp sampler2D;
in highp vec2 guv;
in highp vec3 colorFactor;
layout(location = 0) out highp vec4 outColor;
uniform highp sampler2D smp;
void main() {
    highp vec4 texColor = texture(smp, guv);
    outColor = vec4(texColor.rgb * colorFactor, texColor.a);
})";    

static const char *cube_fragment_3_2 =
R"(#version 320 es
precision highp float;
precision highp sampler2D;
in highp vec2 guv;
in highp vec3 colorFactor;
layout(location = 0) out highp vec4 outColor;
uniform highp sampler2D smp;
uniform highp float u_time;

// Function to calculate rounded corner mask
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

// Function to calculate edge anti-aliasing mask
float edgeAntiAliasMask(vec2 uv) {
    float min_edge_dist = min(min(uv.x, 1.0 - uv.x), min(uv.y, 1.0 - uv.y));
    float aa_width = 0.01;
    return smoothstep(0.0, aa_width, min_edge_dist);
}

// Function to calculate edge glow
float edgeGlow(vec2 uv, float radius) {
    // Distance from edges (sides)
    float edge_dist_x = min(uv.x, 1.0 - uv.x);
    float edge_dist_y = min(uv.y, 1.0 - uv.y);
    float edge_dist = min(edge_dist_x, edge_dist_y);
    
    // Distance from corners - calculate actual distance to rounded corner edge
    vec2 centered_uv = uv - 0.5;
    vec2 corner_dist = abs(centered_uv) - (0.5 - radius);
    
    // Check if we're in corner region
    bool in_corner = corner_dist.x > 0.0 && corner_dist.y > 0.0;
    
    float distance_to_use;
    if (in_corner) {
        // In corner: distance from the rounded arc
        float dist_to_corner_center = length(corner_dist);
        distance_to_use = abs(dist_to_corner_center - radius);
    } else {
        // On straight edge
        distance_to_use = edge_dist;
    }
    
    // Create glow falloff
    float glow_width = 0.03; // Width of the glow effect
    float glow = smoothstep(glow_width, 0.0, distance_to_use);
    
    return glow;
}

void main() {
    // Ripple distortion in the middle
    vec2 center = vec2(0.5);
    vec2 dir = guv - center;
    float dist = length(dir);
    float ripple = sin(dist * 20.0 - u_time * 5.0) * 0.01;
    vec2 distorted_uv = guv + normalize(dir) * ripple;
    
    highp vec4 texColor = texture(smp, distorted_uv);
    
    // Apply rounded corners
    float corner_radius = 0.08;
    float corner_mask = roundedCornerMask(guv, corner_radius);
    float edge_mask = edgeAntiAliasMask(guv);
    float final_mask = min(corner_mask, edge_mask);
    
    // Calculate edge glow
    float glow = edgeGlow(guv, corner_radius);
    
    // Glow color (can be customized)
    vec3 glow_color = vec3(0.4, 0.6, 1.0); // Cyan/blue glow
    float glow_intensity = 0.6; // How bright the glow is
    
    // Optional: Animate glow with pulse
    float pulse = sin(u_time * 2.0) * 0.2 + 0.8; // Oscillates between 0.6 and 1.0
    glow *= pulse;
    
    // Apply lighting and glow
    vec3 finalColor = texColor.rgb * colorFactor;
    finalColor += glow_color * glow * glow_intensity;
    
    float finalAlpha = texColor.a * final_mask;
    
    outColor = vec4(finalColor, finalAlpha);
})";



static const char *cube_fragment_3_2_tron =
R"(#version 320 es
precision highp float;
precision highp sampler2D;
in highp vec2 guv;
in highp vec3 colorFactor;
layout(location = 0) out highp vec4 outColor;
uniform highp sampler2D smp;
uniform highp float u_time;

// TRON-STYLE EDGE CONFIGURATION
#define LINE_WIDTH 0.008                      // Thickness of traveling line
#define LINE_LENGTH 0.12                      // Length of traveling segment
#define GLOW_WIDTH 0.55                      // Glow spread
#define ANIMATION_SPEED 0.2                   // Speed of travel
#define BASE_EDGE_GLOW 0.7                    // Subtle constant edge glow
#define NUM_LINES 12                           // Number of traveling lines

// LINE COLORS - Blue, Red, Green, White
#define LINE_COLOR_0 vec3(0.0, 0.5, 1.0)     // Blue
#define LINE_COLOR_1 vec3(0.0, 0.5, 1.0)      // Red
#define LINE_COLOR_2 vec3(0.0, 0.5, 1.0)      // Green
#define LINE_COLOR_3 vec3(0.0, 0.5, 1.0)      // White

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

float edgeAntiAliasMask(vec2 uv) {
    float min_edge_dist = min(min(uv.x, 1.0 - uv.x), min(uv.y, 1.0 - uv.y));
    float aa_width = 0.01;
    return smoothstep(0.0, aa_width, min_edge_dist);
}

float distanceToEdge(vec2 uv) {
    float dist_left = uv.x;
    float dist_right = 1.0 - uv.x;
    float dist_bottom = uv.y;
    float dist_top = 1.0 - uv.y;
    return min(min(dist_left, dist_right), min(dist_bottom, dist_top));
}

float uvToPerimeter(vec2 uv) {
    vec2 p = uv;
    float edge_dist = distanceToEdge(uv);
    
    if (edge_dist == p.y) {
        return p.x;  // Bottom edge
    } else if (edge_dist == (1.0 - p.x)) {
        return 1.0 + p.y;  // Right edge
    } else if (edge_dist == (1.0 - p.y)) {
        return 3.0 - p.x;  // Top edge
    } else {
        return 4.0 - p.y;  // Left edge
    }
}

// Get color for each line based on index
vec3 getLineColor(int index) {
    if (index == 0) return LINE_COLOR_0;
    if (index == 1) return LINE_COLOR_1;
    if (index == 2) return LINE_COLOR_2;
    return LINE_COLOR_3;
}

void main() {
    vec2 center = vec2(0.5);
    vec2 dir = guv - center;
    float dist = length(dir);
    float ripple = sin(dist * 20.0 - u_time * 5.0) * 0.01;
    vec2 distorted_uv = guv + normalize(dir) * ripple;
    
    highp vec4 texColor = texture(smp, distorted_uv);
    
    float corner_radius = 0.08;
    float corner_mask = roundedCornerMask(guv, corner_radius);
    float edge_mask = edgeAntiAliasMask(guv);
    float final_mask = min(corner_mask, edge_mask);
    
    vec3 finalColor = texColor.rgb * colorFactor;
    
    // TRON TRAVELING LINES (4 DIFFERENT COLORED LINES)
    float edge_dist = distanceToEdge(guv);
    
    if (edge_dist < 0.05) {
        float perimeter_pos = uvToPerimeter(guv);
        float spacing = 4.0 / float(NUM_LINES);
        
        float edge_falloff = smoothstep(0.05, 0.0, edge_dist);
        float sharp_line = smoothstep(LINE_WIDTH * 2.0, 0.0, edge_dist);
        float soft_glow = smoothstep(GLOW_WIDTH, 0.0, edge_dist);
        
        // Create 4 lines, each with its own color
        for (int i = 0; i < NUM_LINES; i++) {
            float line_pos = mod(u_time * ANIMATION_SPEED + float(i) * spacing, 4.0);
            float dist_to_line = abs(perimeter_pos - line_pos);
            dist_to_line = min(dist_to_line, abs(dist_to_line - 4.0));
            
            float line_intensity = smoothstep(LINE_LENGTH, 0.0, dist_to_line);
            float trail_intensity = smoothstep(LINE_LENGTH * 2.0, LINE_LENGTH, dist_to_line) * 0.3;
            line_intensity = max(line_intensity, trail_intensity);
            
            // Get the color for this specific line
            vec3 this_line_color = getLineColor(i);
            vec3 this_glow_color = this_line_color * 0.6;
            
            // Apply this line's color
            finalColor += this_line_color * sharp_line * line_intensity * edge_falloff * 2.0;
            finalColor += this_glow_color * soft_glow * line_intensity * edge_falloff;
        }
        
        // Add subtle constant edge glow (white)
        finalColor += vec3(0.5, 0.5, 0.5) * edge_falloff * BASE_EDGE_GLOW * 0.3;
    }
    
    float finalAlpha = texColor.a * final_mask;
    outColor = vec4(finalColor, finalAlpha);
})";