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


//https://www.shadertoy.com/view/4d3SWM
// Background Fragment Shader - Volumetric space/nebula
static const char *background_fragment_shader = R"(
#version 100
#extension GL_OES_standard_derivatives : enable
precision mediump float;
uniform float u_time;
uniform vec2 u_resolution;
varying vec2 v_uv;

// various noise functions
float Hash3d(vec3 uv)
{
    float f = uv.x + uv.y * 37.0 + uv.z * 521.0;
    return fract(cos(f*3.333)*100003.9);
}

float mixP(float f0, float f1, float a)
{
    return mix(f0, f1, a*a*(3.0-2.0*a));
}

const vec2 zeroOne = vec2(0.0, 1.0);

float noise(vec3 uv)
{
    vec3 fr = fract(uv.xyz);
    vec3 fl = floor(uv.xyz);
    float h000 = Hash3d(fl);
    float h100 = Hash3d(fl + zeroOne.yxx);
    float h010 = Hash3d(fl + zeroOne.xyx);
    float h110 = Hash3d(fl + zeroOne.yyx);
    float h001 = Hash3d(fl + zeroOne.xxy);
    float h101 = Hash3d(fl + zeroOne.yxy);
    float h011 = Hash3d(fl + zeroOne.xyy);
    float h111 = Hash3d(fl + zeroOne.yyy);
    return mixP(
        mixP(mixP(h000, h100, fr.x), mixP(h010, h110, fr.x), fr.y),
        mixP(mixP(h001, h101, fr.x), mixP(h011, h111, fr.x), fr.y)
        , fr.z);
}

float PI = 3.14159265;

#define saturate(a) clamp(a, 0.0, 1.0)

float Density(vec3 p)
{
    float final = noise(p*0.06125);
    float other = noise(p*0.06125 + 1234.567);
    other -= 0.5;
    final -= 0.5;
    final = 0.1/(abs(final*final*other));
    final += 0.5;
    return final*0.0001;
}

void main()
{
    // Convert v_uv to fragCoord
    vec2 fragCoord = v_uv * u_resolution;
    
    // ---------------- First, set up the camera rays for ray marching ----------------
    vec2 uv = fragCoord.xy/u_resolution.xy * 2.0 - 1.0;
    
    // Camera up vector.
    vec3 camUp = vec3(0.0, 1.0, 0.0);
    
    // Camera lookat.
    vec3 camLookat = vec3(0.0, 0.0, 0.0);
    
    // Mouse position (defaulting to center since we don't have mouse input)
    vec2 mouse = u_resolution * 0.5;
    float mx = mouse.x/u_resolution.x*PI*2.0 + u_time * 0.01;
    float my = -mouse.y/u_resolution.y*10.0 + sin(u_time * 0.03)*0.2+0.2;
    
    vec3 camPos = vec3(cos(my)*cos(mx), sin(my), cos(my)*sin(mx))*(200.2);
    
    // Camera setup.
    vec3 camVec = normalize(camLookat - camPos);
    vec3 sideNorm = normalize(cross(camUp, camVec));
    vec3 upNorm = cross(camVec, sideNorm);
    vec3 worldFacing = (camPos + camVec);
    vec3 worldPix = worldFacing + uv.x * sideNorm * (u_resolution.x/u_resolution.y) + uv.y * upNorm;
    vec3 relVec = normalize(worldPix - camPos);
    
    // --------------------------------------------------------------------------------
    float t = 0.0;
    float inc = 0.02;
    float maxDepth = 70.0;
    vec3 pos = vec3(0.0, 0.0, 0.0);
    float density = 0.0;
    
    // ray marching time
    for (int i = 0; i < 37; i++)
    {
        if ((t > maxDepth)) break;
        pos = camPos + relVec * t;
        float temp = Density(pos);
        inc = 1.9 + temp*0.05;
        density += temp * inc;
        t += inc;
    }
    
    // --------------------------------------------------------------------------------
    // Now that we have done our ray marching, let's put some color on this.
    vec3 finalColor = vec3(0.01, 0.1, 1.0) * density * 0.2;
    
    // output the final color with sqrt for "gamma correction"
    gl_FragColor = vec4(sqrt(clamp(finalColor, 0.0, 1.0)), 1.0);
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

GLuint cursor_vbo = 0;  // VBO for cursor circle
GLuint trail_vbo = 0;
glm::vec3 virtual_ray_hit_pos = {0.0f, 0.0f, 0.0f};
bool has_virtual_hit = false;
int hit_workspace_x = -1;  // Which workspace (horizontal index) is at the raycast point
int hit_workspace_y = -1;  // Which workspace row is at the raycast point
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
            return true;  // Window overlaps with this workspace
        }
    }
    
    return false;  // No windows overlap this workspace
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
            LOGI("Generating render instances for view on workspace ", workspace.x, ",", workspace.y);
            
            // Use root node which includes decorations
            auto view_node = view->get_root_node();
            if (view_node)
            {
                size_t before = instances.size();
                view_node->gen_render_instances(instances, push_damage, shown_on);
                LOGI("Generated ", instances.size() - before, " render instances");
            }
        }
        
        LOGI("Total views on workspace ", workspace.x, ",", workspace.y, ": ", view_count);
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
    
//    std::vector<std::vector<wf::scene::render_instance_uptr>> ws_instances_windows;
 //   std::vector<std::vector<std::vector<wf::scene::render_instance_uptr>>> ws_instances_windows_rows;
//    std::vector<wf::region_t> ws_damage_windows;
//    std::vector<std::vector<wf::region_t>> ws_damage_windows_rows;

// continuous window updates
std::vector<std::unique_ptr<wf::scene::render_instance_manager_t>> ws_instance_managers_windows;
std::vector<std::vector<std::unique_ptr<wf::scene::render_instance_manager_t>>> ws_instance_managers_windows_rows;

    // Multiple cube workspaces for all rows
    std::vector<std::vector<std::vector<wf::scene::render_instance_uptr>>> ws_instances_rows;
    std::vector<std::vector<wf::region_t>> ws_damage_rows;
    std::vector<std::vector<wf::auxilliary_buffer_t>> framebuffers_rows;


    std::vector<wf::region_t> ws_damage_windows;
    std::vector<std::vector<wf::region_t>> ws_damage_windows_rows;

    wf::signal::connection_t<wf::scene::node_damage_signal> on_cube_damage =
        [=] (wf::scene::node_damage_signal *ev)
    {
        push_damage(ev->region);
    };

  public:
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
    {}

void schedule_instructions(
    std::vector<wf::scene::render_instruction_t>& instructions,
    const wf::render_target_t& target, wf::region_t& damage) override
{
  

    for (auto& fb : framebuffers_windows)
    {
        fb.free();
    }
    for (auto& row : framebuffers_windows_rows)
    {
        for (auto& fb : row)
        {
            fb.free();
        }
    }

    instructions.push_back(wf::scene::render_instruction_t{
        .instance = this,
        .target   = target.translated(-wf::origin(self->get_bounding_box())),
        .damage   = damage & self->get_bounding_box(),
    });

    auto bbox = self->get_bounding_box();
    damage ^= bbox;

    // Render top cube workspaces (current row) - WITH BACKGROUND
    for (int i = 0; i < (int)ws_instances.size(); i++)
    {
        const float scale = self->cube->output->handle->scale;
        auto bbox = self->workspaces[i]->get_bounding_box();
        framebuffers[i].allocate(wf::dimensions(bbox), scale);

        wf::render_target_t target{framebuffers[i]};
        target.geometry = self->workspaces[i]->get_bounding_box();
        target.scale    = self->cube->output->handle->scale;

        wf::render_pass_params_t params;
        params.instances = &ws_instances[i];
        params.damage    = ws_damage[i];
        params.reference_output = self->cube->output;
        params.target = target;
        params.flags  = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;

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
    
 //   if (self->cube->is_dragging_window)
    {
        // Regenerate instances manually to capture current window positions
        auto dummy_damage = [](wf::region_t){};
        self->regenerate_workspace_instances(i, fresh_instances, dummy_damage);
        instances_ptr = &fresh_instances;
    }
 //   else
 //   {
        // Use cached instances when not dragging
 //       instances_ptr = &ws_instance_managers_windows[i]->get_instances();
 //   }
    
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
            target.scale    = self->cube->output->handle->scale;

            wf::render_pass_params_t params;
            params.instances = &ws_instances_rows[row][i];
            params.damage    = ws_damage_rows[row][i];
            params.reference_output = self->cube->output;
            params.target = target;
            params.flags  = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;

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
      //  else
      //  {
            // Use cached instances when not dragging
      //      instances_ptr = &ws_instance_managers_windows_rows[row][i]->get_instances();
      //  }
        
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
    damage |= vg;  // Add geometry to damage region
    
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
    std::vector<std::shared_ptr<wf::scene::node_t>> workspaces;  // Changed type
    std::vector<std::vector<std::shared_ptr<wf::scene::node_t>>> workspaces_all_rows;  // Changed type
    
    std::vector<std::shared_ptr<wf::scene::node_t>> workspaces_windows;
    std::vector<std::vector<std::shared_ptr<wf::scene::node_t>>> workspaces_windows_rows;
    
    wayfire_cube *cube;
    };

    std::unique_ptr<wf::input_grab_t> input_grab;
    std::shared_ptr<cube_render_node_t> render_node;

    wf::option_wrapper_t<double> XVelocity{"vertical_expo/speed_spin_horiz"},
    YVelocity{"vertical_expo/speed_spin_vert"}, ZVelocity{"vertical_expo/speed_zoom"};
    wf::option_wrapper_t<double> zoom_opt{"vertical_expo/zoom"};
    wf::option_wrapper_t<bool> enable_window_popout{"vertical_expo/enable_window_popout"};
    wf::option_wrapper_t<double> popout_scale{"vertical_expo/popout_scale"};  // e.g., 1.15 = 15% larger
    wf::option_wrapper_t<double> popout_opacity{"vertical_expo/popout_opacity"};  // 0.0 to 1.0
    OpenGL::program_t cap_program;  // Separate program for caps
    wf::option_wrapper_t<bool> enable_caps{"vertical_expo/enable_caps"};
    wf::option_wrapper_t<double> cap_alpha{"vertical_expo/cap_alpha"};
    wf::option_wrapper_t<wf::color_t> cap_color_top{"vertical_expo/cap_color_top"};
    wf::option_wrapper_t<wf::color_t> cap_color_bottom{"vertical_expo/cap_color_bottom"};
    wf::option_wrapper_t<std::string> cap_texture_top{"vertical_expo/cap_texture_top"};
    wf::option_wrapper_t<std::string> cap_texture_bottom{"vertical_expo/cap_texture_bottom"};
    
    OpenGL::program_t background_program;
    GLuint background_vbo = 0;


    struct TrailPoint {
        glm::vec2 position;
        float timestamp;
        float alpha;
    };
    
    std::vector<TrailPoint> cursor_trail;
    const int MAX_TRAIL_POINTS = 20;      // Number of trail points
    const float TRAIL_LIFETIME = 0.3f;    // How long trail lasts (seconds)
    const float TRAIL_SPACING = 0.02f;    // Minimum distance between points

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
    float ndc_y = 1.0f - (2.0f * screen_y) / og.height;  // Flip Y
    
    // Create NDC point
    glm::vec4 ray_clip(ndc_x, ndc_y, -1.0f, 1.0f);
    
    // Get inverse matrices
    auto vp = calculate_vp_matrix(target);
    glm::mat4 inv_vp = glm::inverse(vp);
    
    // Transform to world space
    glm::vec4 ray_world = inv_vp * ray_clip;
    ray_world /= ray_world.w;  // Perspective divide
    
    // Return as direction vector
    glm::vec3 ray_origin(0.0f, 0.0f, 0.0f);  // Camera at origin
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
        if (std::abs(denom) > 1e-6)  // Not parallel
        {
            glm::vec3 p0_to_center = glm::vec3(face_center_world) - ray_origin;
            float t = glm::dot(p0_to_center, normal) / denom;
            
            if (t >= 0 && t < closest_distance)  // In front of camera
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
    int row_offset = 0;  // Relative row offset from current
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
    origin.y -= grid_height;  // Move ray origin down by grid height

    return {origin, dir};
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
    
    // Get all views and filter by target workspace
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
        
        // Check if view is on our target workspace
        auto view_ws = ws_set->get_view_main_workspace(view);
        if (view_ws != target_ws)
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
            LOGI("  ✓ FOUND on target workspace!");
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
            
            HitInfo hit = raycast_to_workspace(ray.origin, ray.dir, output);
            
            if (hit.ws.x >= 0)
            {
                LOGI("Hit workspace (", hit.ws.x, ",", hit.ws.y, ") at UV (", hit.local_uv.x, ",", hit.local_uv.y, ")");
                
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
                float virtual_x = (hit.ws.x + hit.local_uv.x) * static_cast<float>(bbox.width);
                //float virtual_y = (hit.ws.y + (1.0f - hit.local_uv.y)) * static_cast<float>(bbox.height);
                float virtual_y = (hit.ws.y + hit.local_uv.y) * static_cast<float>(bbox.height);
                wf::pointf_t virtual_cursor{virtual_x, virtual_y};
                
                LOGI("Virtual cursor (with WS offset): (", virtual_x, ",", virtual_y, ")");
                LOGI("Cursor indicator updated at workspace (", hit.ws.x, ",", hit.ws.y, ")");
                
                // Find window using virtual cursor
                dragged_view = find_window_at_cursor_on_face(virtual_cursor, hit.ws);
                
                dragged_window_face_index = hit.ws.x;  // Keep for compatibility, now absolute
                
                is_dragging_window = true;
                
                if (dragged_view)
                {
                    auto ws = output->wset()->get_view_main_workspace(dragged_view);
                    drag_start_workspace = ws;
                    
                    // Store the offset from the window position to where the cursor clicked
                    auto geom = dragged_view->get_geometry();
                    drag_offset.x = virtual_cursor.x - geom.x;
                    drag_offset.y = virtual_cursor.y - geom.y;
                    
                    LOGI("✓ Dragging '", dragged_view->get_title(), "' on WS (", 
                         hit.ws.x, ",", hit.ws.y, ")");
                    LOGI("  Window at (", geom.x, ",", geom.y, "), cursor at (", 
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
                // Before clearing the drag state, move the window to the correct workspace
                if (dragged_view)
                {
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
            LOGI("Right-click - deactivating cube");
            
            // Disable cursor indicator when exiting
            cursor_indicator.active = false;
            
            input_ungrabbed();
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
        workspace_offset_y = (dy > 0) ? 1 : -1;  // +dy = move down = +y
        
        // Option 2: If workspace Y increases going UP (row above)
        // workspace_offset_y = (dy > 0) ? -1 : 1;  // +dy = move down = -y
        
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
                LOGI("  ✓ FOUND on current workspace!");
                return tview;
            }
            
            // Otherwise, save as potential match
            if (!best_match)
            {
                best_match = tview;
                LOGI("  ~ Potential match (different workspace)");
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
    } else
    {
#ifdef USE_GLES32
        // Desktop backgrounds - full tessellation shader
        auto id = GL_CALL(glCreateProgram());
        GLuint vss, fss, tcs, tes, gss;
        vss = OpenGL::compile_shader(cube_vertex_3_2, GL_VERTEX_SHADER);
        fss = OpenGL::compile_shader(cube_fragment_3_2, GL_FRAGMENT_SHADER);
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
win_vss = OpenGL::compile_shader(cube_vertex_3_2_simple, GL_VERTEX_SHADER);  // Use simple vertex
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
            -1.0f,  1.0f,
             1.0f,  1.0f
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
    GL_CALL(glDepthFunc(GL_LEQUAL));  // Use LEQUAL
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

        animation.cube_animation.rotation.set(angle, angle);
        animation.cube_animation.zoom.set(zoom, zoom);
        animation.cube_animation.ease_deformation.set(ease, ease);

        animation.cube_animation.offset_y.set(0, 0);
        animation.cube_animation.offset_z.set(offset_z, offset_z);

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

 output->wset()->set_workspace({0, 0});

        render_node = std::make_shared<cube_render_node_t>(this);
        wf::scene::add_front(wf::get_core().scene(), render_node);
        output->render->add_effect(&pre_hook, wf::OUTPUT_EFFECT_PRE);
        output->render->set_require_depth_buffer(true);
        output->wset()->set_workspace({0, 0});


      //  wf::get_core().hide_cursor();
        input_grab->grab_input(wf::scene::layer::OVERLAY);

        auto wsize = output->wset()->get_workspace_grid_size();
        animation.side_angle = 2 * M_PI / float(wsize.width);
        identity_z_offset    = 0.5 / std::tan(animation.side_angle / 2);
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

animation.cube_animation.zoom.set(1.0f, 1.0f);
animation.cube_animation.rotation.set(0, 0);
animation.cube_animation.offset_y.set(0, 0);
animation.cube_animation.offset_z.set(required_z, required_z);
animation.cube_animation.ease_deformation.set(0, 0);
        
reload_background();
        
        popout_scale_animation.animate(1.0, popout_scale);
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
  //  popout_scale_animation.animate(1.0);
    
    // Don't actually deactivate until animation finishes
   // animation.in_exit = true;
    is_dragging_window = false;
    dragged_view = nullptr;
    wf::scene::remove_child(render_node);
    output->render->damage_whole();

    render_node = nullptr;
    output->render->rem_effect(&pre_hook);
  //  output->render->set_require_depth_buffer(false);

wf::gles::run_in_context([&]
{
    GL_CALL(glClear(GL_DEPTH_BUFFER_BIT));
});



    input_grab->ungrab_input();
    output->deactivate_plugin(&grab_interface);
    wf::get_core().unhide_cursor();
    on_motion_event.disconnect();

    /* Figure out how much we have rotated and switch workspace */
    int size = get_num_faces();
    int dvx  = calculate_viewport_dx_from_rotation();
    
    // NEW: Calculate vertical workspace change based on camera position
    int dvy = calculate_viewport_dy_from_camera();

    auto cws = output->wset()->get_current_workspace();
    auto grid = output->wset()->get_workspace_grid_size();
    
    int nvx = (cws.x + (dvx % size) + size) % size;
    int nvy = (cws.y + dvy) % grid.height;
    
    // Clamp to valid workspace range
    nvy = std::max(0, std::min(nvy, grid.height - 1));
    
    output->wset()->set_workspace({nvx, nvy});

has_virtual_hit = false;
virtual_ray_hit_pos = {0.0f, 0.0f, 0.0f};

    /* We are finished with rotation, make sure the next time cube is used
     * it is properly reset */
  //  animation.cube_animation.rotation.set(0, 0);
 //   camera_y_offset.set(0, 0);  // Reset camera position
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
    
    animation.cube_animation.start();
    update_view_matrix();
    output->render->schedule_redraw();
    return true;
}
// Modified reset_attribs to maintain camera position during transitions
void reset_attribs()
{
    animation.cube_animation.zoom.restart_with_end(1.0);
    animation.cube_animation.offset_z.restart_with_end(
        identity_z_offset + Z_OFFSET_NEAR);
    animation.cube_animation.offset_y.restart_with_end(0);
    animation.cube_animation.ease_deformation.restart_with_end(0);
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
      //  reset_attribs();
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
        float current_zoom     = animation.cube_animation.zoom;

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

glm::mat4 calculate_model_matrix(int i, float vertical_offset = 0.0f, float scale = 1.0f, bool is_window_layer = false)
{
    // FLAT GRID LAYOUT: Instead of rotating around Y axis, lay out workspaces horizontally
    float horizontal_spacing = CUBE_SPACING;
    
    // Calculate horizontal position based on workspace index and rotation
    // rotation controls smooth sliding between workspaces
    float x_offset = (i * horizontal_spacing) + (animation.cube_animation.rotation / animation.side_angle) * horizontal_spacing;
    
    // No rotation - all faces point forward toward camera
    auto rotation = glm::mat4(1.0);
    
    double additional_z = 0.0;
    if (get_num_faces() == 2)
    {
        additional_z = 1e-3;
    }
    
   double window_z_offset = 0.0;
    if (is_window_layer)
    {
        window_z_offset = 0.2;  // Move 20% closer to camera
    }
    
    auto translation = glm::translate(glm::mat4(1.0),
        glm::vec3(x_offset, 0, identity_z_offset + additional_z + window_z_offset));
    
    
    // Apply uniform scaling to everything
    auto scale_matrix = glm::scale(glm::mat4(1.0), glm::vec3(scale, scale, scale));
    
    // Apply vertical offset AFTER scaling so it doesn't get scaled
    auto vertical_translation = glm::translate(glm::mat4(1.0),
        glm::vec3(0, vertical_offset, 0));
    
    // NEW: Physics-based pivot/tilt towards raycast hit point (virtual or actual)
    glm::mat4 tilt = glm::mat4(1.0f);
    glm::mat4 pivot_rotation = glm::mat4(1.0f);
    
    if (has_virtual_hit && !is_window_layer) {
        // Calculate which workspace this face represents
        auto cws = output->wset()->get_current_workspace();
        int this_workspace_x = i;  // Workspace horizontal index
        
        // Calculate the row based on vertical_offset
        int row_offset = static_cast<int>(std::round(vertical_offset / std::abs(CUBE_VERTICAL_SPACING)));
        int this_workspace_y = cws.y + row_offset;
        
        // Check if this workspace has any windows on it
        bool has_windows = workspace_has_windows(this_workspace_x, this_workspace_y);
        
        if (!has_windows) {
            // No windows on this workspace - apply rotation
            glm::vec3 hit_pos = virtual_ray_hit_pos;
            
            // ALL ROWS COPY TOP ROW: Only use X distance, ignore vertical offset completely
            float dx = x_offset - hit_pos.x;  // Only horizontal distance matters
            float dist_x = std::abs(dx);      // Distance in X only
            
            // Apply rotation based on X distance only - same for all rows
            const float inner_radius = 0.35f * CUBE_SPACING;
            const float outer_radius = 2.8f * CUBE_SPACING;
            
            if (dist_x >= inner_radius && dist_x <= outer_radius) {
                float t = (dist_x - inner_radius) / (outer_radius - inner_radius);
                float rotation_strength = std::sin(t * M_PI);
                float pivot_angle = rotation_strength * glm::radians(40.0f) * (dx > 0 ? 1.0f : -1.0f);
                
                pivot_rotation = glm::rotate(glm::mat4(1.0f), pivot_angle, glm::vec3(0.0f, 1.0f, 0.0f));
            }
            
            // Tilt effect - also only based on X distance
            const float max_ws_radius = 4.0f;
            const float ws_dist = dist_x / CUBE_SPACING;
            
            if (ws_dist > 1.0f && ws_dist <= max_ws_radius) {
                float tilt_factor = smoothstep(1.0f, max_ws_radius, ws_dist);
                float max_tilt_rad = glm::radians(180.0f);
                
                if (dist_x > 1e-6f && tilt_factor > 0.0f) {
                    float tilt_y = -(dx / dist_x) * max_tilt_rad * tilt_factor;
                    
                    auto rot_y = glm::rotate(glm::mat4(1.0f), tilt_y, glm::vec3(0.0f, 1.0f, 0.0f));
                    tilt = rot_y;
                }
            }
        }
        // If has_windows == true, both tilt and pivot_rotation stay as identity (no rotation)
    }
    
    // Order: vertical_translation * translation * tilt * pivot_rotation * scale_matrix * rotation
    return vertical_translation * translation * tilt * pivot_rotation * scale_matrix * rotation;
}
    /* Render the sides of the cube, using the given culling mode - cw or ccw */
void render_cube(GLuint front_face, std::vector<wf::auxilliary_buffer_t>& buffers, 
                 const glm::mat4& vp,
                 float vertical_offset = 0.0f, float scale = 1.0f, bool is_window_layer = false)
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
    for (int i = 0; i < get_num_faces(); i++)
    {
        int index = (cws.x + i) % get_num_faces();
        auto tex_id = wf::gles_texture_t::from_aux(buffers[index]).tex_id;
        
        GL_CALL(glBindTexture(GL_TEXTURE_2D, tex_id));
        auto model = calculate_model_matrix(i, vertical_offset, scale, is_window_layer);
        active_program.uniformMatrix4f("model", model);
        
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
     //   render_shader_background(data.target);

        GL_CALL(glClear(GL_DEPTH_BUFFER_BIT));

        auto vp = calculate_vp_matrix(data.target);
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
        

        
        // RESTORE CUBE PROGRAM STATE after caps
        program.use(wf::TEXTURE_TYPE_RGBA);
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glDepthMask(GL_TRUE));
        
        // ============================================
        // RENDER DESKTOP BACKGROUNDS (false = desktop)
        // ============================================
        
        // BACK FACES - Desktop backgrounds for all rows
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CCW, buffers_rows[row],  vp,  vertical_offset, 1.0f, false);  // ← false = desktop
        }
        render_cube(GL_CCW, buffers, vp, 0.0f, 1.0f, false);  // ← false = desktop
        
        // FRONT FACES - Desktop backgrounds for all rows
        for (int row = (int)buffers_rows.size() - 1; row >= 0; row--)
        {
            float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
            render_cube(GL_CW, buffers_rows[row], vp, vertical_offset, 1.0f, false);  // ← false = desktop
        }
        render_cube(GL_CW, buffers, vp, 0.0f, 1.0f, false);  // ← false = desktop
        

        // RESTORE STATE for window popout cubes
        program.use(wf::TEXTURE_TYPE_RGBA);
        program.attrib_pointer("position", 2, 0, vertexData);
        program.attrib_pointer("uvPosition", 2, 0, coordData);
        program.uniformMatrix4f("VP", vp);
        GL_CALL(glEnable(GL_CULL_FACE));
        GL_CALL(glDepthFunc(GL_LESS));
        GL_CALL(glDepthMask(GL_TRUE));
        
        // ============================================
        // RENDER WINDOWS (true = windows, in front!)
        // ============================================
        
        if (enable_window_popout)
        {
            float scale = popout_scale_animation;
            
            // BACK FACES - Windows for all rows
            for (int row = (int)buffers_windows_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                render_cube(GL_CCW, buffers_windows_rows[row], vp, vertical_offset, scale, true);  // ← true = windows
            }
            render_cube(GL_CCW, buffers_windows, vp, 0.0f, scale, true);  // ← true = windows
            
            // FRONT FACES - Windows for all rows
            for (int row = (int)buffers_windows_rows.size() - 1; row >= 0; row--)
            {
                float vertical_offset = -(row + 1) * CUBE_VERTICAL_SPACING;
                render_cube(GL_CW, buffers_windows_rows[row], vp, vertical_offset, scale, true);  // ← true = windows
            }
            render_cube(GL_CW, buffers_windows, vp, 0.0f, scale, true);  // ← true = windows
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


// =========================================================================
// EXPLANATION:
// =========================================================================
// The issue was that render_cursor_indicator() was being called BEFORE
// glDisable(GL_DEPTH_TEST), which meant the cursor was being depth-tested
// against the cube faces and getting hidden behind them.
//
// By moving render_cursor_indicator() to AFTER glDisable(GL_DEPTH_TEST),
// the cursor will always render on top regardless of its Z position.
//
// Order matters:
// 1. Render all cubes WITH depth test
// 2. Deactivate cube program
// 3. Disable depth test
// 4. Render cursor (no depth test = always visible on top)
// 5. Disable blend



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

        ev->event->delta_x    = 0;
        ev->event->delta_y    = 0;
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
                LOGI("  workspace=(", hit.ws.x, ",", hit.ws.y, ")");
                LOGI("  UV=(", hit.local_uv.x, ",", hit.local_uv.y, ")");
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
   if (is_dragging_window && dragged_view)
    {
        // Get the actual mouse cursor position and convert to 3D ray
        Ray ray = screen_to_world_ray(cursor.x, cursor.y, output);
        HitInfo hit = raycast_to_workspace(ray.origin, ray.dir, output);
        
        if (hit.ws.x >= 0)
        {
            // Compute virtual cursor position - add workspace offset to get absolute position
            auto bbox = output->get_layout_geometry();
            
            float virtual_x = (hit.ws.x + hit.local_uv.x) * static_cast<float>(bbox.width);
            float virtual_y = (hit.ws.y + (1.0f - hit.local_uv.y)) * static_cast<float>(bbox.height);
            
            // Position the window so that the drag_offset point stays under the cursor
            wf::geometry_t new_geom = dragged_view->get_geometry();
            new_geom.x = virtual_x - drag_offset.x;
            new_geom.y = virtual_y - drag_offset.y;
            
            // CRITICAL: Just set the geometry, DON'T move to new workspace during drag
            // The overlap detection will make it visible on adjacent workspaces
            dragged_view->set_geometry(new_geom);
            
            // DEBUG: Log the window position
            auto ws_set = output->wset();
            auto current_ws = ws_set->get_view_main_workspace(dragged_view);
            LOGI("DRAG: Window at (", new_geom.x, ",", new_geom.y, ") size (", 
                 new_geom.width, "x", new_geom.height, ") on workspace (", 
                 current_ws.x, ",", current_ws.y, ")");
            
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
        return;  // Don't rotate camera
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
    float local_y = -(cursor_indicator.uv_position.y - 0.5f);  // Flip Y
    
    // Create cursor model matrix
    glm::mat4 cursor_model = model;
    cursor_model = glm::translate(cursor_model, glm::vec3(local_x, local_y, 0.9f));  // Moved closer (was 0.01f)
    cursor_model = glm::scale(cursor_model, glm::vec3(0.3f, 0.3f, 1.0f));  // MUCH BIGGER (was 0.05f)
    
    glm::mat4 mvp = vp * cursor_model;
    
    // Render cursor
    GL_CALL(glEnable(GL_BLEND));
    GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA));
    GL_CALL(glDisable(GL_DEPTH_TEST));  // Always on top
    
    cursor_program.use(wf::TEXTURE_TYPE_RGBA);
    cursor_program.uniformMatrix4f("mvp", mvp);
    cursor_program.uniform1f("u_time", cursor_indicator.timestamp);
    cursor_program.uniform3f("u_color", 1.0f, 0.0f, 0.0f);  // BRIGHT RED (was cyan 0.2, 1.0, 0.8)
    
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, cursor_vbo));
    cursor_program.attrib_pointer("position", 2, 0, nullptr);
    
    GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 34));  // 1 center + 33 points
    
    GL_CALL(glBindBuffer(GL_ARRAY_BUFFER, 0));
    cursor_program.deactivate();
    
    GL_CALL(glEnable(GL_DEPTH_TEST));
}

void pointer_scrolled(double amount)
{
    if (animation.in_exit)
    {
        return;
    }

    animation.cube_animation.ease_deformation.restart_with_end(
        animation.cube_animation.ease_deformation.end);

    float target_zoom = animation.cube_animation.zoom;
    float start_zoom  = target_zoom;

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
    
    animation.cube_animation.offset_y.restart_with_end(0);
    animation.cube_animation.offset_z.restart_with_end(target_offset_z);
    animation.cube_animation.rotation.restart_with_end(animation.cube_animation.rotation.end);

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