// NOTE: Shader automatically converted from Godot Engine 3.5.1.stable's SpatialMaterial.

shader_type spatial;
render_mode async_visible,blend_mix,depth_draw_opaque,cull_back,diffuse_toon,specular_toon;
uniform vec4 albedo : hint_color;
uniform sampler2D texture_albedo : hint_albedo;
uniform float specular;
uniform float metallic;
uniform float roughness : hint_range(0,1);
uniform float point_size : hint_range(0,128);
uniform vec3 uv1_scale;
uniform vec3 uv1_offset;
uniform vec3 uv2_scale;
uniform vec3 uv2_offset;

uniform vec4 ambient: hint_color;


void vertex() {
	UV=UV*uv1_scale.xy+uv1_offset.xy;
}


void fragment() {
	vec2 base_uv = UV;
	vec4 albedo_tex = texture(texture_albedo,base_uv);
	ALBEDO = albedo.rgb * albedo_tex.rgb;
	METALLIC = metallic;
	ROUGHNESS = roughness;
	SPECULAR = specular;
}
