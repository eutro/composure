#!/usr/bin/env bash

HERE="$(dirname "$(readlink -f $0)")"
cd "$HERE/project"

rm -r ./export/

godot --export "Linux/X11"

godot --export "Windows Desktop"

mkdir -p ./export/HTML5/
# sed one-liner my beloved
sed -si '/\[rendering\]/{$!{N;s#\[rendering\]\n#\0\nquality/driver/driver_name="GLES2"#;ty;P;D;:y}}' ./project.godot
godot --export "HTML5"
sed -si '/\[rendering\]/{$!{N;N;s#\[rendering\]\n\n.*#[rendering]\n#;ty;P;D;:y}}' ./project.godot
cp ./assets/textures/title.svg ./export/HTML5/
