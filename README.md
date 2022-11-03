# Composure

![](./project/assets/textures/title.svg)

Compose your thoughts and functions. You'll be alright.

## Building

This game was written in [GDLisp](https://github.com/eutro/gdlisp) targetting [Godot](https://godotengine.org/) 3.5(.1).

To build you'll need to have [Racket](https://racket-lang.org/)
installed, and a version of [Godot](https://godotengine.org/download)
that you can run, along with the export templates (which you can grab
from the same site, or download through Godot's GUI).

Clone this repository, install the dependencies (make sure you trust
me not to blow up your computer etc.):

```shell
# in the folder with info.rkt
raco pkg install
```

Compile GDLisp to GDScript:

```shell
raco gdlisp ./project
```

Open the project at [./project/project.godot](./project/project.godot)
in Godot, and export according to [their
documentation](https://docs.godotengine.org/en/stable/tutorials/export/exporting_projects.html).
