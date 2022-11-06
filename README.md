# ![Composure](./project/assets/textures/title.svg)

_Compose your thoughts and functions. You'll be alright._

Composure is a game about solving puzzles with functions built in
point-free style: there are no variables, only combinators.

## How to Play

You can download native releases for Linux x86_64 and Windows from
[itch.io](https://eutro.itch.io/composure) (or from CI), or play the Web version
[here](https://composure.eutro.dev). Native versions are recommended since they
run quicker, look prettier, and play audio better. Mobile platforms are
unfortunately not supported.

## Building

This game was written in [GDLisp](https://github.com/eutro/gdlisp) targetting
[Godot](https://godotengine.org/) 3.5.1.

To build you'll need to have [Racket](https://racket-lang.org/)
installed, and a version of [Godot](https://godotengine.org/download)
that you can run, along with the export templates (which you can grab
from the same site, or download through Godot's GUI).

Clone this repository, install the dependencies (make sure you trust
me not to blow up your computer, of course):

```shell
# in the folder with info.rkt
raco pkg install
```

Compile GDLisp to GDScript:

```shell
raco gdlisp ./project
```

Open the project at [./project/project.godot](./project/project.godot) in Godot,
and export according to [their
documentation](https://docs.godotengine.org/en/stable/tutorials/export/exporting_projects.html).

---

You can also look at [how CI is set up](./.github/workflows/build.yml), if you
like reading YAML.

## Licenses

- Code is licensed MIT (see [LICENSE](./LICENSE)).
- Audio is original and licensed here as [CC-BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
  - It's also available on streaming platforms:
    [Spotify](https://open.spotify.com/album/6UnKYWVt2Q7DRZwdYLFmie),
    [Apple Music](https://music.apple.com/gb/album/composure-original-video-game-soundtrack-ep/1653015569).
- Models are mostly sourced from [Quaternius](https://quaternius.com/) and are
  [public domain](https://creativecommons.org/publicdomain/zero/1.0/) here as
  well, if you want my mildly mangled versions specifically.
- Fonts all come with their own license information (OFL).

I also won't personally run after you with a lawyer if you do manage to violate
the copyleft licenses.
