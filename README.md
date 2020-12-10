# rememorate

WIP

## Prerequisites

Unless you enjoy compiling for hours at end, you should use the reflex-platform Nix cache by following the [instructions here][cache].

## Development

Running locally using GHC and jsaddle-warp:

```bash
nix-shell --run 'ghcid -T :main'
# Or, to run with a custom port
nix-shell --run 'JSADDLE_WARP_PORT=8080 ghcid -T :main'
```

Build JS using GHCJS:

```bash
nix-build
open ./result/index.html
```

[cache]: https://github.com/obsidiansystems/obelisk#installing-obelisk
