# apple-container.el

[![test](https://github.com/antonhibl/apple-container.el/actions/workflows/test.yml/badge.svg)](https://github.com/antonhibl/apple-container.el/actions/workflows/test.yml)

Emacs front end for Apple's [`container`](https://github.com/apple/container) runtime. It talks to the `container` CLI directly through its `--format json` output, so there's no Docker compatibility layer involved.

Requires Emacs 29.1+ (built-in `transient`, `tabulated-list`, TRAMP). Developed for Emacs 31 on Apple silicon.

## Install

```elisp
;; From a local checkout
(use-package apple-container
  :load-path "~/src/apple-container"
  :commands (apple-container apple-container-containers)
  :bind ("C-c d" . apple-container))

;; Or straight from GitHub (Emacs 30+):
;; (use-package apple-container
;;   :vc (:url "https://github.com/antonhibl/apple-container.el" :rev :newest))
```

GUI Emacs on macOS often doesn't have `/usr/local/bin` on its PATH. The package falls back to `/usr/local/bin/container` automatically; set `apple-container-program` if yours lives elsewhere.

## Use

`M-x apple-container` opens a menu for everything. The list buffers:

| Command | Buffer |
|---|---|
| `apple-container-containers` | `*containers*` |
| `apple-container-images` | `*container images*` |
| `apple-container-volumes` | `*container volumes*` |
| `apple-container-networks` | `*container networks*` |

In any list, `?` shows the actions for that list, `g` refreshes, and `m` / `u` / `U` / `t` mark, unmark, unmark all and toggle marks. Actions apply to marked rows, or the row at point.

Containers list keys:

| Key | Action |
|---|---|
| `RET` | Inspect (pretty JSON) |
| `s` / `S` / `R` | Start / stop / restart |
| `K` | Kill (`C-u` to choose the signal) |
| `D` | Delete (`C-u` to force) |
| `x` | Prune stopped containers |
| `l` / `L` | Logs / follow logs (`C-u l` for the VM boot log) |
| `b` | Interactive shell in a Portal (Ghostty) terminal; `C-u b` starts another |
| `e` | Run a one-off command |
| `f` | Dired inside the container over TRAMP |
| `c` / `C` | Copy a file in / out |
| `a` | Toggle showing stopped containers |

Images: `RET` inspect, `r` run, `p` pull, `P` push, `T` tag, `D` delete, `x` prune. Volumes and networks: `RET` inspect, `+` create, `D` delete, `x` prune.

`apple-container-build` builds from a directory (`C-u` to pick a Dockerfile). `apple-container-run` runs detached (`-d`) runs in a compilation buffer and everything else in a terminal, so `-it` works.

## TRAMP

Files in a running container are reachable as `/container:ID:/path`, with completion of container IDs:

```
C-x C-f /container:web:/etc/nginx/nginx.conf
```

This works for Dired, Magit, `M-x compile`, Eshell and anything else that's TRAMP-aware. Change the method name with `apple-container-tramp-method` (set it before TRAMP loads).

## Options

| Variable | Default |
|---|---|
| `apple-container-program` | auto-detect |
| `apple-container-list-all` | `t` (show stopped containers) |
| `apple-container-terminal-backend` | `auto` (Portal → eat → vterm → term) |
| `apple-container-shell-command` | bash if present, else sh |
| `apple-container-logs-tail` | 500 lines |
| `apple-container-run-default-args` | `"-d"` |

## Tests

```sh
emacs -Q --batch -L . -l test/apple-container-test.el -f ert-run-tests-batch-and-exit
```

The tests use `test/mock-container`, a fake CLI that serves fixture JSON and runs `exec` commands locally, so they don't need macOS.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
