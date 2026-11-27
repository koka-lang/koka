# Self-hosted compiler modules

Modules of the self-hosted compiler, ported from the Haskell compiler in `src/`. The full self-hosted compiler lives in [koka-community/compiler](https://github.com/koka-community/compiler); this directory holds the modules that build against this repository's standard library, so that a change to the Haskell compiler and the matching change to the self-hosted compiler can land together.
The rules for keeping them in sync are in the root [AGENTS.md](../AGENTS.md), section 3.

## Checking

```sh
stack build --fast
compiler/check.sh
```

`check.sh` compiles every module in `compiler/` and `pretty/` with `--library -c` against `lib/` and `kklib/`, and fails on an error, a warning, or a missing object file.

## Modules

| module | ported from | synced to |
| --- | --- | --- |
| `pretty/pprint` | `src/Lib/PPrint.hs` | `d7b02a2` |
| `pretty/printer` | `src/Lib/Printer.hs` | `32ada87` |
| `compiler/common/color-scheme` | `src/Common/ColorScheme.hs` | `0c7d513` |
| `compiler/common/failure` | `src/Common/Failure.hs` | `48b9d26` |
| `compiler/common/file` | `src/Common/File.hs` | `336bf9b` (behind: `File.hs` has changed since) |
| `compiler/common/range` | `src/Common/Range.hs` | `d026468` |
| `compiler/common/resume-kind` | `src/Common/ResumeKind.hs` | `b8074c2` |
| `compiler/common/syntax` | `src/Common/Syntax.hs` | `eb98b61d` |
| `compiler/compile/cc` | `src/Compile/Options.hs` (C compiler detection) | none recorded |
| `compiler/compile/options` | `src/Compile/Options.hs` | none recorded |
| `compiler/compile/vfs` | `src/Compile/Build.hs` (`VFS`) | none recorded |
| `compiler/lib/trace` | `src/Lib/Trace.hs` | `32ada87` |
| `compiler/platform/config` | `src/Platform/cpp/Platform/Config.hs` | `edc29e7` (approximate) |

The "synced to" column repeats each module's header; the header is authoritative.

## Keeping the copies identical

The modules are copies of koka-community/compiler's, at the same paths.
A fix to one of these modules goes into both repositories.

Known problems that are not fixed yet are in [port-todo.md](port-todo.md).

## What blocks the remaining modules

Measured 2026-11-21 by compiling each module of koka-community/compiler against this `lib/`, with an import-closure scan for the rest. 13 of its 133 modules (`compiler/` and `pretty/`) are here.

Standard-library modules the port imports that `lib/` does not have, with the number of port modules each one blocks (directly or through imports):

| module | blocks |
| --- | --- |
| `std/data/rb-map` | 110 |
| `std/data/rb-set`, `rbtree`, `rbtree-bu` | 102 |
| `std/data/sort` | 89 |
| `std/data/int-map`, `int-set` | 86 |
| `std/data/trie` | 34 |
| `std/data/hash` | 29 |
| `std/data/json` | 24 |
| `std/async/thread` | 18 |
| `std/test` | 105, through two imports (`common/message`, `common/name-collections`) that appear to be unused |

`std/core/list` here has `list/is-prefix-of` and `default/maximum`. Additions to existing `std/core` modules that the port still uses:

| module | additions |
| --- | --- |
| `std/core/list` | `and`, `or`, `indexed`, `monotonic`, `nub`, `nub-by`, `unzip-either`, `zip3`, `key/contains`, `key/get-default`, `key/lookup`, `key/update` |
| `std/core/char` | `is-space`, `to-lower`, `to-upper` |
| `std/core/sslice` | `is-prefix-of` |
| `std/core/vector` | `vector-init-list`, `unchecked-idx`, `unchecked-assign`, `unchecked-vector` |

`interpreter/command` is otherwise ready and needs `char/to-lower`.
`lib/isocline` and `lsp/stdio` need native libraries (isocline, libuv) and are left out for now.
