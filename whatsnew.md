# Welcome to Koka

<img style="float: right; width:12em" src="images/koka-logo-filled.svg">

Koka is a strongly typed functional-style language with effect types and handlers --
generating direct C code without needing a runtime system. To learn more:

* Read the [Koka book][kokabook] for a tour of the Koka language and its specification.
* View the Koka __Samples__ in VS Code by opening the command panel (`Ctrl/Cmd+Shift+P`),
  and running the `Koka: Open samples` command.  
  (when you start typing the command will surface to the top).

### v3.1.1, 2024-03-03

- Fix crash in language server; fix build on older gcc versions.

### v3.1.0, 2024-02-14

- Language Server now supports the stdio protocol via the `--language-server --lsstdio` combination of flags.

- Building is now highly concurrent with much faster build times.

- Internal redesign of (named) effect generation to match the formal systems more closely. See `samples/handlers/named` for examples. 

- Clarified evidence vector API, and various other improvements and bug fixes.

### v3.0.4, 2024-01-25

- Improved VS Code integration with better hover and inlay information. 

- Revised `std/core` which is now split in separate modules.

- Fixed infinite expansion bug in the implicit parameter resolver, and various other small bug fixes.

### v3.0.1, 2024-01-13:

<img style="float:right; height:18em; border:1px solid gray; margin:1ex" src="images/impliciteq.png"> 

- Introducing __locally qualified names__ to always be able to refer to (overloaded) identifiers explicitly.
  See the `samples/basic/qualifiers` example for more information. (use the `Koka: Open samples` command to open the samples directory).

- __Implicit parameters__ are a new experimental feature that allow parameterized overloading of equality, show, etc.
  See the `samples/basic/implicits` example for more information.

- Many improvements to the VS Code integration. Press and hold `ctrl+alt` (or `ctrl+option` on
  MacOS) to show __inlay hints__ -- showing inferred types, fully qualified names, and implicit arguments.

  <img style="height:5em; border:1px solid grey; margin:1ex" src="images/inlayhints-off.png"> &nbsp;versus&nbsp; <img style="height:5em; border:1px solid grey; margin:1ex" src="images/inlayhints.png">


### v2.6.0, 2023-12-30:

- Using the new __first-class constructor contexts__ to improve efficiency of various `std/core` functions
  like `partition`. See the `samples/syntax/contexts.kk` example for an overview, and 
  the accompanying [paper][fccontext] for an in-depth discussion.

- Using further __fully in-place__ `fip` and `fbip` annotations for various `std/core` functions.
  See the `samples/syntax/fip.kk` example for an overview and the [paper][fip] for a more technical overview.

- Initial VS Code language support with type information, jump to definition,
  run test functions directly from the editor, automatic Koka installation, and many more things.
  Special thanks to [Tim Whiting](https://github.com/TimWhiting) and [Fredrik Wieczerkowski](https://github.com/fwcd) for all their work on making this possible!

- The ability to run `main`, `test...`, and `example...` functions directly from
  the editor by clicking on the `run debug | optimized` code lenses.

  <img style="width:15em; border:1px solid grey" src="images/codelens.png">

- The VS Code extension prompts to automatically install the latest Koka compiler.

- Various bug fixes and extended bit-level operations on `int32`/`int64`.

Enjoy!

[install]: https://koka-lang.github.io/koka/doc/book.html
[why]: https://koka-lang.github.io/koka/doc/book.html#why
[kokabook]: https://koka-lang.github.io/koka/doc/book.html
[tour]: https://koka-lang.github.io/koka/doc/book.html#tour
[libraries]: https://koka-lang.github.io/koka/doc/toc.html
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf
[kokarepo]: https://github.com/koka-lang/koka
[kokaproject]: http://research.microsoft.com/en-us/projects/koka

[fip]: https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf
[fccontext]: https://www.microsoft.com/en-us/research/uploads/prod/2023/07/fiptree-tr-v4.pdf


## Previous Releases

* `v2.4.2`, 2023-07-03: interim release with support for the new `fip` and `fbip` keywords
  to support fully-in-place programming. Various bug fixes and performance
  enhancements.
* `v2.4.0`, 2022-02-07: automatic generation of installation packages for various Linux
  distributions (by [Rubikscraft](https://github.com/rubikscraft)), improved specialization and integer add/sub, add `rbtree-fbip` sample,
  improve grammar (`pub` (instead of `public`, remove private (as it is always default)),
  `final ctl` (instead of `brk`), underscores in number literals, etc),
  rename `double` to `float64`, various bug fixes.
* `v2.3.8`, 2021-12-27: improved `int` performance, various bug fixes, update wasm backend,
  initial conan support, fix js backend.
* `v2.3.6`, 2021-11-26: fix specialization bug, add `std/os/readline` module.
* `v2.3.4`, 2021-11-26: `maybe`-like types are already value types, but now also no longer need heap allocation
  if not nested (and `[Just(1)]` uses the same heap space as `[1]`),
  improved atomic refcounting (by Anton Lorenzen), improved specialization (by Steven Fontanella),
  various small fixes, fix build on freeBSD.
* `v2.3.2`, 2021-10-15: initial wasm support (use `--target=wasm`, and install [emscripten] and [wasmtime]),
  improved reuse specialization (by Anton Lorenzen),
  fix default color scheme for non-dark shells (#190), stack-less free and marking, add `--stack` option,
  [musl] support (use `--cc=musl-gcc`), fix `vcpkg` support on macOS with homebrew installed vcpkg, various bug fixes.
* `v2.3.1`, 2021-09-29: improved TRMC optimizations, and improved reuse
  (the [rbtree](https://github.com/koka-lang/koka/tree/master/test/bench/koka/rbtree.kk) benchmark is faster as C++ now).
  Improved effect operation speed. Allow elision of `->` in anonymous
  function expressions (e.g. `xs.map( fn(x) x + 1 )`) and operation clauses. Allow `ctl` for `control`.
  New default output directory as `.koka` and improved command line options to be more in line with
  other compilers (with `-o` specifying the final output, and `-e` to execute the program).
* `v2.3.0`, 2021-09-20: many changes: new layout rule to [elide braces][nobrace] and no more need to
  parenthesize `if` and `match` conditions (see the [`samples/basic/rbtree`](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.kk) for
  an example of this), updated the JavaScript backend (`--target=js`) to use standard ES6 modules and using the new `BigInt` for arbitrary precision integers, improved runtime layout with support for 128-bit arm CHERI,
  add the `std/num/int64` module and `int64` primitive type, add the [binarytrees](https://github.com/koka-lang/koka/tree/master/test/bench/koka/binarytrees.kk)
  benchmark, initial support for parallel tasks (in `std/os/task`), improved simplification and inlining giving
  much improved effect operations, updated isocline for the interactive environment.
* `v2.2.1`, 2021-09-05: improved optimization, initial parallel tasks, binary-trees benchmark,
  still slightly slower effect handling, upgrade isocline, fix minor bugs.
* `v2.2.0`, 2021-08-26: improved case-of-known simpification (by Rakshika B), improve cross-module specialization
  (by Steven Fontanella), initial borrowing annotations and improved reuse analysis (by Anton Lorenzen),
  improved line editing in the interactive environment, improved inlining. Note: due to the new inline phases,
  effect handling may currently be a tad slower in this release but will be improved for the next release.

* `v2.1.9`, 2021-06-23: initial support for cross-module specialization (by Steven Fontanella).
* `v2.1.8`, 2021-06-17: initial support for macOS M1 and Linux arm64, improved readline, minor fixes.
* `v2.1.6`, 2021-06-10: initial support for shallow resumptions, fix space leak with vectors, allow `gcc` with `--fasan`,
  improved `vcpkg` support, add `--fstdalloc` flag, improved VS code syntax highlighting, improved `valgrind` support,
  added `--no-optimize` flag for extended debug information.
* `v2.1.4`, 2021-05-31: remove dependency on cmake, support library linking, support vckpg, updated `std/text/regex`,
  improved Windows installer with `clang` install included, remove dependency on Visual Studio on Windows,
  improved `--fasan` support, fixed space leak on boxed value types, use signed `size_t` internally, various small bug fixes.
* `v2.1.2`, 2021-05-01: various bug fixes, allow pattern bindings in parameters of anonymous functions (by Steven Fontanella),
  initial Emacs syntax highlighting (by Kamoii).
* `v2.1.1`, 2021-03-08: bug fixes, use right-associative (++) for string- and list append (instead of (+)), improved internal
  string handling.
* `v2.0.16`, 2021-02-14: bug fixes, fix short-circuit evaluation of logical operations, improved utf-8 handling.
* `v2.0.14`, 2020-12-11: bug fixes, improved var escape checking.
* `v2.0.12`, 2020-12-02: syntax highlighting support for VS Code and Atom, improved uninstall, more samples.
* `v2.0.9`, 2020-11-27: now with binary [releases] for Windows, macOS, and Linux.
* `v2.0.7`, 2020-11-23: more small fixes, improved scoped handlers, improved higher-rank type propagation, more samples.
* `v2.0.5`, 2020-11-15: many bug fixes and improvements. Improved codegen, named handlers, added samples, docker support, direct C
  compilation, local install support.
* `v2.0.0`, 2020-08-21: initial v2 release.
