<img align="left" width="100" height="100" src="doc/logo/koka-logo-filled.png"/>

<!--
[<img align="right" src="https://travis-ci.org/koka-lang/koka.svg?branch=master"/>](https://travis-ci.org/koka-lang/koka)
-->
<a  href="https://matrix.to/#/#koka-lang_koka:gitter.im">
<img align="right" style="margin: 5px" src="https://img.shields.io/matrix/koka-lang:matrix.org?label=chat%20on%20matrix"/>
</a>
<a href="https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge">
<img align="right" style="margin: 5px" src="https://badges.gitter.im/koka-lang/koka.svg"/>
</a>

&nbsp;

# Koka: a Functional Language with Effects

_Koka v2 is a research language that currently under heavy development with the new C backend_
_Latest release_: v2.4.0, 2022-02-07 ([Install]).

<a href="https://koka-lang.github.io/koka/doc/book.html#why-handlers"><img align="right" width="300" src="doc/snippet-yield.png" /></a>

Koka is a strongly typed functional-style language with effect types and handlers.

* The core of Koka consists of a small set of well-studied language features, like first-class functions,
  a polymorphic type- and effect system, algebraic data types, and effect handlers. Each of these is
  composable and avoid the addition of “special” extensions by being as general as possible.
* Koka tracks the (side) _effects_ of every
  function in its type, where pure and effectful computations are
  distinguished. The precise effect typing gives Koka _rock-solid
  semantics_ backed by well-studied category theory, which makes Koka
  particularly easy to reason about for both humans and compilers.
* _Effect handlers_ let you define advanced control abstractions,
  like exceptions, async/await, or probabilistic programs,
  as a user library in a typed and composable way.
* [Perceus](https://koka-lang.github.io/koka/doc/book.html#why-perceus) is an advanced compilation method for reference counting.
  Together with [evidence passing][evidence], this lets Koka compile directly to C code _without needing
  a garbage collector or runtime system_.
  Perceus also performs [reuse analysis](https://koka-lang.github.io/koka/doc/book.html#why-fbip) and optimizes
  functional-style programs to use in-place updates when possible.

To learn more:

* [Install] Koka and compile your first programs.
* Read the [Koka book][kokabook] for a tour of the Koka language and its specification.
* Browse the [library documentation][libraries].
* Help with [development](#tasks)

[why-mingen]: https://koka-lang.github.io/koka/doc/book.html#why-mingen
[why-effects]: https://koka-lang.github.io/koka/doc/book.html#why-effects
[why-handlers]: https://koka-lang.github.io/koka/doc/book.html#why-handlers
[why-perceus]: https://koka-lang.github.io/koka/doc/book.html#why-perceus
[why-fbip]: http://koka-lang.github.io/koka/doc/book.html#why-fbip

[install]: https://koka-lang.github.io/koka/doc/book.html
[why]: https://koka-lang.github.io/koka/doc/book.html#why
[kokabook]: https://koka-lang.github.io/koka/doc/book.html
[tour]: https://koka-lang.github.io/koka/doc/book.html#tour
[libraries]: https://koka-lang.github.io/koka/doc/toc.html
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf
[kokarepo]: https://github.com/koka-lang/koka
[kokaproject]: http://research.microsoft.com/en-us/projects/koka

[evidence]: https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/
[releases]: https://github.com/koka-lang/koka/releases
[build]: #build-from-source
[Perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
[vsprompt]: https://docs.microsoft.com/en-us/cpp/build/how-to-enable-a-64-bit-visual-cpp-toolset-on-the-command-line?view=vs-2019
[winclang]: https://llvm.org/builds
[vcpkg]: https://vcpkg.io/en/getting-started.html
[ghcup]: https://www.haskell.org/ghcup
[nobrace]: https://koka-lang.github.io/koka/doc/book.html#sec-layout
[m1arch]: https://cpufun.substack.com/p/setting-up-the-apple-m1-for-native
[bigint]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
[emscripten]: https://emscripten.org/docs/getting_started/downloads.html
[musl]: https://musl.libc.org/
[wasmtime]: https://wasmtime.dev/

Enjoy,
  Daan Leijen

Special thanks to: [Anton Lorenzen](https://antonlorenzen.de/) for his work on frame-limited
reuse in Perceus [[10]](#references), [Ningning Xie](https://xnning.github.io/) for her work on the theory and practice of evidence passing [[9,6]](#references) and the formalization of Perceus reference counting [[8]](#references),
[Alex Reinking](https://alexreinking.com/) for the implementation of the Perceus reference counting analysis [[8]](#references),
and all previous interns working on earlier versions of Koka: Daniel Hillerström, Jonathan Brachthäuser, Niki Vazou, Ross Tate, Edsko de Vries, and Dana Xu.

## Recent Releases

* `v2.4.0`, 2022-02-07: improved specialization and integer add/sub, add `rbtree-fbip` sample,
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
  (the [rbtree](test/bench/koka/rbtree.kk) benchmark is faster as C++ now).
  Improved effect operation speed. Allow elision of `->` in anonymous
  function expressions (e.g. `xs.map( fn(x) x + 1 )`) and operation clauses. Allow `ctl` for `control`.
  New default output directory as `.koka` and improved command line options to be more in line with
  other compilers (with `-o` specifying the final output, and `-e` to execute the program).
* `v2.3.0`, 2021-09-20: many changes: new layout rule to [elide braces][nobrace] and no more need to
  parenthesize `if` and `match` conditions (see the [`samples/basic/rbtree`](samples/basic/rbtree.kk) for
  an example of this), updated the JavaScript backend (`--target=js`) to use standard ES6 modules and using the new [`BigInt`][bigint] for arbitrary precision integers, improved runtime layout with support for 128-bit arm CHERI,
  add the `std/num/int64` module and `int64` primitive type, add the [binarytrees](test/bench/koka/binarytrees.kk)
  benchmark, initial support for parallel tasks (in `std/os/task`), improved simplification and inlining giving
  much improved effect operations, updated isocline for the interactive environment.
* `v2.2.1`, 2021-09-05: improved optimization, initial parallel tasks, binary-trees benchmark,
  still slightly slower effect handling, upgrade isocline, fix minor bugs.
* `v2.2.0`, 2021-08-26: improved case-of-known simpification (by Rakshika B), improve cross-module specialization
  (by Steven Fontanella), initial borrowing annotations and improved reuse analysis (by Anton Lorenzen),
  improved line editing in the interactive environment, improved inlining. Note: due to the new inline phases,
  effect handling may currently be a tad slower in this release but will be improved for the next release.
* [Older release notes](#older-release-notes).

<!--
<img align="right" width="200" src="doc/system-logos.png">
-->

# Install

Koka has [binary installers][install] for Windows (x64), macOS (x64, M1), Linux (x64, arm64), and FreeBSD (x64).
For other platforms, you need to build the compiler from source.

# Build from Source

Koka has few dependencies and should build from source
without problems on most common platforms, e.g. Windows (including WSL), macOS, and
Unix. The following programs are required to build Koka:

* [Stack](https://docs.haskellstack.org/) to run the Haskell compiler.
  Use `brew install haskell-stack` on macOS, `curl -sSL https://get.haskellstack.org/ | sh` on Unix,
  or the binary [installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) on Windows.
* Optional: [vcpkg] to be able to link easily with C libraries.
  Use `brew install vcpkg` on macOS. On other systems use the vcpkg [install][vcpkg]
  instructions (Koka can find vcpkg automatically if installed to `~/vcpkg`).
* Optional: [nodejs](http://nodejs.org) if using the Javascript backend.
* Optional: [emscripten] and [wasmtime] if using the Wasm backend.
* Optional: On Windows it is recommended to install the [clang][winclang] C compiler, or the [Visual Studio](https://visualstudio.microsoft.com/downloads/) C compiler.

Now clone the repository and build the compiler as (note the `--recursive` flag):

```sh
$ git clone --recursive https://github.com/koka-lang/koka
$ cd koka
$ stack update
$ stack build
$ stack exec koka
```

You can also use `stack build --fast` to build a debug version of the compiler.
Use `stack test --fast` to run the test-suite.

(See the [build notes](#build-notes) below if you have issues when running- or installing `stack`).

## Create an Install Bundle

Koka can generate a binary install bundle that can be installed
on the local machine:

```sh
$ stack exec koka -- -e util/bundle
...
distribution bundle created.
  bundle : bundle/v2.3.9/koka-v2.3.9-linux-x64.tar.gz
  cc     : gcc
  version: v2.3.9
```

This takes a while as it pre-compiles the standard libraries in three build
variants (`debug`, `drelease` (release with debug info), and `release`).
After generating the bundle, you can install it locally as:

```sh
$ util/install.sh  bundle/v2.3.9/koka-v2.3.9-linux-x64.tar.gz
```

(use `util/install.bat` on Windows).
After installation, you can now directly invoke `koka`:

```sh
$ koka --version
```

Koka is by default installed for the current user in `<prefix>/bin/koka`,
(with architecture specific files under `<prefix>/lib/koka/v2.x.x`
and libraries and samples under `<prefix>/share/koka/v2.x.x`).
On Unix and macOS the default prefix is `/usr/local` while
on Windows the default prefix is `%LOCALAPPDATA%\koka`.

It is also possible to generate installation packages for
various Linux platforms (RHEL, Debian, Alpine, etc.). See
the [readme][util/packaging] for further information.

# Benchmarks

These are initial benchmarks of Koka v2 with [Perceus] reference counting
versus state-of-the-art memory reclamation implementations in
various other languages. Since we compare across languages we need to
interpret these results with care -- the results depend not only on memory
reclamation but also on the different optimizations performed by each
compiler and how well we can translate each benchmark to that particular
language. We view these results therefore mostly as _evidence that the
current Koka implementation of reference counting is viable and can be competitive_
and _not_ as a direct comparison of absolute performance between languages and systems.

As such, we select here only benchmarks that stress memory allocation, and
we tried to select mature comparison systems that use a range of memory
reclamation techniques and are considered best-in-class. The systems we
compare are, Koka 2.0.3 (compiling the generated C code with gcc 9.3.0),
[OCaml](https://ocaml.org) 4.08.1, [Haskell](https://www.haskell.org) GHC 8.6.5,
[Swift](https://swift.org/) 5.3, [Java](https://www.java.com) SE 15.0.1 with the Hotspot G1 collector,
and [C++](http://www.cplusplus.org) gcc 9.3.0.

<img align="right" width="400" src="doc/bench-amd3600-nov-2020.png" style="border:1px solid black">

The benchmarks are all available in [`test/bench`](test/bench) (see the
readme there for build instructions), and all
stress memory allocation with little computation:
`rbtree` (inserts 42 million items into a red-black tree),
`rbtree-ck` (a variant of `rbtree` that keeps a list of every 5th
subtree and thus shares many subtrees), `deriv`
(the symbolic derivative of a large expression),
`nqueens` (calculates all solutions for the n-queens problem of size 13
into a list, and returns the length of that list where the solution lists
share many sub-solutions), and `cfold` (constant-folding over a large symbolic expression).

Note: in C++, without automatic memory management, many benchmarks are
difficult to express directly as they use persistent and
partially shared data structures. To implement these faithfully would
essentially require manual reference counting. Instead, we use C++ as
our performance baseline: we either use in-place updates
without supporting persistence (as in `rbtree` which uses ``std::map``)
or we do not reclaim memory at all (as in `deriv`, `nqueens`, and `cfold`).

The execution times and peak working set averaged over 10 runs and normalized to Koka are in
the figure on the right (on a 3.8Ghz AMD3600XT on Ubuntu 20.04, Nov 2020).

We can see that even though Koka has currently few
optimizations besides the reference counting ones, it performs very well
compared to these mature systems, often outperforming by a significant
margin -- both in execution time and peak working set.
Clearly, these benchmarks are allocation heavy but it is encouraging
to see this initial performance from Koka.

A full discussion of these benchmarks and systems can be found
in the [Perceus] report.

# Tasks

Please help develop Koka: there are many opportunities to improve Koka or do research with Koka. We need:

* [ ] Emacs (partially done) and Vim syntax highlighting.
* [ ] Add more samples, improve documentation, landing page etc. Make it easier for people to contribute.
* [ ] Many library modules are incomplete (like `std/os/file`) or missing (like `std/data/map`).
* [ ] Update file and os primitives for the JavaScript backend.
* [x] Run the full test suite.
* [x] Run the Bayesian probalistic machine learning program with large parameters.
* [x] Functions with a pattern match in the argument (by Steven Fontanella).
* [x] Support `int64` operations

More advanced projects:

* [x] Update the JavaScript backend to 1) use modern modules instead of amdefine, 2) use the new bigints instead of
  bigint.js, and 3) add support for int64. (landed in the `dev` branch)
* [x] Port `std/text/regex` from v1 (using PCRE)
* [ ] Package management of Koka modules.
* [x] Compile to WASM (using emscripten on the current C backend)
* [ ] Extend TRMC to include (1) return results with pairs (like `unzip` or `partition`), (2) associative functions
      (like `+` in `length`), and (3) mutually recursive functions.
* [ ] Improve compilation of local state to use local variables directly (in C) without allocation. Tricky though due to multiple resumptions.
* [ ] Improve performance of array/mutable reference programming. Koka is has great performance for
      algebraic datatypes but lags when using more imperative array algorithms. This requires better
      integration with the reference counting (faster in-place update for vectors) and integration local mutable references.
* [ ] To support optimal Btree's we need _mutable fields_ in constructors, and perhaps intrusive vector fields.
* [ ] The current parallel task support is very basic; we need a great work-stealing thread pool, LVar's etc.
* [ ] Expose the "bytes" primitive data together with views..
* [ ] Improve C code generation by identifying output that could be better; also in effectful code we generate many join-points (see [9]),
      can we increase the sharing/reduce the extra code.
* [ ] The compiler always analyses module dependencies and builds any needed dependencies. The current code
      (in `src/Compiler/Compile.hs`) is not great and it would be nice to factorize the "make" functionality out
      and also allow for parallel builds.

Master/PhD level:

* [ ] Better language level FBIP support with guaranteed datatype matching, automatic derivative and visitor generation.
* [ ] Can we use C++ exceptions to implement "zero-cost" `if yielding() ...` branches and remove the need join points (see [9]).
* [x] Float up `open` calls to improve effect handling (worked on by Naoya Furudono)
* [x] Formalize opening and closing effect row types (worked on by Kazuki Ikemori)

Currently being worked on:

* [x] Various standard optimizations like case-of-case, join points, case-of-known constructor, etc.
* [x] Implement inline specialization where functions like `map`, `fold` etc get specialized for the function
  with which they are called. This is an important optimization for functional style languages to reduce the allocation of lambda's.
  (contact: Steven Fontanella)
* [x] Borrowing analysis for Perceus and improved reuse analysis. (contact: Anton Lorenzen)
* [ ] Improve case-of-known simplification with shape information

The following is the immediate todo list to be completed in the coming months:

* [ ] Port `std/async` (using `libuv`).
* [ ] Proper overloading with (a form of) type classes. (in design phase).

LSP Related Tasks:
* [ ] Generate completions for effect handlers (with empty bodies of all the functions)
* [ ] Generate show / (==) for datatypes
* [ ] Find references

Extension Related Tasks:

VSCode:
* [ ] Add support for running an executable
* [ ] Add support for debugging an executable

Contact me if you are interested in tackling some of these :-)

# Build Notes

## Branches

The main development branches are:
* `master`: latest stable version.
* `dev`: current development branch -- submit PR's to this branch.
* `v1-master`: last stable version of Koka v1: this is Koka with the Javascript (and C#)
  backend which does not use evidence translation.
  This version supports `std/async` and should compile examples from published papers.

## Building on macOS M1

You need at least `stack` version 2.7.4.
Furthermore, you may need to add the `brew`
installed LLVM to your path afterwards, or otherwise stack cannot find the LLVM tools.
Add the following to your `~/.zshrc` script and open an fresh prompt:

```sh
export PATH=/opt/homebrew/opt/llvm/bin:$PATH
```

<!--
Moreover, sometimes `stack` segfaults but running it inside `bash` seems to resolve the issue.
Also, we need to tell stack to use the system installed ghc and skip the version check as
it can currently not install GHC for arm64 yet:
```
bash:~$ git clone --recursive https://github.com/koka-lang/koka
bash:~$ cd koka
bash:~/koka$ stack --system-ghc --skip-ghc-check build
bash:~/koka$ stack --system-ghc --skip-ghc-check exec koka
```

and pass the `--system-ghc` flag to create an installation bundle as well:
```
bash:~/koka$ stack --system-ghc --skip-ghc-check exec koka -- -e util/bundle -- --system-ghc
```
-->

## Building with Cabal

Some platforms (like Linux arm64 and FreeBSD) do not
always support `stack` well. In these cases we can also
use `ghc` and `cabal` directly. Install these packages as:

```sh
$ sudo apt update
$ sudo apt install ghc cabal-install
```

On macOS (x64 and arm64) we use `brew` instead:

```sh
$ brew install pkg-config ghc cabal-install
```

On FreeBSD, use `pkg`:

```sh
$ sudo pkg update
$ sudo pkg install ghc hs-cabal-install   # or: hs-haskell-platform
```

Optionally, install `vcpkg` as well. If you
install this in the `~/vcpkg` directory Koka will find
it automatically when needed:

```sh
~$ git clone https://github.com/microsoft/vcpkg
~$ ./vcpkg/bootstrap-vcpkg.sh
~$ vcpkg/vcpkg install pcre
```

We can now build the compiler using `cabal` as:

```sh
~$ git clone --recursive https://github.com/koka-lang/koka
~$ cd koka
~/koka$ cabal new-update
~/koka$ cabal new-build
~/koka$ cabal new-run koka
```

We can also run tests as:

```sh
~/koka$ cabal new-run koka-test
```

or create an installer:

```sh
~/koka$ cabal new-run koka -- -e util/bundle
```

## Building with minbuild

If neither `stack` nor `cabal` are functional, you may try to
run the minimal build script to build Koka:

```sh
~/koka$ ./util/minbuild.sh
```

which directly invokes `ghc` to build the compiler.
You can create an install bundle from a minbuild as:

```sh
~/koka$ .koka/minbuild/koka -e util/bundle.kk -- --koka=.koka/minbuild/koka
```

## Windows C Compilers

The Koka compiler on Windows requires a C compiler. By default
when using `stack exec koka` the C compiler supplied with `ghc` is used (`mingw`)
but that is only visible within a stack environmet.

It is therefore recommended to install the [clang][winclang] compiler for
Windows (which is automatically installed when running `util/install.bat`).
However, Koka can also use the Microsoft Visual C++ compiler (`cl`) if you
run `koka` from a [Visual Studio x64 toolset](vsprompt) command prompt (in
order to link correctly with the Windows system libraries).

Generally, for Koka code, `mingw` (`gcc`) optimizes best, closely followed `clang-cl`.
On a 3.8Gz AMD 3600XT, with `mingw` 7.2.0, `clang-cl` 11.0.0, and `cl` 19.28 we get:

```sh
$ stack exec out\v2.0.5\mingw-release\test_bench_koka_rbtree -- --kktime
420000
info: elapsed: 0.624s, user: 0.625s, sys: 0.000s, rss: 163mb

$ out\v2.0.5\clang-cl-release\test_bench_koka_rbtree --kktime
420000
info: elapsed: 0.727s, user: 0.734s, sys: 0.000s, rss: 164mb

$ out\v2.0.5\cl-release\test_bench_koka_rbtree --kktime
420000
info: elapsed: 1.483s, user: 1.484s, sys: 0.000s, rss: 164mb
```

## Language Server

The language server is started by running the koka compilter with the --language-server flag
and then connecting to it with a client that supports the Language Server Protocol (LSP)

For example, using VSCode, install the Koka extension or run the extension debug configuration in the project.
Open up a folder and start editing `.kk` files. (The extension finds the koka executable and then automatically starts the language server for you).

The VSCode extension searches in the following locations for the koka executable:
- A koka development environment in ~/koka
- A local install ~/.local/bin
- The PATH environment variable

To specify the command to start the language server manually, such as to provide additional flags to the koka compiler override the `koka.languageServer.command` VSCode setting.
To specify the current working directory to run the compiler from use the `koka.languageServer.cwd` setting. If there are problems with the language server, you can enable the `koka.languageServer.trace.server` setting to see the language server logs, or turn off the language server by setting the `koka.languageServer.enable` setting to `false`.

To develop the language server, you can use the VSCode debug configuration in the project.

- Run `npm install && npm run build` in the `support/vscode/koka.language-koka` directory 
- Update the LSP server in the `src/LanguageServer` directory with your changes
- Run `stack build` 
- Restart the debug configuration and make sure a notification pops up that you are using the development version of the koka sdk

## Older Release Notes

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

# References

1. Daniel Hillerström, and Sam Lindley. &ldquo;Liberating Effects with Rows and Handlers.&rdquo; In _Proceedings of the 1st International Workshop on Type-Driven Development_, 15--27. TyDe 2016. Nara, Japan. 2016. doi:[10.1145/2976022.2976033](https://dx.doi.org/10.1145/2976022.2976033).

2. Daan Leijen. &ldquo;Koka: Programming with Row Polymorphic Effect Types.&rdquo; In _Mathematically Structured Functional Programming 2014_. EPTCS. Mar. 2014. arXiv:[1406.2061](http://arxiv.org/abs/1406.2061).

3. Daan Leijen. _Algebraic Effects for Functional Programming_. MSR-TR-2016-29. Microsoft Research. Aug. 2016. <https://www.microsoft.com/en-us/research/publication/algebraic-effects-for-functional-programming>. Extended version of [4].

4. Daan Leijen. &ldquo;Type Directed Compilation of Row-Typed Algebraic Effects.&rdquo; In _Proceedings of Principles of Programming Languages (POPL’17)_. Paris, France. Jan. 2017.

5. Nicolas Wu, Tom Schrijvers, and Ralf Hinze. &ldquo;Effect Handlers in Scope.&rdquo; In _Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell_, 1--12. Haskell ’14. ACM, New York, NY, USA. 2014. doi:[10.1145/2633357.2633358](https://dx.doi.org/10.1145/2633357.2633358)

6. Ningning Xie, Jonathan Brachthäuser, Daniel Hillerström, Philipp Schuster, Daan Leijen. &ldquo;Effect Handlers, Evidently&rdquo;
The 25th ACM SIGPLAN International Conference on Functional Programming (ICFP), August 2020. doi:[10.1145/3408981](https://doi.org/10.1145/3408981), [pdf](https://www.microsoft.com/en-us/research/uploads/prod/2020/07/evidently-with-proofs-5f0b7d860b387.pdf). See also [9] which improves upon this work.

7. Ningning Xie and Daan Leijen. &ldquo;Effect Handlers in Haskell, Evidently&rdquo; The 13th ACM SIGPLAN International Haskell Symposium, August 2020.
[pdf](https://www.microsoft.com/en-us/research/uploads/prod/2020/07/effev.pdf)
See also the [Ev.Eff](https://github.com/xnning/EvEff) and [Mp.Eff](https://github.com/xnning/MpEff) repositories.

8. Alex Reinking, Ningning Xie, Leonardo de Moura, and Daan Leijen: &ldquo; Perceus: Garbage Free Reference Counting with Reuse&rdquo; MSR-TR-2020-42, Nov 22, 2020. Distinguished paper at PLDI'21.
[pdf](https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/)

9. Ningning Xie and Daan Leijen. &ldquo; Generalized Evidence Passing for Effect Handlers&rdquo; In The 26th ACM SIGPLAN International Conference on Functional Programming (ICFP), August 2021.
Also as MSR-TR-2021-5, Mar, 2021.
[pdf](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/)

10. Anton Lorenzen and Daan Leijen. &ldquo; Reference Counting with Frame-Limited Reuse&rdquo; Microsoft Research
technical report MSR-TR-2021-30, Nov 2021, (updated Mar 2022, v2). [pdf](https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse-extended-version/)
