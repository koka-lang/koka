<img align="left" width="100" height="100" src="doc/logo/koka-logo-filled.png"/>

<!--
[<img align="right" src="https://travis-ci.org/koka-lang/koka.svg?branch=master"/>](https://travis-ci.org/koka-lang/koka)
-->
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

&nbsp;

# Koka: a functional language with effects

_Koka v2 is a research language that currently under heavy development with the new C backend_  
_Latest release_: v2.1.6, 2021-06-10 ([Install](#install)).

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

For more information, see:

* [Why Koka?][why]
* The [Koka book][kokabook] for a tour of the Koka language and its specification.
* The [Library documentation][libraries].
* Help with [development](#tasks)


[why-mingen]: https://koka-lang.github.io/koka/doc/book.html#why-mingen
[why-effects]: https://koka-lang.github.io/koka/doc/book.html#why-effects
[why-handlers]: https://koka-lang.github.io/koka/doc/book.html#why-handlers
[why-perceus]: https://koka-lang.github.io/koka/doc/book.html#why-perceus
[why-fbip]: http://koka-lang.github.io/koka/doc/book.html#why-fbip

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

Enjoy,
  Daan Leijen

Special thanks to:
- [Ningning Xie](https://xnning.github.io/) for her work on the theory and practice of evidence passing [[9,6]](#references) and the formalization of Perceus reference counting [[8]](#references).
- [Alex Reinking](https://alexreinking.com/) for the implementation of the Perceus reference counting analysis [[8]](#references).
- And all previous interns working on earlier versions of Koka: Daniel Hillerström, Jonathan Brachthäuser, Niki Vazou, Ross Tate, Edsko de Vries, and Dana Xu.

Releases:
- `v2.1.6`, 2021-06-10: initial support for shallow resumptions, fix space leak with vectors, allow `gcc` with `--fasan`,
  improved `vcpkg` support, add `--fstdalloc` flag, improved VS code syntax highlighting, improved `valgrind` support,
  added `--no-optimize` flag for extended debug information.
- `v2.1.4`, 2021-05-31: remove dependency on cmake, support library linking, support vckpg, updated `std/text/regex`,
  improved Windows installer with `clang` install included, remove dependency on Visual Studio on Windows,
  improved `--fasan` support, fixed space leak on boxed value types, use signed `size_t` internally, various small bug fixes.
- `v2.1.2`, 2021-05-01: various bug fixes, allow pattern bindings in parameters of anonymous functions (by Steven Fontanella),
  initial Emacs syntax highlighting (by Kamoii).
- `v2.1.1`, 2021-03-08: bug fixes, use right-associative (++) for string- and list append (instead of (+)), improved internal 
  string handling.
- `v2.0.16`, 2021-02-14: bug fixes, fix short-circuit evaluation of logical operations, improved utf-8 handling.
- `v2.0.14`, 2020-12-11: bug fixes, improved var escape checking.
- `v2.0.12`, 2020-12-02: syntax highlighting support for VS Code and Atom, improved uninstall, more samples.
- `v2.0.9`, 2020-11-27: now with binary [releases] for Windows, macOS, and Linux.
- `v2.0.7`, 2020-11-23: more small fixes, improved scoped handlers, improved higher-rank type propagation, more samples.
- `v2.0.5`, 2020-11-15: many bug fixes and improvements. Improved codegen, named handlers, added samples, docker support, direct C 
  compilation, local install support.
- `v2.0.0`, 2020-08-21: initial v2 release.

<!--
<img align="right" width="200" src="doc/system-logos.png">
-->

# Install

For Linux and macOS (x64), you can install Koka using:
````
curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh
````
For Windows (x64), open a `cmd` prompt and use:
````
curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat
````
This also installs syntax highlighting for the VS Code and Atom editors.

After installation, verify if Koka installed correctly:
````
$ koka
 _          _           ____
| |        | |         |__  \
| | __ ___ | | __ __ _  __) |
| |/ // _ \| |/ // _` || ___/ welcome to the koka interpreter
|   <| (_) |   <| (_| ||____| version 2.0.12, Dec  2, 2020, libc 64-bit (gcc)
|_|\_\\___/|_|\_\\__,_|       type :? for help, and :q to quit

loading: std/core
loading: std/core/types
loading: std/core/hnd
````

Type ``:q`` to exit the interpreter.

For detailed installation instructions and other platforms see the [releases] page.
It is also straightforward to build the compiler [from source][build].

## Running the compiler

You can compile a Koka source using `-c` (note that all [`samples`](https://github.com/koka-lang/koka/tree/master/samples) are pre-installed):

    $ koka -c samples/basic/caesar.kk
    compile: samples/basic/caesar.kk
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    loading: std/num/double
    loading: std/text/parse
    loading: std/num/int32
    check  : samples/basic/caesar
    linking: samples_basic_caesar
    created: out/v2.0.9/gcc-debug/samples_basic_caesar

and run the resulting executable:

    $ out/v2.0.9/gcc-debug/samples_basic_caesar
    plain  : Koka is a well-typed language
    encoded: Krnd lv d zhoo-wbshg odqjxdjh
    cracked: Koka is a well-typed language


The ``-O2`` flag builds an optimized program. Let's try it on a purely functional implementation
of balanced insertion in a red-black tree ([`rbtree.kk`](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.kk):

    $ koka -O2 -c samples/basic/rbtree.kk
    ...
    linking: samples_basic_rbtree
    created: out/v2.0.10/gcc-drelease/samples_basic_rbtree

    $ time out/v2.0.10/gcc-drelease/samples_basic_rbtree
    420000
    real    0m0.750s
    ...

We can compare this against an in-place updating C++ implementation using ``stl::map``
([``rbtree.cpp``](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.cpp)) (which also uses a
[red-black tree](https://code.woboq.org/gcc/libstdc++-v3/src/c++98/tree.cc.html) internally):

    $ clang++ --std=c++17 -o cpp-rbtree -O3 /usr/local/share/koka/v2.0.12/lib/samples/basic/rbtree.cpp
    $ time ./cpp-rbtree
    420000
    real    0m0.864s
    ...

The excellent performance relative to C++ here (on an AMD 3600XT) is the result of Perceus automatically
transforming the fast path of the pure functional rebalancing to use mostly in-place updates,
closely mimicking the imperative rebalancing code of the hand optimized C++ library.

## Running the interactive compiler

Without giving any input files, the interactive interpreter runs by default:
````
$ koka
 _          _           ____
| |        | |         |__  \
| | __ ___ | | __ __ _  __) |
| |/ // _ \| |/ // _` || ___/ welcome to the koka interpreter
|   <| (_) |   <| (_| ||____| version 2.0.9, Nov 27 2020, libc 64-bit (gcc)
|_|\_\\___/|_|\_\\__,_|       type :? for help

loading: std/core
loading: std/core/types
loading: std/core/hnd
>
````

Now you can test some expressions:

    > println("hi koka")
    check  : interactive
    check  : interactive
    linking: interactive
    created: out\v2.0.9\mingw-debug\interactive

    hi koka

    > :t "hi"
    string

    > :t println("hi")
    console ()

Or load a demo (use ``tab`` completion to avoid typing too much):

    > :l samples/basic/fibonacci
    compile: samples/basic/fibonacci.kk
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    check  : samples/basic/fibonacci
    modules:
      samples/basic/fibonacci

    > main()
    check  : interactive
    check  : interactive
    linking: interactive
    created: out\v2.0.9\mingw-debug\interactive

    The 10000th fibonacci number is 33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875

And quit the interpreter:

    > :q

    I think of my body as a side effect of my mind.
      -- Carrie Fisher (1956)

The ``samples/syntax`` and ``samples/basic`` directories contain various basic Koka examples to start with. If you type:

    > :l samples/

in the interpreter, you can ``tab`` twice to see the available sample files and directories.
Use ``:s`` to see the source of a loaded module.

If you use VS Code or Atom, or if you set the ``koka_editor`` environment variable,
you can type ``:e`` in the interactive prompt to edit your program further. For example,

    > :l samples/basic/caesar
    ...
    check  : samples/basic/caesar
    modules:
        samples/basic/caesar

    > :e 
    
    <edit the source and reload>

    > :r
    ...
    check  : samples/basic/caesar
    modules:
        samples/basic/caesar

    > main()

## What next?

* Read about the [core concepts][why] of Koka.
* Read a [Tour of Koka][tour] in the Koka book.
* Check the [Libraries][libraries] documentation.
* Write some cool Koka programs :-)


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

- Emacs (partially done) and Vim syntax highlighting.
- Improve documentation, landing page etc. Make it easier for people to contribute.
- More examples
- Many library modules are incomplete (like `std/os/file`) or missing (like `std/data/map`).

More advanced projects:

- Update the JavaScript backend to 1) use proper modules instead of amdefine, 2) use the new bigints instead of 
  bigint.js, and 3) add support for int64. This requires mostly changes to `Backend/JS/FromCore.hs` together 
  with `lib/core/core-inline.js`.
- Partially done: see PR #100. A language server for Visual Studio Code and Atom. Koka can already generate a 
  typed [range map](src/Syntax/RangeMap.hs) so this should be managable.
- Package management of Koka modules.
- Proper overloading with (a form of) type classes. (in design phase).

Currently being worked on:

- Various standard optimizations like case-of-case, join points, case-of-known constructor, etc.
- Implement inline specialization where functions like `map`, `fold` etc get specialized for the function 
  with which they are called.
  This is an important optimization for functional style languages to reduce the allocation of lambda's.
  (contact: Steven Fontanella)
- Borrowing analysis for Perceus and improved reuse analysis. (contact: Anton Lorenzen)


The following is the immediate todo list to be completed in the coming months:

- Port `std/async` (using `libuv`).
- Improve compilation of local state to use local variables directly (in C).

Contact me if you are interested in tackling some of these :-)

Main branches:
- `master`: latest stable version.
- `dev`: current development branch -- submit PR's to this branch.
- `v1-master`: last stable version of Koka v1: this is Koka with the Javascript (and C#) backend which does not use evidence translation.
               This version supports `std/async` and should compile examples from published papers.

Recently completed tasks:

- Ported `std/text/regex` (using PCRE)
- Run the full test suite.
- Run the Bayesian probalistic machine learning program with large parameters.
- Functions with a pattern match in the argument (by Steven Fontanella).


# Build from source

Koka has few dependencies and should build from source
without problems on most common platforms, e.g. Windows (including WSL), macOS X, and
Unix. The following programs are required to build Koka:

* [Stack](https://docs.haskellstack.org/) to run the Haskell compiler.  
  (use `> curl -sSL https://get.haskellstack.org/ | sh` on Unix and macOS X)
* Optional: the [NodeJS](http://nodejs.org) runtime if using the Javascript backend.
* Optional: On Windows it is recommended to install the [clang][winclang] C compiler, or [Visual Studio](https://visualstudio.microsoft.com/downloads/).

Build the compiler (note the `--recursive` flag):
```
$ git clone --recursive https://github.com/koka-lang/koka
$ cd koka
$ stack build
$ stack exec koka
```
You can also use `stack build --fast` to build a debug version of the compiler.

## Source install

You can also build a local distribution bundle yourself from source and install
that locally. The `util/bundle.kk` script creates a local distribution:
```
$ stack exec koka -- util/bundle
...
distribution bundle created.
  bundle : dist/koka-v2.0.9-linux-amd64.tar.gz
  cc     : gcc
  version: v2.0.9
```
This takes a while as it pre-compiles the standard libraries in three build
variants (`debug`, `drelease` (release with debug info), and `release`).
After generating the bundle, it can be installed locally as:
```
$ util/install.sh -b dist/koka-v2.0.9-linux-amd64.tar.gz
```
(use `util/install.bat` on Windows). After installation, you can now directly invoke `koka`:

```
$ koka --version
```
Koka is by default installed for the current user in `<prefix>/bin/koka`,
(with architecture specific files under `<prefix>/lib/koka/v2.x.x`
and libraries and samples under `<prefix>/share/koka/v2.x.x`).

## Source install on Windows

On Windows, the default install is to the userprofile `%APPDATA%\local` which
is usually already on the search path as `stack` is installed there as well.
However, when using `koka` you need to have a C compiler (when
using `stack exec koka` the C compiler supplied with `ghc` is used (`mingw`)
but that is not generally available).

It is recommended to install the [clang][winclang] compiler for
Windows (which is automatically installed when running `util/install.bat`)
Koka can also use the Microsoft Visual C++ compiler (`cl`) if you run `koka` from a
[Visual Studio x64 toolset](vsprompt) command prompt (in order to link correctly with the Windows system libraries).

Generally, for Koka code, `mingw` (`gcc`) optimizes best, closely followed `clang-cl`.
On a 3.8Gz AMD 3600XT, with `mingw` 7.2.0, `clang-cl` 11.0.0, and `cl` 19.28 we get:
```
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

9. Ningning Xie and Daan Leijen: &ldquo; Generalized Evidence Passing for Effect Handlers&rdquo; MSR-TR-2021-5, Mar, 2021. 
[pdf](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/)
