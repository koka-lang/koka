<img align="left" width="100" height="100" src="doc/logo/koka-logo-v2-800.png"/>

<!--
[<img align="right" src="https://travis-ci.org/koka-lang/koka.svg?branch=master"/>](https://travis-ci.org/koka-lang/koka)
-->
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

&nbsp;

# Koka: a function-oriented language with effect inference

_Note: Koka v2 is currently under heavy development with the new evidence translation and C backend -- various features may be lacking,
documentation may be outdated, not all tests will run, and there may be bugs.
Use the branch [`v1-master`](https://github.com/koka-lang/koka/tree/v1-master) to use the older stable Koka v1 with the Javascript backend._

Koka is a strongly typed, strict functional language which tracks the (side) _effects_ of every function in its type.
Koka syntax is Javascript/C like,
the evaluation is strict like OCaml/C, and the type system is Haskell like, where pure and effectful computations are distinguished.
This gives Koka rock-solid semantics backed by well-studied categorical semantics which makes it particularly easy to reason about (for 
humans and compilers). 

A function without any effect is called _total_ and corresponds to mathematically total functions.
There are the builtin effects for partial functions that can raise exceptions, as _exn_, or potentially non-terminating functions as _div_ (divergent).
The combination of _exn_ and _div_ is called _pure_ as that corresponds to Haskell's notion of purity. 

Koka also has full support for algebraic effect handlers. This enables powerful control-flow abstraction that allows users to define
compositional control-flow abstractions as a library; this includes advanced abstractions like exceptions, iterators, async-await concurrency,
ambient state, backtracking parser combinators, probablistic programming, Bayesian machine learning, etc. Algebraic effect handlers subsume (free) 
monads, and are compositional without needing lifting or monad transformers.

Recent work on [evidence translation](#evidence-translation) and [Perceus](#perceus) precise compiler guided reference counting enable Koka to compile directly 
to plain C code _without needing a garbage collector_ or runtime system. Initial performance benchmarks are very promising, where
a naive functional Koka implementation of a red-black tree balanced insertion ([`rbtree.kk`](test/bench/koka/rbtree.kk)) is within 10% of 
the performance of an in-place updating C++ implementation using `stl::map` ([`rbtree.cpp`](test/bench/cpp/rbtree.cpp)) (which uses the GNU 
[`RBTree`](https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/tsearch.c;h=cdc401a4e5411221ab2feb2baf8745991bde7868;hb=HEAD) implementation internally).
It is our goal to generally fall within a factor 2&times; of C++ performance without needing manual memory management. 

For more background information, see:

* The [A tour of Koka][kokabook] for a specification of the Koka language and a primer on algebraic effect handlers.
* The [library documentation][libraries].
* The [Koka research page][kokaproject].
* The article _Algebraic Effects for Functional Programming_ [[3]](#references) about the algebraic effects in Koka.

[kokabook]: https://koka-lang.github.io/koka/doc/kokaspec.html  
[libraries]: https://koka-lang.github.io/koka/doc/toc.html
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf
[kokarepo]: https://github.com/koka-lang/koka
[kokaproject]: http://research.microsoft.com/en-us/projects/koka
[rise4fun]: http://rise4fun.com/koka/tutorial

Enjoy,
  Daan Leijen
  
Special thanks to:
- [Ningning Xie](https://xnning.github.io/): for her work on the theory of [evidence translation](#evidence-translation) for algebraic effect handlers [6].
- [Alex Reinking](https://alexreinking.com/): for the ongoing work on the [Perceus](#perceus) reference counting analysis.
- And all previous interns working on earlier versions of Koka: Daniel Hillerström, Jonathan Brachthäuser, Niki Vazou, Ross Tate, and Edsko de Vries.
  
(Koka is the Japanese word for _effective_ ([Kōka](https://translate.google.com/#view=home&op=translate&sl=auto&tl=en&text=%E5%8A%B9%E6%9E%9C), 効果)).


## Installing the compiler

At this point there are no binary releases of Koka and you need to build
the compiler yourself. Fortunately, Koka has few dependencies and should build
without problems on most common platforms, e.g. Windows (including WSL), MacOSX, and
Unix.

The following programs are required to build Koka:

* [Stack](https://docs.haskellstack.org/) to run the Haskell compiler .
* [CMake](https://cmake.org/download/) to compile the generated C files (use `> sudo apt-get install cmake` on Ubuntu).
* Optional: The [Ninja](https://ninja-build.org/) build system for faster build times (required on Windows, use `> sudo apt-get install ninja-build` on Ubuntu).
* Optional: the [NodeJS](http://nodejs.org) runtime if using the Javascript backend.

Building Koka:
```
> git clone https://github.com/koka-lang/koka
> cd koka
> stack build
```
You can also use `stack build --fast` to build a debug version of the compiler.
You can invoke the compiler now as: (this takes a while as it needs to build the core libraries as well)
```
> stack exec koka -- -c test/algeff/common
compile: test/algeff/common.kk
loading: std/core
loading: std/core/types
loading: std/core/hnd
check  : test/algeff/common
> cmake --build "out\Debug\cbuild" --target test_algeff_common
...
[5/5] Linking C executable test_algeff_common.exe
compiled: out\Debug\test_algeff_common.exe
```
and run the resulting executable:
```
> out\Debug\test_algeff_common.exe
42
Hello there, there
hi
hi
1
2
[False,True,True,False]
([False,False,True,True,False],2)
```

If you leave out the `-c` flag, Koka will execute the compiled program automatically.
Use `-O2` to build an optimized program:
```
> stack exec koka -- -O2 -c test/bench/koka/rbtree32.kk
...
> cmake --build "out/RelWithDebInfo/cbuild" --target test_bench_koka_rbtree32
[15/15] Linking C executable test_bench_koka_rbtree32
compiled: out/RelWithDebInfo/test_bench_koka_rbtree32

> time out/RelWithDebInfo/test_bench_koka_rbtree32
420000
real    0m1.132s
...
> g++ -o cpp_rbtree -O3 test/bench/cpp/rbtree.cpp
> time ./cpp_rbtree
420000
real    0m1.096s
...
```

Without giving any input files, the interpreter runs by default:
```
> stack exec koka
```

The [Atom](https://atom.io/) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

`> jake atom`

(or use `jake sublime`) for the [Sublime](http://www.sublimetext.com) editor).
If `node` is not installed, you can also copy the grammar files
manually from the `koka/support` directory.


## Running the interactive compiler

After running the plain ``stack exec koka`` command, the Koka interactive environment will start:
````
 _          _           ____
| |        | |         |__  \
| | __ ___ | | __ __ _  __) |
| |/ // _ \| |/ // _` || ___/ welcome to the koka interpreter
|   <| (_) |   <| (_| ||____| version 2.0.0-alpha, Aug 23 2020, libc 64-bit
|_|\_\\___/|_|\_\\__,_|       type :? for help

loading: std/core
loading: std/core/types
loading: std/core/hnd

>
````
Now you can test some expressions:

    > println("hi koka")
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    check  : interactive
    > cmake --build "out\Debug\cbuild" --target interactive
    [2/2] Linking C executable interactive.exe
    compiled: out\Debug\interactive.exe

    hi koka
    
    > :t "hi"
    string

    > :t println("hi")
    console ()

Or load a demo:

    > :l test/medium/fibonacci
    compile: test/medium/fibonacci.kk
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    check  : test/medium/fibonacci
    modules:
      test/medium/fibonacci

    > main()
    >> cmake --build "out/Debug/cbuild" --target interactive
    [2/2] Linking C executable interactive
    compiled: out/Debug/interactive

    The 10000th fibonacci number is 33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875

And quit the interpreter:

    > :q

    Before the effect one believes in different causes than one does after the effect.
     -- Friedrich Nietzsche


## Algebraic effect handlers

A novel feature of Koka is a compiled and typed implementation of algebraic
effect handlers (described in detail in [[3]](#references)).
In the interactive environment, you can load various demo files with algebraic
effects which are located in the ``test/algeff`` directory. 

    > :f test/algeff/common

where ``:f`` forces a recompile (versus ``:l`` which avoids a recompile if possible). 
Use the ``:?`` command to get an overview of all commands. After
loading the ``common`` demo, we can run it directly from the interpreter:

    > :f test/algeff/common
    loading: test/algeff/common
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    modules:
      test/algeff/common
      
    > :t test2    
    () -> console ()

    > test2()
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    loading: test/algeff/common
    check  : interactive
    > cmake --build "out/Debug/cbuild" --target interactive
    [2/2] Linking C executable interactive
    compiled: out/Debug/interactive

    Hello there, there

Some interesting demos are:

* ``common.kk``: Various examples from the paper "_Algebraic Effects for
  Functional Programming_" [[3]](#references). Shows how to implement
  common control-flow abstractions like exceptions, state, iterators,
  ambiguity, and asynchronous programming.

* ``nim.kk``: Various examples from the paper "_Liberating effects with
  rows and handlers_" [[1]](#references).


# Benchmarks

There is a standard benchmark suite. It is still basic but more benchmarks 
with effect handlers are coming. We only test on Linux and the benchmarks
need `gcc`, `ghc` (should be there already), `ocamlopt` (use `sudo apt-get install ocaml`), 
and `swiftc` in the path. The Swift compiler can be downloaded [here](https://swift.org/download/)
and the benchmarks expect `switfc` to be installed at `/opt/swift/bin`.
The benchmarks are build using:

```
> cd test/bench
> mkdir build
> cd build
> cmake .. -DCMAKE_BUILD_TYPE=Release
> cmake --build .
```

We can then run all benchmarks as:
```
> ctest .
```
Or only run benchmarks for one language with `-L <lang>`:
```
> ctest -L koka
```
Or run specific benchmarks using `-R <regex>`,
like the symbolic derivative benchmark:
```
> ctest -R deriv      
Test project /home/daan/dev/koka/test/bench/build
    Start  4: hs-deriv
1/4 Test  #4: hs-deriv .........................   Passed    2.29 sec
    Start 10: kk-deriv
2/4 Test #10: kk-deriv .........................   Passed    1.25 sec
    Start 19: ml-deriv
3/4 Test #19: ml-deriv .........................   Passed    1.73 sec
    Start 25: sw-deriv
4/4 Test #25: sw-deriv .........................   Passed    2.88 sec

100% tests passed, 0 tests failed out of 4
...
```

# Testing

To run tests, use stack:

```
> stack test                                              # All tests
> stack test --test-arguments="--match /parc/"            # One category
> stack test --test-arguments="--mode new"                # Create output files
> stack test --test-arguments="--mode update"             # Update output files
> stack test --test-arguments="--match /parc/ --mode new" # Combined
```

# Environment

## Windows

On Windows, Koka's C backend can compile with the Stack-supplied MinGW compiler.
However, the MinGW runtime libraries are not added to the PATH by default. In
this case, you can prefix any command with `stack exec` (not just those that
Stack itself built). For example, to use Intel VTune to profile a Koka program:

```
stack exec "C:\Program Files (x86)\IntelSWTools\VTune Profiler 2020\bin64\vtune-gui.exe"
```

# More on Evidence Translation and Perceus

Koka compiles directly to plain C code without needing a garbage collector or runtime system.
There are two crucial ingredients to make this possible: evidence translation and Perceus.

## Evidence translation

As described in the paper _Effect Handlers, Evidently_, Xie _et al._ [6] show how to translate algebraic effect handlers at compilation
time down to pure lambda calculus where all control flow is explicit again. This is done by Koka to remove any dependence on
runtime mechanisms like split-stacks (as in Multi-core OCaml) or C stack copying [7]. Moreover, as the evidence for each handler
is passed down to the call site, all _tail-resumptive_ operations can be executed in-place without needing to do an expensive
yield- and resume. This makes the cost of tail-resumptive operations on effects comparable to a virtual method call.

## Perceus

Even a pure core intermediate language with explicit control flow is not yet good enough to compile to C directly: without manual memory 
management functional languages still need a (tracing) garbage collector (like OCaml or Haskell). A well performing concurrent generational 
garbage collector is very hard to build and is invasive as it needs to be able to scan the roots and stack. Even the best garbage collectors 
still suffer from unpredictable latencies (especially with large live sets) and tend to require (much) more memory than achievable with 
manual memory management (as with C/C++ and Rust). 

With Koka we took a new approach based on reference counting. The usual wisdom is that  reference counting does not perform well due to various factors but
in Koka we believe we can do better: 1) we know that all inductive and co-inductive datatypes are never cyclic so we can identify potential cycle introducing
datatypes statically (like mutable references, and these are not so often used in mostly functional Koka), 2) again due to the strict type system
we can statically track which values may become shared across threads and avoid expensive atomic operations for the majority of operations, and 
finally 3) due to the explicit control-flow we can do deep analysis on variable life times.
In particular, we use aggressive static analysis to insert _precise_ reference count instructions where memory is freed as soon as 
it is no longer live (and in particular, we do not hold on to memory based on lexical scope as in almost all reference counting implementations
in the wild, like Swift, Python, C++ `shared_ptr` etc). 

_Percues_  stands for _Precise automatic reference counting with reuse and specialization_: the _reuse_ component transform functional
style pattern matches into _in-place update_ when possible, while _specialization_ specialize the reference counting based on the call sites and
removes most rc operations in the fast path. For example, a simple `map` function:
```koka
fun map( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons( f(x), map(xx,f) )
    Nil        -> Nil
  }
}
```
will update the list _in place_ (reusing the `Cons` nodes that are matched) if the list happens to be not shared (and makes a copy otherwise). 
This dynamically adjust the program from in-place update to persistence and is the main reason why it can approach the performance of 
hand-optimized C++ on the red-black tree benchmark.

Talk and paper are coming soon...


# References

1. Daniel Hillerström, and Sam Lindley. &ldquo;Liberating Effects with Rows and Handlers.&rdquo; In _Proceedings of the 1st International Workshop on Type-Driven Development_, 15--27. TyDe 2016. Nara, Japan. 2016. doi:[10.1145/2976022.2976033](https://dx.doi.org/10.1145/2976022.2976033).

2. Daan Leijen. &ldquo;Koka: Programming with Row Polymorphic Effect Types.&rdquo; In _Mathematically Structured Functional Programming 2014_. EPTCS. Mar. 2014. arXiv:[1406.2061](http://arxiv.org/abs/1406.2061).

3. Daan Leijen. _Algebraic Effects for Functional Programming_. MSR-TR-2016-29. Microsoft Research. Aug. 2016. <https://www.microsoft.com/en-us/research/publication/algebraic-effects-for-functional-programming>. Extended version of [4].

4. Daan Leijen. &ldquo;Type Directed Compilation of Row-Typed Algebraic Effects.&rdquo; In _Proceedings of Principles of Programming Languages (POPL’17)_. Paris, France. Jan. 2017.

5. Nicolas Wu, Tom Schrijvers, and Ralf Hinze. &ldquo;Effect Handlers in Scope.&rdquo; In _Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell_, 1--12. Haskell ’14. ACM, New York, NY, USA. 2014. doi:[10.1145/2633357.2633358](https://dx.doi.org/10.1145/2633357.2633358)

6. Ningning Xie, Jonathan Brachthäuser, Daniel Hillerström, Philipp Schuster, Daan Leijen. &ldquo;Effect Handlers, Evidently&rdquo;
The 25th ACM SIGPLAN International Conference on Functional Programming (ICFP), August 2020. doi:[10.1145/3408981](https://doi.org/10.1145/3408981), [pdf](https://www.microsoft.com/en-us/research/uploads/prod/2020/07/evidently-with-proofs-5f0b7d860b387.pdf)

7. Ningning Xie and Daan Leijen. &ldquo;Effect Handlers in Haskell, Evidently&rdquo; The 13th ACM SIGPLAN International Haskell Symposium, August 2020.
[pdf](https://www.microsoft.com/en-us/research/uploads/prod/2020/07/effev.pdf)
