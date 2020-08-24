<img align="left" width="100" height="100" src="doc/logo/koka-logo-v2-800.png"/>

<!--
[<img align="right" src="https://travis-ci.org/koka-lang/koka.svg?branch=master"/>](https://travis-ci.org/koka-lang/koka)
-->
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

&nbsp;

# Koka: a function-oriented language with effect inference

Koka v2 is currently under heavy development with the new evidence translation and C backend, and features may be lacking.
Use the branch `v1-master` to use the older stable Koka with the Javascript backend.

Koka is a functional style language that is strongly typed. Koka has two specific features that set it apart: it tracks
the (side) _effects_ of every function in the type of the functions, and it has full support for algebraic effect
handlers. Recent work on _evidence translation_ and _Perceus_ reference counting enable Koka to compile directly 
to plain C code without needing a garbage collector or runtime system. Initial performance benchmarks are very promising, where
a naive Koka implementation of a red-black tree balanced insertion ([`rbtree.kk`](test/bench/koka/rbtree.kk)) is within 10% of 
the performance of a C++ implementation using `stl::map` ([`rbtree.cpp`](test/bench/cpp/rbtree.cpp)) (which is based on the highly efficient GNU 
[`RBTree`](https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/tsearch.c;h=cdc401a4e5411221ab2feb2baf8745991bde7868;hb=HEAD) implementation)!

For more background information, see:

* The [A tour of Koka][kokabook] for a specification of the Koka language and a primer on algebraic effects.
* The [library documentation][libraries].
* The [Koka research page][kokaproject].
* The article _Algebraic Effects for Functional Programming_ [[3]](#references) about the algebraic effects in Koka.

[kokabook]: https://koka-lang.github.io/koka/doc/kokaspec.html  
[libraries]: https://koka-lang.github.io/koka/doc/toc.html
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf
[kokarepo]: https://github.com/koka-lang/koka
[kokaproject]: http://research.microsoft.com/en-us/projects/koka
[rise4fun]: http://rise4fun.com/koka/tutorial


## Installing the compiler

At this point there are no binary releases of Koka and you need to build
the compiler yourself. Fortunately, Koka has few dependencies and builds
without problems on most common platforms, e.g. Windows, MacOSX, and
Unix.

The following programs are required to build Koka:

* [Stack](https://docs.haskellstack.org/) to run the Haskell compiler.
* [CMake](https://cmake.org/download/) to compile the generated C files.
* Optional: the [NodeJS](http://nodejs.org) runtime if using the Javascript backend.

Building Koka:
```
> git clone https://github.com/koka-lang/koka
> cd koka
> stack build
```
You can also use `stack build --fast` to build a debug version of the compiler.
You can invoke the compiler as:
```
> stack exec koka -- -c test/algeff/common
compile: test/algeff/common.kk
loading: std/core
loading: std/core/types
loading: std/core/hnd
check  : test/algeff/common
> cmake --build "out\Debug\cbuild" --target test_algeff_common
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
Without input files, the interpreter runs by default:
```
> stack exec koka
```

The [Atom](https://atom.io/) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

`> jake atom`

(or use `jake sublime`) for the [Sublime](http://www.sublimetext.com) editor).


## Running the interactive compiler

After running a plain ``stack exec koka`` command, the Koka interactive environment will start:
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

    > :l demo/collatz
    compile: lib/demo/collatz.kk
    check  : demo/collatz
    modules:
      demo/collatz

    > main()
    Collatz(27) took 111 steps.

And quit the interpreter:

    > :q

    Before the effect one believes in different causes than one does after the effect.
     -- Friedrich Nietzsche


## Algebraic effect handlers

A novel feature of Koka is a compiled and typed implementation of algebraic
effect handlers (described in detail in [[3]](#references)).
In the interactive environment, you can load various demo files with algebraic
effects which are located in the ``test/algeff`` directory. This is by default
included in the search path, so we can load them directly using
the _load_ (``:l``) command:

    > :l scoped

Use the ``:?`` command to get an overview of all commands. After
loading the ``scoped`` demo, we can run it directly from the interpreter:

    > :l scoped
    compile: test/algeff/scoped.kk
    check  : scoped
    modules:
      scoped

    > main()
    [[[3]],[2,1],[1,2],[1,1,1]]
    (state=12, [[[3]],[2,1],[1,2],[1,1,1]])
    [(state=1, [[3]]),(state=5, [2,1]),(state=5, [1,2]),(state=9, [1,1,1])]
    [[[3]]]
    [42]

Some interesting demos are:

* ``common.kk``: Various examples from the paper "_Algebraic Effects for
  Functional Programming_" [[3]](#references). Shows how to implement
  common control-flow abstractions like exceptions, state, iterators,
  ambiguity, and asynchronous programming.

* ``scoped.kk``: Various examples from the paper "_Effect handlers in
  Scope_" [[5]](#references).

* ``nim.kk``: Various examples from the paper "_Liberating effects with
  rows and handlers_" [[1]](#references).

* ``async*.kk``: Various asynchronous effect examples.

* ``parser.kk``: Implements parser combinators as an effect.

Enjoy,
  -- Daan

# Environment

On Windows, Koka's C backend can compile with the Stack-supplied MinGW compiler.
However, the MinGW runtime libraries are not added to the PATH by default. In
this case, you can prefix any command with `stack exec` (not just those that
Stack itself built). For example, to use Intel VTune to profile a Koka program:

```
stack exec "C:\Program Files (x86)\IntelSWTools\VTune Profiler 2020\bin64\vtune-gui.exe"
```

# Testing

To run tests, use stack:

```
stack test                                              # All tests
stack test --test-arguments="--match /parc/"            # One category
stack test --test-arguments="--mode new"                # Create output files
stack test --test-arguments="--mode update"             # Update output files
stack test --test-arguments="--match /parc/ --mode new" # Combined
```

# References

1. Daniel Hillerström, and Sam Lindley. &ldquo;Liberating Effects with Rows and Handlers.&rdquo; In _Proceedings of the 1st International Workshop on Type-Driven Development_, 15--27. TyDe 2016. Nara, Japan. 2016. doi:[10.1145/2976022.2976033](https://dx.doi.org/10.1145/2976022.2976033).

2. Daan Leijen. &ldquo;Koka: Programming with Row Polymorphic Effect Types.&rdquo; In _Mathematically Structured Functional Programming 2014_. EPTCS. Mar. 2014. arXiv:[1406.2061](http://arxiv.org/abs/1406.2061).

3. Daan Leijen. _Algebraic Effects for Functional Programming_. MSR-TR-2016-29. Microsoft Research. Aug. 2016. <https://www.microsoft.com/en-us/research/publication/algebraic-effects-for-functional-programming>. Extended version of [4].

4. Daan Leijen. &ldquo;Type Directed Compilation of Row-Typed Algebraic Effects.&rdquo; In _Proceedings of Principles of Programming Languages (POPL’17)_. Paris, France. Jan. 2017.

5. Nicolas Wu, Tom Schrijvers, and Ralf Hinze. &ldquo;Effect Handlers in Scope.&rdquo; In _Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell_, 1--12. Haskell ’14. ACM, New York, NY, USA. 2014. doi:[10.1145/2633357.2633358](https://dx.doi.org/10.1145/2633357.2633358)
