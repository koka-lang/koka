# Koka: a function-oriented language with effect inference

For more background information, see:

* The [Koka book][kokabook] for a specification of the Koka language and a primer on algebraic effects.
* The [library documentation][libraries].
* The [Koka research page][kokaproject] and the [slides] of a talk presented Lang.Next (April 2012).
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
without problems on most common platforms, &eg; Windows, MacOSX, and
Unix.

The following programs are required to build Koka:

* The [Haskell platform](http://www.haskell.org/platform) (version 7.4 or later).
* The [NodeJS](http://nodejs.org) runtime (version 4.2 LTS or later).
* Some version of [Git](https://help.github.com/articles/set-up-git/) for version control.

All these programs are very easy to install on most platforms.
Now we can build Koka itself: 

1. First clone the Koka sources with algebraic effects support:

   `> git clone https://github.com/koka-lang/koka.git`

  You can also use the flag `-b dev` to get the latest development version.

2. Go to the newly created Koka directory:

   `> cd koka`

3. Install any needed Node libraries using the Node package manager: 

   `> npm install`

   If you are running on MacOSX or Unix, you may have to run this as
   ``sudo npm install`` so that the ``npm`` package manager has enough
  permissions to install the ``jake`` and ``madoko`` tools.

4. Finally, build the compiler and run the Koka interactive environment:

   `> jake`

   You can type ``jake help`` to see an overview of all make targets.

The excellent [Sublime](http://www.sublimetext.com) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

`> jake sublime`

After this ``.kk`` files will be properly highlighted. It is also
recommended to use the newly installed ``snow`` color theme which is
designed to work well with Koka files.


## Running the interactive compiler

After running a plain ``jake`` command, the Koka interactive environment will start:
````
__          _
| |        | |
| | __ ___ | | __ __ _
| |/ // _ \| |/ // _` | welcome to the koka interpreter
|   <| (_) |   <| (_| | version 0.7.0-dev (debug), Jun 30 2016
|_|\_\\___/|_|\_\\__,_| type :? for help

loading: std/core
````
Now you can test some expressions:

    > println("hi koka")
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

You can also run examples in the browser by setting the host:

    > :set --host=browser
    > 1+2

Some browser specific demo to try is for example ``demo/dom/conway.kk``.

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


# References

1. Daniel Hillerström, and Sam Lindley. &ldquo;Liberating Effects with Rows and Handlers.&rdquo; In _Proceedings of the 1st International Workshop on Type-Driven Development_, 15--27. TyDe 2016. Nara, Japan. 2016. doi:[10.1145/2976022.2976033](https://dx.doi.org/10.1145/2976022.2976033).

2. Daan Leijen. &ldquo;Koka: Programming with Row Polymorphic Effect Types.&rdquo; In _Mathematically Structured Functional Programming 2014_. EPTCS. Mar. 2014. arXiv:[1406.2061](http://arxiv.org/abs/1406.2061).

3. Daan Leijen. _Algebraic Effects for Functional Programming_. MSR-TR-2016-29. Microsoft Research. Aug. 2016. <https://www.microsoft.com/en-us/research/publication/algebraic-effects-for-functional-programming>. Extended version of [4].

4. Daan Leijen. &ldquo;Type Directed Compilation of Row-Typed Algebraic Effects.&rdquo; In _Proceedings of Principles of Programming Languages (POPL’17)_. Paris, France. Jan. 2017.

5. Nicolas Wu, Tom Schrijvers, and Ralf Hinze. &ldquo;Effect Handlers in Scope.&rdquo; In _Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell_, 1--12. Haskell ’14. ACM, New York, NY, USA. 2014. doi:[10.1145/2633357.2633358](https://dx.doi.org/10.1145/2633357.2633358)
