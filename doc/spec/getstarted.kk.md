# Getting started

Welcome to Koka. This manual provides an overview and formal specification of the language.
For more background information, see:

* The [library documentation][libraries].
* The [github repository][kokarepo].
* The article _Algebraic Effects for Functional Programming_ [@Leijen:algeff] about the algebraic effect handlers in Koka.
* An article about the type system and semantics of Koka [@Leijen:msfp].

[langspec]: https://koka-lang.github.io/koka/doc/kokaspec.html  {target='_top'}
[libraries]: https://koka-lang.github.io/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[kokarepo]: https://github.com/koka-lang/koka {target='_top'}
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}

[releases]: https://github.com/koka-lang/koka/releases
[build]: https://github.com/koka-lang/koka/#build-from-source
[Perceus]: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf
[vsprompt]: https://docs.microsoft.com/en-us/cpp/build/how-to-enable-a-64-bit-visual-cpp-toolset-on-the-command-line?view=vs-2019

## Installing the compiler

For Linux and macOS on x86 64-bit, you can install Koka using:
````
> curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh
````
After installation, verify if Koka installed correctly:
````
> koka
 _          _           ____
| |        | |         |__  \
| | __ ___ | | __ __ _  __) |
| |/ // _ \| |/ // _` || ___/ welcome to the koka interpreter
|   <| (_) |   <| (_| ||____| version 2.0.10, Nov 28 2020, libc 64-bit (gcc)
|_|\_\\___/|_|\_\\__,_|       type :? for help

loading: std/core
loading: std/core/types
loading: std/core/hnd
````

Type ``:q`` to exit the interpreter.

For detailed instructions and other platforms (including Windows) see the [releases] page.
It is also straightforward to build the compiler [from source][build].

## Running the compiler

You can compile a Koka source using `-c` (note that all [`samples`](https://github.com/koka-lang/koka/tree/master/samples) are pre-installed):

    > koka -c samples/basic/caesar.kk
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

    > out/v2.0.9/gcc-debug/samples_basic_caesar
    plain  : Koka is a well-typed language
    encoded: Krnd lv d zhoo-wbshg odqjxdjh
    cracked: Koka is a well-typed language

The ``-O2`` flag builds an optimized program. Let's try it on a purely functional implementation
of balanced insertion in a red-black tree ([`rbtree.kk`](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.kk):

    > koka -O2 -c samples/basic/rbtree.kk
    ...
    linking: samples_basic_rbtree
    created: out/v2.0.10/gcc-drelease/samples_basic_rbtree

    > time out/v2.0.10/gcc-drelease/samples_basic_rbtree
    420000
    real    0m0.750s
    ...

We can compare this against an in-place updating C++ implementation using ``stl::map``
([``rbtree.cpp``](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.cpp)) (which also uses a
[red-black tree](https://code.woboq.org/gcc/libstdc++-v3/src/c++98/tree.cc.html) internally):

    > clang++ --std=c++17 -o cpp-rbtree -O3 samples/basic/rbtree.cpp
    > time ./cpp-rbtree
    420000
    real    0m0.864s
    ...

The excellent performance relative to C++ here (on an AMD 3600XT) is the result of Perceus automatically
transforming the fast path of the pure functional rebalancing to use mostly in-place updates,
closely mimicking the imperative rebalancing code of the hand optimized C++ library.


## Running the interactive compiler

Without giving any input files, the interactive interpreter runs by default:
````
> koka
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

The [``samples/syntax``](https://github.com/koka-lang/koka/tree/master/samples/syntax)
and [``samples/basic``](https://github.com/koka-lang/koka/tree/master/samples/basic) 
directories contain various basic Koka examples to start with. If you type:

    > :l samples/

in the interpreter, you can ``tab`` twice to see the available sample files and directories.
Use ``:s`` to see the source of a loaded module.


## Algebraic effect handlers

A novel feature of Koka is a compiled and typed implementation of algebraic
effect handlers (described in detail in [[3]](#references)).
In the interactive environment, you can load various demo files with algebraic
effects which are located in the [``samples/handlers``](https://github.com/koka-lang/koka/tree/master/samples/handlers) directory.

    > :f samples/handlers/basic

where ``:f`` forces a recompile (versus ``:l`` which avoids a recompile if possible).
Use the ``:?`` command to get an overview of all commands. After
loading the ``common`` demo, we can run it directly from the interpreter:

    > :f samples/handlers/basic
    compile: samples/handlers/basic.kk
    ...
    check  : samples/handlers/basic
    modules:
      samples/handlers/basic

    > :t test2    
    () -> console ()

    > test2()
    check  : interactive
    check  : interactive
    linking: interactive
    created: out\v2.0.5\mingw-debug\interactive

    Hello there, there

Some interesting demos are:

* ``basic.kk``: Various examples from the paper "_Algebraic Effects for
  Functional Programming_" [[3]](#references). Shows how to implement
  common control-flow abstractions like exceptions, state, iterators,
  ambiguity, and asynchronous programming.

* ``nim.kk``: Various examples from the paper "_Liberating effects with
  rows and handlers_" [[1]](#references).

* ``scoped.kk``: Examples from the paper "_Effect Handlers in Scope_" [[5]](#references).
