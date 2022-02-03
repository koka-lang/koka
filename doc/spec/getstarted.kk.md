<!-- #00547D -->
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


# Getting started { #getstarted; }

Welcome to &koka; -- a strongly typed functional-style language with effect types and handlers.

[Why &koka;? &adown;][#why]{.learn}
[A Tour of Koka &adown;][#tour]{.learn}
[Install &adown;][#install]{.learn}
[Discussion forum][forum]{.learn}
[Github][kokarepo]{.learn}
[Libraries][libraries]{.learn}
{text-align:left;}

~ smaller { font-size:smaller; }
Note: &koka; v2 is a research language that is currently under development
and not ready for production use. 
Nevertheless, the language is stable and the compiler
implements the full specification. The main things lacking at the moment are 
libraries, package management, and deep IDE integration. 

[INCLUDE=news.mdk]

~ 

[langspec]: https://koka-lang.github.io/koka/doc/kokaspec.html  {target='_top'}
[libraries]: https://koka-lang.github.io/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[kokarepo]: https://github.com/koka-lang/koka {target='_top'}
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}
[forum]: https://github.com/koka-lang/koka/discussions

[samples]: https://github.com/koka-lang/koka/tree/master/samples
[rbtree]: https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.kk
[evidence-passing]: https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/
[Perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
[releases]: https://github.com/koka-lang/koka/releases
[build]: https://github.com/koka-lang/koka/#build-from-source
[vsprompt]: https://docs.microsoft.com/en-us/cpp/build/how-to-enable-a-64-bit-visual-cpp-toolset-on-the-command-line?view=vs-2019
[reusetech]: https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse-extended-version/

## Installing the compiler { #install }

For Linux (x64, arm64), macOS (x64, M1), and FreeBSD (x64), you can install &koka; using:

&acopy;
{.copy; data-value:"curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh"}

    \(**curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh &bar; sh**\)

For Windows (x64), open a ``cmd`` prompt and use:

&acopy;
{.copy; data-value:"curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat"}

    \(**curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat**\)

This also installs syntax highlighting for the VS Code and Atom editors.

After installation, verify if &koka; installed correctly:

    $ koka
     _         _
    | |       | |
    | | _ ___ | | _ __ _
    | |/ / _ \| |/ / _' |  welcome to the koka interactive compiler
    |   ( (_) |   ( (_| |  version 2.3.1, Sep 21 2021, libc x64 (gcc)
    |_|\_\___/|_|\_\__,_|  type :? for help, and :q to quit

    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    >

Type ``:q`` to exit the interactive environment.

For detailed installation instructions and other platforms see the [releases] page.
It is also straightforward to build the compiler [from source][build].

## Running the compiler

You can compile a &koka; source as (note that all [`samples`][samples] are pre-installed):

    $ koka samples/basic/caesar.kk
    compile: samples/basic/caesar.kk
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    loading: std/num/float64
    loading: std/text/parse
    loading: std/num/int32
    check  : samples/basic/caesar
    linking: samples_basic_caesar
    created: .koka/v2.3.1/gcc-debug/samples_basic_caesar

and run the resulting executable:

    $ .koka/v2.3.1/gcc-debug/samples_basic_caesar
    plain  : Koka is a well-typed language
    encoded: Krnd lv d zhoo-wbshg odqjxdjh
    cracked: Koka is a well-typed language

The ``-O2`` flag builds an optimized program. Let's try it on a purely functional implementation
of balanced insertion in a red-black tree ([`rbtree.kk`](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.kk)):

    $ koka -O2 -o kk-rbtree samples/basic/rbtree.kk
    ...
    linking: samples_basic_rbtree
    created: .koka/v2.3.1/gcc-drelease/samples_basic_rbtree
    created: kk-rbtree

    $ time ./kk-rbtree
    420000
    real    0m0.626s
    ...

(On Windows you can give the `--kktime` option to see the elapsed time).
We can compare this against an in-place updating C++ implementation using ``stl::map``
([``rbtree.cpp``](https://github.com/koka-lang/koka/tree/master/samples/basic/rbtree.cpp)) (which also uses a
[red-black tree](https://github.com/llvm/llvm-project/blob/main/libcxx/include/__tree) internally):

    $ clang++ --std=c++17 -o cpp-rbtree -O3 /usr/local/share/koka/v2.3.1/lib/samples/basic/rbtree.cpp
    $ time ./cpp-rbtree
    420000
    real    0m0.667s
    ...

The excellent performance relative to C++ here (on Ubuntu 20.04 with an AMD 5950X) 
is the result of Perceus automatically
transforming the fast path of the pure functional rebalancing to use mostly in-place updates,
closely mimicking the imperative rebalancing code of the hand optimized C++ library.


## Running the interactive compiler

Without giving any input files, the interactive environment runs by default:

    $ koka
     _         _
    | |       | |
    | | _ ___ | | _ __ _
    | |/ / _ \| |/ / _' |  welcome to the koka interactive compiler
    |   ( (_) |   ( (_| |  version 2.3.1, Sep 21 2021, libc x64 (clang-cl)
    |_|\_\___/|_|\_\__,_|  type :? for help, and :q to quit

    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    >

Now you can test some expressions:

    > println("hi koka")
    check  : interactive
    check  : interactive
    linking: interactive
    created: .koka\v2.3.1\clang-cl-debug\interactive

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
    created: .koka\v2.3.1\clang-cl-debug\interactive

    The 10000th fibonacci number is 33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875

You can also set command line options in the interactive environment using ``:set <options>``.
For example, we can load the ``rbtree`` example again and print out the elapsed runtime with ``--showtime``:

    > :set --showtime

    > :l samples/basic/rbtree.kk
    ...
    linking: interactive
    created: .koka\v2.3.1\clang-cl-debug\interactive

    > main()
    ...
    420000
    info: elapsed: 4.104s, user: 4.046s, sys: 0.062s, rss: 231mb

and then enable optimizations with ``-O2`` and run again (on Windows with an AMD 5950X):

    > :set -O2
    
    > :r 
    ...
    linking: interactive
    created: .koka\v2.3.1\clang-cl-drelease\interactive
    
    > main()
    ...
    420000
    info: elapsed: 0.670s, user: 0.656s, sys: 0.015s, rss: 198mb

And finally we quit the interpreter:

    > :q

    I think of my body as a side effect of my mind.
      -- Carrie Fisher (1956)


## Samples and Editors

The [``samples/syntax``](https://github.com/koka-lang/koka/tree/master/samples/syntax)
and [``samples/basic``](https://github.com/koka-lang/koka/tree/master/samples/basic) 
directories contain various basic &koka; examples to start with. If you type:

    > :l samples/

in the interpreter, you can ``tab`` twice to see the available sample files and directories.
Use ``:s`` to see the source of a loaded module.

If you use VS Code or Atom (or if you set the ``koka_editor`` environment variable manually),
you can type ``:e`` in the interactive prompt to edit your program further. For example,

    > :l samples/basic/caesar
    ...
    modules:
        samples/basic/caesar

    > :e 
    
    <edit the source and reload>

    > :r
    ...
    modules:
        samples/basic/caesar

    > main()
    

What next?

[Basic &koka; syntax &adown;][#sec-basics]{.learn}
[Browse the Library documentation][libraries]{.learn}

