# Getting started

Welcome to Koka. This manual provides an overview and formal specification of the language. 
For more background information, see:

* The [library documentation][libraries].
* The [github repository][kokarepo].
* The article _Algebraic Effects for Functional Programming_ [@Leijen:algeff] about the algebraic effects in Koka.
* An article about the type system and semantics of Koka [@Leijen:msfp].


[langspec]: https://koka-lang.github.io/koka/doc/kokaspec.html  {target='_top'}
[libraries]: https://koka-lang.github.io/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[kokarepo]: https://github.com/koka-lang/koka {target='_top'} 
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}
[rise4fun]: http://rise4fun.com/koka/tutorial

## Installing the compiler

At this point there are no binary releases of Koka and you need to build
the compiler yourself. Fortunately, Koka has few dependencies and should build
without problems on most common platforms, e.g. Windows (including WSL), macOS X, and
Unix.

The following programs are required to build Koka:

* [Stack](https://docs.haskellstack.org/) to run the Haskell compiler.  
  (use ``> curl -sSL https://get.haskellstack.org/ | sh`` on Unix and macOS X)
* [CMake](https://cmake.org/download/) to compile the generated C files.  
  (use ``> sudo apt-get install cmake`` on Ubuntu, ``> brew install cmake`` on macOS X).
* Optional: The [Ninja](https://ninja-build.org/) build system for faster build times.  
  (required on Windows, use ``> sudo apt-get install ninja-build`` on Ubuntu, ``> brew install ninja`` on macOS X).
* Optional: the [NodeJS](http://nodejs.org) runtime if using the Javascript backend.

Building Koka (note the ``--recursive`` flag):

    > git clone --recursive https://github.com/koka-lang/koka
    > cd koka
    > stack build

You can also use ``stack build --fast`` to build a debug version of the compiler.
You can invoke the compiler now as: (this takes a while as it needs to build the core libraries as well)

    > stack exec koka -- -c test/algeff/common
    compile: test/algeff/common.kk
    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    check  : test/algeff/common
    cmake --build "out\Debug\cbuild" --target test_algeff_common
    ...
    [5/5] Linking C executable test_algeff_common.exe
    compiled: out\Debug\test_algeff_common.exe

and run the resulting executable:

    > out\Debug\test_algeff_common.exe
    42
    Hello there, there
    hi
    hi
    1
    2
    [False,True,True,False]
    ([False,False,True,True,False],2)

If you leave out the ``-c`` flag, Koka will execute the compiled program automatically.
The ``-O2`` flag builds an optimized program. Let's try it on a functional implementation
of balanced insertion in a red-black tree balanced ([`rbtree.kk`](https://github.com/koka-lang/koka/tree/master/test/bench/koka/rbtree.kk))

    > stack exec koka -- -O2 -c test/bench/koka/rbtree32.kk
    ...
    cmake --build "out/RelWithDebInfo/cbuild" --target test_bench_koka_rbtree32
    [15/15] Linking C executable test_bench_koka_rbtree32
    compiled: out/RelWithDebInfo/test_bench_koka_rbtree32

    > time out/RelWithDebInfo/test_bench_koka_rbtree32
    420000
    real    0m1.132s

We can compare this against an in-place updating C++ implementation using ``stl::map``
([``rbtree.cpp``](https://github.com/koka-lang/koka/tree/master/test/bench/cpp/rbtree.cpp)) (which uses the GNU C++
[``tree.cc``](https://code.woboq.org/gcc/libstdc++-v3/src/c++98/tree.cc.html) implementation internally):

    > g++ --std=c++17 -o cpp_rbtree -O3 test/bench/cpp/rbtree.cpp
    > time ./cpp_rbtree
    420000
    real    0m1.096s
    ...

The close performance to C++ here is a result of [Perceus](#perceus) automatically
transforming the fast path of the pure functional rebalancing to use mostly in-place updates,
closely mimicking the imperative rebalancing code of the hand optimized C++ library.

Without giving any input files, the interpreter runs by default:

    > stack exec koka

The [Atom](https://atom.io/) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

    > jake atom

(or use ``jake sublime``) for the [Sublime](http://www.sublimetext.com) editor).
If ``node`` is not installed, you can also copy the grammar files
manually from the ``support/atom`` directory to ``~/.atom/packages/language-koka``.


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
    cmake --build "out\Debug\cbuild" --target interactive
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
    cmake --build "out/Debug/cbuild" --target interactive
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
    cmake --build "out/Debug/cbuild" --target interactive
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


<!--
## A primer on effect handlers

Another small demo is ``effs2`` that demonstrates the ambiguity
and state effect:

    > :l effs2
    compile: test/algeff/effs2.kk
    check  : effs2
    modules:
      effs1
    
    > main()
    \(`[False,True,True,False]`\)
    \(`[False,False,True,True,False]`\)
    \(`[False,False]`\)

The `effs2.kk` module starts by defining the `:amb` effect:
```
effect amb {
  control flip() : bool
}
```
This declares `amb` as a new effect with a single operation `flip`.
We can use this as:
```
fun xor() : amb bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}
```
where the type of `xor` reflects that it has a `amb` effect and
returns a boolean.

Next, let's write a handler for this effect:
```
val amb = handler {
  return x -> [x]
  flip()   -> append(resume(False), resume(True))
}
```
When a `flip` operation is issued, this handler will catch it
dynamically. In the above handler, we resume twice: once with a `False`
value as the result for `flip`, and once with a `True` value. The
`return` clause wraps the final result in a list which are concatenated
by the `flip` handler. The type of the `amb` handler is a function that
removes the `amb` effect from its argument, and return a list of results:

    > :t amb
    \(|`:forall<a,e> (action : () -> <amb|e> a) -> e list<a>`\)

We can now run the `xor` function using the `amb` handler to 
handle the `flip` operations:

```
fun test1() {
  amb(xor).show.println
}
```
Here we used _dot_ syntax introduced in Section [#sec-dot]. If we run
`test1` in the interpreter, we see all possible results:

    > test1()
    \(`[False,True,True,False]`\)

\
** Adding state**\
Let's combine the ambiguity effect with state. The definition
of the state effect is polymorphic in its value:
```
effect state<s> {
  fun get()    : s
  fun set(i:s) : ()
}
```
Next we define a function that uses both ambiguity and the state
effect:
```
fun foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}
```
The handler for the `:state` effect uses a local variable:
```
fun state(init : a, action : () -> <state<a>|e> b ) : e b {
  var s := init
  handle({mask<local>(action)}) {
    fun get()  -> s 
    fun set(i) -> s := i
  }
}
```
where the `state` handler takes an initial state as the first argument.
We can now combine the ambiguity handler with the state handler in
two ways (see Section [#sec-anon] for a primer on anonymous function syntax):
```
fun test2()  {
  state(0){ amb(foo) }
}

fun test3()  {
  amb{ state(0,foo) }
}
```
In `test2` the state handler is outside and every ambiguity execution
modifies the same global state. In `test3` the state handler is inside
and every ambiguity execution has its own local state instead. Can you
predict the outcomes of running the tests?

    > test2()
    \(`[False,False,True,True,False]`\)
    
    > test3()
    \(`[False,False]`\)


-->
