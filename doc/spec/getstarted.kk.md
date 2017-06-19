# Getting started

Welcome to the Koka book. This provides an overview and
formal specification of the language. 
For more background information, see:

* The [library documentation][libraries].
* The [Koka research page][kokaproject] and the [slides] of a talk presented Lang.Next (April 2012).
* The [source code][kokarepo] of the Koka compiler.
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

       > git clone https://github.com/koka-lang/koka.git 

   You can also use the flag ``-b dev`` to get the latest development version.

2. Go to the newly created Koka directory:

       > cd koka

3. Install any needed Node libraries using the Node package manager: 

       > npm install

   If you are running on MacOSX or Unix, you may have to run this as
   ``sudo npm install`` so that the ``npm`` package manager has enough
  permissions to install the ``jake`` and ``madoko`` tools.

4. Finally, build the compiler and run the Koka interactive environment:
       > jake

   You can type ``jake help`` to see an overview of all make targets.

The excellent [Sublime](http://www.sublimetext.com) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

    > jake sublime

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
    \(`:string`\)

    > :t println("hi")
    \(`:console ()`\)

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
effect handlers (described in detail in [@Leijen:algeff]).
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
    [[3],[2,1],[1,2],[1,1,1]]
    (state=12, [[3],[2,1],[1,2],[1,1,1]])
    [(state=1, [3]),(state=5, [2,1]),(state=5, [1,2]),(state=9, [1,1,1])]
    [[3]]
    [42]

Some interesting demos are:

* ``common.kk``: Various examples from the paper "_Algebraic Effects for
  Functional Programming_" [@Leijen:algeff]. Shows how to implement
  common control-flow abstractions like exceptions, state, iterators,
  ambiguity, and asynchronous programming.

* ``scoped.kk``: Various examples from the paper "_Effect handlers in
  Scope_" [@Wu:hscope].

* ``nim.kk``: Various examples from the paper "_Liberating effects with
  rows and handlers_" [@Lindley:liberate].

* ``async*.kk``: Various asynchronous effect examples.

* ``parser.kk``: Implements parser combinators as an effect.

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
  fun flip() : bool
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
  flip()   -> resume(False) + resume(True)
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
The handler for the `:state` effect takes a local parameter that
is propagated through the `resume` function. 
```
val state = handler(i) {
  return x -> x
  get()    -> resume(i,i)
  set(j)   -> resume(j,())
}
```
Type of the `state` handler takes an initial state as an extra argument:

    > :t state
    \(|`:forall<a,b,e>. () -> ((i : a, action : () -> <state<a>|e> b) -> e b)`\)

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
