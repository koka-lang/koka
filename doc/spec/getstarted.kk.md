# Getting started

At this point there are no binary releases of Koka and you need to build
the compiler yourself. Fortunately, Koka has few dependencies and builds
without problems on most common platforms, &eg; Windows, MacOSX, and
Unix.

The following programs are required to build Koka:

* The [Haskell platform](http://www.haskell.org/platform) (version 7.4 or later).
* The [NodeJS](http://nodejs.org) runtime (version 4.2 or later).
* Some version of Mercurial, like the excellent [TortoiseHg](http://tortoisehg.bitbucket.org/download).

Next, get the compiler and libraries from codeplex, and build them: 

* ``> hg clone https://hg.codeplex.com/koka -b algeff``\
  (clone the Koka sources with algebraic effects support)

* ``> cd koka``\
  (go to the new Koka directory) 

* ``> npm install``\
  (install needed Node libraries) 

* ``> jake``\
  (build the compiler and run the Koka interactive environment)


The excellent [Sublime](http://www.sublimetext.com) text editor is recommended
to edit Koka programs. You can install support for Koka programs using

    > jake sublime

After this ``.kk`` files will be properly highlighted, especially
with the newly installed ``snow`` color scheme which is designed to
work well with Koka files.

You can type ``jake help`` to see an overview of all make targets.


## Running the interpreter

After running a plain ``jake`` command, the Koka interpreter will start:
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

## Algebra&iuml;c effect handlers

When in the interpreter, you can load various demo files with algebra&iuml;c 
effects which are located in the ``test/algeff`` directory. This is by default
included in the search path, so we can load them directly:
    
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

* ``scoped.kk``: Various examples from the paper "_Effect handlers in Scope_"
  [@Wu:hscope].

* ``nim.kk``: Various examples from the paper "_Liberating effects with rows and handlers_"
  by Daniel Hillerstr&ouml;m and Sam Lindley [@Lindley:liberate].

* ``async*.kk``: Various asynchronous effect examples.

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

It is defined as:

```
effect amb {
  flip() : bool
}
```
This declares a new effect `amb` with a single operation `flip`.
We can use this as:
```
fun xor() : amb bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}
```
where the type of `xor` reflects that it is a `amb` effect with
a boolean result.

Next, let's write a handler for this effect:
```
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}
```
The `flip` operation will execute the following code twice, once with
a `False` value, and once with a `True` value. The final result is wrapped
as a list which are concatenated by `flip`. The type of the `handler` will
remove the `amb` effect and return a list of results:

    > :t amb
    \(|`:forall<a,e> (action : () -> <amb|e> a) -> e list<a>`\)

We can now run the `xor` function using the `amb` handler:

```
fun test1() {
  amb(xor).show.println
}
```
Here we used _dot_ syntax, you can read more about that in
Section [#sec-dot]. If we run this in the interpreter, we see 
all possible results:

    > test1()
    \(`[False,True,True,False]`\)

\
** Adding state**\
Let's combine the ambiguity effect with state. The definition
of the state effect is polymorphic in its value:
```
effect state<s> {
  get()    : s
  set(i:s) : ()
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
