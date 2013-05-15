Koka: a function-oriented language with effect inference
--------------------------------------------------------

To build Koka from source you need to install:

  * The latest [Haskell platform](http://www.haskell.org/platform) (version 7 or later).
  * The latest [NodeJS](http://nodejs.org) runtime (version 0.10 or later).

After installing the above tools, go to the Koka directory and type:

    > npm install

which will install any prerequisites. Now build and run Koka by typing:

    > jake 

To see more build options, type:

    > jake help


Starting out
------------

After running `jake`, the Koka interpreter will start:
    __          _
    | |        | |
    | | __ ___ | | __ __ _
    | |/ // _ \| |/ // _` | welcome to the koka interpreter
    |   <| (_) |   <| (_| | version 0.5.0-dev (debug), Apr  8 2013
    |_|\_\\___/|_|\_\\__,_| type :? for help

    loading: std/core

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

Or a browser based demo:

    > :l demo/dom/starfield
    loading: demo/dom/starfield
    loading: sys/dom
    loading: sys/dom/types
    loading: sys/dom/document
    loading: sys/dom/html/window
    loading: sys/dom/html/html-element
    loading: sys/dom/html/html-canvas-element
    loading: sys/dom/html/canvas-rendering-context2d
    loading: sys/dom/css/css-style-declaration
    loading: demo/dom/microsoft-logo
    loading: sys/dom/html/html-table-element
    loading: sys/dom/html/html-table-row-element
    loading: sys/dom/html/html-table-data-cell-element
    modules:
      demo/dom/starfield
      
    > main()

And quit the interpreter:

    > :q

    Before the effect one believes in different causes than one does after the effect.
     -- Friedrich Nietzsche

Have fun!
  Daan Leijen


Development
-----------

For development we recommend:

  * The [TortoisHg](http://tortoisehg.bitbucket.org/download) distribution for mercurial source control.

  * The excellent [SublimeText](http://www.sublimetext.com) text editor. There is a full Koka and Haskell
    language mode for SublimeText (run `jake sublime` to install the Koka mode on your system).

