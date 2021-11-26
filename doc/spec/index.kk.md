Title         : The &koka; Programming Language
Title Note    : [(Daan Leijen, &date;)]{font-size:75%}
Heading Base  : 1
Heading Depth : 0
Toc Depth     : 3
Css           : styles/koka.css
Css           : styles/book.css
Css           : https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css
Script        : scripts/book.js
Colorizer     : unchecked.json
Colorizer     : koka.json
Bibliography  : koka.bib
Description   : Koka Language Specification
Mapsto        : [$\rightsquigarrow$]{.mapsto}
Koka          : Koka
Bibliography  : koka.bib

Css Header    :
    body.madoko, .body.madoko {
      max-width: 100%;
      padding: 0pt;      
    }
    .wide p, .wide h1 {
      color: white;
    }
    .wide.heading {
      margin-top:0pt;
      padding:0pt;
      padding-top:2em;
    }
    .wide {
      margin: 2em 0em;
      padding: 1px;
      background-color: #FAF9F8;
      /* border: 1px solid #AAA;
         border-left: none;
         border-right: none; */
    }    
    .content, h1 {
      max-width: 62rem;
      margin: 0em auto;
      padding: 0em 1em;
    }   
    .heading pre {
      font-size: 70% !important;
      width: 27em;
      border: 1px solid #AAA !important;
      box-shadow: 2px 2px 3px #aaa;
    }
    .heading-title p {
      text-align: left;
    }
    .heading-side {
      float: right;
      clear: right;
      margin: 0em 0em 1em 2em;
    }
    .banners .banner {
      padding: 0ex;
    }
    .heading-references {
      display:none !important;
    }


[INCLUDE=book-style.md]
body {
  .colored  
}

[koka-logo]: images/koka-logo-filled.png { max-height: 120px; padding:0em; }
[kokabook]: https://koka-lang.github.io/koka/doc/book.html  {target='_top'}
[libraries]: https://koka-lang.github.io/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[kokarepo]: https://github.com/koka-lang/koka {target='_top'}
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}
[releases]: https://github.com/koka-lang/koka/releases
[build]: https://github.com/koka-lang/koka/#build-from-source
[Perceus]: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf
[forum]: https://github.com/koka-lang/koka/discussions


~ begin wide { .heading }
~ begin content { .heading }


[![koka-logo]](book.html){float:left; margin-right:1em;}
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

~ heading-title 
[Koka]{font-size:200%; font-weight:bold; display:inline-block; padding-top:0.45em; }\
[A Functional Language with Effect Types and Handlers]{font-size:140%; display:inline-block; margin: -0.5ex 0ex 1ex 0ex; }
~

~ heading-side
```
// A generator effect with one operation
effect yield<a>
  fun yield( x : a ) : ()

// Traverse a list and yield the elements
fun traverse( xs : list<a> ) : yield<a> () 
  match xs
    Cons(x,xx) -> { yield(x); traverse(xx) }
    Nil        -> ()

fun main() : console () 
  with fun yield(i : int)
    println("yielded " ++ i.show)   
  [1,2,3].traverse
```
~


~ end content
~ end wide

~ begin content

Welcome to &koka; -- a strongly typed functional-style language with effect types and handlers.

[Install &adown;][#install]{.learn}
[Get Started][kokabook]{.learn}
[Documentation][kokabook]{.learn}
[Github][kokarepo]{.learn}
{text-align:left;}

~ smaller { font-size:smaller; }
Note: &koka; v2 is a research language that is currently under development
and not ready for production use. 
Nevertheless, the language is stable and the compiler
implements the full specification. The main things lacking at the moment are 
libraries, package management, and deep IDE integration. 

[INCLUDE=news]

~

~ end content

~ wide
# Why Koka? { #why }
~

~ begin content


~ begin banners
~ banner { caption:"Minimal but General" }
The core of &koka; consists of a small set of well-studied language
features, like first-class functions, a polymorphic type- and effect
system, algebraic data types, and effect handlers. Each of these is
composable and avoid the addition of
"special" extensions by being as general as possible.
~

~ banner { caption:"Effect Types"}
&koka; tracks the (side) _effects_ of every
function in its type, where pure and effectful computations are
distinguished. The precise effect typing gives &koka; _rock-solid
semantics_ backed by well-studied category theory, which makes &koka;
particularly easy to reason about for both humans and compilers.
~

~ banner { caption:"Effect Handlers" }
Effect handlers let you define advanced control abstractions,
like exceptions, async/await, iterators, parsers, ambient
state, or probabilistic programs, 
as a user library in a typed and composable way.
~

~ banner { caption:"Perceus Reference Counting" }
Perceus is an advanced compilation method for reference counting.
This lets &koka; compile directly to C code _without needing
a garbage collector or runtime system_! This also gives &koka; 
excellent performance in practice.
~

~ banner { caption:"Reuse Analysis"}
Perceus also enables _reuse analysis_ and lets &koka; optimize 
functional-style programs to use in-place updates
when possible. 
<!--
This makes many functional algorithms behave
like their imperative counterparts on uniquely owned parameters while
degrading gracefully to use copying when persistence is required.
-->
~
<!--
~ banner { caption:"FBIP: Functional But In-Place"}
Reuse analysis leads to a new style of programming that we call _FBIP_.
Just like tail-recursion lets us write loops in terms of 
function calls, reuse analysis lets us write many imperative 
algorithms in a functional style.
~
-->

~ banner { caption:"Learn more"}

[Read more about these core concepts](book.html#why){.learn}
~

~ end banners

~ end content

~ wide
# Install { #install }
~

~ begin content
For  Linux (x64, arm64) and macOS (x64, M1), you can install &koka; using:

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
    |   ( (_) |   ( (_| |  version 2.1.7, Jun 15 2021, libc x64 (gcc)
    |_|\_\___/|_|\_\__,_|  type :? for help, and :q to quit

    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    > 

Type ``:q`` to exit the interactive environment.

For detailed installation instructions and other platforms see the [releases] page.
It is also straightforward to build the compiler [from source][build].

[Get started with the compiler](book.html#sec-running-the-compiler){.learn}

~ end content


<!--
~ wide
# References
~

~ begin content
[BIB]
~ end content

~ invisible
[@Leijen:msfp;@Leijen:algefftr;@Leijen:algeff;@Xie:effev;@Leijen:scopedlabels;@Leijen:fcheap;@Leijen:async]
~
-->