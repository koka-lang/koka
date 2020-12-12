Title         : The &koka; Programming Language
Title Note    : [(Daan Leijen, &date;)]{font-size:75%}
Heading Base  : 1
Heading Depth : 0
Toc Depth     : 3
Css           : styles/koka.css
Css           : styles/book.css
Css           : https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css
Colorizer     : unchecked.json
Colorizer     : koka.json
Bibliography  : koka.bib
Description   : Koka Language Specification
Mapsto        : [$\rightsquigarrow$]{.mapsto}
Koka          : Koka
ReadCollapse  : Expand
Bibliography  : koka.bib

Js Footer     :
    window.onload = function() {   
      const locals = document.querySelectorAll("a:not(.localref)");
      const currentUrl = window.location.href.replace(/#.*$/,"");
      locals.forEach( function(link){
        const child = link.firstElementChild;
        const ctag = (child==null ? "" : child.tagName);
        const linkUrl = link.href.replace(/#.*$/,"");
        if (linkUrl != currentUrl && link.className.indexOf("bib") < 0 && ctag != "IMG" && ctag != "SPAN") {  // no bib, image or code links
          link.innerHTML = link.innerHTML + "<i class=\"fa fa-external-link\"></i>";
        }
      });
    };

Css Header    :
    body.madoko, .body.madoko {
      max-width: 100%;
      padding: 0pt;      
    }
    .wide p, .wide h1 {
      color: white;
    }
    .wide:first-child {
      margin-top:0pt;
      padding-top:2em;
    }
    .wide {
      margin: 2em 0em;
      padding: 1px;
      background-color: #10606C;
      background: linear-gradient(#105A65,#1095a4);
      /* border: 1px solid #AAA;
         border-left: none;
         border-right: none; */
    }
    .content, h1 {
      max-width: 62rem;
      margin: 0em auto;
    }
    .header1 {
      padding-bottom:2em;
      padding-top: 1em;
    }
    .heading .aside {
      margin: 0em 0em 1em 1rem;
      font-size: 60% !important;
      width: 27em;
      border: 1px solid #AAA !important;
      box-shadow: 1px 1px 3px #aaa;
    }
    xh1 {
      border: 1px solid #aaa;
      background-color: #faf9f8;
      padding: 1ex;
    }
    .collapse {
      display:none;
      margin-top:-50vh;
      padding-top: 50vh;
    }
    .collapse:target {
      display:block;
    }
    .heading-references {
      display:none !important;
    }


[INCLUDE=book-style.md]


[koka-logo]: images/koka-logo-filled.png { max-height: 120px; padding:0em; }
[kokabook]: https://koka-lang.github.io/koka/doc/book.html  {target='_top'}
[libraries]: https://koka-lang.github.io/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[kokarepo]: https://github.com/koka-lang/koka {target='_top'}
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}
[releases]: https://github.com/koka-lang/koka/releases
[build]: https://github.com/koka-lang/koka/#build-from-source
[Perceus]: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf



~ begin wide 
~ begin content { .heading }

```koka {.aside}
// A generator effect with one operation
effect yield<a> {
  fun yield( x : a ) : ()
}

// Traverse a list and yield the elements
fun traverse( xs : list<int> ) : yield<int> () {
  match(xs) {
    Cons(x,xx) -> { yield(x); traverse(xx) }
    Nil        -> ()
  }
}

fun main() : console () {
  with fun yield(i : int) {
    println("yielded " + i.show)    
  }
  [1,2,3].traverse
}
```
[<img align="right" src="https://badges.gitter.im/koka-lang/koka.svg"/>](https://gitter.im/koka-lang/koka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


~~ Header1
[![koka-logo]](book.html){float:left; margin-right:1em;}

[Koka]{font-size:200%; font-weight:bold; display:inline-block; padding-top:0.45em; }\
[A Functional Language with Effect Types and Handlers]{font-size:140%;}
~~

~ end content
~ end wide

~ begin content


Welcome to &koka; -- a strongly typed functional-style language with effect types and handlers.

[Install][#install]{.learn}
[Get Started][kokabook]{.learn}
[The Koka Book][kokabook]{.learn}
[Github][kokarepo]{.learn}
[Libraries][libraries]{.learn}

Note: &koka; v2 is a research language that is currently under heavy development. 
Nevertheless, the language is stable and the compiler
implements the full specification. The main things lacking at the moment are 
libraries, package management, and IDE integration. 
{font-size:small;}

&bigskip;

&bigskip;

~ end content

~ wide
# Why Koka?
~

~ begin content

[INCLUDE=why.md]

~ end content

~ wide
# Install { #install }
~

~ begin content
For Linux and macOS on x86 64-bit, you can install &koka; using:

    \(> **curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh &bar; sh**\)

This also installs syntax highlighting for the VS Code and Atom editors.

    > koka
     _          _           ____
    | |        | |         |__  \
    | | __ ___ | | __ __ _  __) |
    | |/ // _ \| |/ // _' || ___/ welcome to the koka interpreter
    |   <| (_) |   <| (_| ||____| version 2.0.10, Nov 28 2020, libc 64-bit (gcc)
    |_|\_\\___/|_|\_\\__,_|       type :? for help

    loading: std/core
    loading: std/core/types
    loading: std/core/hnd
    >

Type ``:q`` to exit the interpreter.

For detailed instructions and other platforms (including Windows) see the [releases] page.
It is also straightforward to build the compiler [from source][build].

[Read more on running the compiler](book.html#install){.learn}
[Read a Tour of the Koka language](book.html#tour){.learn}

~ end content

~ wide
# References
~

~ begin content
[BIB]
~ end content

~ invisible
[@Leijen:msfp;@Leijen:algefftr;@Leijen:algeff;@Xie:effev;@Leijen:scopedlabels;@Leijen:fcheap;@Leijen:async]
~
