Title         : The &koka; Programming Language
Title Note    : [(Daan Leijen, &date;)]{font-size:75%}
Heading Base  : 1
Heading Depth : 3
Toc Depth     : 3
Css           : styles/koka.css
Css           : styles/book.css
Css           : https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css
Css           : https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/brands.min.css
Script        : scripts/book.js
Colorizer     : unchecked
Colorizer     : koka
Colorizer     : arm64
Bibliography  : koka.bib
Description   : Koka Language Specification
Mapsto        : [$\rightsquigarrow$]{.mapsto}
Koka          : Koka

[INCLUDE=./book.mdk]
[INCLUDE=./styles/webanchors.mdk]
[INCLUDE=./styles/webtoc.mdk]
[INCLUDE=./book-style.md]

body {
  .colored
}

~bar          : before='|'
~many         : before='{ ' after=' }'
~manyn        : before='{ ' after=' }~_n_~'
~manyx        : before='{ ' after=' }'
~opt          : before='[ ' after=' ]'


[koka-logo]: images/koka-logo-filled.png { max-height: 120px; padding:1rem 1rem 1rem 1.5rem; }

~ Begin MainHeader

[TITLE]

~ End MainHeader

~ Begin FlexBody

~ Begin SidePanel

[![koka-logo]](https://github.com/koka-lang/koka)

[TOC]

~ End SidePanel

~ Begin MainPanel

~ Begin MainContent

[INCLUDE=getstarted.kk.md]

# Why &koka;? { #why; }

There are many new languages being designed, but only few
bring fundamentally new concepts -- like Haskell with
pure versus monadic programming, or Rust with borrow checking.
&koka; distinguishes itself through _effect typing_, _effect handlers_,
and _Perceus_ memory management:

[INCLUDE=why.kk.md]

[INCLUDE=tour.kk.md]

[INCLUDE=spec.kk.md]

[BIB]

# Appendix {-; toc:clear; }

# Full grammar specification { @h1:'A'; }

## Lexical syntax { #sec:full-lexical }

~ div {#full-lexical}
~

## Context-free syntax  { #sec:full-grammar }

~ div {#full-grammar}
~

~ End MainContent

~ End MainPanel

~ End FlexBody
