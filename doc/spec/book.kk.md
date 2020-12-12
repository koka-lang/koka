Title         : The &koka; Programming Language
Title Note    : [(Daan Leijen, &date;)]{font-size:75%}
Heading Base  : 1
Heading Depth : 3
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

[INCLUDE=book]
[INCLUDE=styles/webanchors]
[INCLUDE=styles/webtoc]

~bar          : before='|'
~many         : before='{ ' after=' }'
~opt          : before='[ ' after=' ]'

toc {
  .expand-all;
}

toc.toc-contents {
  before:clear;
}

.math-inline {
  input: mathpre;
}

.pre-fenced3, .code1 {
  language: koka;
}

.pre-indented, .console {
  replace: "/^( *>[^\n\r]+)/\(**``\1``**\)/mg";
}

.mapsto {
  padding: 0ex 1em;
  align-self: center;
  font-size: 125%;
}

.learn {
  .button;
}

@if preview {
  .code1 {
    border-bottom: 1px solid green;
  }

  .pre-fenced3 {
    border-left: 0.5ex solid green;
  }

  .token.predefined {
    color: navy;
  }
}

h4 {
  @h1-h2-h3-h4: upper-alpha;
}

.toc, h1, h2, h3, h4, h5 {
  font-family: 'Nunito', 'Segoe UI', sans-serif;
}

li {
  margin-bottom: 1ex;
}

.note {
  font-style: italic;
}

.advanced {
  .boxed;
  before: "[advanced]{.boxed-label}";
}

.translate {
  .boxed;
  replace: "[translation]{.boxed-label}&nl;~begin translate-row&nl;&source;&nl;~end translate-row&nl;";
}

.boxed {
  border: 1px solid #AAA;
  padding: 0em 1em;  
}

.boxed-label {
  display: block;
  float: left;
  margin: -1.5em 0em -1em -1em;
  font-size: 70%;
  color: #999;
}

[koka-logo]: images/koka-logo.png { max-height: 120px; padding:1rem 1rem 1rem 1.5rem; }

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
