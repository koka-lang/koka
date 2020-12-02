Title         : The Koka Manual
Heading Base  : 1
Heading Depth : 3
Toc Depth     : 3
Css           : styles/koka.css
Css           : styles/manual.css
Css           : https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css
Script        : scripts/manual.js
Colorizer     : unchecked.json
Colorizer     : koka.json
Bibliography  : koka.bib
Description   : Koka Language Specification
Mapsto        : [$\rightsquigarrow$]{.mapsto}

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

[koka-logo]: images/koka-logo.png { max-height: 120px; padding:1rem 1rem 1rem 1.5rem; }

[TITLE]

~ Begin FlexBody

~ Begin SidePanel

[![koka-logo]](https://github.com/koka-lang/koka)

[TOC]

~ End SidePanel

~ Begin MainPanel

~ Begin MainContent

[INCLUDE=getstarted.kk.md]

[INCLUDE=overview.kk.md]

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
