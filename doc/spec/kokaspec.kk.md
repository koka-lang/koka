Title         : The Koka Manual
Heading Base  : 1
Heading Depth : 4
Toc Depth     : 4
Css           : styles/koka.css
Css           : styles/kokaspec.css 
Script        : scripts/kokaspec.js
Colorizer     : unchecked.json 
Colorizer     : koka.json
Bibliography  : koka.bib
Description   : Koka Language Specification
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

.pre-fenced3, .code1 {
  language: koka;
}

.pre-indented, .console {
  replace: "/^( *>.*)/\(**``\1``**\)/mg";
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

[koka-logo]: images/koka-logo.png { max-height: 120px; padding-left:1rem; }

[TITLE]

~ Begin FlexBody

~ Begin SidePanel

![koka-logo]

[TOC]

~ End SidePanel

~ Begin MainPanel

[INCLUDE=getstarted.kk.md]

[INCLUDE=overview.kk.md]

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

~ End MainPanel

~ End FlexBody
