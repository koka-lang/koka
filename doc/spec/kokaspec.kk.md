Title         : The Koka Book
Sub Title     : An Introduction and Specification of the Koka language.
Heading Base  : 1
Heading Depth : 4
Toc Depth     : 4
Css           : https://fonts.googleapis.com/css?family=Noto+Sans:400,400italic,700,700italic
Css           : https://fonts.googleapis.com/css?family=Noto+Serif:400,400italic,700,700italic
Css           : https://fonts.googleapis.com/css?family=Roboto+Mono:400,500,700,400italic
Css           : https://fonts.googleapis.com/css?family=Roboto+Slab:300,400,700
Css           : styles/koka.css
Css           : styles/kokaspec.css 
Script        : scripts/kokaspec.js
Colorizer     : unchecked.json
Colorizer     : koka.json
Bibliography  : koka.bib
Description   : Koka Language Specification
[INCLUDE=book]
[INCLUDE=webtoc]
[INCLUDE=webanchors]

~bar          : before='|' 
~many         : before='{ ' after=' }'
~opt          : before='[ ' after=' ]'
xlapp          : &#x2987;
xlidx          : &#12310;
lapp          : _lapp_
lidx          : _lidx_

h1, h2, h3 {
  before: "[&#x1F517;](#&id;){.entity-anchor}"
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

body {
  font-family: 'Noto Serif','Cambria', "Times New Roman", "Liberation Serif", "Times", serif;
}

.toc, h1, h2, h3, h4, h5 {
  font-family: 'Noto Sans', 'Segoe UI', sans-serif;
}


Html Header   : 
  <!-- NO_CLICK_TRACKING -->
  <!--
    Copyright 2012-2016 Microsoft Corporation.
   
    This is free software; you can redistribute it and/or modify it under the
    terms of the Apache License, Version 2.0. A copy of the License can be
    found in the file "license.txt" at the root of this distribution.
  -->

[TITLE]

~ Begin SidePanel

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