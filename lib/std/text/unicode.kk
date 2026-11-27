/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Basic unicode support.

*/
module std/text/unicode

import std/core/undiv
import std/text/unicode-tables

// Grapheme's are an alias for `:string`.
// Each grapheme is a self-contained symbol consisting of
// a unicode character followed by combining characters and/or
//  combining marks.
pub alias grapheme = string

extern import
  c  file "unicode-tables.c"

extern import
  c  file "unicode-inline.c"
  cs file "unicode-inline.cs"
  js file "unicode-inline.js"

//--------------------------------------------------------------
// Calculation of graphemes
//--------------------------------------------------------------

// Is this a combining character? (i.e. a mark with general category `Mn`, `Mc`, or `Me`)
pub fun is-combining( c : char ) : bool
  marks.force.contains(c.int)

// Join combining characters with their base into a grapheme.
fun join-combining( cs : list<char>, comb : list<char> = [], acc : list<grapheme> = []) : list<grapheme> {
  match(cs) {
    Cons(c,cc) -> if (c.is-combining)
                   then cc.join-combining( Cons(c,comb), acc )
                   else cc.join-combining( [c], consrev(comb,acc) )
    Nil        -> consrev(comb,acc).reverse
  }
}
fun consrev(xs : list<char>, xss : list<string>) : list<string> 
  if (xs.is-nil) then xss else Cons(xs.reverse.string,xss)


// Convert a string to a list of graphemes.
// Each grapheme will be in `NFC` normalized form.
pub fun graphemes( s  : string ) : list<grapheme> {
  //s.vgraphemes.list
  s.normalize(NFC).list.join-combining
}

//--------------------------------------------------------------
// Normalization
//--------------------------------------------------------------

// Type of unicode [normalization](https://www.unicode.org/reports/tr15)
pub type normalization {
  // Canonical composition (default). Use composed characters when possible.
  con NFC
  // Canonical decomposition.
  con NFD
  // Compatibility composition.
  con NFKC
  // Compatibility decomposition.
  con NFKD
}


extern normalizex( s  : string, norm : string ) : string {
  c  "kk_unicode_normalize"
  cs inline "Unicode.Normalize(#1,#2)"
  js inline "_normalize(#1,#2)"
}

// Normalize a unicode string. If no normalization form is given, `NFC` is used.
// On the C backend, the compatibility forms `NFKC` and `NFKD` are treated as `NFC` and `NFD`.
pub fun normalize( s  : string, norm : normalization = NFC ) : string {
  val norms = match(norm) {
    NFC -> "NFC"
    NFD -> "NFD"
    NFKC -> "NFKC"
    NFKD -> "NFKD"
  }
  s.normalizex(norms)
}


//--------------------------------------------------------------
// Column widths
//--------------------------------------------------------------

// Return the column-width of a unicode character.
// Equivalent to ``wcwidth``
pub fun char/width( c : char ) : int
  val i = c.int
  if zero-widths.force.contains(i) then 0
  elif wides.force.contains(i) then 2
  else 1

// Return the total column-width of a string.
pub fun string/width( s : string ) : int {
  var total := 0
  s.foreach( fn(c : char) {
    total := total + c.width
  })
  total
}


//--------------------------------------------------------------
// Range trees, built on demand from the generated range lists
//--------------------------------------------------------------

val marks       : delayed<total,rtree> = delay{ build-rtree(mark-ranges()) }
val zero-widths : delayed<total,rtree> = delay{ build-rtree(zero-width-ranges()) }
val wides       : delayed<total,rtree> = delay{ build-rtree(wide-ranges()) }

// A binary search tree of disjoint inclusive ranges
type rtree
  Tip
  Bin( lo : int, hi : int, left : rtree, right : rtree )

fun rtree/contains( t : rtree, i : int ) : bool
  match t
    Bin(lo,hi,l,r) -> if i < lo then l.contains(i) elif i > hi then r.contains(i) else True
    Tip -> False

// Build a balanced tree from sorted disjoint ranges
fun build-rtree( rs : list<(int,int)> ) : rtree
  val (left,right) = rs.split(rs.length / 2)
  match right
    Cons((lo,hi),rest) -> Bin(lo, hi, build-rtree(pretend-decreasing(left)), build-rtree(pretend-decreasing(rest)))
    Nil -> Tip
