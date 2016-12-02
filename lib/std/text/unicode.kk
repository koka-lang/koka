/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Basic unicode support.

*/
module std/text/unicode

// Grapheme's are an alias for `:string`. 
// Each grapheme is a self-contained symbol consisting of
// a unicode character followed by combining characters and/or 
// combining marks.
public alias grapheme = string

extern include {
  cs file "unicode-inline.cs"
  js file "unicode-inline.js"
}

//--------------------------------------------------------------
// Calculation of graphemes
//--------------------------------------------------------------

// Is this a combining character? 
public fun combining?( c : char ) : bool {
  val i = c.int
  ((i >= 0x0300 && i <= 0x036F) ||
   (i >= 0x1AB0 && i <= 0x1AFF) ||
   (i >= 0x1DC0 && i <= 0x1DFF) ||
   (i >= 0x20D0 && i <= 0x20FF) ||
   (i >= 0xFE20 && i <= 0xFE2F))
}

// Join combining characters with their base into a grapheme.
fun join-combining( cs : list<char>, comb : list<char> = [], acc : list<grapheme> = []) : list<grapheme> {
  match(cs) {
    Cons(c,cc) -> if (c.combining?) 
                   then cc.join-combining( Cons(c,comb), acc )
                   else cc.join-combining( [c], consrev(comb,acc) )
    Nil        -> consrev(comb,acc).reverse
  }
}
fun consrev(xs,xss) {
  if (xs.nil?) then xss else Cons(xs.reverse.string,xss)
}

// Convert a string to a list of graphemes.
// Each grapheme will be in `NFC` normalized form.
public fun graphemes( s  : string ) : list<grapheme> {
  //s.vgraphemes.list
  s.normalize(NFC).list.join-combining
}

//--------------------------------------------------------------
// Normalization
//--------------------------------------------------------------

// Type of unicode [normalization](http://www.unicode.org/reports/tr15)
public type normalization {
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
  cs inline "Unicode.Normalize(#1,#2)"
  js inline "_normalize(#1,#2)"
}

// Normalize a unicode string. If no normalization form is given, `NFC` is used.
public fun normalize( s  : string, norm : normalization = NFC ) : string {
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
public fun width( c : char ) : int {
  if (zero-widths.contains?(c.int)) then 0
  elif (asian-wide.contains?(c.int)) then 2
  else 1
}

// Return the total column-width of a string.
public fun width( s : string ) : int {
  var total := 0
  s.foreach( fun(c) {
    total := total + c.width
  })
  total
}

//--------------------------------------------------------------
// Families: asian-wide, zero-widths
//--------------------------------------------------------------

// These characters are considered wide, i.e. 2 columns wide.
val asian-wide : rtree = build-rtree([
  single(0x1100,0x115F),
  single(0x2329,0x2329), 
  single(0x232A,0x232A),
  single(0x2E80,0x303E), 
  single(0x3040,0xA4CF),
  single(0xAC00,0xD7A3), 
  single(0xF900,0xFAFF), 
  single(0xFE10,0xFE19), 
  single(0xFE30,0xFE6F), 
  single(0xFF00,0xFF60), 
  single(0xFFE0,0xFFE6),
  single(0x20000,0x2FFFD),
  single(0x30000,0x3FFFD),
])

// These characters have zero width
val zero-widths : rtree = build-rtree([
  single(0x0000, 0x001F), single(0x007F, 0x00A0),                                      
  single(0x0300, 0x036F), single(0x0483, 0x0486), single(0x0488, 0x0489),
  single(0x0591, 0x05BD), single(0x05BF, 0x05BF), single(0x05C1, 0x05C2),
  single(0x05C4, 0x05C5), single(0x05C7, 0x05C7), single(0x0600, 0x0603),
  single(0x0610, 0x0615), single(0x064B, 0x065E), single(0x0670, 0x0670),
  single(0x06D6, 0x06E4), single(0x06E7, 0x06E8), single(0x06EA, 0x06ED),
  single(0x070F, 0x070F), single(0x0711, 0x0711), single(0x0730, 0x074A),
  single(0x07A6, 0x07B0), single(0x07EB, 0x07F3), single(0x0901, 0x0902),
  single(0x093C, 0x093C), single(0x0941, 0x0948), single(0x094D, 0x094D),
  single(0x0951, 0x0954), single(0x0962, 0x0963), single(0x0981, 0x0981),
  single(0x09BC, 0x09BC), single(0x09C1, 0x09C4), single(0x09CD, 0x09CD),
  single(0x09E2, 0x09E3), single(0x0A01, 0x0A02), single(0x0A3C, 0x0A3C),
  single(0x0A41, 0x0A42), single(0x0A47, 0x0A48), single(0x0A4B, 0x0A4D),
  single(0x0A70, 0x0A71), single(0x0A81, 0x0A82), single(0x0ABC, 0x0ABC),
  single(0x0AC1, 0x0AC5), single(0x0AC7, 0x0AC8), single(0x0ACD, 0x0ACD),
  single(0x0AE2, 0x0AE3), single(0x0B01, 0x0B01), single(0x0B3C, 0x0B3C),
  single(0x0B3F, 0x0B3F), single(0x0B41, 0x0B43), single(0x0B4D, 0x0B4D),
  single(0x0B56, 0x0B56), single(0x0B82, 0x0B82), single(0x0BC0, 0x0BC0),
  single(0x0BCD, 0x0BCD), single(0x0C3E, 0x0C40), single(0x0C46, 0x0C48),
  single(0x0C4A, 0x0C4D), single(0x0C55, 0x0C56), single(0x0CBC, 0x0CBC),
  single(0x0CBF, 0x0CBF), single(0x0CC6, 0x0CC6), single(0x0CCC, 0x0CCD),
  single(0x0CE2, 0x0CE3), single(0x0D41, 0x0D43), single(0x0D4D, 0x0D4D),
  single(0x0DCA, 0x0DCA), single(0x0DD2, 0x0DD4), single(0x0DD6, 0x0DD6),
  single(0x0E31, 0x0E31), single(0x0E34, 0x0E3A), single(0x0E47, 0x0E4E),
  single(0x0EB1, 0x0EB1), single(0x0EB4, 0x0EB9), single(0x0EBB, 0x0EBC),
  single(0x0EC8, 0x0ECD), single(0x0F18, 0x0F19), single(0x0F35, 0x0F35),
  single(0x0F37, 0x0F37), single(0x0F39, 0x0F39), single(0x0F71, 0x0F7E),
  single(0x0F80, 0x0F84), single(0x0F86, 0x0F87), single(0x0F90, 0x0F97),
  single(0x0F99, 0x0FBC), single(0x0FC6, 0x0FC6), single(0x102D, 0x1030),
  single(0x1032, 0x1032), single(0x1036, 0x1037), single(0x1039, 0x1039),
  single(0x1058, 0x1059), single(0x1160, 0x11FF), single(0x135F, 0x135F),
  single(0x1712, 0x1714), single(0x1732, 0x1734), single(0x1752, 0x1753),
  single(0x1772, 0x1773), single(0x17B4, 0x17B5), single(0x17B7, 0x17BD),
  single(0x17C6, 0x17C6), single(0x17C9, 0x17D3), single(0x17DD, 0x17DD),
  single(0x180B, 0x180D), single(0x18A9, 0x18A9), single(0x1920, 0x1922),
  single(0x1927, 0x1928), single(0x1932, 0x1932), single(0x1939, 0x193B),
  single(0x1A17, 0x1A18), single(0x1B00, 0x1B03), single(0x1B34, 0x1B34),
  single(0x1B36, 0x1B3A), single(0x1B3C, 0x1B3C), single(0x1B42, 0x1B42),
  single(0x1B6B, 0x1B73), single(0x1DC0, 0x1DCA), single(0x1DFE, 0x1DFF),
  single(0x200B, 0x200F), single(0x202A, 0x202E), single(0x2060, 0x2063),
  single(0x206A, 0x206F), single(0x20D0, 0x20EF), single(0x302A, 0x302F),
  single(0x3099, 0x309A), single(0xA806, 0xA806), single(0xA80B, 0xA80B),
  single(0xA825, 0xA826), single(0xFB1E, 0xFB1E), single(0xFE00, 0xFE0F),
  single(0xFE20, 0xFE23), single(0xFEFF, 0xFEFF), single(0xFFF9, 0xFFFB),
  single(0x10A01, 0x10A03), single(0x10A05, 0x10A06), single(0x10A0C, 0x10A0F),
  single(0x10A38, 0x10A3A), single(0x10A3F, 0x10A3F), single(0x1D167, 0x1D169),
  single(0x1D173, 0x1D182), single(0x1D185, 0x1D18B), single(0x1D1AA, 0x1D1AD),
  single(0x1D242, 0x1D244), single(0xE0001, 0xE0001), single(0xE0020, 0xE007F),
  single(0xE0100, 0xE01EF)
])


//--------------------------------------------------------------
// Range trees
//--------------------------------------------------------------

// Type of range trees
type rtree {
  Tip
  Bin( lo : int, hi : int, left : rtree, right : rtree )
}

// Is a value contained in a range tree?
fun contains?( t : rtree, i : int ) : bool {
  match(t) {
    Tip -> True
    Bin(lo,hi,l,r) -> {
      if (i < lo || i > hi)
       then False
       else l.contains?(i) || r.contains?(i)
    }
  }
}

fun build-rtree( xs : list<rtree> ) : rtree {
  unsafe-nodiv{xs.combine}
}

fun single(lo,hi) {
  Bin(lo,hi,Tip,Tip)
}

fun bin(x,y) {
  match((x,y)) {
    (Bin(xlo,xhi),Bin(ylo,yhi)) -> Bin(min(xlo,ylo),max(xhi,yhi),x,y) 
    (Tip,_) -> y
    _ -> x
  }  
}

fun combine( xs : list<rtree> ) : div rtree {
  match (combine-pair(xs)) {
    Nil         -> Tip
    Cons(x,Nil) -> x
    ys          -> combine(ys)
  }
}

fun combine-pair( xs : list<rtree> ) : list<rtree> {
  match(xs) {
    Cons(x,Cons(y,rest)) -> Cons( bin(x,y), combine-pair(rest) ) 
    _ -> xs
  }
}
