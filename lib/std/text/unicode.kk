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

// Grapheme's are an alias for `:string`.
// Each grapheme is a self-contained symbol consisting of
// a unicode character followed by combining characters and/or
// combining marks.
pub alias grapheme = string

extern import {
  cs file "unicode-inline.cs"
  js file "unicode-inline.js"
}

//--------------------------------------------------------------
// Calculation of graphemes
//--------------------------------------------------------------

// Is this a combining character?
pub fun is-combining( c : char ) : bool {
  val i = c.int
  ((i >= 0x0300 && i <= 0x036F) ||
   (i >= 0x1AB0 && i <= 0x1AFF) ||
   (i >= 0x1DC0 && i <= 0x1DFF) ||
   (i >= 0x20D0 && i <= 0x20FF) ||
   (i >= 0xFE20 && i <= 0xFE2F) ||
   (i >= 0xFE00 && i <= 0xFE0F)) // Added variation selectors
   // Should we instead add `zero-widths.force.contains(i)`?
}

// Join combining characters with their base into a grapheme.
fun join-combining( cs : list<char>, comb : list<char> = [], acc : list<grapheme> = []) : list<grapheme> {
  match(cs) {
    Cons(zwj, cc) | zwj.int == 0x200D -> // Handle zero-width-joiner
      match cc
        Cons(c, cc') -> cc'.join-combining(Cons(c, Cons(zwj,comb)), acc)
        Nil -> cc.join-combining(Cons(zwj, comb), acc)
    Cons(c,cc) -> if (c.is-combining)
                   then cc.join-combining( Cons(c,comb), acc )
                   else cc.join-combining( [c], consrev(comb,acc) )
    Nil        -> consrev(comb,acc).reverse
  }
}
fun consrev(xs,xss) {
  if (xs.is-nil) then xss else Cons(xs.reverse.string,xss)
}

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
  c  inline "#1"                         // TODO: use utf8proc library?
  cs inline "Unicode.Normalize(#1,#2)"
  js inline "_normalize(#1,#2)"
}

// Normalize a unicode string. If no normalization form is given, `NFC` is used.
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
pub fun char/width( c : char ) : int {
  if (zero-widths.force.contains(c.int)) then 0
  elif (asian-wide.force.contains(c.int)) then 2
  else 1
}

// Return the total column-width of a string.
pub fun string/width( s : string ) : int {
  var total := 0
  s.foreach( fn(c) {
    total := total + c.width
  })
  total
}

//--------------------------------------------------------------
// Families: asian-wide, zero-widths
//--------------------------------------------------------------

// These characters are considered wide, i.e. 2 columns wide.
// https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
// See ranges with postfix ;W
// 
// Update with `stack exec koka -- util/update-unicode.kk -- -a`
// TODO: Handle 'unassigned' ranges: (Following is an excerpt from https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt)
// - All code points, assigned or unassigned, that are not listed
//      explicitly are given the value "N".
// - The unassigned code points in the following blocks default to "W":
//        CJK Unified Ideographs Extension A: U+3400..U+4DBF
//        CJK Unified Ideographs:             U+4E00..U+9FFF
//        CJK Compatibility Ideographs:       U+F900..U+FAFF
// - All undesignated code points in Planes 2 and 3, whether inside or
//     outside of allocated blocks, default to "W":
//        Plane 2:                            U+20000..U+2FFFD
//        Plane 3:                            U+30000..U+3FFFD
val asian-wide : delayed<total,rtree> = delay{
  build-rtree([
    single(0x1100,0x115F),
    single(0x231A,0x231B),
    single(0x2329,0x2329),
    single(0x232A,0x232A),
    single(0x23E9,0x23EC),
    single(0x23F0,0x23F0),
    single(0x23F3,0x23F3),
    single(0x25FD,0x25FE),
    single(0x2614,0x2615),
    single(0x2648,0x2653),
    single(0x267F,0x267F),
    single(0x2693,0x2693),
    single(0x26A1,0x26A1),
    single(0x26AA,0x26AB),
    single(0x26BD,0x26BE),
    single(0x26C4,0x26C5),
    single(0x26CE,0x26CE),
    single(0x26D4,0x26D4),
    single(0x26EA,0x26EA),
    single(0x26F2,0x26F3),
    single(0x26F5,0x26F5),
    single(0x26FA,0x26FA),
    single(0x26FD,0x26FD),
    single(0x2705,0x2705),
    single(0x270A,0x270B),
    single(0x2728,0x2728),
    single(0x274C,0x274C),
    single(0x274E,0x274E),
    single(0x2753,0x2755),
    single(0x2757,0x2757),
    single(0x2795,0x2797),
    single(0x27B0,0x27B0),
    single(0x27BF,0x27BF),
    single(0x2B1B,0x2B1C),
    single(0x2B50,0x2B50),
    single(0x2B55,0x2B55),
    single(0x2E80,0x2E99),
    single(0x2E9B,0x2EF3),
    single(0x2F00,0x2FD5),
    single(0x2FF0,0x2FFF),
    single(0x3001,0x3003),
    single(0x3004,0x3004),
    single(0x3005,0x3005),
    single(0x3006,0x3006),
    single(0x3007,0x3007),
    single(0x3008,0x3008),
    single(0x3009,0x3009),
    single(0x300A,0x300A),
    single(0x300B,0x300B),
    single(0x300C,0x300C),
    single(0x300D,0x300D),
    single(0x300E,0x300E),
    single(0x300F,0x300F),
    single(0x3010,0x3010),
    single(0x3011,0x3011),
    single(0x3012,0x3013),
    single(0x3014,0x3014),
    single(0x3015,0x3015),
    single(0x3016,0x3016),
    single(0x3017,0x3017),
    single(0x3018,0x3018),
    single(0x3019,0x3019),
    single(0x301A,0x301A),
    single(0x301B,0x301B),
    single(0x301C,0x301C),
    single(0x301D,0x301D),
    single(0x301E,0x301F),
    single(0x3020,0x3020),
    single(0x3021,0x3029),
    single(0x302A,0x302D),
    single(0x302E,0x302F),
    single(0x3030,0x3030),
    single(0x3031,0x3035),
    single(0x3036,0x3037),
    single(0x3038,0x303A),
    single(0x303B,0x303B),
    single(0x303C,0x303C),
    single(0x303D,0x303D),
    single(0x303E,0x303E),
    single(0x3041,0x3096),
    single(0x3099,0x309A),
    single(0x309B,0x309C),
    single(0x309D,0x309E),
    single(0x309F,0x309F),
    single(0x30A0,0x30A0),
    single(0x30A1,0x30FA),
    single(0x30FB,0x30FB),
    single(0x30FC,0x30FE),
    single(0x30FF,0x30FF),
    single(0x3105,0x312F),
    single(0x3131,0x318E),
    single(0x3190,0x3191),
    single(0x3192,0x3195),
    single(0x3196,0x319F),
    single(0x31A0,0x31BF),
    single(0x31C0,0x31E3),
    single(0x31EF,0x31EF),
    single(0x31F0,0x31FF),
    single(0x3200,0x321E),
    single(0x3220,0x3229),
    single(0x322A,0x3247),
    single(0x3250,0x3250),
    single(0x3251,0x325F),
    single(0x3260,0x327F),
    single(0x3280,0x3289),
    single(0x328A,0x32B0),
    single(0x32B1,0x32BF),
    single(0x32C0,0x32FF),
    single(0x3300,0x33FF),
    single(0x3400,0x4DBF),
    single(0x4E00,0x9FFF),
    single(0xA000,0xA014),
    single(0xA015,0xA015),
    single(0xA016,0xA48C),
    single(0xA490,0xA4C6),
    single(0xA960,0xA97C),
    single(0xAC00,0xD7A3),
    single(0xF900,0xFA6D),
    single(0xFA6E,0xFA6F),
    single(0xFA70,0xFAD9),
    single(0xFADA,0xFAFF),
    single(0xFE10,0xFE16),
    single(0xFE17,0xFE17),
    single(0xFE18,0xFE18),
    single(0xFE19,0xFE19),
    single(0xFE30,0xFE30),
    single(0xFE31,0xFE32),
    single(0xFE33,0xFE34),
    single(0xFE35,0xFE35),
    single(0xFE36,0xFE36),
    single(0xFE37,0xFE37),
    single(0xFE38,0xFE38),
    single(0xFE39,0xFE39),
    single(0xFE3A,0xFE3A),
    single(0xFE3B,0xFE3B),
    single(0xFE3C,0xFE3C),
    single(0xFE3D,0xFE3D),
    single(0xFE3E,0xFE3E),
    single(0xFE3F,0xFE3F),
    single(0xFE40,0xFE40),
    single(0xFE41,0xFE41),
    single(0xFE42,0xFE42),
    single(0xFE43,0xFE43),
    single(0xFE44,0xFE44),
    single(0xFE45,0xFE46),
    single(0xFE47,0xFE47),
    single(0xFE48,0xFE48),
    single(0xFE49,0xFE4C),
    single(0xFE4D,0xFE4F),
    single(0xFE50,0xFE52),
    single(0xFE54,0xFE57),
    single(0xFE58,0xFE58),
    single(0xFE59,0xFE59),
    single(0xFE5A,0xFE5A),
    single(0xFE5B,0xFE5B),
    single(0xFE5C,0xFE5C),
    single(0xFE5D,0xFE5D),
    single(0xFE5E,0xFE5E),
    single(0xFE5F,0xFE61),
    single(0xFE62,0xFE62),
    single(0xFE63,0xFE63),
    single(0xFE64,0xFE66),
    single(0xFE68,0xFE68),
    single(0xFE69,0xFE69),
    single(0xFE6A,0xFE6B),
    single(0x16FE0,0x16FE1),
    single(0x16FE2,0x16FE2),
    single(0x16FE3,0x16FE3),
    single(0x16FE4,0x16FE4),
    single(0x16FF0,0x16FF1),
    single(0x17000,0x187F7),
    single(0x18800,0x18AFF),
    single(0x18B00,0x18CD5),
    single(0x18D00,0x18D08),
    single(0x1AFF0,0x1AFF3),
    single(0x1AFF5,0x1AFFB),
    single(0x1AFFD,0x1AFFE),
    single(0x1B000,0x1B0FF),
    single(0x1B100,0x1B122),
    single(0x1B132,0x1B132),
    single(0x1B150,0x1B152),
    single(0x1B155,0x1B155),
    single(0x1B164,0x1B167),
    single(0x1B170,0x1B2FB),
    single(0x1F004,0x1F004),
    single(0x1F0CF,0x1F0CF),
    single(0x1F18E,0x1F18E),
    single(0x1F191,0x1F19A),
    single(0x1F200,0x1F202),
    single(0x1F210,0x1F23B),
    single(0x1F240,0x1F248),
    single(0x1F250,0x1F251),
    single(0x1F260,0x1F265),
    single(0x1F300,0x1F320),
    single(0x1F32D,0x1F335),
    single(0x1F337,0x1F37C),
    single(0x1F37E,0x1F393),
    single(0x1F3A0,0x1F3CA),
    single(0x1F3CF,0x1F3D3),
    single(0x1F3E0,0x1F3F0),
    single(0x1F3F4,0x1F3F4),
    single(0x1F3F8,0x1F3FA),
    single(0x1F3FB,0x1F3FF),
    single(0x1F400,0x1F43E),
    single(0x1F440,0x1F440),
    single(0x1F442,0x1F4FC),
    single(0x1F4FF,0x1F53D),
    single(0x1F54B,0x1F54E),
    single(0x1F550,0x1F567),
    single(0x1F57A,0x1F57A),
    single(0x1F595,0x1F596),
    single(0x1F5A4,0x1F5A4),
    single(0x1F5FB,0x1F5FF),
    single(0x1F600,0x1F64F),
    single(0x1F680,0x1F6C5),
    single(0x1F6CC,0x1F6CC),
    single(0x1F6D0,0x1F6D2),
    single(0x1F6D5,0x1F6D7),
    single(0x1F6DC,0x1F6DF),
    single(0x1F6EB,0x1F6EC),
    single(0x1F6F4,0x1F6FC),
    single(0x1F7E0,0x1F7EB),
    single(0x1F7F0,0x1F7F0),
    single(0x1F90C,0x1F93A),
    single(0x1F93C,0x1F945),
    single(0x1F947,0x1F9FF),
    single(0x1FA70,0x1FA7C),
    single(0x1FA80,0x1FA88),
    single(0x1FA90,0x1FABD),
    single(0x1FABF,0x1FAC5),
    single(0x1FACE,0x1FADB),
    single(0x1FAE0,0x1FAE8),
    single(0x1FAF0,0x1FAF8),
    single(0x20000,0x2A6DF),
    single(0x2A6E0,0x2A6FF),
    single(0x2A700,0x2B739),
    single(0x2B73A,0x2B73F),
    single(0x2B740,0x2B81D),
    single(0x2B81E,0x2B81F),
    single(0x2B820,0x2CEA1),
    single(0x2CEA2,0x2CEAF),
    single(0x2CEB0,0x2EBE0),
    single(0x2EBE1,0x2EBEF),
    single(0x2EBF0,0x2EE5D),
    single(0x2EE5E,0x2F7FF),
    single(0x2F800,0x2FA1D),
    single(0x2FA1E,0x2FA1F),
    single(0x2FA20,0x2FFFD),
    single(0x30000,0x3134A),
    single(0x3134B,0x3134F),
    single(0x31350,0x323AF),
    single(0x323B0,0x3FFFD)
  ])
}

// These characters have zero width
val zero-widths : delayed<total,rtree> = delay{
  build-rtree([
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
}


//--------------------------------------------------------------
// Range trees
//--------------------------------------------------------------

// Type of range trees
type rtree {
  Tip
  Bin( lo : int, hi : int, left : rtree, right : rtree )
}

// Is a value contained in a range tree?
fun contains( t : rtree, i : int ) : bool {
  match(t) {
    Tip -> True
    Bin(lo,hi,l,r) -> {
      if (i < lo || i > hi)
       then False
       else l.contains(i) || r.contains(i)
    }
  }
}

fun build-rtree( xs : list<rtree> ) : rtree {
  xs.combine
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

fun combine( xs : list<rtree> ) : rtree {
  match (combine-pairs(xs)) {
    Nil         -> Tip
    Cons(x,Nil) -> x
    ys          -> combine(pretend-decreasing(ys))
  }
}

fun combine-pairs( xs : list<rtree> ) : list<rtree> {
  match(xs) {
    Cons(x,Cons(y,rest)) -> Cons( bin(x,y), combine-pairs(rest) )
    _ -> xs
  }
}
