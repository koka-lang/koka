/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:char` functions.
//
// Characters are unicode _codepoint_\/s.
// This is different from a unicode _grapheme_ which represents a single displayed
// symbol and can consists of multiple codepoints due to combining characters and marks.
module std/core/char

import std/core/types
import std/core/int

// ----------------------------------------------------------------------------
// Characters
// ----------------------------------------------------------------------------

// Are two characters equal?
pub inline fip extern (==) : (char,char) -> bool
  inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two characters not equal?
pub inline fip extern (!=) : (char,char) -> bool
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

// Is a character code point lower or equal to that of another?
pub inline fip extern (<=) : (char,char) -> bool
  inline "(#1 <= #2)"

// Is a character code point greater or equal to that of another?
pub inline fip extern (>=) : (char,char) -> bool
  inline "(#1 >= #2)"

// Is a character code point lower to that of another?
pub inline fip extern (<)  : (char,char) -> bool
  inline "(#1 < #2)"

// Is a character code point greater to that of another?
pub inline fip extern (>)  : (char,char) -> bool
  inline "(#1 > #2)"

// Compare character code points.
pub fip fun cmp( x : char, y : char ) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq

// Order two characters in ascending order.
pub fip fun order2( x : char, y : char ) : order2<char>
  if (x==y) then Eq2(x) elif (x < y) then Lt2(x,y) else Gt2(y,x)

// Convert a character to its unicode code point
pub inline fip extern int : (char) -> int
  inline "#1"
  c "kk_integer_from_int"
  cs inline "new BigInteger(#1)"

// Convert a unicode code point to a character
pub inline fip extern int/char( i : int) : char
  inline "(#1)"
  c "kk_integer_clamp32"
  cs inline "Primitive.IntToInt32(#1)"

// Add two character code points
pub fip fun (+)(c : char, d : char) : total char
  (c.int + d.int).char

// Subtract two character code points
pub fip fun (-)(c : char, d : char) : total char
  (c.int - d.int).char

// Is the character a lower-case ASCII character?
pub fip fun is-lower( c : char ) : bool
  c >= 'a' && c <= 'z'

// Is the character an upper-case ASCII character?
pub fip fun is-upper( c : char ) : bool
  c >= 'A' && c <= 'Z'

// Is the character an ASCII digit ?
pub fip fun is-digit( c : char ) : bool
  c >= '0' && c <= '9'

// Is the character an ASCII hexa-decimal digit?
pub fip fun is-hex-digit( c : char ) : bool
  c.is-digit || (c >= 'a' && c <= 'f') || (c >= 'A'  && c <= 'F')

// Is the character an ASCII letter?
pub fip fun is-alpha( c : char ) : bool
  c.is-lower || c.is-upper

// Is the character ASCII letter or digit?
pub fip fun is-alpha-num( c : char ) : bool
  c.is-alpha || c.is-digit

// Is the character an ASCII character, e.g. `c <= '\x7F'`?
pub fip fun is-ascii( c : char )     : bool
  c <= '\x7F'

// Is the character an ASCII control character, e.g. `c < ' '`?
pub fip fun is-control( c : char )   : bool
  c < ' '

// Tests if a character is an element of `" \t\n\r"`
pub fip fun is-white( c : char )     : bool
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
