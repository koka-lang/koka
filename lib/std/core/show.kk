/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard show functions.
module std/core/show

import std/core/types
import std/core/int
import std/core/float64
import std/core/char
import std/core/string
import std/core/list

extern import
  js file "show-inline.js"

// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic show: shows the internal representation of an object as a string
// Note: this breaks parametricity so it should not be public
extern gshow : forall<a> a -> string
  c "kk_show_any"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

extern int-show-hex(i:int,use-capitals:bool) : string
  c "kk_integer_to_hex_string"
  cs "Primitive.IntShowHex"
  js "$std_core_int._int_showhex"

// Show an `:int` as a hexadecimal value.\
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
pub fun show-hex( i : int, width : int = 1, use-capitals : bool = True, pre : string = "0x" )
  (if i<0 then "-" else "") ++ pre ++ int-show-hex(i.abs,use-capitals).pad-left(width,'0')


// Show a character as a string
pub fun show-char( c : char ) : string
  if c < ' ' || c > '~' then
    if c == '\n' then "\\n"
    elif c == '\r' then "\\r"
    elif c == '\t' then "\\t"
    elif c.int <= 0xFF then "\\x" ++ c.int.show-hex(2,pre="")
    elif c.int <= 0xFFFF then "\\u" ++ c.int.show-hex(4,pre="")
    else "\\U" ++ c.int.show-hex(6,pre="")
  else
    if c == '\'' then "\\'"
    elif c == '\"' then "\\\""
    elif c == '\\' then "\\\\"
    else c.string

// Show a `:char` as a character literal
pub fun char/show( c : char ) : string
  "'" ++ c.show-char ++ "'"

// Show a string as a string literal
pub noinline fun string/show( s : string ) : string
  "\"" ++ s.list.map(show-char).join ++ "\""

// Show an `:sslice` as a string literal
pub fun sslice/show( s : sslice ) : string
  s.string.show

// Convert a unit value `()` to a string
pub fun unit/show( u : () ) : string
  "()"

// Show a tuple
pub fun tuple2/show( x : (a,b), ?fst/show : a -> e string, ?snd/show : b -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ ")"

// Show a triple
pub fun tuple3/show( x : (a,b,c), ?fst/show : a -> e string, ?snd/show : b -> e string, ?thd/show : c -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ ")"

// Show a `:maybe` type
pub fun maybe/show( mb : maybe<a>, ?show : a -> e string ) : e string
  match mb
    Just(x) -> "Just(" ++ x.show ++ ")"
    Nothing -> "Nothing"


// _deprecated_, use `list/show` instead.
pub fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
  list/show(xs,?show=show-elem)

// _deprecated_, use `tuple2/show` instead
pub fun show-tuple( x : (a,b), showfst : a -> e string, showsnd : b -> e string ) : e string
  tuple2/show(x,?fst/show=showfst,?snd/show=showsnd)

