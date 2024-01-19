/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:string` functions.
module std/core/string

import std/core/types
import std/core/hnd
import std/core/int
import std/core/order
import std/core/vector

extern import
  c  file "inline/string"
  js file "inline/string.js"


// Convert a character to a string
pub extern char/string : ( c : char ) -> string
  c  "kk_string_from_char"
  cs inline "Primitive.CharToString(#1)"
  js inline "_char_to_string(#1)"

// Convert a vector of characters to a string.
pub extern vector/string : (vector<char>) -> string
  c  "kk_string_from_chars"
  cs inline "Primitive.CharsToString(#1)"
  js inline "_chars_to_string(#1)"

// Convert a string to a vector of characters.
pub extern vector : ( s : string ) -> vector<char>
  c  "kk_string_to_chars"
  cs inline "Primitive.StringToChars(#1)"
  js inline "_string_to_chars(#1)"

// Are two strings equal?
// Uses exact equality between character codes.
pub inline extern (==) : (string,string) -> bool
  c  "kk_string_is_eq"
  cs inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two strings not equal?
pub inline extern (!=) : (string,string) -> bool
  c  "kk_string_is_neq"
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

fip extern string-cmp( ^x : string, ^y : string ) : int
  c  "kk_string_cmp_int_borrow"
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"

// Compare two strings.
// Uses the character codes directly for comparison
pub fip fun cmp( ^x : string, ^y : string) : order
  string-cmp(x,y).order

pub fip fun order2( x : string, y : string ) : (string,string)
  if (x<=y) then (x,y) else (y,x)

pub fip fun (>=)( ^x : string, ^y : string ) : bool { cmp(x,y) > Lt }
pub fip fun (<=)( ^x : string, ^y : string ) : bool { cmp(x,y) < Gt }
pub fip fun (>)( ^x : string, ^y : string ) : bool  { cmp(x,y) == Gt }
pub fip fun (<)( ^x : string, ^y : string ) : bool  { cmp(x,y) == Lt }

// Choose a non-empty string
pub fun (||)( x : string, y : string ) : string
  if x.is-empty then y else x

// O(n). Return the number of characters in a string.
pub extern count( s : string ) : int
  c  "kk_string_count_int"
  cs "Primitive.StringCount"
  js "_string_count"


// Does string `s` contain the string `sub`?
pub inline extern contains: (s : string, sub : string ) -> bool
  c  "kk_string_contains"
  cs inline "((#1).Contains(#2))"
  js inline "((#1).indexOf(#2) >= 0)"

// Concatenate a vector of strings
pub inline extern vector/join: (v : vector<string> ) -> string
  c  "kk_string_join"
  cs "String.Concat"
  js inline "((#1).join(''))"

// Concatenate a vector of strings with a separator `sep`
pub inline extern vectorsep/join: (v : vector<string>, sep : string ) -> total string
  c  "kk_string_join_with"
  cs "Primitive.Concat"
  js inline "((#1).join(#2))"


extern repeatz( s : string, n : ssize_t ) : string
  c  "kk_string_repeat"
  cs "Primitive.Repeat"
  js "_string_repeat"

// Repeat a string `n` times
pub fun repeat( s : string, ^n : int ) : string
  repeatz(s,n.ssize_t)

// Convert a `:maybe` string to a string using the empty sting for `Nothing`
pub fun maybe/string( ms : maybe<string> ) : string
  match ms
    Nothing -> ""
    Just(s) -> s

// Is a string empty?
pub fun is-empty( s : string ) : bool
  s == ""

// Is a string not empty?
pub fun is-notempty( s : string ) : bool
  s != ""

// Transform a string to a maybe type, using `Nothing` for an empty string
pub fun maybe( s : string ) : maybe<string>
  if s.is-empty then Nothing else Just(s)


// Replace every occurrence of `pattern` to `repl` in a string.
pub inline extern replace-all( s : string, pattern : string, repl : string ) : string
  c  "kk_string_replace_all"
  cs inline "(#1).Replace(#2,#3)"
  js inline r"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)"

// Count occurrences of `pattern` in a string.
pub inline extern stringpat/count( s : string, pattern : string ) : int
  c  "kk_string_count_pattern"
  cs inline "Primitive.Count(#1,#2)"
  js inline r"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)"



// Convert a string to upper-case
pub extern to-upper : (s : string) -> string
  c "kk_string_to_upper"
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()"

// Convert a string to lower-case
pub extern to-lower : (s : string) -> string
  c "kk_string_to_lower"
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()"


// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
pub fun pad-left( s : string, ^width : int, fill : char = ' ') : string
  val w = width
  val n = s.count
  if w <= n
    then s
    else fill.string.repeat( w - n ) ++ s

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
pub fun pad-right( s : string, ^width : int, fill : char = ' ') : string
  val w = width
  val n = s.count
  if w <= n
    then s
    else s ++ fill.string.repeat(w - n)

// Trim whitespace on the left and right side of a string
pub fun trim( s : string ) : string
  s.trim-left.trim-right

// Trim the starting white space of a string
pub inline extern trim-left( s : string ) : string
  c  "kk_string_trim_left"
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"

// Trim the ending white space of a string.
pub inline extern trim-right( s : string ) : string
  c  "kk_string_trim_right"
  cs inline "(#1).TrimEnd()"
  js inline "((#1).replace(/\\s+$/,''))"



inline extern splitv( s : string,  sep : string ) : vector<string>
  c  "kk_string_splitv"
  cs inline "(#1.Split(new String[]{#2}, StringSplitOptions.None))"
  js inline "((#1).split(#2))"

inline extern splitvn( s : string,  sep : string, n : ssize_t ) : vector<string>
  c  "kk_string_splitv_atmost"
  cs inline "#1.Split(new String[]{#2},#3, StringSplitOptions.None)"
  js inline "(#1).split(#2, #3)"

// Split a string into parts that were delimited by `sep`. The delimeters are not included in the results.
// For example: `split("1,,2",",") == ["1","","2"]`
pub fun split( s : string, sep : string ) : list<string>
  splitv(s,sep).list

// Split a string into at most `n` parts that were delimited by a string `sep`. The delimeters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
pub fun splitn/split( s : string, sep: string, ^n : int) : list<string>
  splitvn(s,sep,n.ssize_t).list


// Convert a string to a list of characters
pub extern list( s : string ) : total list<char>
  c  "kk_string_to_list"
  cs inline "Primitive.StringToList(#1)"
  js inline "_string_to_list(#1)"

// Convert a list of characters to a string
pub extern listchar/string( cs : list<char> ) : total string
  c  "kk_string_from_list"
  cs inline "Primitive.ListToString(#1)"
  js inline "_list_to_string(#1)"


