/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Efficient views on strings.
module std/core/sslice

import std/core/types
import std/core/undiv
import std/core/unsafe // unsafe-ptr-eq
import std/core/hnd
import std/core/int
import std/core/string

extern import
  c  file "inline/sslice"
  js file "inline/sslice.js"



// A `:sslice` represents a sub-slice of string and
// has a specific start position and character count. It is used
// instead of string indices to make the actual internal representation
// of strings abstract (like UTF-8 or UTF-16). String slices are
// returned by functions that find sub strings or patterns in
// in strings. Use `string:(slice : sslice) -> string` to
// create a fresh substring from a slice.
//
// The start and len fields of a slice are in terms of bytes.
// Slices should only be interacted with safe methods defined in core
// that take care to not cut strings in the middle of a utf codepoint.
abstract value struct sslice( str : string, start : int, len : int )

// Internal export for the regex module
pub fun @new-sslice( str :string, start: int, len : int )
  Sslice(str,start,len)

// Equality based on contents of the slice
// O(`n`+`m`) where `n` and `m` are the lengths of the two strings
pub fun (==)(slice1: sslice, slice2: sslice ): bool
  match (slice1, slice2)
    (Sslice(s1, start1, len1), Sslice(s2, start2, len2)) ->
      if unsafe-ptr-eq(s1,s2) && start1==start2 && len1==len2
        then True // fast path
        else slice1.string == slice2.string

// Inequality based on contents of the slice
// O(`n`+`m`) where `n` and `m` are the lengths of the two strings
pub fun (!=)(slice1: sslice, slice2: sslice ): bool
  !(slice1 == slice2)

// Equality of slices at the same offset and length on an equal string
// (The strings do not have to be referentially identical though)
pub fun (===)(slice1: sslice, slice2: sslice ): bool
  match (slice1, slice2)
    (Sslice(s1, start1, len1), Sslice(s2, start2, len2)) -> (start1==start2 && len1==len2 && s1==s2)

// Inequality of slices at the same offset and length on the same string.
// (The strings do not have to be referentially identical though)
pub fun (!==)(slice1: sslice, slice2: sslice ): bool
  !(slice1===slice2)

// O(n). Return the number of characters in a string slice
pub extern count( slice : sslice ) : int
  c  "kk_slice_count"
  cs "Primitive.SliceCount"
  js "_sslice_count"

extern first1(s : string ) : sslice
  c  "kk_slice_first"
  cs "Primitive.SliceFirst"
  js "_sslice_first"

extern last1(s : string ) : sslice
  c  "kk_slice_last"
  cs "Primitive.SliceLast"
  js "_sslice_last"

// O(`n`). The first `n` (default = `1`) characters in a string.
pub fun first(s : string, n : int = 1) : sslice
  val slice = s.first1
  if n==1 then slice else slice.extend( n - 1 )

// O(`n`). The last `n` (default = `1`) characters in a string
pub fun last(s : string, n : int = 1) : sslice
  val slice = s.last1
  if n==1 then slice
            else slice.advance(1 - n).extend(n - 1)

// Convert the first character of a string to uppercase.
pub fun capitalize( s : string ) : string
  s.first.string.to-upper ++ s.first.after.string

// O(1). The entire string as a slice
pub fun slice( s : string ) : sslice
  Sslice(s,0,s.length)

// An empty slice
val empty = Sslice("",0,0)

// Is a slice empty?
pub fun is-empty( slice : sslice ) : bool
  !slice.len.is-pos

// An invalid slice
val invalid = Sslice("",-1,0)

// Is a slice invalid?
pub fun is-valid( slice : sslice ) : bool
  slice.start >= 0

// Is a slice not empty?
pub fun is-notempty( slice : sslice ) : bool
  slice.len.is-pos

// Return the first character of a string as a string (or the empty string)
pub fun head( s : string ) : string
  s.first.string

// Return the tail of a string (or the empty string)
pub fun tail( s : string ) : string
  s.first.after.string

// Length returns the length in the platform specific encoding (and should not be exported)
inline extern length( s : string ) : int
  c inline "kk_string_len_int(#1,kk_context())"
  cs inline "#1.Length"
  js inline "#1.length"

// Return the common prefix of two strings (upto `upto` characters (default is minimum length of the two strings))
pub extern common-prefix(s : string, t : string, ^upto : int = -1 ) : sslice
  c  "kk_slice_common_prefix_borrow"
  cs "Primitive.SliceCommonPrefix"
  js "_sslice_common_prefix"

// O(`count`). Advance the start position of a string slice by `count` characters
// up to the end of the string.
// A negative `count` advances the start position backwards upto the first position
// in a string.
// Maintains the character count of the original slice upto the end of the string.
// For example:
//
// * `"abc".first.advance(1).string == "b"`,
// * `"abc".first.advance(3).string == ""`,
// * `"abc".last.advance(-1).string == "b"`.
//
pub extern advance( slice : sslice, ^count : int ) : sslice
  c  "kk_slice_advance_borrow"
  cs "Primitive.SliceAdvance"
  js "_sslice_advance"

// O(`count`). Extend a string slice by `count` characters up to the end of the string.
// A negative `count` shrinks the slice up to the empty slice.
// For example:
//
// * `"abc".first.extend(1).string == "ab"`
// * `"abc".last.extend(-1).string == ""`
//
pub extern extend( slice : sslice, ^count : int ) : sslice
  c  "kk_slice_extend_borrow"
  cs "Primitive.SliceExtend"
  js "_sslice_extend"

// O(1). Return the string slice from the start of a string up to the
// start of `slice` argument.
pub fun before(slice : sslice) : sslice
  val Sslice(s,start,_len) = slice
  Sslice(s,0,start)

// O(1). Return the string slice from the end of `slice` argument
// to the end of the string.
pub fun after(slice : sslice) : sslice
  val Sslice(s,start,len) = slice
  Sslice(s,start+len,s.length - (start+len))

// O(n). Copy the `slice` argument into a fresh string.
// Takes O(1) time if the slice covers the entire string.
pub extern string( slice : sslice ) : string
  c  "kk_slice_to_string"
  cs "Primitive.SliceToString"
  js "_slice_to_string"

// If the slice is not empty,
// return the first character, and a new slice that is advanced by 1.
pub extern next( slice : sslice ) : maybe<(char,sslice)>
  c  "kk_slice_next"
  cs "Primitive.SliceNext"
  js "_sslice_next"

// Truncates a slice to length 0
pub fun truncate( slice: sslice ): sslice
  Sslice(slice.str, slice.start, 0)

// Gets up to (`end`-`start`) characters from the slice beginning from `start`.
// If either start or end is negative, returns the original slice
pub fun subslice( slice: sslice, start : int, end : int ): sslice
  if start < 0 || end < 0 then slice else
    // Advances to start, zeros the length, and then extends (end - start characters)
    slice.advance(start).truncate.extend(end - start)

// Gets a slice that only includes up to n characters from the start of the slice.
pub fun take( slice: sslice, n : int): sslice
  if n <= 0 then return slice else slice.truncate.extend(n)

// Gets a slice that drops the first n characters, shrinking the length of the slice by n accordingly.
// If the slice does not have n characters, then the slice is shrunk to an empty slice.
//
// If maintaining the length of the slice is important, use `advance` instead.
pub fun drop( slice: sslice, n : int): sslice
  if n <= 0 then return slice else slice.advance(n).truncate.extend(slice.count - n)

// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.
pub fun foreach-while( slice : sslice, action : (c : char) -> e maybe<a> ) : e maybe<a>
  match slice.next
    Nothing -> Nothing
    Just((c,rest)) ->
      match action(c)
        Nothing -> foreach-while(pretend-decreasing(rest),action)
        res     -> res


// Apply a function for each character in a string slice.
pub fun foreach( slice : sslice, action : (c:char) -> e ()) : e ()
  foreach-while( slice ) fn(c)
    action(c)
    Nothing
  ()

// Invoke a function for each character in a string.
// If `action` returns `Just`, the function returns immediately with that result.
pub fun string/foreach-while( s : string, action : (c:char) -> e maybe<a> ) : e maybe<a>
  s.slice.foreach-while(action)

// Invoke a function for each character in a string
pub fun string/foreach( s : string, action : (c:char) -> e () ) : e ()
  s.slice.foreach(action)

// Return the first character of a string (or `Nothing` for the empty string).
pub fun head-char( s : string ) : maybe<char>
  s.foreach-while( Just )

// Invoke a function for each character in a string
//fun foreach( s : string, f : char -> e () ) : e ()
//  s.list.foreach(f)  // todo: optimize
//

// Count the number of times a predicate is true for each character in a string
pub fun pred/count( s : string, pred : (char) -> e bool ) : e int
  var cnt := 0
  s.foreach fn(c)
    if mask<local>{pred(c)} then cnt := cnt+1
  cnt


// Does string `s`  contain the string `sub`  ?
inline extern xindex-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_index_of1(#1,#2,kk_context())"
  cs inline "((#1).IndexOf(#2) + 1)"
  js inline "((#1).indexOf(#2) + 1)"

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.
pub fun find( s : string, sub : string ) : maybe<sslice>
  val i = s.xindex-of(sub).int
  if i.is-zero then Nothing else Just(Sslice(s,i.dec,sub.length))

// Does string `s`  contain the string `sub`  ?
inline extern xlast-index-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_last_index_of1(#1,#2,kk_context())"
  cs inline "(#1.LastIndexOf(#2) + 1)"
  js inline "((#1).lastIndexOf(#2) + 1)"

// Return the last index of substring `sub` in `s` if it occurs.
pub fun find-last( s : string, sub : string ) : maybe<sslice>
  val i = s.xlast-index-of(sub).int
  if i.is-zero then Nothing else Just(Sslice(s,i.dec,sub.length))

inline extern xstarts-with: (s : string, pre : string ) -> bool
  c  "kk_string_starts_with"
  cs inline "#1.StartsWith(#2)"
  js inline "(#1.substr(0,#2.length) === #2)"

// Is `pre`  a prefix of `s`? If so, returns a slice
// of `s` following `pre` up to the end of `s`.
pub fun starts-with( s : string, pre : string ) : maybe<sslice>
  if xstarts-with(s,pre)
    then Just(Sslice(s,pre.length,s.length - pre.length))
    else Nothing

extern xends-with: (s : string, post : string ) -> bool
  c  "kk_string_ends_with"
  cs inline "#1.EndsWith(#2)"
  js inline "((#1).indexOf(#2, (#1).length - (#2).length) !== -1)"

// Does string `s`  end with `post`?
// If so, returns a slice of `s` from the start up to the `post` string at the end.
pub fun ends-with( s : string, post : string ) : maybe<sslice>
  if (xends-with(s,post))
    then Just(Sslice(s,0,s.length - post.length))
    else Nothing

// Truncate a string to `count` characters.
pub fun string/truncate( s : string, count : int ) : string
  s.first.extend(count - 1).string

// Trim off a substring `sub` while `s` starts with that string.
pub fun trim-left( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.starts-with(sub)
    Just(slice) -> trim-left(pretend-decreasing(slice.string),sub)
    Nothing -> s

// Trim off a substring `sub` while `s` ends with that string.
pub fun trim-right( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.ends-with(sub)
    Just(slice) -> trim-right(pretend-decreasing(slice.string),sub)
    Nothing -> s

