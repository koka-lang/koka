module std/core/string

import std/core/types
import std/core/hnd
import std/core/int
import std/core/vector
import std/core/order

extern import
  js file "string-inline.js"

// ----------------------------------------------------------------------------
// Strings
// ----------------------------------------------------------------------------

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
pub fun "@new-sslice"( str :string, start: int, len : int )
  Sslice(str,start,len)

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
pub extern string/vector : ( s : string ) -> vector<char>
  c  "kk_string_to_chars"
  cs inline "Primitive.StringToChars(#1)"
  js inline "_string_to_chars(#1)"

// Are two strings equal?
// Uses exact equality between character codes.
pub inline extern string/(==) : (string,string) -> bool
  c  "kk_string_is_eq"
  cs inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two strings not equal?
pub inline extern string/(!=) : (string,string) -> bool
  c  "kk_string_is_neq"
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

pub inline fun sslice/(==)(l1: sslice, l2: sslice ): bool
  match (l1, l2)
    (Sslice(s1, i1, f1), Sslice(s2, i2, f2)) ->
      if f1 != f2 then False
      elif i1 != i2 then False
      else s1 == s2

pub inline fun sslice/(!=)(l1: sslice, l2: sslice ): bool
  match (l1, l2)
    (Sslice(s1, i1, f1), Sslice(s2, i2, f2)) ->
      if f1 != f2 then True
      elif i1 != i2 then True
      else s1 != s2

extern string-cmp : ( x : string, y : string ) -> int
  c  "kk_string_cmp_int"
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"

// Compare two strings.
// Uses the character codes directly for comparison
pub fun string/cmp( x : string, y : string) : order
  string-cmp(x,y).order

pub fun string/(>=)( x : string, y : string ) : bool { cmp(x,y) > Lt }
pub fun string/(<=)( x : string, y : string ) : bool { cmp(x,y) < Gt }
pub fun string/(>)( x : string, y : string ) : bool  { cmp(x,y) == Gt }
pub fun string/(<)( x : string, y : string ) : bool  { cmp(x,y) == Lt }

// Append two strings.
pub extern string/(++)(x : string, y : string) : string
  c "kk_string_cat"
  inline "(#1 + #2)"

// Choose a non-empty string
pub fun string/(||)( x : string, y : string ) : string
  if x.is-empty then y else x

// Length returns the length in the platform specific encoding (and should not be exported)
inline extern string/length( s : string ) : int
  c inline "kk_string_len_int(#1,kk_context())"
  cs inline "#1.Length"
  js inline "#1.length"

// O(n). Return the number of characters in a string.
pub extern string/count( s : string ) : int
  c  "kk_string_count_int"
  cs "Primitive.StringCount"
  js "_string_count"

// O(n). Return the number of characters in a string slice
pub extern sslice/count( slice : sslice ) : int
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
pub fun string/last(s : string, n : int = 1) : sslice
  val slice = s.last1
  if n==1 then slice
            else slice.advance(1 - n).extend(n - 1)

// O(1). The entire string as a slice
pub fun slice( s : string ) : sslice
  Sslice(s,0,s.length)

// An empty slice
val empty = Sslice("",0,0)

// Is a slice empty?
pub fun sslice/is-empty( slice : sslice ) : bool
  !slice.len.is-pos

// An invalid slice
val invalid = Sslice("",-1,0)

// Is a slice invalid?
pub fun is-valid( slice : sslice ) : bool
  slice.start >= 0

// Is a slice not empty?
pub fun sslice/is-notempty( slice : sslice ) : bool
  slice.len.is-pos

// Return the first character of a string as a string (or the empty string)
pub fun string/head( s : string ) : string
  s.first.string

// Return the tail of a string (or the empty string)
pub fun string/tail( s : string ) : string
  s.first.after.string

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
pub fun sslice/truncate( slice: sslice ): sslice
  Sslice(slice.str, slice.start, 0)

// Gets up to (`end`-`start`) characters from the slice beginning from `start`.
// If either start or end is negative, returns the original slice
pub fun subslice( slice: sslice, start : int, end : int ): sslice
  if start < 0 || end < 0 then slice else
    // Advances to start, zeros the length, and then extends (end - start characters)
    slice.advance(start).truncate.extend(end - start)

// Gets a slice that only includes up to n characters from the start of the slice.
pub fun sslice/take( slice: sslice, n : int): sslice
  if n <= 0 then return slice else slice.truncate.extend(n)

// Gets a slice that drops the first n characters, shrinking the length of the slice by n accordingly.
// If the slice does not have n characters, then the slice is shrunk to an empty slice.
//
// If maintaining the length of the slice is important, use `advance` instead.
pub fun sslice/drop( slice: sslice, n : int): sslice
  if n <= 0 then return slice else slice.advance(n).truncate.extend(slice.count - n)

// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.
pub fun slice/foreach-while( slice : sslice, action : (c : char) -> e maybe<a> ) : e maybe<a>
  match slice.next
    Nothing -> Nothing
    Just((c,rest)) ->
      match action(c)
        Nothing -> slice/foreach-while(unsafe-decreasing(rest),action)
        res     -> res


// Apply a function for each character in a string slice.
pub fun slice/foreach( slice : sslice, action : (c:char) -> e ()) : e ()
  foreach-while( slice ) fn(c)
    action(c)
    Nothing
  ()

// Does string `s`  contain the string `sub`  ?
inline extern xindex-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_index_of1(#1,#2,kk_context())"
  cs inline "((#1).IndexOf(#2) + 1)"
  js inline "((#1).indexOf(#2) + 1)"

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.
pub fun string/find( s : string, sub : string ) : maybe<sslice>
  val i = s.xindex-of(sub)
  if i.is-zero then Nothing else Just(Sslice(s,i.decr.int,sub.length))

// Does string `s`  contain the string `sub`  ?
inline extern xlast-index-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_last_index_of1(#1,#2,kk_context())"
  cs inline "(#1.LastIndexOf(#2) + 1)"
  js inline "((#1).lastIndexOf(#2) + 1)"

// Return the last index of substring `sub` in `s` if it occurs.
pub fun find-last( s : string, sub : string ) : maybe<sslice>
  val i = s.xlast-index-of(sub)
  if i.is-zero then Nothing else Just(Sslice(s,i.decr.int,sub.length))

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

// Does string `s`  contain the string `sub`  ?
pub inline extern string/contains: (s : string, sub : string ) -> bool
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

// Truncate a string to `count` characters.
pub fun string/truncate( s : string, count : int ) : string
  s.first.extend(count - 1).string

// Trim off a substring `sub` while `s` starts with that string.
pub fun stringsub/trim-left( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.starts-with(sub)
    Just(slice) -> trim-left(unsafe-decreasing(slice.string),sub)
    Nothing -> s

// Trim off a substring `sub` while `s` ends with that string.
pub fun stringsub/trim-right( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.ends-with(sub)
    Just(slice) -> trim-right(unsafe-decreasing(slice.string),sub)
    Nothing -> s

extern repeatz( s : string, n : ssize_t ) : string
  c  "kk_string_repeat"
  cs "Primitive.Repeat"
  js "_string_repeat"

// Repeat a string `n` times
pub fun string/repeat( s : string, ^n : int ) : string
  repeatz(s,n.ssize_t)

// Convert a `:maybe` string to a string using the empty sting for `Nothing`
pub fun maybe/string( ms : maybe<string> ) : string
  match ms
    Nothing -> ""
    Just(s) -> s

// Is a string empty?
pub fun string/is-empty( s : string ) : bool
  s == ""

// Is a string not empty?
pub fun string/is-notempty( s : string ) : bool
  s != ""

// Transform a string to a maybe type, using `Nothing` for an empty string
pub fun string/maybe( s : string ) : maybe<string>
  if s.is-empty then Nothing else Just(s)


// Replace every occurrence of `pattern` to `repl` in a string.
pub inline extern replace-all( s : string, pattern : string, repl : string ) : string
  c  "kk_string_replace_all"
  cs inline "(#1).Replace(#2,#3)"
  js inline r"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)"

// Count occurences of `pattern` in a string.
pub inline extern stringpat/count( s : string, pattern : string ) : int
  c  "kk_string_count_pattern"
  cs inline "Primitive.Count(#1,#2)"
  js inline r"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)"


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
pub fun stringpred/count( s : string, pred : (char) -> e bool ) : e int
  var cnt := 0
  s.foreach fn(c)
    if mask<local>{pred(c)} then cnt := cnt+1
  cnt

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

// Convert the first character of a string to uppercase.
pub fun capitalize( s : string ) : string
  s.first.string.to-upper ++ s.first.after.string

// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
pub fun pad-left( s : string, ^width : int, fill : char = ' ') : string
  val w = width
  val n = s.length
  if w <= n
    then s
    else fill.string.repeat( w - n ) ++ s

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
pub fun pad-right( s : string, ^width : int, fill : char = ' ') : string
  val w = width
  val n = s.length
  if w <= n
    then s
    else s ++ fill.string.repeat(w - n)

// Trim whitespace on the left and right side of a string
pub fun trim( s : string ) : string
  s.trim-left.trim-right

// Trim the starting white space of a string
pub inline extern string/trim-left( s : string ) : string
  c  "kk_string_trim_left"
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"

// Trim the ending white space of a string.
pub inline extern string/trim-right( s : string ) : string
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
// For example: `split("1,,2",",") == ["1","","2]`
pub fun string/split( s : string, sep : string ) : list<string>
  splitv(s,sep).list

// Split a string into at most `n` parts that were delimited by a string `sep`. The delimeters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
pub fun splitn/split( s : string, sep: string, ^n : int) : list<string>
  splitvn(s,sep,n.ssize_t).list


// Convert a string to a list of characters
pub extern string/list( s : string ) : total list<char>
  c  "kk_string_to_list"
  cs inline "Primitive.StringToList(#1)"
  js inline "_string_to_list(#1)"

// Convert a list of characters to a string
pub extern listchar/string( cs : list<char> ) : total string
  c  "kk_string_from_list"
  cs inline "Primitive.ListToString(#1)"
  js inline "_list_to_string(#1)"

