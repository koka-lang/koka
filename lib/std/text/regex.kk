/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Regular expressions.
 
   The regular expressions conform to the regular expressions of JavaScript
   as described at <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions>
*/  
module std/text/regex

import std/num/int32

extern import
  c { conan="pcre2/[ >= 10.37]"; vcpkg="pcre2"; library="pcre2-8" }

extern import
  cs file "regex-inline.cs"
  js file "regex-inline.js"
  c  file "regex-inline.c"


// Abstract type of a regular expression object
abstract value struct regex( obj: any, src : string )

// Return the pattern as a string
pub fun source( r : regex ) : string
  r.src

extern regex-create(source : string, ignore-case : bool, multi-line : bool) : any
  c "kk_regex_create"
  js "$regexCreate"
  cs "RegEx.Create"

extern regex-exec( regex : any, str : string, start : ssize_t) : list<sslice>
  c  "kk_regex_exec"
  js "$regexExec"
  cs "RegEx.Exec"

extern regex-exec-all( regex : any, str : string, start : ssize_t, atmost : ssize_t) : list<list<sslice>>
  c  "kk_regex_exec_all"
  js "$regexExecAll"
  cs "RegEx.ExecAll"


// How many groups are captured by this regex?
pub fun groups-count( r : regex ) : int
  r.source.replace-all(rx-nongroup,"").count;

val rx-nongroup = regex(r"[^\\\[(]+|\\[\s\S]?|\(\?|\[(?:[^\\\]]|\\.)*\]")

// Create a new regular expression. Takes two optional parameters. Set `ignoreCase` to `True`
// to ignore uppercase/lowercase distinction. If  `multiline` is set to `True`, then `^` and `$`
// match also the beginning and end of every line (instead of the entire input).
pub fun regex( regex :string, ignorecase :bool = False, multiline : bool = False ) : regex
  Regex(regex-create(regex,ignorecase,multiline), regex);


// Find a match for a regular expression.
// See also `find` and `contains`
pub fun exec( regex :regex, s : string ) : list<sslice>
  regex-exec(regex.obj,s,0.ssize_t)

// Match a regular expression `regex` over a string `s`. 
// Matches at most `atmost` times (and matches all by default).
// Returns always an odd number of elements where every even
// element is a match and the odd ones the string parts between the 
// matches. 
// See also `find-all` and `strings`.
pub fun exec-all( regex : regex, s : string, atmost : int = -1 ) : list<list<sslice>>
  regex-exec-all(regex.obj,s,0.ssize_t,atmost.ssize_t)


// Return the full matched string of a capture group
pub fun captured( matched : list<sslice> ) : string
  match matched
    Cons(s) -> s.string
    _       -> ""


// Return the full matched string part for a list of matched capture groups.
pub fun captures( xs : list<list<sslice>> ) : list<string>
  xs.map(captured)


// Find a match for a regular expression.
// See also `exec`
pub fun find( s : string,  r : regex ) : maybe<string>
  match r.exec(s)
    Cons(m) -> Just(m.string)
    _       -> Nothing


// Does a regular expression pattern occur in a string `s`?
// (note: called `test` in javascript)
pub fun contains( s : string, r : regex ) : bool
  r.exec(s).is-cons


// Filter only for the matched parts.
fun filter-matches( xs : list<list<sslice>> ) : list<list<sslice>>
  match xs
    Cons(_,Cons(m,mm)) -> Cons(m,filter-matches(mm))
    _                  -> Nil


// Find all matches for a regular expression in a string.
pub fun find-all( s : string,  r : regex, atmost : int = -1 ) : list<string>
  r.exec-all(s,atmost).filter-matches.captures


fun concat-replace( matches : list<list<sslice>>, repl : list<sslice> -> e string, acc : list<string> ) : e string
  match matches
    Cons(pre,Cons(m,mm)) -> concat-replace( mm, repl, Cons(repl(m), Cons( pre.captured,acc)))
    Cons(post,Nil)       -> Cons(post.captured,acc).reverse-join 
    Nil -> acc.reverse-join


// Replace the all occurrences of `regex` by the result of the replacement fun `repl` in a string `s`.
pub fun replace-all( s : string, r : regex, repl : list<sslice> -> e string, atmost : int = -1 ) : e string
  r.exec-all( s, atmost ).concat-replace(repl,[])

// Replace the first occurrence of `regex` by the result of the replacement fun `repl` in a string `s`.
pub fun replace( s : string, r : regex, repl : list<sslice> -> e string ) : e string
  s.replace-all( r, repl, 1 )


// Check if a capture group was matched.
pub fun matched( s : sslice ) : bool
  s.is-valid

// Replace using a replacement string that can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
fun replace-captures( caps : list<sslice>, repl : string ) : string
  replace-all( repl, regex(r"\$(?:(\d)|(\&)|(\$))") )  fn(cap)
    match cap
      [_,digit,amp,dollar] ->
        if dollar.is-valid then "$" else
          val grp = if amp.is-valid then 0 else parse-int-default(digit.string,0)
          match caps[grp]
            Nothing -> ""
            Just(s) -> s.string


      _ -> "$"


// Replace all occurrences of `regex` with the replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
pub fun replace-all( s : string, regex : regex, repl : string, atmost : int = -1 ) : string
  if (repl.contains("$"))
    then replace-all(s, regex, fn(caps){ replace-captures(caps,repl) }, atmost)
    else replace-all(s, regex, fn(_){ repl }, atmost)

// Replace the first occurrence of `regex` with a replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
pub fun replace( s : string, regex : regex, repl : string ) : string
  replace-all( s, regex, repl, 1 )


// Filter only for the non-matched parts.
fun filter-non-matches( xs : list<list<sslice>> ) : list<list<sslice>>
  match xs
    Cons(s,Cons(_,xx)) -> Cons(s,filter-non-matches(xx))
    Cons(s,Nil)        -> [s]
    _                  -> []


// Split a string `s` in at most `atmost` parts using a regular expression `r` as separator.
pub fun split( s : string, r : regex, atmost : int = -1 ) : list<string>
  r.exec-all(s,atmost).filter-non-matches.captures  

pub fun testabc(s) 
  s.contains(regex(r"[ab]+c"))
  
/*
// Split a string `s` over separator `sep` where `sep` does not occur in 
// _tokens_ matching `exclude`.
// For example: ``split-exclude("comma,'sep,arated',values", regex(","),regex("'[^']*'|[^',]"))``
pub fun split-exclude( s : string, sep : regex, exclude : regex ) : list<string>

  if s=="" then [] else
    val splitr = regex( r"^(?:((?:" ++ exclude.source ++ ")+)|(" ++ sep.source ++ "))")
    s.split-excludex(splitr,"")


fun split-excludex( s : string, splitr : regex, acc : string ) : list<string>

  if s=="" return [acc]
  match( s.find(splitr) )
    Nothing -> split-excludex( unsafe-decreasing(s.tail), splitr, acc ++ s.head) // todo: improve efficiency?
    Just(cap) ->
      if (cap.groups.matched(1))
        split-excludex( unsafe-decreasing(cap.after1), splitr, acc ++ cap.matched) 

      else
        Cons(acc, split-excludex( unsafe-decreasing(cap.after1), splitr, "")) // todo: make tail recursive
*/