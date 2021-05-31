/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Regular expressions.

   The regular expressions conform to the regular expressions of JavaScript
   as described at <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions>
*/  
module std/text/regex

import std/num/int32

extern import {
  c { vcpkg="pcre2"; library="pcre2-8" }
}

extern import {
  cs file "regex-inline.cs"
  js file "regex-inline.js"
  c  file "regex-inline.c"
}


// Abstract type of a regular expression object
abstract struct regex( obj: any, src : string )

// Return the pattern as a string
public fun source( r : regex ) : string {
  r.src
}

extern regex-create : (string,bool,bool) -> any {
  c "kk_regex_create"
  cs "RegEx.Create"
  js "$regexCreate"
}

extern regex-exec : (any,string,ssize_t) -> list<sslice> {
  c  "kk_regex_exec"
  cs "RegEx.Exec"
  js "$regexExec"
}

extern regex-exec-all : (any,string,ssize_t) -> list<list<sslice>> {
  c  "kk_regex_exec_all"
  cs "RegEx.ExecAll"
  js "$regexExecAll"
}

extern unsafe-upto(slice1 : sslice, slice2 : sslice) : sslice {
  c "kk_slice_upto"
}

// How many groups are captured by this regex?
public fun groups-count( r : regex ) : int {
  r.source.replace-all(rx-nongroup,"").count;
}
val rx-nongroup = regex(@"[^\\\[(]+|\\[\s\S]?|\(\?|\[(?:[^\\\]]|\\.)*\]")


// Create a new regular expression. Takes two optional parameters. Set `ignoreCase` to `True`
// to ignore uppercase/lowercase distinction. If  `multiline` is set to `True`, then `^` and `$`
// match also the beginning and end of every line (instead of the entire input).
public fun regex( regex :string, ignorecase :bool = False, multiline : bool = False ) : regex {
  Regex(regex-create(regex,ignorecase,multiline), regex);
}


// Find a match for a regular expression.
// See also `contains`
// (note: this fun is called |exec| in JavaScript).
public fun find( s :string,  regex :regex ) : list<sslice> {  
  regex-exec(regex.obj,s,0.ssize_t)
}


// Does a regular expression pattern occur in a string `s`?
// (note: called `test` in javascript)
public fun contains( s :string, r :regex ) : bool {
  return s.find(r).is-cons
}


// Find all matches for a regular expression in a string.
public fun find-all( s : string,  regex : regex ) : list<list<sslice>>
{
  return regex-exec-all(regex.obj,s,0.ssize_t)
}

fun concat-replace( matches : list<list<sslice>>, repl : list<sslice> -> e string, src : sslice, acc : list<string> ) : e string {
  match(matches) {
    Cons(m,mm) {
      val s = match(m) {
        Cons(t) -> t
        _       -> empty
      }
      concat-replace( mm, repl, s.after, Cons(repl(m), Cons( unsafe-upto(src,s).string, acc)) )
    }
    _ -> Cons(src.string, acc).reverse-join
  }
}

// Replace the all occurrences of `regex` by the result of the replacement fun `repl` in a string `s`.
public fun replace-all( s : string, regex : regex, repl : list<sslice> -> e string ) : e string {
  find-all( s, regex ).concat-replace(repl,s.slice,[])
}

// Replace the first occurrence of `regex` by the result of the replacement fun `repl` in a string `s`.
public fun replace( s : string, regex : regex, repl : list<sslice> -> e string ) : e string {
  [find( s, regex )].concat-replace(repl,s.slice,[])
}


// Replace using a replacement string that can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match (`==$0`).
fun replace-captures( caps : list<sslice>, repl : string ) : string {
  replace-all( repl, regex(@"\$(?:(\d)|(\&)|(\$))") ) fn(cap) {
    match(cap) {
      [_,digit,amp,dollar] {
        if (dollar.is-valid) then "$" else {
          val grp = if (amp.is-valid) then 0 else parse-int-default(digit.string,0)
          match (caps[grp]) {
            Nothing -> ""
            Just(s) -> s.string
          }
        }
      }
      _ -> "$"
    }
  }
}

// Replace all occurrences of `regex` with the replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match (`==$0`).
public fun replace-all( s : string, regex : regex, repl : string ) : string {
  if (repl.contains("$"))
    then replace-all(s,regex,fn(caps){ replace-captures(caps,repl) })
    else replace-all(s,regex,fn(_){ repl })
}

// Replace the first occurrence of `regex` with a replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match (`==$0`).
public fun replace( s : string, regex : regex, repl : string ) : string {
  if (repl.contains("$"))
    then replace(s,regex,fn(caps){ replace-captures(caps,repl) })
    else replace(s,regex,fn(_){ repl })
}

/*
// Split a string `s` over separator `sep` where `sep` does not occur in 
// _tokens_ matching `exclude`.
// For example: ``split-exclude("comma,'sep,arated',values", regex(","),regex("'[^']*'|[^',]"))``
public fun split-exclude( s : string, sep : regex, exclude : regex ) : list<string>
{
  if (s=="") then [] else {
    val splitr = regex( @"^(?:((?:" ++ exclude.source ++ ")+)|(" ++ sep.source ++ "))")
    s.split-excludex(splitr,"")
  }
}

fun split-excludex( s : string, splitr : regex, acc : string ) : list<string>
{
  if (s=="") return [acc]
  match( s.find(splitr) ) {
    Nothing -> split-excludex( unsafe-decreasing(s.tail), splitr, acc ++ s.head) // todo: improve efficiency?
    Just(cap) -> {
      if (cap.groups.matched(1)) {
        split-excludex( unsafe-decreasing(cap.after1), splitr, acc ++ cap.matched) 
      }
      else {
        Cons(acc, split-excludex( unsafe-decreasing(cap.after1), splitr, "")) // todo: make tail recursive
      }
    }
  }
}
*/