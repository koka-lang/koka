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
module std/regex

extern include {
  cs file "regex-inline.cs"
  js file "regex-inline.js"
}

// Abstract type of a regular expression object
abstract struct regex( obj: any )

// The result of a regular expression match, where `index` is the index in the
// original source string where the match begins, `next` is the index just after
// this match (`next == index + min(1,matched.length)`), `matched` is the content that
// was matched, and `groups` is a vector that contains the content for each 
// capture group (where `groups[0] == matched`).
public struct matched( index :int, next :int, matched :string, groups : groups )

abstract struct groups( grp : any )

extern regex-source( r : any ) : string {
  cs "RegEx.Source"
  js inline "#1.regex.source"
}

extern groups-index( groups : any, index : int ) : string {
  cs "RegEx.GroupsIndex"
  js inline "((#1)[#2] != null ? (#1)[#2] : '')"
} 

extern inline groups-matched-on( groups : any, index : int ) : int {
  cs "RegEx.GroupsMatchedOn"
  js inline "(#1[#2] ? 1 : 0)"  // mmm, doesn't catch empty group match.. but FireFox does not distinguish those :-(
} 

// For alternative regular expressions, return the alternative that was matched.
// If this was not an alternative regex, returns `-1`
public extern alternative( groups : groups ) : int {
  cs inline "-1"
  js inline "(#1.alternative!=null ? #1.alternative : -1)"
}

extern regex-create : (string,int,int) -> any {
  cs "RegEx.Create"
  js "$regexCreate"
}

extern regex-create-alt : (vector<string>,int,int) -> any {
  cs "RegEx.CreateAlt"
  js "$regexCreateAlt"
}

extern regex-exec : (any,string,int) -> maybe<matched> {
  cs "RegEx.Exec"
  js "$regexExec"
}

extern regex-exec-all : (any,string,int) -> vector<matched> {
  cs "RegEx.ExecAll"
  js "$regexExecAll"
}

extern regex-replace-fun : forall<e> (any, string, (matched) -> e string, int, int ) -> e string {
  cs "RegEx.ReplaceFun"
  js "$regexReplaceFun"
}

extern regex-replace : (any, string, string, int, int ) -> string {
  cs "RegEx.Replace"
  js "$regexReplace"
}

extern regex-split : (any, string, int, int ) -> vector<string> {
  cs "RegEx.Split"
  js "$regexSplit"
}


// Return the pattern as a string
public fun source( r : regex ) : string {
  regex-source(r.obj)
}

// How many groups are captured by this regex?
public fun groups-count( r : regex ) : int 
{
  r.source.replace-all(rxNonGroup,"").count;
}
val rxNonGroup = regex(@"[^\\\[(]+|\\[\s\S]?|\(\?|\[(?:[^\\\]]|\\.)*\]")

// Return first group that was matched (or -1 if nothing was matched) 
public fun first-matched?( groups : groups, start : int = 1, end : int = 10 ) : int
{
  match (list(start,end).find(fun(i) { groups.matched?(i) })) {
    Nothing -> ~1
    Just(i) -> i
  }
}

// Return the first matched group (or "" if nothing matched) starting at `start`
// and returning at most group `end`
public fun first-matched(groups : groups, start : int = 1, end : int = 10 ) : string
{
  val i = groups.first-matched?(start,end)
  if (i >= 0) then groups[i] else ""
}

// Return the string captured by a particular group or the empty string.
public fun []( groups : groups, index : int ) : string {
  groups-index(groups.grp, index)
}

// Returns `True` if a particular capture group actually matched.
// This is used if the group can match, but may capture the empty string.
public fun matched?( groups : groups, index : int ) : bool {
  groups-matched-on(groups.grp, index).bool
}

// Create a new regular expression. Takes two optional parameters. Set `ignoreCase` to `True`
// to ignore uppercase/lowercase distinction. If  `multiline` is set to `True`, then `^` and `$`
// match also the beginning and end of every line (instead of the entire input).
public fun regex( regex :string, ignorecase :bool = False, multiline : bool = False ) : regex
{
  Regex(regex-create(regex,if (ignorecase) then 1 else 0, if (multiline) then 1 else 0));
}

// Create a new _alternative_ regular expression. 
// Matches any of the given patterns but the groups are local to each alternative.
// See `alternative` to find out which alternative was matched. Contains an optimization
// where a common prefix of all patterns is lifted out of the alternative to increase efficiency.
// Takes two optional parameters. Set `ignoreCase` to `True`
// to ignore uppercase/lowercase distinction. If  `multiline` is set to `True`, then `^` and `$`
// match also the beginning and end of every line (instead of the entire input).  
// Note: currently only supported in the javascript backend.
public fun regex-alt( regexs :list<string>, ignorecase :bool = False, multiline : bool = False ) : regex
{
  Regex(regex-create-alt(regexs.vector,if (ignorecase) then 1 else 0, if (multiline) then 1 else 0));
}

private extern inline length( s : string ) : int {
  cs inline "#1.Length"
  js inline "#1.length"
}

private extern inline substr( s : string, start : int ) : string {
  js inline "(#1).substr(#2)"
}
private extern inline substr( s : string, start : int, len : int ) : string {
  js inline "(#1).substr(#2,#3)"
}

// Find a match for a regular expression starting at start position `start` (by default `0`).
// See also `contains`
// (note: this fun is called |exec| in JavaScript).
public fun find( s :string,  regex :regex, start :int = 0 ) : maybe<matched>
{  
  if (start >= s.length) return Nothing
  regex-exec(regex.obj,s,max(start,0))
  //return (if (matched.index < 0) then Nothing else Just(matched))
}

// Find all matches for a regular expression in a string.
public fun find-all( s :string,  regex :regex, start :int = 0 ) : vector<matched>
{
  if (start > s.length) return vector()  
  return regex-exec-all(regex.obj,s,max(start,0))
}

fun replace-ex( s :string, regex :regex, repl :(matched :matched) -> e string, all :bool = False, start :int = 0) : e string
{
  if (start > s.length) return s
  return regex-replace-fun(regex.obj, s, repl, if (all) then 1 else 0, max(start,0));
}

fun replace-ex( s :string, regex :regex, repl :string, all :bool = False, start :int = 0) : string
{
  if (start > s.length) return s
  return regex-replace(regex.obj, s, repl, if (all) then 1 else 0, max(start,0));
}

// Split a string `s` in at most `n` parts using a regular expression `r` as separator.
public fun split( s :string, r :regex, n :int = max-int, start : int = 0 ) : vector<string>
{
  if (n<=0) return vector();
  if (start>s.length) return vector(1,s);
  return regex-split( r.obj, s, if (n==max-int) then 0 else n, max(start,0) );
}

// Replace the first occurrence of `regex` by the result of the replacement fun `repl` in a string `s`.
public fun replace( s : string, regex: regex, repl : (matched :matched) -> e string, start : int = 0) : e string {
  replace-ex( s, regex, repl, all = False, start = start )
}

// Replace the all occurrences of `regex` by the result of the replacement fun `repl` in a string `s`.
public fun replace-all( s : string, regex: regex, repl : (matched :matched) -> e string, start : int = 0) : e string {
  replace-ex( s, regex, repl, all = True, start = start )
}

// Replace the first occurrence of `regex` by the result a replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match (`==$0`).
public fun replace( s : string, regex: regex, repl :string, start : int = 0) : string {
  replace-ex( s, regex, repl, all = False, start = start )
}

// Replace all ocurrences of `regex` by the result of a replacement string `repl` in a string `s`.
public fun replace-all( s : string, regex: regex, repl :string, start : int = 0) : string {
  replace-ex( s, regex, repl, all = True, start = start )
}

// Does a regular expression pattern occur in a string `s`?
// (note: called `test` in javascript)
public fun contains( s :string, r :regex, start :int = 0 ) : bool 
{
  return s.find(r,start).bool
}


// Split a string `s` over separator `sep` where `sep` does not occur in 
// _tokens_ matching `exclude`.
// For example: ``splitExclude("comma,'sep,arated',values", regex(","),regex("'[^']*'|[^',]"))``
public fun split-exclude( s : string, sep : regex, exclude : regex ) : list<string>
{
  if (s=="") then [] else {
    val splitr = regex( @"^(?:((?:" + exclude.source + ")+)|(" + sep.source + "))")
    s.split-excludex(splitr,"")
  }
}

fun split-excludex( s : string, splitr : regex, acc : string ) : list<string>
{
  if (s=="") return [acc]
  match( s.find(splitr) ) {
    Nothing -> split-excludex( unsafe-decreasing(s.substr(1)), splitr, acc + s.substr(0,1)) // todo: improve efficiency?
    Just(cap) -> {
      if (cap.groups.matched?(1)) {
        split-excludex( unsafe-decreasing(s.substr(cap.next)), splitr, acc + s.substr(0,cap.next)) 
      }
      else {
        Cons(acc, split-excludex( unsafe-decreasing(s.substr(cap.next)), splitr, "")) // todo: make tail recursive
      }
    }
  }
}
