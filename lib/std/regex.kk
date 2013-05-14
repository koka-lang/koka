/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module supports JavaScriptStyle regular expressions.

   The regular expressions conform to the regular expressions of JavaScript
   as described at https://developer.mozilla.org/enUS/docs/JavaScript/Guide/Regular_Expressions   
*/   
module std/regex

external include {
  cs file "regex-inline.cs"
  js file "regex-inline.js"
}

// Abstract type of a regular expression object
abstract struct regex( obj: any )

// The result of a regular expression match, where "index" is the index in the
// original source string where the match begins, "matched" is the content that
// was matched, and "groups" is a vector that contains the content for each 
// capture groups (where "groups[0] == matched").
public struct matched( index :int, matched :string, groups :vector<string>  )

external regexCreate : (string,int,int) -> exn any {
  cs "RegEx.Create"
  js "$regexCreate"
}

external regexExec : (any,string,int) -> matched {
  cs "RegEx.Exec"
  js "$regexExec"
}

external regexExecAll : (any,string,int) -> vector<matched> {
  cs "RegEx.ExecAll"
  js "$regexExecAll"
}

external regexReplaceFun : forall<e> (any, string, (matched) -> e string, int, int ) -> e string {
  cs "RegEx.ReplaceFun"
  js "$regexReplaceFun"
}

external regexReplace : (any, string, string, int, int ) -> string {
  cs "RegEx.Replace"
  js "$regexReplace"
}

external regexSplit : (any, string, int, int ) -> vector<string> {
  cs "RegEx.Split"
  js "$regexSplit"
}

// Create a new regular expression. Takes two optional parameters. Set "ignoreCase" to "True"
// to ignore uppercase/lowercase distinction. If  "multiline" is set to "True", then "^" and "$"
// match also the beginning and end of every line (instead of the entire input).
public function regex( regex :string, ignorecase :bool = False, multiline : bool = False ) : exn regex
{
  Regex(regexCreate(regex,if (ignorecase) then 1 else 0, if (multiline) then 1 else 0));
}

// Find a match for a regular expression starting at start position "start" (by default "0").
// See also "contains"
// (note: this function is called |exec| in JavaScript).
public function find( s :string,  regex :regex, start :int = 0 ) : maybe<matched>
{  
  if (start >= s.length) return Nothing
  matched = regexExec(regex.obj,s,max(start,0))
  return (if (matched.index < 0) then Nothing else Just(matched))
}

// Find all matches for a regular expression in a string.
public function findAll( s :string,  regex :regex, start :int = 0 ) : vector<matched>
{
  if (start >= s.length) return vector()  
  return regexExecAll(regex.obj,s,max(start,0))
}

function replaceEx( s :string, regex :regex, repl :(matched :matched) -> e string, all :bool = False, start :int = 0) : e string
{
  if (start >= s.length) return s
  return regexReplaceFun(regex.obj, s, repl, if (all) then 1 else 0, max(start,0));
}

function replaceEx( s :string, regex :regex, repl :string, all :bool = False, start :int = 0) : string
{
  if (start >= s.length) return s
  return regexReplace(regex.obj, s, repl, if (all) then 1 else 0, max(start,0));
}

// Split a string "s" in at most "n" parts using a regular expression "r" as separator.
public function split( s :string, r :regex, n :int = maxInt, start : int = 0 ) : vector<string>
{
  if (n<=0) return vector();
  if (start>=s.length) return vector(1,s);
  return regexSplit( r.obj, s, if (n==maxInt) then 0 else n, max(start,0) );
}

// Replace the first occurrence of "regex" by the result of the replacement function "repl" in a string "s".
public function replace( s : string, regex: regex, repl : (matched :matched) -> e string, start : int = 0) : e string {
  replaceEx( s, regex, repl, all = False, start = start )
}

// Replace the all occurrences of "regex" by the result of the replacement function "repl" in a string "s".
public function replaceAll( s : string, regex: regex, repl : (matched :matched) -> e string, start : int = 0) : e string {
  replaceEx( s, regex, repl, all = True, start = start )
}

// Replace the first occurrence of "regex" by the result a replacement string "repl" in a string "s".
// The replacement string can contain |$$| for a |$| sign, |$n| for a capture group,
// |$&| for the entire match (|==$0|).
public function replace( s : string, regex: regex, repl :string, start : int = 0) : string {
  replaceEx( s, regex, repl, all = False, start = start )
}

// Replace all ocurrences of "regex" by the result of a replacement string "repl" in a string "s".
public function replaceAll( s : string, regex: regex, repl :string, start : int = 0) : string {
  replaceEx( s, regex, repl, all = True, start = start )
}

// Does a regular expression pattern occur in a string "s"?
// (note: called |test| in javascript)
public function contains( s :string, r :regex, start :int = 0 ) : bool 
{
  return s.find(r,start).bool
}
