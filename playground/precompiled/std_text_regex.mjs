// Koka generated module: std/text/regex, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var $regexCache = {}
function $regexCreate( s, ignoreCase, multiLine )
{
  // we always use "g" flag.
  // to re-use safely, we always need to explicitly set 'lastIndex' on each use!
  var flags = "u" + (ignoreCase!==0 ? "i" : "") + (multiLine!==0 ? "m" : "");
  var key = s+flags;
  if ($regexCache[key]) return $regexCache[key];
  var regx = null;
  try {
    regx = new RegExp( s, "gd" + flags );
  }
  catch(exn) {
    if (exn instanceof SyntaxError && exn.message != null && exn.message.includes("Invalid flags")) {
      regx = new RegExp( s, "g" + flags );
    }
    else {
      throw exn;
    }
  }
  var rx = { regex: regx, flags: flags };
  $regexCache[key] = rx;
  return rx
}
// Execute
function $regexExecEx( r,  s, start )
{
  r.regex.lastIndex = start;
  const match = r.regex.exec(s);
  if (!match) {
    return { match: match, slices: $std_core_types.Nil }; // Matched(-1,0,"",[""]);
  }
  else if (match.indices instanceof Array) {
    const indices = match.indices;
    var res = $std_core_types.Nil;
    for( var i = indices.length - 1; i >= 0; i--) {
      var rng = indices[i];
      var sslice;
      if (rng == null) {
        sslice = $std_core_sslice._new_sslice(s,0,0)
      }
      else {
        const idx = rng[0];
        const len = rng[1] - idx;
        sslice = $std_core_sslice._new_sslice(s,idx,len);
      }
      res = $std_core_types.Cons( sslice, res);
    }
    return { match: match, slices: res };
  }
  else {
    // older JS, no 'd' flag for indices; emulate by creating slices directly on the match strings
    const next = r.regex.lastIndex;
    var res = $std_core_types.Nil;
    for( var i = match.length - 1; i > 0; i--) {
      const sub = match[i];
      var sslice;
      if (sub == null) {
        sslice = $std_core_sslice._new_sslice(s,0,0);
      }
      else {
        sslice = $std_core_sslice._new_sslice(sub,0,sub.length);
      }
      res = $std_core_types.Cons( sslice, res);
    }
    res = $std_core_types.Cons( $std_core_sslice._new_sslice(s,match.index,next - match.index), res);
    return { match: match, slices: res };
  }
}
function $regexExec( r,  s, start )
{
  return $regexExecEx(r,s,start).slices;
}
function $regexExecAll( r,  s, start, atmost )
{
  if (atmost < 0) { atmost = Number.MAX_SAFE_INTEGER; }
  var result = [];
  while (atmost > 0) {
    var res = $regexExecEx(r,s,start);
    if (res.match == null) break;
    var pre = $std_core_sslice._new_sslice( s, start, res.match.index - start );
    result.push( $std_core_types.Cons(pre,$std_core_types.Nil) ); // singleton pre list
    result.push( res.slices );
    // avoid loop on zero-length match
    atmost--;
    if (r.regex.lastIndex <= start) {
      start++;
    }
    else {
      start = r.regex.lastIndex;
    }
  }
  // and push the post string
  var post = $std_core_sslice._new_sslice( s, start, s.length - start );
  result.push( $std_core_types.Cons(post,$std_core_types.Nil) ); // singleton post list
  return $std_core_vector._vlist(result,null);
}
/*
function $regexCreateAlt( regexs, ignoreCase, multiLine ) {
  var offsets = [];
  var alts = []
  var current = 1;
  var prefix = $findPrefix(regexs);
  var prefixCount = $countGroups(prefix);
  if (prefix !== "") {
    // TODO: fix prefix that has a halfway capture group
    regexs = regexs.map( function(r) { return r.substr(prefix.length); } );
    current += prefixCount;
  }
  regexs.map( function(regex) {
    offsets.push(current)
    regex = regex.replace(/\\(\d)/g, function(match,digit) {
      var d = parseInt(digit);
      return (d <= prefixCount ? match : ("\\" + (d + current - prefixCount)))
    })
    current += (1 + $countGroups(regex));
    alts.push( "(" + regex + ")" );
  })
  offsets.push(current) // final
  var alt = $regexCreate( prefix + "(?:" + alts.join("|") + ")", ignoreCase, multiLine );
  alt.offsets = offsets;
  return alt;
}
// cache a non-global version too
function $regex_nong( r ) {
  if (r.regexng) return r.regexng;
  r.regexng = new RegExp( r.regex.source, r.flags )
  return r.regexng;
}
function $countGroups( regex ) {  // (string) -> int
  // (?x)(?<!(\\|(?!\\)\(\?))\((?!\?)
  var parens = regex.replace(/[^\\\[(]+|\\[\s\S]?|\(\?|\[(?:[^\\\]]|\\.)*\]/g, "");
  return parens.length
}
function $findPrefix( xs ) { // ([string]) -> string
  if (!xs) return "";
  if (xs.length == 0) return "";
  if (xs.length == 1) return xs[0];
  var prefix = "";
  var minlen = xs[0].length;
  xs.map( function(s) { if (s.length < minlen) minlen = s.length; });
  for( var i = 0; i < minlen; i++) {
    var c = xs[0][i];
    for (var j = 1; j < xs.length; j++ ) {
      if (xs[j][i] !== c) {
        return prefix;
      }
    }
    prefix = prefix + c;
  }
  return prefix;
}
function $regexGroups( r, match ) {
  if (!r.offsets || r.offsets.length <= 1) return match;
  var groups = [match[0]]
  groups.alternative = -1;
  for( var i = 0; i < r.offsets.length-1; i++ ) {
    if (match[r.offsets[i]] != null) {
      groups.alternative = i;
      // first push prefix groups
      var j = 1;
      while( j < r.offsets[0] ) {
        groups.push(match[j]);
        j++;
      }
      // then the groups of the alternative we matched
      var j = r.offsets[i] + 1;
      while( j < r.offsets[i+1] ) {
        groups.push(match[j])
        j++;
      }
      break;
    }
  }
  return groups;
}
function $regexSearch( r, s, start ) {
  var res = $regexExec(r,s,start);
  return res.index;
}
function $regexSplit( r, s, n, start ) {
  r.regex.lastIndex = start;
  return (n <= 0 ? s.split( r.regex ) : s.split( r.regex, n ));
}
function $regexReplaceFun( r,  s, repl, all, start)
{
  var regex = (all === 0 ? $regex_nong(r) : r.regex);
  //if (!s || !regex.test(s)) return s
  regex.lastIndex = start;
  return s.replace( regex, function() {
    if (arguments.length < 3) return ""; // should never happen!
    var index = arguments[arguments.length-2];
    var match = [];
    for(var i = 0; i < arguments.length-2; i++) {
      match[i] = arguments[i];
    }
    var matched = match[0] ? match[0] : ""
    var next = index + matched.length;
    // if (next<=index) next = index+1;
    var groups = (r.offsets ? $regexGroups(r,match) : match);
    var slice = $std_core._new_sslice(s,index,next - index);
    return repl( Matched( slice, matched, Groups(groups) ) );
  });
}
function $regexReplace( r, s, repl, all, start )
{
  var regex = (all === 0 ? $regex_nong(r) : r.regex);
  //if (!s || !regex.test(s)) return s
  regex.lastIndex = start;
  return s.replace( regex, repl ); // TODO: wrong for alt regex's
}
*/
 
// type declarations
// type regex
export function Regex(obj, src) /* (obj : any, src : string) -> regex */  {
  return { obj: obj, src: src };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `obj` constructor field of the `:regex` type.
export function regex_fs_obj(regex_0) /* (regex : regex) -> any */  {
  return regex_0.obj;
}
 
 
// Automatically generated. Retrieves the `src` constructor field of the `:regex` type.
export function regex_fs_src(regex_0) /* (regex : regex) -> string */  {
  return regex_0.src;
}
 
export function regex_fs__copy(_this, obj, src) /* (regex, obj : ? any, src : ? string) -> regex */  {
  if (obj !== undefined) {
    var _x0 = obj;
  }
  else {
    var _x0 = _this.obj;
  }
  if (src !== undefined) {
    var _x1 = src;
  }
  else {
    var _x1 = _this.src;
  }
  return Regex(_x0, _x1);
}
 
 
// Return the pattern as a string
export function source(r) /* (r : regex) -> string */  {
  return r.src;
}
 
export function regex_create(source_0, ignore_case, multi_line) /* (source : string, ignore-case : bool, multi-line : bool) -> any */  {
  return $regexCreate(source_0,ignore_case,multi_line);
}
 
export function regex_exec(regex_0, str, start) /* (regex : any, str : string, start : ssize_t) -> list<sslice/sslice> */  {
  return $regexExec(regex_0,str,start);
}
 
export function regex_exec_all(regex_0, str, start, atmost) /* (regex : any, str : string, start : ssize_t, atmost : ssize_t) -> list<list<sslice/sslice>> */  {
  return $regexExecAll(regex_0,str,start,atmost);
}
 
 
// Create a new regular expression. Takes two optional parameters. Set `ignoreCase` to `True`
// to ignore uppercase/lowercase distinction. If  `multiline` is set to `True`, then `^` and `$`
// match also the beginning and end of every line (instead of the entire input).
export function regex(regex_0, ignorecase, multiline) /* (regex : string, ignorecase : ? bool, multiline : ? bool) -> regex */  {
  var _x2 = (ignorecase !== undefined) ? ignorecase : false;
  var _x3 = (multiline !== undefined) ? multiline : false;
  return Regex(regex_create(regex_0, _x2, _x3), regex_0);
}
 
export var rx_nongroup;
var _x5 = undefined;
var _x4 = (_x5 !== undefined) ? _x5 : false;
var _x7 = undefined;
var _x6 = (_x7 !== undefined) ? _x7 : false;
var rx_nongroup = Regex(regex_create("[^\\\\\\[(]+|\\\\[\\s\\S]?|\\(\\?|\\[(?:[^\\\\\\]]|\\\\.)*\\]", _x4, _x6), "[^\\\\\\[(]+|\\\\[\\s\\S]?|\\(\\?|\\[(?:[^\\\\\\]]|\\\\.)*\\]");
 
 
// Find a match for a regular expression.
// See also `find` and `contains`
export function exec(regex_0, s) /* (regex : regex, s : string) -> list<sslice/sslice> */  {
  var _x8 = regex_0.obj;
  return regex_exec(_x8, s, 0);
}
 
 
// Match a regular expression `regex` over a string `s`.
// Matches at most `atmost` times (and matches all by default).
// Returns always an odd number of elements where every even
// element is a match and the odd ones the string parts between the
// matches.
// See also `find-all` and `strings`.
export function exec_all(regex_0, s, atmost) /* (regex : regex, s : string, atmost : ? int) -> list<list<sslice/sslice>> */  {
  var _x9 = regex_0.obj;
  var _x10 = (atmost !== undefined) ? atmost : -1;
  return regex_exec_all(_x9, s, 0, $std_core_int.ssize__t(_x10));
}
 
 
// Return the full matched string of a capture group
export function captured(matched_0) /* (matched : list<sslice/sslice>) -> string */  {
  if (matched_0 !== null) {
    return $std_core_sslice.string(matched_0.head);
  }
  else {
    return "";
  }
}
 
 
// Return the full matched string part for a list of matched capture groups.
export function captures(xs) /* (xs : list<list<sslice/sslice>>) -> list<string> */  {
  return $std_core_list.map(xs, captured);
}
 
 
// Find a match for a regular expression.
// See also `exec`
export function find(s, r) /* (s : string, r : regex) -> maybe<string> */  {
  var _x12 = r.obj;
  var _x11 = regex_exec(_x12, s, 0);
  if (_x11 !== null) {
    return $std_core_types.Just($std_core_sslice.string(_x11.head));
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Does a regular expression pattern occur in a string `s`?
// (note: called `test` in javascript)
export function contains(s, r) /* (s : string, r : regex) -> bool */  {
   
  var _x13 = r.obj;
  var list_10011 = regex_exec(_x13, s, 0);
  return (list_10011 !== null);
}
 
 
// Filter only for the matched parts.
export function _trmc_filter_matches(xs, _acc) /* (xs : list<list<sslice/sslice>>, ctx<list<list<sslice/sslice>>>) -> list<list<sslice/sslice>> */  { tailcall: while(1)
{
  if (xs !== null && xs.tail !== null) {
     
    var _trmc_x10055 = undefined;
     
    var _trmc_x10056 = $std_core_types.Cons(xs.tail.head, _trmc_x10055);
    {
      // tail call
      var _x13 = $std_core_types._cctx_extend(_acc,_trmc_x10056,({obj: _trmc_x10056, field_name: "tail"}));
      xs = xs.tail.tail;
      _acc = _x13;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Filter only for the matched parts.
export function filter_matches(xs_0) /* (xs : list<list<sslice/sslice>>) -> list<list<sslice/sslice>> */  {
  return _trmc_filter_matches(xs_0, $std_core_types._cctx_empty());
}
 
 
// Find all matches for a regular expression in a string.
export function find_all(s, r, atmost) /* (s : string, r : regex, atmost : ? int) -> list<string> */  {
   
  var _x14 = r.obj;
  var _x15 = (atmost !== undefined) ? atmost : -1;
  var xs_10070 = regex_exec_all(_x14, s, 0, $std_core_int.ssize__t(_x15));
   
  var xs_10015 = _trmc_filter_matches(xs_10070, $std_core_types._cctx_empty());
  return $std_core_list.map(xs_10015, captured);
}
 
 
// monadic lift
export function _mlift_concat_replace_10069(acc, mm, pre, repl, _y_x10059) /* forall<e> (acc : list<string>, mm : list<list<sslice/sslice>>, pre : list<sslice/sslice>, repl : (list<sslice/sslice>) -> e string, string) -> e string */  {
  if (pre !== null) {
    var _x14 = $std_core_sslice.string(pre.head);
  }
  else {
    var _x14 = "";
  }
  return concat_replace(mm, repl, $std_core_types.Cons(_y_x10059, $std_core_types.Cons(_x14, acc)));
}
 
export function concat_replace(matches, repl_0, acc_0) /* forall<e> (matches : list<list<sslice/sslice>>, repl : (list<sslice/sslice>) -> e string, acc : list<string>) -> e string */  { tailcall: while(1)
{
  if (matches !== null && matches.tail !== null) {
     
    var x_10071 = repl_0(matches.tail.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10059_0 /* string */ ) {
        return _mlift_concat_replace_10069(acc_0, matches.tail.tail, matches.head, repl_0, _y_x10059_0);
      });
    }
    else {
      {
        // tail call
        if (matches.head !== null) {
          var _x16 = $std_core_sslice.string(matches.head.head);
        }
        else {
          var _x16 = "";
        }
        var _x15 = $std_core_types.Cons(x_10071, $std_core_types.Cons(_x16, acc_0));
        matches = matches.tail.tail;
        acc_0 = _x15;
        continue tailcall;
      }
    }
  }
  else if (matches !== null && matches.tail === null) {
    if (matches.head !== null) {
      var _x17 = $std_core_sslice.string(matches.head.head);
    }
    else {
      var _x17 = "";
    }
    return $std_core_list.reverse_join($std_core_types.Cons(_x17, acc_0));
  }
  else {
    return $std_core_list.reverse_join(acc_0);
  }
}}
 
 
// Replace the all occurrences of `regex` by the result of the replacement fun `repl` in a string `s`.
export function replace_all(s, r, repl, atmost) /* forall<e> (s : string, r : regex, repl : (list<sslice/sslice>) -> e string, atmost : ? int) -> e string */  {
  var _x18 = r.obj;
  var _x19 = (atmost !== undefined) ? atmost : -1;
  return concat_replace(regex_exec_all(_x18, s, 0, $std_core_int.ssize__t(_x19)), repl, $std_core_types.Nil);
}
 
 
// Replace using a replacement string that can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
export function replace_captures(caps, repl) /* (caps : list<sslice/sslice>, repl : string) -> string */  {
   
  var _x21 = undefined;
  var _x20 = (_x21 !== undefined) ? _x21 : false;
  var _x23 = undefined;
  var _x22 = (_x23 !== undefined) ? _x23 : false;
  var r_10075 = Regex(regex_create("\\$(?:(\\d)|(\\&)|(\\$))", _x20, _x22), "\\$(?:(\\d)|(\\&)|(\\$))");
  var _x20 = r_10075.obj;
  var _x22 = undefined;
  var _x21 = (_x22 !== undefined) ? _x22 : -1;
  return concat_replace(regex_exec_all(_x20, repl, 0, $std_core_int.ssize__t(_x21)), function(cap /* list<sslice/sslice> */ ) {
      if (cap !== null && cap.tail !== null && cap.tail.tail !== null && cap.tail.tail.tail !== null && cap.tail.tail.tail.tail === null) {
        var _x24 = cap.tail.tail.tail.head.start;
        var _x23 = $std_core_types._int_ge(_x24,0);
        if (_x23) {
          return "$";
        }
        else {
           
          var _x26 = cap.tail.tail.head.start;
          var _x25 = $std_core_types._int_ge(_x26,0);
          if (_x25) {
            var grp = 0;
          }
          else {
             
            var s_10028 = $std_core_sslice.string(cap.tail.head);
             
            var _x28 = undefined;
            var _x27 = (_x28 !== undefined) ? _x28 : false;
            var m_10026 = $std_core_int.xparse(s_10028, _x27);
            var grp = (m_10026 === null) ? 0 : m_10026.value;
          }
          var _x25 = $std_core_list._index(caps, grp);
          if (_x25 === null) {
            return "";
          }
          else {
            return $std_core_sslice.string(_x25.value);
          }
        }
      }
      else {
        return "$";
      }
    }, $std_core_types.Nil);
}
 
 
// Replace all occurrences of `regex` with the replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
export function string_fs_replace_all(s, regex_0, repl, atmost) /* (s : string, regex : regex, repl : string, atmost : ? int) -> string */  {
  if (((repl).indexOf(("$")) >= 0)) {
    var _x26 = regex_0.obj;
    var _x27 = (atmost !== undefined) ? atmost : -1;
    return concat_replace(regex_exec_all(_x26, s, 0, $std_core_int.ssize__t(_x27)), function(caps /* list<sslice/sslice> */ ) {
        return replace_captures(caps, repl);
      }, $std_core_types.Nil);
  }
  else {
    var _x28 = regex_0.obj;
    var _x29 = (atmost !== undefined) ? atmost : -1;
    return concat_replace(regex_exec_all(_x28, s, 0, $std_core_int.ssize__t(_x29)), function(___wildcard_x158__35 /* list<sslice/sslice> */ ) {
        return repl;
      }, $std_core_types.Nil);
  }
}
 
 
// How many groups are captured by this regex?
export function groups_count(r) /* (r : regex) -> int */  {
  if (((("")).indexOf(("$")) >= 0)) {
    var _x31 = rx_nongroup.obj;
    var _x32 = r.src;
    var _x34 = undefined;
    var _x33 = (_x34 !== undefined) ? _x34 : -1;
    var _x30 = concat_replace(regex_exec_all(_x31, _x32, 0, $std_core_int.ssize__t(_x33)), function(caps /* list<sslice/sslice> */ ) {
        return replace_captures(caps, "");
      }, $std_core_types.Nil);
  }
  else {
    var _x35 = rx_nongroup.obj;
    var _x36 = r.src;
    var _x38 = undefined;
    var _x37 = (_x38 !== undefined) ? _x38 : -1;
    var _x30 = concat_replace(regex_exec_all(_x35, _x36, 0, $std_core_int.ssize__t(_x37)), function(___wildcard_x158__35 /* list<sslice/sslice> */ ) {
        return "";
      }, $std_core_types.Nil);
  }
  return $std_core_string.chars_fs_count(_x30);
}
 
 
// Replace the first occurrence of `regex` by the result of the replacement fun `repl` in a string `s`.
export function replace(s, r, repl) /* forall<e> (s : string, r : regex, repl : (list<sslice/sslice>) -> e string) -> e string */  {
  var _x39 = r.obj;
  return concat_replace(regex_exec_all(_x39, s, 0, 1), repl, $std_core_types.Nil);
}
 
 
// Check if a capture group was matched.
export function matched(s) /* (s : sslice/sslice) -> bool */  {
  var _x40 = s.start;
  return $std_core_types._int_ge(_x40,0);
}
 
 
// Replace the first occurrence of `regex` with a replacement string `repl` in a string `s`.
// The replacement string can contain `$$` for a `$` sign, `$n` for a capture group,
// `$&` for the entire match `==$0`.
export function string_fs_replace(s, regex_0, repl) /* (s : string, regex : regex, repl : string) -> string */  {
  if (((repl).indexOf(("$")) >= 0)) {
    var _x41 = regex_0.obj;
    return concat_replace(regex_exec_all(_x41, s, 0, 1), function(caps /* list<sslice/sslice> */ ) {
        return replace_captures(caps, repl);
      }, $std_core_types.Nil);
  }
  else {
    var _x42 = regex_0.obj;
    return concat_replace(regex_exec_all(_x42, s, 0, 1), function(___wildcard_x158__35 /* list<sslice/sslice> */ ) {
        return repl;
      }, $std_core_types.Nil);
  }
}
 
 
// Filter only for the non-matched parts.
export function _trmc_filter_non_matches(xs, _acc) /* (xs : list<list<sslice/sslice>>, ctx<list<list<sslice/sslice>>>) -> list<list<sslice/sslice>> */  { tailcall: while(1)
{
  if (xs !== null && xs.tail !== null) {
     
    var _trmc_x10057 = undefined;
     
    var _trmc_x10058 = $std_core_types.Cons(xs.head, _trmc_x10057);
    {
      // tail call
      var _x43 = $std_core_types._cctx_extend(_acc,_trmc_x10058,({obj: _trmc_x10058, field_name: "tail"}));
      xs = xs.tail.tail;
      _acc = _x43;
      continue tailcall;
    }
  }
  else if (xs !== null && xs.tail === null) {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Cons(xs.head, $std_core_types.Nil)));
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Filter only for the non-matched parts.
export function filter_non_matches(xs_0) /* (xs : list<list<sslice/sslice>>) -> list<list<sslice/sslice>> */  {
  return _trmc_filter_non_matches(xs_0, $std_core_types._cctx_empty());
}
 
 
// Split a string `s` in at most `atmost` parts using a regular expression `r` as separator.
export function split(s, r, atmost) /* (s : string, r : regex, atmost : ? int) -> list<string> */  {
   
  var _x44 = r.obj;
  var _x45 = (atmost !== undefined) ? atmost : -1;
  var xs_10106 = regex_exec_all(_x44, s, 0, $std_core_int.ssize__t(_x45));
   
  var xs_10041 = _trmc_filter_non_matches(xs_10106, $std_core_types._cctx_empty());
  return $std_core_list.map(xs_10041, captured);
}
 
export function testabc(s) /* (s : string) -> bool */  {
   
  var _x45 = undefined;
  var _x44 = (_x45 !== undefined) ? _x45 : false;
  var _x47 = undefined;
  var _x46 = (_x47 !== undefined) ? _x47 : false;
  var r_10047 = Regex(regex_create("[ab]+c", _x44, _x46), "[ab]+c");
   
  var _x48 = r_10047.obj;
  var list_10051 = regex_exec(_x48, s, 0);
  return (list_10051 !== null);
}