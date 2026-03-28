// Koka generated module: std/core/string, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
/*-----------------------------------------------------------
  String codepoints
  Javascript treats strings as UCS2/UTF-16 vectors.
  We need to explicitly decode/encode to see strings
  as unicode codepoints.
-------------------------------------------------------------*/
export function _is_high_surrogate(c) {
  return (c >= 0xD800 && c <= 0xDBFF);
}
export function _is_low_surrogate(c) {
  return (c >= 0xDC00 && c <= 0xDFFF);
}
export function _from_surrogate(hi,lo) {
  return ((hi - 0xD800) * 0x0400) + (lo - 0xDC00) + 0x10000;
}
export function _char_to_string( code ) {
  if (code < 0) {
    return "";
  }
  else if (code <= 0xFFFF) {
    return String.fromCharCode(code);
  }
  else if (code > 0x10FFFF) {
    return String.fromCharCode(0xFFFD);
  }
  else {
    code = code - 0x10000;
    return String.fromCharCode( (code / 0x0400) + 0xD800, (code % 0x0400) + 0xDC00 );
  }
}
export function _char_iter( s, from, f ) {
  if (from < 0) from = 0;
  for(var i = from; i < s.length; i++) {
    var i0 = i;
    var c = s.charCodeAt(i);
    if (_is_high_surrogate(c) && i < s.length-1) {
      var lo = s.charCodeAt(i+1);
      if (_is_low_surrogate(lo)) {
        i++;
        c = _from_surrogate(c,lo);
      }
    }
    if (f(c,i0,i+1)) break;
  };
}
export function _char_reviter( s, from, f ) {
  for(var i = (from!=null ? from : s.length-1); i >= 0; i--) {
    var i0 = i;
    var c = s.charCodeAt(i);
    if (_is_low_surrogate(c) && i > 0) {
      var hi = s.charCodeAt(i-1);
      if (_is_high_surrogate(hi)) {
        i--;
        c = _from_surrogate(hi,c);
      }
    }
    if (f(c,i,i0+1)) break;
  }
}
// Convert a string to a list of characters
function _string_to_list( s ) {
  var xs = $std_core_types.Nil;
  _char_reviter(s, undefined, function(c,i,next) {
    xs = $std_core_types.Cons(c,xs);
  });
  return xs;
}
// Convert a string to a vector of codepoints
function _string_to_chars(s) {
  var xs = [];
  _char_iter(s, 0, function(c,i,inext) { xs.push(c); });
  return xs;
}
function _string_count(s) {
  var count = 0;
  _char_iter(s, 0, function(c,i,inext) { count++; } );
  return count;
}
// Convert a vector of code points back to a string
function _chars_to_string( v ) {
  var s = "";
  for(var i = 0; i < v.length; i++) {
    s += _char_to_string(v[i]);
  };
  return s;
}
// convert list to string
function _list_to_string(list) {
  var s = "";
  while(list) {
    s += _char_to_string(list.head);
    list = list.tail;
  }
  return s;
}
function _slice_to_string(sl) {
  if (sl.start===0 && sl.len===sl.str.length) return sl.str;
  return sl.str.substr(sl.start,sl.len);
}
function _sslice_first( s ) {
  var len;
  if (s.length===0) len = 0;
  else if (_is_high_surrogate(s.charCodeAt(0))) len = 2
  else len = 1;
  return { str: s, start: 0, len: len };
}
function _sslice_last( s ) {
  var len;
  if (s.length===0) len = 0;
  else if (_is_low_surrogate(s.charCodeAt(s.length-1))) len = 2
  else len = 1;
  return { str: s, start: s.length-len, len: len };
}
function _sslice_count(slice) {
  if (slice.len<=0) return 0;
  var count = 0;
  var end = slice.start + slice.len;
  _char_iter(slice.str, slice.start, function(c,i,nexti) {
    count++;
    return (nexti >= end);
  });
  return count;
}
// Extend the length of slice
function _sslice_extend( slice, count ) {
  if (count===0) return slice;
  var idx = slice.start + slice.len;
  if (count > 0) {
    _char_iter(slice.str, idx, function(c,i,nexti) {
      count--;
      idx = nexti;
      return (count <= 0);
    });
  }
  else {
    _char_reviter(slice.str, idx-1, function(c,i,nexti) {
      count++;
      idx = i;
      return (count >= 0 || idx <= slice.start);
    });
  }
  return { str: slice.str, start: slice.start, len: (idx > slice.start ? idx - slice.start : 0) };
}
// advance the start position of a slice
function _sslice_advance( slice, count ) {
  if (count===0) return slice;
  var idx = slice.start;
  var end = slice.start + slice.len;
  var slicecount = _sslice_count(slice); // todo: optimize by caching the character count?
  if (count > 0) {
    var extra = 0;
    _char_iter(slice.str, idx, function(c,i,nexti) {
      extra++;
      idx = nexti;
      return (extra >= count);
    });
    if (extra < slicecount && idx < end) { // optimize
      return _sslice_extend({ str: slice.str, start: idx, len: end-idx }, extra);
    }
  }
  else {
    var extra = 0;
    _char_reviter(slice.str, idx-1, function(c,i,nexti) {
      extra++;
      idx = i;
      return (extra >= -count);
    });
    if (extra < slicecount && idx < slice.start) {  // optimize
      return _sslice_extend({ str: slice.str, start: idx, len: slice.start-idx }, slicecount - extra);
    }
  }
  return _sslice_extend( { str: slice.str, start: idx, len: 0 }, slicecount );
}
// iterate through a slice
function _sslice_next( slice ) {
  if (slice.len <= 0) return null;
  var c = slice.str.charCodeAt(slice.start);
  var n = 1;
  if (_is_high_surrogate(c) && slice.len > 1) {
    var lo = slice.str.charCodeAt(slice.start+1);
    if (_is_low_surrogate(lo)) {
      c = _from_surrogate(c,lo);
      n = 2;
    }
  }
  return $std_core_types.Just( {fst: c, snd: { str: slice.str, start: slice.start+n, len: slice.len-n }} );
}
// return the common prefix of two strings
function _sslice_common_prefix( s, t, upto ) {
  var i;
  var max = Math.min(s.length,t.length);
  for(i = 0; i < max && upto > 0; i++) {
    var c = s.charCodeAt(i);
    if (c !== t.charCodeAt(i)) break;
    if (!_is_low_surrogate(c)) upto--; // count characters
  }
  return { str: s, start: 0, len: i };
}
function _string_repeat(s,n) {
  if (n<=0)  return "";
  if (n===1) return s;
  if (n===2) return s+s;
  var res = "";
  while(n > 0) {
    if (n & 1) res += s;
    n >>>= 1;
    s += s;
  }
  return res;
}
 
// type declarations
 
// declarations
 
 
// Convert a character to a string
export function char_fs_string(c) /* (c : char) -> string */  {
  return _char_to_string(c);
}
 
 
// Convert a vector of characters to a string.
export function vector_fs_string(_arg_x1) /* (vector<char>) -> string */  {
  return _chars_to_string(_arg_x1);
}
 
 
// Convert a string to a vector of characters.
export function vector(s) /* (s : string) -> vector<char> */  {
  return _string_to_chars(s);
}
 
export function string_cmp(x, y) /* (x : string, y : string) -> int */  {
  return (x===y ? 0 : (x > y ? 1 : -1));
}
 
 
// Compare two strings.
// Uses the character codes directly for comparison
export function cmp(x, y) /* (x : string, y : string) -> order */  {
   
  var i_10000 = string_cmp(x, y);
  if ($std_core_types._int_lt(i_10000,0)) {
    return $std_core_types.Lt;
  }
  else {
    return ($std_core_types._int_gt(i_10000,0)) ? $std_core_types.Gt : $std_core_types.Eq;
  }
}
 
export function order2(x, y) /* (x : string, y : string) -> order2<string> */  {
  var _x0 = cmp(x, y);
  if (_x0 === 1) {
    return $std_core_types.Lt2(x, y);
  }
  else if (_x0 === 3) {
    return $std_core_types.Gt2(y, x);
  }
  else {
    return $std_core_types.Eq2(x);
  }
}
 
export function _lp__lt__rp_(x, y) /* (x : string, y : string) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(x, y), $std_core_types.Lt);
}
 
export function _lp__lt__eq__rp_(x, y) /* (x : string, y : string) -> bool */  {
  return $std_core_order._lp__lt__rp_(cmp(x, y), $std_core_types.Gt);
}
 
export function _lp__gt__rp_(x, y) /* (x : string, y : string) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(x, y), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(x, y) /* (x : string, y : string) -> bool */  {
  return $std_core_order._lp__gt__rp_(cmp(x, y), $std_core_types.Lt);
}
 
 
// O(n). Return the number of characters in a string.
export function chars_fs_count(s) /* (s : string) -> int */  {
  return _string_count(s);
}
 
export function repeatz(s, n) /* (s : string, n : ssize_t) -> string */  {
  return _string_repeat(s,n);
}
 
 
// Repeat a string `n` times
export function repeat(s, n) /* (s : string, n : int) -> string */  {
  return repeatz(s, $std_core_int.ssize__t(n));
}
 
 
// Convert a `:maybe` string to a string using the empty sting for `Nothing`
export function maybe_fs_string(ms) /* (ms : maybe<string>) -> string */  {
  return (ms === null) ? "" : ms.value;
}
 
 
// Is a string empty?
export function is_empty(s) /* (s : string) -> bool */  {
  return (s === (""));
}
 
 
// Choose a non-empty string
export function _lp__bar__bar__rp_(x, y) /* (x : string, y : string) -> string */  {
  return ((x === (""))) ? y : x;
}
 
 
// Is a string not empty?
export function is_notempty(s) /* (s : string) -> bool */  {
  return (s !== (""));
}
 
 
// Transform a string to a maybe type, using `Nothing` for an empty string
export function maybe(s) /* (s : string) -> maybe<string> */  {
  if ((s === (""))) {
    return $std_core_types.Nothing;
  }
  else {
    return $std_core_types.Just(s);
  }
}
 
 
// Convert a string to upper-case
export function to_upper(s) /* (s : string) -> string */  {
  return (s).toUpperCase();
}
 
 
// Convert a string to lower-case
export function to_lower(s) /* (s : string) -> string */  {
  return (s).toLowerCase();
}
 
 
// Convert a string to a list of characters
export function list(s) /* (s : string) -> list<char> */  {
  return _string_to_list(s);
}
 
 
// Convert a list of characters to a string
export function listchar_fs_string(cs) /* (cs : list<char>) -> string */  {
  return _list_to_string(cs);
}
 
 
// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
export function pad_left(s, width, fill) /* (s : string, width : int, fill : ? char) -> string */  {
   
  var n = chars_fs_count(s);
  if ($std_core_types._int_le(width,n)) {
    return s;
  }
  else {
     
    var _x1 = (fill !== undefined) ? fill : 0x0020;
    var s_0_10003 = char_fs_string(_x1);
     
    var n_0_10004 = $std_core_types._int_sub(width,n);
    return $std_core_types._lp__plus__plus__rp_(repeatz(s_0_10003, $std_core_int.ssize__t(n_0_10004)), s);
  }
}
 
 
// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
export function pad_right(s, width, fill) /* (s : string, width : int, fill : ? char) -> string */  {
   
  var n = chars_fs_count(s);
  if ($std_core_types._int_le(width,n)) {
    return s;
  }
  else {
     
    var _x1 = (fill !== undefined) ? fill : 0x0020;
    var s_0_10007 = char_fs_string(_x1);
     
    var n_0_10008 = $std_core_types._int_sub(width,n);
    return $std_core_types._lp__plus__plus__rp_(s, repeatz(s_0_10007, $std_core_int.ssize__t(n_0_10008)));
  }
}
 
 
// Trim whitespace on the left and right side of a string
export function trim(s) /* (s : string) -> string */  {
  return (((((s).replace(/^\s\s*/,'')))).replace(/\s+$/,''));
}
 
 
// Split a string into parts that were delimited by `sep`. The delimiters are not included in the results.
// For example: `split("1,,2",",") == ["1","","2"]`
export function sep_fs_split(s, sep) /* (s : string, sep : string) -> list<string> */  {
   
  var v_10011 = ((s).split(sep));
  return $std_core_vector.vlist(v_10011);
}
 
 
// Split a string into at most `n` parts that were delimited by a string `sep`. The delimiters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
export function splitn_fs_split(s, sep, n) /* (s : string, sep : string, n : int) -> list<string> */  {
   
  var v_10012 = (s).split(sep, ($std_core_int.ssize__t(n)));
  return $std_core_vector.vlist(v_10012);
}