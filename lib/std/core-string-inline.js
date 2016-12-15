/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*-----------------------------------------------------------
  String codepoints

  Ugh, Javascript treats strings as UCS2/UTF-16 vectors.
  We need to explicitly decode/encode to see strings
  as unicode codepoints.
-------------------------------------------------------------*/

function _is_high_surrogate(c) {
  return (c >= 0xD800 && c <= 0xDBFF);
}

function _is_low_surrogate(c) {
  return (c >= 0xDC00 && c <= 0xDFFF);
}

function _from_surrogate(hi,lo) {
  return ((hi - 0xD800) * 0x0400) + (lo - 0xDC00) + 0x10000;
}

function _char_to_string( code ) {
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

function _char_iter( s, from, f ) {
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

function _char_reviter( s, from, f ) {
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
  var xs = Nil;
  _char_reviter(s, undefined, function(c,i,next) {
    xs = Cons(c,xs);
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
  return Just( {fst: c, snd: { str: slice.str, start: slice.start+n, len: slice.len-n }} );
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
