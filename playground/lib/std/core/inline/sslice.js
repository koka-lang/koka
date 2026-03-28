/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

function _slice_to_string(sl) {
  if (sl.start===0 && sl.len===sl.str.length) return sl.str;
  return sl.str.substr(sl.start,sl.len);
}

function _sslice_first( s ) {
  var len;
  if (s.length===0) len = 0;
  else if ($std_core_string._is_high_surrogate(s.charCodeAt(0))) len = 2
  else len = 1;
  return { str: s, start: 0, len: len };
}

function _sslice_last( s ) {
  var len;
  if (s.length===0) len = 0;
  else if ($std_core_string._is_low_surrogate(s.charCodeAt(s.length-1))) len = 2
  else len = 1;
  return { str: s, start: s.length-len, len: len };
}

function _sslice_count(slice) {
  if (slice.len<=0) return 0;
  var count = 0;
  var end = slice.start + slice.len;
  $std_core_string._char_iter(slice.str, slice.start, function(c,i,nexti) {
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
    $std_core_string._char_iter(slice.str, idx, function(c,i,nexti) {
      count--;
      idx = nexti;
      return (count <= 0);
    });
  }
  else {
    $std_core_string._char_reviter(slice.str, idx-1, function(c,i,nexti) {
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
    $std_core_string._char_iter(slice.str, idx, function(c,i,nexti) {
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
    $std_core_string._char_reviter(slice.str, idx-1, function(c,i,nexti) {
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
  if ($std_core_string._is_high_surrogate(c) && slice.len > 1) {
    var lo = slice.str.charCodeAt(slice.start+1);
    if ($std_core_string._is_low_surrogate(lo)) {
      c = $std_core_string._from_surrogate(c,lo);
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
    if (!$std_core_string._is_low_surrogate(c)) upto--; // count characters
  }
  return { str: s, start: 0, len: i };
}

