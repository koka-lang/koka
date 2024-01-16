/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Create a list with from a vector in constant stack space
export function _vlist(elems,tail) {
  var xs = tail || $std_core_types.Nil;
  if (elems!=null && elems.length>0) {
    for(var i = elems.length - 1; i >= 0; i--) {
      var elem = elems[i];
      if (elem !== undefined) xs = $std_core_types.Cons(elem,xs);
    }
  }
  return xs;
}

// Create an array from a list with constant stack space
export function _unvlist(list) {
  var elems = [];
  while(list) {
    elems.push(list.head);
    list = list.tail;
  }
  return elems;
}

// Create a vector with a default value
export function _vector_initz(n, x) {
  if (n<=0) return [];
  var a = new Array(n);
  for(var i = 0; i < n; i++) {
    a[i] = x;
  }
  return a;
}

// Create a vector with a function initializer
export function _vector_initf(n, f) {
  if (n<=0) return [];
  var a = new Array(n);
  for(var i = 0; i < n; i++) {
    a[i] = f(i);
  }
  return a;
}

// Index a vector
export function _vector_at( v, i ) {
  var j = _int_to_number(i);
  var x = v[j];
  if (x === undefined) { $std_core_exn.exn_error_range(); }
  return x;
}

