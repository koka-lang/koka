/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard vector functions.
module std/core/vector

import std/core/types
import std/core/undiv
import std/core/hnd
import std/core/exn
import std/core/int

extern import
  c  file "inline/vector"
  js file "inline/vector.js"

// ----------------------------------------------------------------------------
//  Vectors
// ----------------------------------------------------------------------------
// TODO: Consider in-place updates when reference count is 1 throughout!
// Also, start benchmarking  

// Return the element at position `index`  in vector `v`  without bounds check!
inline extern unsafe-idx( ^v : vector<a>, index : ssize_t ) : total a
  c  "kk_vector_at_borrow"
  cs inline "(#1)[#2]"
  js inline "(#1)[#2]"

inline extern unsafe-assign : forall<a> ( v : vector<a>, i : ssize_t, x : a ) -> total ()
  c "kk_vector_unsafe_assign"
  cs inline "(#1)[#2] = #3"
  js inline "(#1)[#2] = #3"

inline extern unsafe-vector : forall<a> ( n : ssize_t ) -> total vector<a>
  c  inline "kk_vector_alloc(#1,kk_box_null(),kk_context())"
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"

pub extern @unsafe-vector : forall<a> ( n : ssize_t ) -> total vector<a>
  c  inline "kk_vector_alloc(#1,kk_box_null(),kk_context())"
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"

// Assign to an entry in a local `:vector` variable.
pub inline extern assign/@index( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn|e> ()
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"  // check bounds?
  js inline "(#1).value[#2] = #3"       // todo: check bounds!

// Length of a vector.
inline extern lengthz( ^v : vector<a> ) : ssize_t
  c  "kk_vector_len_borrow"
  cs inline "((#1).Length)"
  js inline "((#1).length)"

// Create a new vector of length `n`  with initial elements `init`` .
extern vector-alloc(n : ssize_t, init : a) : e vector<a>
  c "kk_vector_alloc"
  cs inline "Primitive.NewArray<##1>(#1,#2)"
  js inline "_vector_initz(#1,#2)"

// Create a new vector of length `n`  with initial elements given by a total function `f` .
extern vector-alloc-total(n : ssize_t, f : ssize_t -> a) : vector<a>
  c "kk_vector_init_total"
  cs inline "Primitive.NewArray<##1>(#1,#2)"
  js inline "_vector_initf(#1,#2)"

// Create an empty vector.
pub inline extern unit/vector : forall<a> () -> vector<a>
  c inline "kk_vector_empty()"
  cs inline "new ##1[0]"
  js inline "[]"


// Return the element at position `index`  in vector `v`.
// Raise an out of bounds exception if `index < 0`  or `index >= v.length`.
pub fun @index( ^v : vector<a>, ^index : int ) : exn a
  val idx = index.ssize_t()
  if idx < v.lengthz then unsafe-idx(v,idx) else throw("index out of bounds",ExnRange)

// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds
pub fun at( ^v : vector<a>, ^index : int ) : maybe<a>
  val idx = index.ssize_t
  if idx < v.lengthz then Just(unsafe-idx(v,idx)) else Nothing

// Return the length of a vector.
pub fun length( ^v : vector<a> ) : int
  v.lengthz.int

// Create a new vector of length `n`  with initial elements `default` .
pub fun vector( ^n : int, default : a) : vector<a>
  vector-alloc(n.ssize_t, default)

// Create a new vector of length `n`  with initial elements given by _total_ function `f`.
// (can be more efficient than `vector-init`)
pub fun vector-init-total( ^n : int, f : int -> a ) : vector<a>
  vector-alloc-total( n.ssize_t, fn(i) f(i.int) )

// Create a new vector of length `n`  with initial elements given by function `f` which can have a control effect.
pub fun vector-init( ^n : int, f : int -> e a ) : e vector<a>
  val len = n.ssize_t
  val v = unsafe-vector(len)
  forz( len ) fn(i)
    unsafe-assign(v,i,f(i.int))
  v

// Invoke a function `f` for each element in a vector `v`
pub fun foreach( v : vector<a>, f : (a) -> e () ) : e ()
  v.foreach-indexedz( fn(_,x) { f(x) })

// Invoke a function `f` for each element in a vector `v`
pub fun foreach-indexed( v : vector<a>, f : (int,a) -> e () ) : e ()
  foreach-indexedz( v, fn(i,x) { f(i.int,x) } )

fun foreach-indexedz( v : vector<a>, f : (ssize_t,a) -> e () ) : e ()
  forz( v.lengthz ) fn(i)
    f(i,v.unsafe-idx(i))

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
pub fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b>
  for-whilez( v.lengthz ) fn(i)
    f(v.unsafe-idx(i))

// Apply a function `f` to each element in a vector `v`
pub fun map( v : vector<a>, f : a -> e b ) : e vector<b>
  val w = unsafe-vector(v.length.ssize_t)
  v.foreach-indexedz fn(i,x)
    unsafe-assign(w,i,f(x))
  w

// Apply a function `f` to each element in a vector `v`
pub fun map-indexed(v : vector<a>, f: (int, a) -> e b): e vector<b>
  val w = unsafe-vector(v.length.ssize_t)
  v.foreach-indexedz fn(i,x)
    unsafe-assign(w,i,f(i.int, x))
  w

// Fold over a vector from the start, accumulating a result
pub fun foldl( v : vector<a>, initial : b, f : (b,a) -> e b ) : e b
  val length = v.lengthz
  fun loop(i, acc)
    if i < length then
      val acc2 = f(acc, v.unsafe-idx(i))
      loop(i.incr.pretend-decreasing, acc2)
    else acc
  loop(0.ssize_t, initial)

// Fold over a vector from the end, accumulating a result
pub fun foldr( v : vector<a>, initial : b, f : (a,b) -> e b ) : e b
  val length = v.lengthz
  fun loop(i, acc)
    if i >= 0.ssize_t then
      val acc2 = f(v.unsafe-idx(i), acc)
      loop(i.decr.pretend-decreasing, acc2)
    else acc
  loop(length.decr, initial)

// Zip two vectors into a vector of pairs.
pub fun zip( v1 : vector<a>, v2 : vector<b> ) : vector<(a,b)>
  val l1 = v1.lengthz
  val l2 = v2.lengthz
  val l = if l1 <= l2 then l1 else l2
  val w = unsafe-vector(l)
  forz( l ) fn(i)
    unsafe-assign(w,i,(v1.unsafe-idx(i), v2.unsafe-idx(i)))
  w

// Zip two vectors using a function `f` to combine elements.
pub fun zip-with( v1 : vector<a>, v2 : vector<b>, f : (a,b) -> c ) : vector<c>
  val l1 = v1.lengthz
  val l2 = v2.lengthz
  val l = if l1 <= l2 then l1 else l2
  val w = unsafe-vector(l)
  forz( l ) fn(i)
    unsafe-assign(w,i,f(v1.unsafe-idx(i), v2.unsafe-idx(i)))
  w

// Zip two vectors using a function `f` to combine elements.
// The current index is passed as the first argument to `f`.
pub fun zip-with-indexed( v1 : vector<a>, v2 : vector<b>, f : (int,a,b) -> c ) : vector<c>
  val l1 = v1.lengthz
  val l2 = v2.lengthz
  val l = if l1 <= l2 then l1 else l2
  val w = unsafe-vector(l)
  forz( l ) fn(i)
    unsafe-assign(w,i,f(i.int, v1.unsafe-idx(i), v2.unsafe-idx(i)))
  w

// Equality of vectors
pub fun vector/(==)(v1 : vector<a>, v2: vector<a>, ?(==): (a, a) -> e bool): e bool
  val l = v1.lengthz
  if l != v2.lengthz then 
    False
  else
    val eq = for-whilez(l) fn(i)
      // Found a difference can end iteration and return False immediately
      if !(v1.unsafe-idx(i) == v2.unsafe-idx(i)) then Just(())
      else Nothing
    match eq
      Just() -> False
      Nothing -> True

// In-equality of vectors (specialized for performance)
pub fun vector/(!=)(v1 : vector<a>, v2: vector<a>, ?(==): (a, a) -> e bool): e bool
  val l = v1.lengthz
  if l != v2.lengthz then 
    True
  else
    val eq = for-whilez(l) fn(i)
      // Found a difference can end iteration and return True immediately
      if !(v1.unsafe-idx(i) == v2.unsafe-idx(i)) then Just(())
      else Nothing
    match eq
      Just() -> True
      Nothing -> False

// Take the first `n` elements from a vector.
pub fun take( v : vector<a>, ^n : int ) : vector<a>
  val len = v.lengthz
  val tn = if n.ssize_t < len then n.ssize_t else len
  val w = unsafe-vector(tn)
  forz( tn ) fn(i)
    unsafe-assign(w,i,v.unsafe-idx(i))
  w

// Take the first `n` elements from a vector as a list.
pub fun take-list( v : vector<a>, ^n : int ) : list<a>
  val len = v.lengthz
  val tn = if n.ssize_t < len then n.ssize_t else len
  fun loop(i : ssize_t, acc : ctx<list<_>>) : list<_>
    if i < tn then
      loop(i.incr.pretend-decreasing, acc ++ ctx Cons(v.unsafe-idx(i), hole))
    else
      acc ++. Nil
  loop(0.ssize_t, ctx hole)

// Drop the first `n` elements from a vector.
pub fun drop( v : vector<a>, ^n : int ) : vector<a>
  val len = v.lengthz
  val ns = n.ssize_t
  val dn = if ns < 0.ssize_t then 0.ssize_t elif ns < len then ns else len
  val newlen = len - dn
  val w = unsafe-vector(newlen)
  forz( newlen ) fn(i)
    unsafe-assign(w,i,v.unsafe-idx(i + dn))
  w

// Take a slice from the vector from `start` (inclusive) to `end` (exclusive).
pub fun slice( v : vector<a>, ^start : int, ^end : int ) : vector<a>
  val len = v.lengthz
  val starts = start.ssize_t
  val ends = end.ssize_t
  val s = if starts < 0.ssize_t then 0.ssize_t elif starts > len then len else starts
  val e = if ends < s then s elif ends > len then len else ends
  val slen = e - s
  val w = unsafe-vector(slen)
  forz( slen ) fn(i)
    unsafe-assign(w,i,v.unsafe-idx(i + s))
  w

// Reverses a vector
pub fun reverse( v : vector<a> ) : vector<a>
  val len = v.lengthz
  val w = unsafe-vector(len)
  forz( len ) fn(i)
    unsafe-assign(w,i,v.unsafe-idx(len - i - 1.ssize_t))
  w

// Append two vectors
pub fun append( v1 : vector<a>, v2 : vector<a> ) : vector<a>
  val l1 = v1.lengthz
  val l2 = v2.lengthz
  val l = l1 + l2
  val w = unsafe-vector(l)
  forz( l1 ) fn(i)
    unsafe-assign(w,i,v1.unsafe-idx(i))
  forz( l2 ) fn(i)
    unsafe-assign(w,i + l1,v2.unsafe-idx(i))
  w

// Append two vectors
pub fun vector/(++)( v1 : vector<a>, v2 : vector<a> ): vector<a>
  append(v1,v2)

// Convert a vector to a list.
pub fun list( v : vector<a> ) : list<a>
  v.vlist

// Convert a vector to a list with an optional tail.
pub extern vlist( v : vector<a>, tail : list<a> = [] ) : list<a>
  c  "kk_vector_to_list"
  cs inline "Primitive.VList<##1>(#1,#2)"
  js inline "_vlist(#1,#2)"

// Convert a list to a vector.
pub fun list/vector( xs : list<a> ) : vector<a>
  xs.unvlist

extern unvlist( xs : list<a> ) : vector<a>
  c  "kk_list_to_vector"
  cs inline "Primitive.UnVList<##1>(#1)"
  js inline "_unvlist(#1)"


// Executes `action` `n` times for each integer between [`0`,`n`)  (excluding `n` ).
// If `n <= 0`  the function returns without any call to `action` .
fun forz( n : ssize_t, action : (ssize_t) -> e () ) : e ()
  fun rep( i : ssize_t )
    if i < n then
      action(i)
      rep(pretend-decreasing(i.incr))
  rep(0.ssize_t)

// Executes `action` at most `n` times for each integer between `0`  upto `n`  (excluding `n` ).
// If `n <= 0`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
fun for-whilez( n : ssize_t, action : (ssize_t) -> e maybe<a> ) : e maybe<a>
  fun rep( i : ssize_t )
    if i < n then
      match action(i)
        Nothing -> rep(pretend-decreasing(i.incr))
        Just(x) -> Just(x)
    else Nothing
  rep(0.ssize_t)




// Minimal set of operations that we need in `std/core`.
inline fip extern ssize_t/(==) : (ssize_t,ssize_t) -> bool
  inline "(#1 == #2)"

inline fip extern ssize_t/(!=) : (ssize_t,ssize_t) -> bool
  inline "(#1 != #2)"

inline fip extern ssize_t/(<=) : (ssize_t,ssize_t) -> bool
  inline "(#1 <= #2)"

inline fip extern ssize_t/(>=) : (ssize_t,ssize_t) -> bool
  inline "(#1 >= #2)"

inline fip extern ssize_t/(<)  : (ssize_t,ssize_t) -> bool
  inline "(#1 < #2)"

inline fip extern ssize_t/(>)  : (ssize_t,ssize_t) -> bool
  inline "(#1 > #2)"

inline fip extern ssize_t/(+)  : (ssize_t,ssize_t) -> ssize_t
  inline "(#1 + #2)"
  js inline "((#1 + #2)|0)"

inline fip extern ssize_t/(-)  : (ssize_t,ssize_t) -> ssize_t
  inline "(#1 - #2)"
  js inline "((#1 - #2)|0)"

inline fip extern ssize_t/is-pos( i : ssize_t ) : bool
  inline "(#1 > 0)"

inline fip extern ssize_t/is-neg( i : ssize_t ) : bool
  inline "(#1 < 0)"

fip extern ssize_t/is-zero( i : ssize_t ) : bool
  inline "(#1 == 0)"
  js inline "(#1 === 0)"

fip extern ssize_t/decr(i : ssize_t ) : ssize_t
  inline "(#1 - 1)"

fip extern ssize_t/incr(i : ssize_t ) : ssize_t
  inline "(#1 + 1)"
