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

// Return the element at position `index`  in vector `v`  without bounds check!
inline extern unsafe-idx( ^v : vector<a>, index : ssize_t ) : total a
  c  "kk_vector_at_borrow"
  cs inline "(#1)[#2]"
  js inline "(#1)[#2]"

// Obtains the value transferring ownership and replaces the value with uninitialized value
// Precondition: `v` is unique
inline fip extern unsafe-extract( ^v : vector<a>, index : ssize_t ) : a
  c "kk_vector_unsafe_extract_borrow"
  cs inline "(#1)[#2]" // js / cs need to copy prior to calling this to satisfy the preconditions
  js inline "(#1)[#2]"

// Assign to an entry in a vector.
inline fip extern unsafe-assign( ^v : vector<a>, i : ssize_t, x : a ): total ()
  c "kk_vector_unsafe_assign_borrow"
  cs inline "(#1)[#2] = #3"
  js inline "(#1)[#2] = #3"

// Create a new vector of length `n`  with uninitialized elements.
pub extern @unsafe-vector : forall<a> ( n : ssize_t ) -> total vector<a>
  c  inline "kk_vector_alloc(#1,kk_box_null(),kk_context())"
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"

// This is unsafe, since the behavior that depends on this check needs to be observationally equivalent
inline fip extern unsafe-is-unique( ^v : vector<a> ) : bool
  c inline "kk_datatype_is_unique(#1, kk_context())"
  js inline "false"
  cs inline "false"

// Ensures the vector is unique, copying if necessary
// This is technically not fip, but we pretend it to be since it will only copy when non-unique
pub inline fip fun vec/ensure-unique(v: vector<a>): vector<a>
  if v.unsafe-is-unique then v else v.unsafe-copy

// Copies the vector
// This is technically not fip, but we pretend it to be, since it will only copy when non-unique 
// (only used for copy which satisfies the non-unique precondition)
inline fip extern unsafe-copy(v: vector<a>): vector<a>
  c "kk_vector_copy"
  js inline "[...(#1)]"
  cs inline "(#1).Clone()"

// Copies the vector
// If you only want to ensure uniqueness use `ensure-unique` instead, which will only copy if non-unique
pub inline extern copy(v: vector<a>): vector<a>
  c "kk_vector_copy"
  js inline "[...(#1)]"
  cs inline "(#1).Clone()"
 
// Assign to an entry in a local `:vector` variable.
// ```
// var v := vector(10, 0) 
// var v[0] := 1 // updates v in place
// ```
// TODO: Is this safe in cs / js? I think we might need to conservatively copy?
pub inline extern assign/@index( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn> ()
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"  // check bounds?
  js inline "(#1).value[#2] = #3"       // todo: check bounds!

// Assign to an entry in a local `:vector` variable.
pub inline fun assign/update( ^self : local-var<s,vector<a>>, ^index : int, f : a -> <local<s>,exn,div|e> a ) : <local<s>,exn,div|e> ()
  val i = index.ssize_t
  ensure-unique(std/core/types/@byref(self))
  val x = self.unsafe-extract(i)
  val res = f(x)
  self[index] := res

// Ensures that a vector in a local variable is unique
pub inline fun local/ensure-unique(^v: local-var<s,vector<a>>): <local<s>,exn,div|e> ()
  if v.unsafe-is-unique then () else v := unsafe-copy(v)

// Length of a vector.
inline fip extern lengthz( ^v : vector<a> ) : ssize_t
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

// Sets the value at a particular location in a vector
// If the index is out of bounds, an exception is thrown
// If the vector is unique the value will be updated in place
pub fun set(v: vector<a>, ^index: int, x: a): exn vector<a>
  val idx = index.ssize_t()
  if idx >= v.lengthz then 
    throw("index out of bounds", ExnRange)
  else
    val v' = v.ensure-unique // Ensure unique prior to assigning
    unsafe-assign(v', idx, x)
    v'

// Updates the value at a particular location in a vector
// If the index is out of bounds, an exception is thrown
// If the vector is unique the value will be updated in place
pub fun update(v: vector<a>, ^index: int, ^f: a -> <exn|e> a): <exn|e> vector<a>
  val idx = index.ssize_t()
  if idx >= v.lengthz then 
    throw("index out of bounds", ExnRange)
  else
    val v' = v.ensure-unique // Ensure unique before owning the value
    val res = f(unsafe-extract(v',idx))
    val v'' = v'.ensure-unique // Ensure unique after effects
    unsafe-assign(v'', idx, res)
    v''

// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds
pub fun at( ^v : vector<a>, ^index : int ) : maybe<a>
  val idx = index.ssize_t
  if idx < v.lengthz then Just(unsafe-idx(v,idx)) else Nothing

// Return the length of a vector.
pub fun length( ^v : vector<a> ) : int
  v.lengthz.int

// Create a new vector of length `n`  with initial elements `default` .
pub fun default/vector( ^n : int, default : a) : vector<a>
  vector-alloc(n.ssize_t, default)

// Create a new vector of length `n`  with initial elements given by _total_ function `f`.
// (can be more efficient than `vector-init`)
pub fun vector-init-total( ^n : int, f : int -> a ) : vector<a>
  vector-alloc-total( n.ssize_t, fn(i) f(i.int) )

// Create a new vector of length `n`  with initial elements given by function `f` which can have a control effect.
pub fun vector-init( ^n : int, f : int -> e a ) : e vector<a>
  val len = n.ssize_t
  val v = @unsafe-vector(len)
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
pub fip fun map( v : vector<a>, ^f : a -> e b ) : e vector<b>
  map-rec(v.ensure-unique, f, 0.ssize_t).unsafe-vector-cast

// Inner loop for `map` which does in place updates when unique, otherwise copies the vector
// `f` is safe to have effects that do not resume (since the vector will be unique and freed)
//      or to resume more than once (since the vector will be copied)
//      the element type cast is safe for the same reasons
// Precondition `v` is unique
fip fun map-rec(v: vector<a>, ^f: a -> e b, i: ssize_t): e vector<b>
  if i >= v.lengthz then
    v.unsafe-vector-cast
  else
    // v is unique, so we can own the value at index i
    val a = unsafe-extract(v, i)
    // If the function `f` does not resume, the vector will be dropped
    //  since it is guaranteed to be unique at this point,
    //  it will be freed and the uninitialized value will not be observable 
    val res = unsafe-cast(f(a))
    // If the function `f` captures a resumption such that v is no longer unique, we need to copy the vector
    val v' = v.ensure-unique
    // We copy before assigning to avoid the result value being dupped during a non-unique copy
    unsafe-assign(v', i, res)
    map-rec(v'.pretend-decreasing, f, i.incr)

// Only use in map-rec, which guarantees either:
//  - all vector elements will eventually be converted to type `b` 
//  - or the whole vector is unique and will be freed
inline fip extern unsafe-cast( x : a ) : b
  c inline "#1"
  js inline "#1"
  cs inline "#1"

// Casts a vector type, this is unsafe and should only be used internally in `vector/map` 
inline fip extern unsafe-vector-cast(v: vector<a>): vector<b>
  c inline "#1"
  js inline "#1"
  cs inline "#1"

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
inline fip extern ssize_t/(<=) : (ssize_t,ssize_t) -> bool
  inline "(#1 <= #2)"

inline fip extern ssize_t/(>=) : (ssize_t,ssize_t) -> bool
  inline "(#1 >= #2)"

inline fip extern ssize_t/(<)  : (ssize_t,ssize_t) -> bool
  inline "(#1 < #2)"

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
