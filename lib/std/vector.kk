/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Immutable vectors and mutable arrays.

*/
public module std/vector

// ----------------------------------------------------------------------------
// Arrays
// ----------------------------------------------------------------------------

// The type of mutable arrays. See also `:vector` for immutable arrays.
type array :: (H,V) -> V

// used internally to have non-bound-checked access
private extern inline unsafe-idx : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>|e> a with(hdiv<h,a,e>) {
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

private extern inline unsafe-assign : forall<h,a> ( a : array<h,a>, i : int, x : a ) -> write<h> () {
  cs inline "(#1)[#2] = #3";
  js inline "(#1)[#2] = #3";
}

// Return the `index` element of an array. Raises an exception for out-of-bounds access.
extern inline [] : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>,exn|e> a with(hdiv<h,a,e>) 
  { inline "(#1)[#2]" }

// Return the length of an `:array`.
extern inline length : forall<h,a> ( self : array<h,a> ) -> int { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length"
}

// Assign to element `i`  in an array `a` . May raise an out-of-bounds exception
extern inline [] : forall<h,a> ( a : array<h,a>, i : int, assigned : a ) -> <write<h>,exn> ()  
  { inline "(#1)[#2] = #3" }

// Create a new array of length `n`  with initial elements `a` .
extern inline array  : forall<h,a> (n : int, default : a) -> array<h,a> { 
  cs inline "Primitive.NewArray<##2>(#1,#2)"
  js inline "Array(#2)"
}
 
// Create a zero length array
extern inline array : forall<h,a> () -> array<h,a> {
  cs inline "new ##2[0]"
}

private extern inline unsafe-array  : forall<h,a> (n : int) -> array<h,a> { 
  cs inline "new ##2[#1]"
  js inline "Array(#1)"
}

private extern inline array-copy    : forall<h,a> ( src : array<h,a>, dest : array<h,a>, len : int ) -> read<h> ()  { 
  cs inline "Array.Copy(#1,#2,#3)"
  js inline "error('arrayCopy')"
}

private extern inline unsafe-freeze : forall<h,a> ( self : array<h,a> ) -> read<h> vector<a> { 
  cs inline "(#1)"
  js inline "(#1)"
}

// Copy an array
fun copy( self : array<h,a> ) : read<h> array<h,a> {
  val dest = unsafe-array(self.length)
  array-copy(self,dest,self.length)
  return dest
}

// Convert an array to a vector (to guarantee safety, a copy is made)
fun vector( self : array<h,a> ) : read<h> vector<a>
{
  copy(self).unsafe-freeze()
}


// Convert a list to an array.
fun array( xs : list<a> ) : array<h,a>
{
  unsafe-total {
    val n = xs.length
    val a = unsafe-array(n)
    xs.foreach-indexed fun(i,x) {
      a[i] := x
    }
    a
  }
}

// Convert an array to a list
fun list( a : array<h,a> ) : read<h> list<a>
{
  unsafe-total {
    val len = a.length()
    fun build( i : int, acc : list<_a> )
    {
      if (i >= 0) then build(i - 1, Cons(a[i],acc)) else acc
    }

    build(len - 1,Nil)
  }
}

// Invoke a function `f` for each element in a an array `a`.   
// Note: this can diverge by storing self referential funs in the array
fun foreach( a : array<h,a>, f : a -> <read<h>,div|e> () ) : <read<h>,div|e> ()
{
  for( 0, a.length - 1 ) fun(i) {
    f(a.unsafe-idx(i))
  }
}

// Destructively apply function `f`  to each element in an array `a`.   
// Note: this can diverge by storing self referential funs in the array
fun map( a : array<h,a>, f : a -> <st<h>,div|e> a ) : <st<h>,div|e> array<h,a>
{
  for( 0, a.length - 1 ) fun(i) {
    a.unsafe-assign( i, f(a.unsafe-idx(i)) )
  }
  a
}

// ----------------------------------------------------------------------------
//  Vectors
// ----------------------------------------------------------------------------

// The type of immutable arrays is called `:vector`.
type vector<a>

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
private extern inline unsafe-idx : forall<a> ( v : vector<a>, index : int ) -> total a { 
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
extern inline [] : forall<a> ( v : vector<a>, index : int ) -> exn a  { inline "(#1)[#2]" }

// Return the length of a vector.
extern inline length : forall<a> (v : vector<a> ) -> int  { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length";
}

// Create a new vector of length `n`  with initial elements `default` .
public fun vector(n : int, default : a) : vector<a> {
  list(1,n).map( fun(i){ default }).vector
} 

// Create an empty vector.
extern inline vector : forall<a> () -> vector<a> {
  cs inline "new ##1[0]"
  js inline "[]"
}

private extern inline unsafe-unfreeze : forall<h,a> (v : vector<a>) -> array<h,a> { inline "(#1)" }

// Invoke a function `f` for each element in a vector `v`
fun foreach( v : vector<a>, f : a -> e () ) : e ()
{
  for( 0, v.length - 1 ) fun(i) {
    f(v.unsafe-idx(i))
  }
}

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b>
{
  for-while( 0, v.length - 1 ) fun(i) {
    f(v.unsafe-idx(i))
  }
}

// Apply function `f` to each element in a vector `v`Ju
fun map( v : vector<a>, f : a -> e b ) : e vector<b>
{
  val n = v.length
  val a = unsafe-array(n)
  for( 0, v.length - 1 ) fun(i) {
    a.unsafe-assign( i, inject-st(f)(v.unsafe-idx(i)) )
  }
  a.unsafe-freeze
}

// Convert a vector to a list.
fun list( v : vector<a> ) : list<a>
{
  unsafe-total{
    v.unsafe-unfreeze.list
  }
}

// Convert a list to a vector.
fun vector( xs : list<a> ) : vector<a>
{
  xs.array.unsafe-freeze
}

// Convert a vector to an array (by making a copy)
extern inline array : forall<h,a> ( v : vector<a> ) -> array<h,a> { 
  cs inline "(##2[])Array.Clone(#1)";
  js inline "#1"
}
