/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Mutable arrays.

*/
public module std/data/array

// ----------------------------------------------------------------------------
// Arrays
// ----------------------------------------------------------------------------

// The type of mutable arrays. See also `:vector` for immutable arrays.
type array :: (H,V) -> V

// used internally to have non-bound-checked access
private extern inline unsafe-idx : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>|e> a with(hdiv<h,a,e>) {
  cs inline "(#1)[(int)#2]";
  js inline "(#1)[#2]";
}

private extern inline unsafe-assign : forall<h,a> ( a : array<h,a>, i : int, x : a ) -> write<h> () {
  cs inline "(#1)[(int)#2] = #3";
  js inline "(#1)[#2] = #3";
}

// Return the `index` element of an array. Raises an exception for out-of-bounds access.
extern inline [] : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>,exn|e> a with(hdiv<h,a,e>) { 
  cs inline "(#1)[(int)#2]" 
  js inline "(#1)[#2]"
}

// Return the length of an `:array`.
extern inline length : forall<h,a> ( self : array<h,a> ) -> int { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length"
}

// Assign to element `i`  in an array `a` . May raise an out-of-bounds exception
extern inline [] : forall<h,a> ( a : array<h,a>, i : int, assigned : a ) -> <write<h>,exn> ()  { 
  cs inline "(#1)[(int)#2] = #3" 
  js inline "(#1)[#2] = #3" 
}

// Create a new array of length `n`  with initial elements `a` .
extern inline array  : forall<h,a> (n : int, default : a) -> array<h,a> { 
  cs inline "Primitive.NewArray<##2>((int)#1,#2)"
  js inline "Array(#1)"
}
 
// Create a zero length array
/*
extern inline array : forall<h,a> () -> array<h,a> {
  cs inline "new ##2[0]"
}
*/

private extern inline unsafe-array  : forall<h,a> (n : int) -> array<h,a> { 
  cs inline "new ##2[(int)#1]"
  js inline "Array(#1)"
}

private extern inline unsafe-freeze : forall<h,a> ( self : array<h,a> ) -> read<h> vector<a> { 
  cs inline "(#1)"
  js inline "(#1)"
}

// Copy an array
extern inline clone( self : array<h,a> ) : read<h> array<h,a> {
  cs inline "(##1[])(#1).Clone()"
  js inline "(#1).slice(0)"
}

// Convert an array to a vector (to guarantee safety, a copy is made)
fun vector( self : array<h,a> ) : read<h> vector<a>
{
  clone(self).unsafe-freeze()
}


// Convert a vector to an array (by making a copy)
extern inline array : forall<h,a> ( v : vector<a> ) -> array<h,a> { 
  cs inline "(##1[])(#1).Clone()"
  js inline "(#1).slice()"
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
// Note: this can diverge by storing self referential functions in the array
fun foreach( a : array<h,a>, f : a -> <read<h>,div|e> () ) : <read<h>,div|e> ()
{
  for( 0, a.length - 1 ) fun(i) {
    f(a.unsafe-idx(i))
  }
}

// Destructively apply function `f`  to each element in an array `a`.   
// Note: this can diverge by storing self referential functions in the array
fun map( a : array<h,a>, f : a -> <st<h>,div|e> a ) : <st<h>,div|e> array<h,a>
{
  for( 0, a.length - 1 ) fun(i) {
    a.unsafe-assign( i, f(a.unsafe-idx(i)) )
  }
  a
}

// Concatenate an array of strings
extern inline join: forall<h> (a : array<h,string> ) -> read<h> string { 
  cs inline "String.Concat(#1)";
  js inline "((#1).join(''))";
}

// Concatenate an array of strings with a separator `sep`
extern join: forall<h> (v : array<h,string>, sep : string ) -> read<h> string { 
  cs inline "Primitive.Concat(#1,#2)";
  js inline "(#1).join(#2)"
}
