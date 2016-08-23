/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module defines standard operations that are always available.

   Some types and operations are required to be defined for the compiler
   to work correctly (i.e. types like `:int` or `:list`)
*/
public module std/core

infixr 80  (^)
infixl 70  (*), (%), (/), cdiv, cmod
infixr 60  (+), (-)
infix  40  (!=), (==), (<=), (>=), (<), (>) 
infixr 30  (&&)
infixr 20  (||)

// prefix     (!), (-) 

extern include {
  cs file "core-inline.cs"
  js file "core-inline.js"
}

extern include {
  js file "core-console-inline.js"
}

extern include {
  js file "core-handlers-inline.js"
}

// ----------------------------------------------------------------------------
// Core types
// ----------------------------------------------------------------------------

// A 32-bit signed integer (using two's complement to represent negative numbers).
type int

// A unicode string is a sequence of unicode characters (`char`).
// The encoding of a string is internal and not exposed and there
// is no direct indexing of characters in string. 
// Use the `:sslice` type for efficient matching and retrieving
// sub-strings from string.
type string

// A 64-bit IEEE 754 floating point value
type double

// An any type. Used for extern calls
type any

// A unicode character. 
// Characters are unicode _codepoint_\/s.
// This is different from a unicode _grapheme_ which represents a single displayed
// symbol and can consists of multiple codepoints due to combining characters and marks.
// (see also `std/unicode/grapheme`)
type char


// ----------------------------------------------------------------------------
// Core effects
// ----------------------------------------------------------------------------

// The _total_ effect represents the absence of any effect.
type <> :: E

// The effect constructor extends an effect with another effect.
type <|> :: (X,E) -> E

// The exception effect: a partial function may raise an exception.
type exn :: X

// The divergence effect: a divergent function may not terminate.
type div :: X

// NonDeterminism: a nonDeterministic function may return varying
// results even when called with the same input values.
type ndet :: X

// The alloc effect signifies a function may allocate in a heap `:h`
type alloc :: H -> X

// The read effect: signifies that a function may read from from heap `:h`.
type read :: H -> X

// The write effect: signifies that a function may write to heap `:h`.
type write :: H -> X

// The console effect signifies that a function may read or write to the console.
type console :: X

// An alias for the empty effect.
alias total = <>

// An alias for pure effects: a pure function always returns the same result
// when called with the same arguments but may not terminate or raise an exception.
alias pure = <exn,div>

// Stateful funs can manipulate heap `:h` using allocations, reads and writes.
alias st<h> = <read<h>, write<h>, alloc<h>>

// The `:global` heap is a special heap constant to denote the global shared heap
type global :: H

// The `:net` effect signifies a function may access the network
type net :: X

// The `:file` effect signifies a function may access the file system
type file :: X

// The `:ui` effect signifies a function may access the graphics system
type ui :: X

// The `:blocking` effect signifies that a function may block
type blocking :: X

// The `:io` effect is used for funs that perform arbitrary I/O operations.
alias io = <exn,div,ndet,console,net,file,ui,st<global>>

// ----------------------------------------------------------------------------
// Injection
// ----------------------------------------------------------------------------

// Add the divergent effect to a function effect.
extern inline inject-div : forall<a,b,e> (a -> e b) -> (a -> <div|e> b) { inline "#1" }

// Add the state effect to a function effect.
extern inline inject-st : forall<a,b,h,e> ((a) -> e b) -> total ((a) -> <st<h>|e> b)   { inline "#1" }

// Add the state effect to a function effect.
extern inline inject-st : forall<a,h,e> (() -> e a) -> total (() -> <st<h>|e> a)  { inline "#1" }


// ----------------------------------------------------------------------------
// Exceptions
// ----------------------------------------------------------------------------

// Exceptions
type exception

// Return a system dependent description of an exception
extern show( exn : exception ) : string {
  cs inline "#1.ToString()"
  js inline "(#1 ? #1.toString() : 'unknown exception')"
}

// Catch an exception raised by `error` and handle it.
// Use `on-exn` or `on-fail` when appropiate.
extern catch : forall<e,a> ( action : () -> <exn|e> a, hndl: exception -> e a) -> e a {
  cs inline "Primitive.Catch<##2>(#1,#2)" // todo: handle cps
  js inline "_primcatch(action,hndl, (typeof _k === 'undefined' ? undefined : _k))"
}
  
// Execute a `hndler` no matter what exception was raised in `action`.
// Use of `on-exit` is preferred (which has its arguments swapped).
extern finally : forall<e,a> (action: () -> e a, hndl: () -> e ()) -> e a  {
  cs inline "Primitive.Finally<##2>(#1,#2)" // todo: handle cps
  js inline "_primfinally(action,hndl, (typeof _k === 'undefined' ? undefined : _k))"
}

// Raise an exception with a specified message.
extern error : forall<a> string -> exn a { 
    cs "Primitive.Error";
    js "_error"
}

// Throw an exception
extern throw : forall<a> exception -> exn a {
  cs inline "Primitive.Throw<##1>(#1)"
  js "error(#1)"
}

// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
extern error-pattern-match : forall<a> (range : string, def : string) -> exn a { 
    cs "Primitive.PatternMatchError"
    js "_error_pattern_match"
}

// Set a `hndler` that is called when an exception is raised in the `action` block.
fun on-fail( hndler : () -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a
{
  catch-on-fail( action, fun(exn){ hndler(); throw(exn) })
}

// Return a default value when an exception is raised
fun on-exn( value : a , action : () -> <exn|e> a ) : e a {
  catch( action, fun(_) { value })
}

private extern catch-on-fail : forall<e,a> ( action : () -> e a, hndler: exception -> e a) -> e a {
  cs inline "Primitive.Catch<##2>(#1,#2)"
  js "_catch"
}


// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
fun on-exit( hndler : () -> e (), action : () -> e a ) : e a
{
  finally(action,hndler)  
}

// Set a `hndler` that is called if `action` returns without raising an exception.
fun on-success( hndler : () -> e (), action : () -> e a ) : e a
{
  val x = action()
  hndler()
  return x
}

// Ignore any Exceptions
fun ignore( action : () -> <exn|e> () ) : e () {
  catch( action, fun(_) { () })
}


// ----------------------------------------------------------------------------
// Internal types
// ----------------------------------------------------------------------------

// Optional is used internally by the compiler to pass optional arguments.
// It is usually displayed as `:?a` for some type `:a`.
type optional<a> {
  // The `Optional` constructor is used when an optional argument is given.
  con Optional(value:a)
  // `None` is used when an optional argument is not provided.
  con None
}


// ----------------------------------------------------------------------------
// Standard functions
// ----------------------------------------------------------------------------

// The identity function returns its argument unchanged.
fun id(x) {
  x;
}

// The `const` funs returns its first argument and ignores the second.
fun const(x,y) {
  x;
}

// Apply a function `f` to a specified argument `x`.
fun apply(f,x) {
  f(x);
}

// Compose two funs `f` and `g`.
fun o(f,g) {
  (fun(x){ f(g(x)) })
}


// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

// The `:void` type is empty and has no constructors.
// See also the `:()` unit type and the `:bool` type.
type void

// The type of booleans has two inhabitants: `True` and `False`.
type bool {
  con False
  con True
}


fun int( b : bool ) : int  -> if (b) then 1 else 0

fun mbint( m : maybe<int> ) : int {
  match(m) {
    Nothing -> 0
    Just(i) -> i
  }
}

// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
fun bool( i : int ) : bool  -> (i!=0)

// Convert a `:maybe` type to a boolean using `False` for `Nothing` and `True` for `Just`.
fun bool( m : maybe<a> ) : bool {
  match(m) {
    Nothing -> False
    _       -> True
  }
}

// Convert a string to a boolean, using `False` for the empty string and `True` otherwise.
fun bool( s : string ) : bool  -> (s!="")

// The unit type `:()` is inhabited by just a single value, namely `()`.
// See also the `:void` type and the `:bool` type.
struct ()

// A pair of values `:a` and `:b`.
struct (,)<a,b>(fst:a,snd:b)

// A triple of values.
struct (,,)<a,b,c>(fst:a,snd:b,thd:c)

// A quadruple of values.
struct (,,,)<a,b,c,d>(fst:a,snd:b,thd:c,field4:d)

// A quintuple of values.
struct (,,,,)<a,b,c,d,e>(fst:a,snd:b,thd:c,field4:d,field5:e)

// The `:maybe` type is used to represent either a value (`Just(x)`) or `Nothing`.
// This type is often used to represent values that can be _null_.
type maybe<a> {
  con Nothing
  con Just( value : a )
}

// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
fun maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
{
  match(m) {
    Nothing -> onNothing
    Just(x) -> onJust(x)
  }
}

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
fun maybe( m : maybe<a>, nothing : a ) : a {
  match(m) {
    Nothing -> nothing
    Just(x) -> x
  }
}

// Get the value of the `Just` constructor or raise an exception
fun unjust( m : maybe<a> ) : exn a {
  match(m) {
    Just(x) -> x
  }
}

fun map( m : maybe<a>, f : a -> e b ) : e maybe<b> {
  match(m) {
    Nothing -> Nothing
    Just(x) -> Just(f(x))
  }
}

// The choice type represents one of two possible types `:a` or `:b`.
type either<a,b> {
  con Left( left : a )
  con Right( right : b )
}

// Convert a `:either` to a `:maybe` type discarding the value of the `Left` constructor
// and using `Just` for the `Right` constructor.
fun maybe( e : either<a,b> ) : maybe<b> 
{
  match(e) {
    Left -> Nothing
    Right(x) -> Just(x)
  }
}

// The type of lists, which can be either empty (`Nil`) or an element followed
// by a list (`Cons`).
type list<a> {
  // The empty list.
  con Nil
  // A ``head``  element followed by the ``tail``  of the list.
  con Cons(head:a, tail : list<a> )
}

// Return the head of list if the list is not empty.
fun head( xs : list<a> ) : maybe<a> {
  match(xs) {
    Cons(x) -> Just(x)
    _       -> Nothing
  }
}

// Return the head of list if the list is not empty, or use `default` otherwise
fun head( xs : list<a>, default : a ) : a {
  match(xs) {
    Cons(x) -> x
    _       -> default
  }
}

// Return the tail of list if the list is not empty
// Note: if a `:maybe` type is inconvenient,
// you can use `drop(1)` to return an empty list if the input list was empty.
fun tail( xs : list<a> ) : maybe<list<a>> {
  match(xs) {
    Cons(_,xx) -> Just(xx)
    _          -> Nothing
  }
}


// A `:stream` is a co-inductive type reprenting an infinite list of elements.
cotype stream<a> {
  con Next(head:a, tail: stream<a> )
}

// An enumeration to represent order
type order {
  Lt
  Eq
  Gt
}

fun int( x : order ) : int
{
  match(x) {
    Lt -> 0 - 1
    Eq -> 0
    Gt -> 1
  }
}

fun order( i : int ) : order
{
  if (i < 0) Lt
  elif (i > 0) Gt
  else Eq
}

fun (==)( x : order, y : order ) : bool  -> (x.int == y.int)
fun (!=)( x : order, y : order ) : bool  -> (x.int != y.int)
fun (>=)( x : order, y : order ) : bool  -> (x.int >= y.int)
fun (<=)( x : order, y : order ) : bool  -> (x.int <= y.int)
fun (>)( x : order, y : order ) : bool   -> (x.int > y.int)
fun (<)( x : order, y : order ) : bool   -> (x.int < y.int)

fun map( t : (a,a), f : a -> e b ) : e (b, b) {
  (t.fst.f, t.snd.f)
}

fun map( t : (a,a,a), f : a -> e b ) : e (b, b, b) {
  (t.fst.f, t.snd.f, t.thd.f)
}

fun map( t : (a,a,a,a), f : a -> e b ) : e (b,b,b,b) {
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f)
}

// ----------------------------------------------------------------------------
// List funs
// ----------------------------------------------------------------------------

// Returns a singleton list.
fun single(x) {
  return Cons(x,Nil)
}

// Returns the length of a list.
fun length(xs)
{
  fun len(acc,ys) {
    match(ys)
    {
      Cons(_,yy) -> len( acc+1, yy )
      Nil        -> acc
    }
  }

  return len(0,xs)
}

// Returns an integer list of increasing elements from `lo`  to `hi` 
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
fun list( lo: int, hi: int ) : total list<int>
{
  fun enumerate( low:int, high:int, acc: list<int> ) : list<int>
  {
    if (low > high)
     then acc
     else enumerate(low, unsafe-decreasing(high - 1), Cons(high,acc))
  }

  enumerate(lo,hi,[]) 
}

// Applies a function `f` to list of increasing elements from `lo`  to `hi` 
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
fun list( lo : int, hi: int, f : int -> e a) : e list<a> {
  fun enumerate( low:int, high:int, acc ) 
  {
    if (low > high)
     then acc
     else enumerate(low, unsafe-decreasing(high - 1), Cons(f(high),acc))
  }
  enumerate(lo,hi,[]) 
}


// Create a list of characters from `lo`  to `hi`  (inclusive).
function list( lo : char, hi : char ) : total list<char>
{
  list(lo.int, hi.int).map( char )
}

private val maxListStack = 200


private fun zipwith-acc( f, i, acc, xs, ys )
{
  match(xs) {
    Nil -> reverse(acc)
    Cons(x,xx) ->
      match(ys) {
        Nil -> reverse(acc)
        Cons(y,yy) -> zipwith-acc(f, i+1, Cons( f(i,x,y), acc), xx, yy)
      }
  }
}

private fun zipwith-iter( f, i, xs, ys )
{
  // recurse for the first `maxListStack` elements over the stack (to avoid extra heap allocation)
  if (i > maxListStack) then zipwith-acc( f, i, Nil, xs, ys )
  else {
    match(xs) {
      Nil -> Nil
      Cons(x,xx) ->
        match(ys) {
          Nil -> Nil
          Cons(y,yy) -> Cons( f(i,x,y), zipwith-iter(f,i+1,xx,yy))
        }
    }
  }
}

// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
fun zip( xs : list<a>, ys : list<b> ) : list<(a,b)>
{
  zipwith-indexed( xs, ys, fun(i,x,y) { (x,y) } )
}

// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
fun zipwith( xs : list<a>, ys :list<b>, f : (a,b) -> e c ) : e list<c>
{
  zipwith-indexed(xs,ys,fun(i,x,y) { f(x,y) })
}

// Zip two lists together by apply a function `f` to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
fun zipwith-indexed( xs : list<a>, ys :list<b>, f : (int,a,b) -> e c ) : e list<c>
{
  zipwith-iter(f,0,xs,ys)
}

// Unzip a list of pairs into two lists
fun unzip( xs : list<(a,b)> ) : (list<a>,list<b>)
{
  fun iter( ys, acc1, acc2 )
  {
    match(ys) {
      Cons((x,y),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2))
      Nil            -> (reverse(acc1),reverse(acc2))
    }
  }
  
  iter(xs,[],[])
}

// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
fun take( xs : list<a>, n : int ) : list<a>
{
  if (n <= 0) return Nil
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(x,take(xx,n - 1))
  }
}

// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)
fun drop( xs : list<a>, n : int ) : list<a>
{
  if (n <= 0) return xs
  match(xs) {
    Nil -> Nil
    Cons(_,xx) -> drop(xx,n - 1)
  }
}



// Apply a function `f`  to each element of the input list in sequence.
fun map(xs : list<a>, f : a -> e b) : e list<b>
{
  xs.map-indexed-peek( fun(i,x,xx) { f(x) } )
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
fun map-indexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b>
{
  xs.map-indexed-peek( fun(i,x,xx) { f(i,x) })
}

// Apply a function `f`  to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
fun map-peek(xs : list<a>, f : (value : a, rest : list<a>) -> e b) : e list<b>
{
  xs.map-indexed-peek( fun(i,x,xx) { f(x,xx) })
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun map-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  // recurse using an accumulator using constant heap space
  fun map-acc(g : (int,a,list<a>) -> e b , n : int, acc : list<b>, ys : list<a>) : e list<b> {

    match(ys) {
      Cons(x,xx) -> map-acc(g,n+1,Cons(g(n,x,xx),acc),xx)
      Nil -> reverse(acc)
    }
  }

  // recurse for the first `maxListStack` elements over the stack (to avoid extra heap allocation)
  fun map-iter(g : (int,a,list<a>) -> e b, n : int, ys : list<a>) : e list<b> {
    if (n > maxListStack) {
      map-acc(g,n,Nil,ys)
    }
    else {
      match(ys) {
        Cons(x,xx) -> Cons(g(n,x,xx),map-iter(g,n+1,xx))
        Nil        -> Nil
      }
    }
  }

  return map-iter(f,0,xs)
}

// Reverse a list.
fun reverse(xs : list<a>) : list<a>
{
  fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a> {
    match(ys) {
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc
    }
  }
  reverse-acc(Nil,xs)
}

// Append two lists.
fun (+)(xs : list<a>, ys : list<a> ) : list<a>
{
  // append using _constant_ stack space (by reversing the argument list)
  fun rev-append(xx,yy) {
    match(xx) {
      Cons(z,zz) -> rev-append(zz,Cons(z,yy))
      _          -> yy
    }
  }
  // append for the first `maxListStack` elements over the stack
  fun append(n,xx,yy) {
    if (n > maxListStack) {
      rev-append(reverse(xx),yy)
    }
    else {
      match(xx) {
        Cons(z,zz) -> Cons(z,append(n+1,zz,yy))
        Nil -> yy
      }
    }
  }

  match(ys) {
    Nil -> xs
    _   -> match(xs) {
             Nil -> ys
             _   -> append(0,xs,ys)
           }
  }
}

// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)` 
// Note, `foldr` is less efficient than `foldl` as it reverses the list first. 
fun foldr(xs,z,f)
{
  xs.reverse.foldl(z,fun(x,y){f(y,x)}) 
}

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2` 
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f` 
fun foldl(xs,z,f)
{
  match(xs) {
    Cons(x,xx) -> foldl(xx,f(z,x),f)
    Nil        -> z
  }
}

fun foldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
{
  match(xs) {
    Cons(x,xx) -> xx.foldl(x,f)
  }
}

fun foldr1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
{
  match(xs) {
    Cons(x,Nil) -> x
    Cons(x,xx)  -> f(x,xx.foldr1(f))
  }
}

// Create a list of `n`  repeated elementes `x` 
fun replicate( x : a, n : int ) : list<a>
{
  fun enumerate( i : int, acc : list<_a> ) {
    if (i <= 0) then acc else enumerate(i - 1,Cons(x,acc))
  }

  unsafe-nodiv{ enumerate(n,[]) }
}

// split a list at position `n` 
fun split( xs : list<a>, n : int ) : (list<a>, list<a>)
{
  (xs.take(n), xs.drop(n))
}


fun span( xs : list<a>, predicate : a -> e bool ) : e (list<a>,list<a>)
{
  fun span-acc( ys, acc)
  {
    match(ys) {
      Cons(y,yy) -> if (y.predicate) then yy.span-acc(Cons(y,acc)) else (acc.reverse,ys)
      _ -> (acc.reverse, ys)
    }
  }

  xs.span-acc( [] )
}

// Keep only those initial elements that satisfy `predicate` 
fun take-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
{
  xs.span(predicate).fst
}

// Drop all initial elements that satisfy `predicate` 
fun drop-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
{
  xs.span(predicate).snd
}

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd) == [1,3]`
fun filter( xs : list<a>, pred : a -> e bool ) : e list<a>
{
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> if (pred(x)) then Cons(x,xx.filter(pred)) else xx.filter(pred)
  }
}


// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd) == [2]`
fun remove( xs : list<a>, pred : a -> bool ) : list<a> {
  xs.filter( fun(x) { !pred(x) } )
}

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd) == ([1,3],[2])`
fun partition( xs : list<a>, pred : a -> bool ) : (list<a>,list<a>)
{
  partition-acc( xs, pred, Nil, Nil)
}

private fun partition-acc( xs : list<a>, pred : a -> bool, acc1 : list<a>, acc2 : list<a> ) : (list<a>, list<a>)
{
  match(xs) {
    Nil -> (acc1.reverse, acc2.reverse)
    Cons(x,xx) -> if (pred(x)) 
                    then partition-acc(xx,pred,Cons(x,acc1),acc2)
                    else partition-acc(xx,pred,acc1,Cons(x,acc2))
  }
}


// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fun(i) { if (odd(i)) then Nothing else Just(i*i) }) == [4]`
fun filter-map( xs : list<a>, pred : a -> e maybe<b> ) : e list<b>
{
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> match(pred(x)) {
      Nothing -> xx.filter-map(pred)
      Just(y) -> Cons(y,xx.filter-map(pred))
    }
  }
}

// Find the first element satisfying some predicate
fun find( xs : list<a>, pred : a -> bool ) : maybe<a>
{
  xs.filter(pred).maybe
}

// Convert a list to a `:maybe` type, using `Nothing` for an empty list, and otherwise `Just` on the head element.
// Note: this is just `head`.
fun maybe( xs : list<a> ) : maybe<a>
{
  match(xs) {
    Nil -> Nothing
    Cons(x,_) -> Just(x)
  }
}

// Convert a `:maybe` type to a list type.
fun list( m : maybe<a> ) : list<a> 
{
  match(m) {
    Nothing -> Nil
    Just(x) -> Cons(x,Nil)
  }
}

private fun index-of-acc( xs : list<a>, pred : a -> bool, idx : int ) : int
{
  match(xs) {
    Nil -> 0 - 1
    Cons(x,xx) -> if (pred(x)) then idx else index-of-acc(xx,pred,idx+1)
  }
}

// Returns the index of the first element where `pred` holds, or `-1` if no such element exists.
fun index-of( xs : list<a>, pred : a -> bool ) : int
{
  index-of-acc( xs, pred, 0 )
}

// Invoke `action` for each element of a list
fun foreach( xs : list<a>, action : (a) -> e () ) : e ()
{
  match(xs) {
    Nil        -> return ()
    Cons(x,xx) -> { action(x); xx.foreach(action) }
  }
}

// Invoke `action` for each element of a list while `action` return `Nothing`
fun foreach-until( xs : list<a>, action : (a) -> e maybe<b> ) : e maybe<b>
{
  match(xs) {
    Nil        -> Nothing
    Cons(x,xx) -> { 
      match(action(x)) {
        Nothing -> xx.foreach-until(action) 
        just    -> just
      }
    }
  }
}

// Invoke `action` on each element of a list while `action` returns `Just`
fun map-while( xs : list<a>, action : (a) -> e maybe<b> ) : e list<b>
{
  match(xs) {
    Nil        -> Nil
    Cons(x,xx) -> { 
      match(action(x)) {
        Just(y) -> Cons(y,xx.map-while(action))
        Nothing -> Nil
      }
    }
  }
}

private extern inline inject-st : forall<a,b,c,h,e> ((a,b) -> e c) -> total ((a,b) -> <st<h>|e> c)  { inline "#1" }

// Invoke `action` for each element of a list, passing also the position of the element.
fun foreach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
{
  var i := 0
  xs.foreach fun(x) {
    inject-st(action)(i,x)
    i := i+1
  }
}

// Insert a separator `sep`  between all elements of a list `xs` .
fun intersperse( xs : list<a>, sep : a ) : list<a>
{
  //TODO: make tail recursive
  fun before(ys,s) {
    match(ys) {
      Cons(y,yy) -> Cons(s,Cons(y,before(yy,s)))
      Nil        -> Nil
    }
  }

  match(xs) {
    Cons(x,xx)  -> Cons(x, xx.before(sep))
    Nil         -> Nil
  }
}

// Concatenate all strings in a list
fun join( xs : list<string> ) : string
{
  fun join-acc( ys : list<string>, acc : string ) {
    match(ys) {
      Cons(y,yy) -> join-acc(yy, acc+y)
      Nil -> acc
    }
  }
  
  match(xs) {
    Nil -> ""
    Cons(x,xx) -> match(xx) {
      Nil -> x
      _   -> join-acc(xx,x)
    }
  }
  //xs.foldl( "", fun(x,y){ x + y } )  // TODO: should use a string builder
}

// Concatenate all strings in a list using a specific separator
fun join( xs : list<string>, sep : string ) : string
{
  return xs.intersperse(sep).join()
}

// Concatenate all lists in a list (e.g. flatten the list). (tail-recursive)
fun concat( xs : list<list<a>> ) : list<a>
{
  fun concat-rev( xss : list<list<a>>, acc : list<a> ) : list<a> {
    match(xss) {
      Cons(ys,yss) -> concat-rev( yss, ys + acc )
      Nil          -> acc
    }
  }
  concat-rev(xs.reverse,[])
}

// Concatenate the result lists from applying a function to all elements.
fun flatmap( xs: list<a>, f : a -> e list<b> ) : e list<b>
{
  xs.map(f).concat()
}

// Concatenate a list of `:maybe` values
fun concat-maybe( xs : list<maybe<a>> ) : list<a>
{
  xs.map(list).concat()
}

// Return the last element of a list. Raise an exception if the empty list is passed.
fun last( xs : list<a> ) : exn a
{
  match(xs) {
    Cons(x,Nil) -> x
    Cons(_,xx)  -> last(xx)
  }
}

// Return the list without its last element.
// Return an empty list for an empty list.
fun init( xs : list<a> ) : list<a>
{
  match(xs) {
    Cons(x,xx)  -> match(xx) {
                     Nil -> Nil
                     _   -> Cons(x,init(xx))
                   }
    Nil         -> Nil
  }
}

// Get (zero-based) element `n`  of a list. Return a `:maybe` type.
fun []( xs : list<a>, n : int ) : maybe<a>
{
  match(xs) {
    Cons(x,xx) -> if (n==0) then Just(x) else xx[n - 1]
    Nil -> Nothing
  }
}

// Do all elements satisfy a predicate ?
fun all( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Nil -> True
    Cons(x,xx) -> if (predicate(x)) then xx.all(predicate) else False
  }
}


// Are there any elements in a list that satisfy a predicate ?
fun any( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Nil -> False
    Cons(x,xx) -> if (predicate(x)) then True else xx.any(predicate)
  }
}

// ----------------------------------------------------------------------------
// Characters
// ----------------------------------------------------------------------------

extern inline (==) : (char,char) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
extern inline (!=) : (char,char) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
extern inline (<=) : (char,char) -> bool  { inline "(#1 <= #2)" }
extern inline (>=) : (char,char) -> bool  { inline "(#1 >= #2)" }
extern inline (<)  : (char,char) -> bool  { inline "(#1 < #2)" }
extern inline (>)  : (char,char) -> bool  { inline "(#1 > #2)" }

fun compare( x : char, y : char ) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}

// Convert a character to its unicode code point
extern inline int : (char) -> int { inline "(#1)" }

// Convert a unicode code point to a character
extern inline char : (int) -> char { inline "(#1)" }

// Add two character code points
fun (+)(c : char, d : char) : total char  -> (c.int + d.int).char
// Substract two character codePoints
fun (-)(c : char, d : char) : total char  -> (c.int - d.int).char

// Is the character a lower-case ASCII character ?
fun lower?( c : char ) : bool  -> (c >= 'a' && c <= 'z')
// Is the character an upper-case ASCII character ?
fun upper?( c : char ) : bool  -> (c >= 'A' && c <= 'Z')
// Is the character an ASCII digit ?
fun digit?( c : char ) : bool  -> (c >= '0' && c <= '9')
// Is the character an ASCII hexa-decimal digit ?
fun hex-digit?( c : char ) : bool  -> (c.digit? || (c >= 'a' && c <= 'f') || (c >= 'A'  && c <= 'F'))
// Is the character an ASCII letter ?
fun alpha?( c : char ) : bool  -> (c.lower? || c.upper?)
// Is the character ASCII letter or digit?
fun alpha-num?( c : char ) : bool  -> (c.alpha? || c.digit?)
// Is the character an ASCII character, e.g. `c <= '\x7F'`  ?
fun ascii?( c : char )     : bool  -> (c <= '\x7F')
// Is the character an ASCII control character, e.g. `c < ' '`  ?
fun control?( c : char )   : bool  -> (c < ' ')
// Tests if a character is an element of `" \t\n\r"` 
fun white?( c : char )     : bool  -> (c == ' ' || c == '\t' || c == '\n' || c == '\r')


// ----------------------------------------------------------------------------
// Booleans
// ----------------------------------------------------------------------------

// For short-circuiting we use extern here
// fun (&&)( x : bool, y : bool) : bool = if (x) then y else false
// fun (||)( x : bool, y : bool) : bool = if (x) then true else y

extern inline (&&) : (bool,bool) -> bool  { inline "(#1 && #2)" }
extern inline (||) : (bool,bool) -> bool  { inline "(#1 || #2)" }

// for efficiency we use extern here
// fun (!)( b : bool ) : bool = if (x) then false else true
// fun not( b : bool ) : bool = if (x) then false else true

extern inline not  : (bool) -> bool  { inline "!(#1)" }
extern inline (!)  : (bool) -> bool  { inline "!(#1)" }

fun (==)( x : bool, y : bool) : bool  -> if (x) then y else !y
fun (!=)( x : bool, y : bool) : bool  -> if (x) then !y else y

fun (<)( x : bool, y : bool) : bool   -> (!x && y)
fun (<=)( x : bool, y : bool) : bool  -> !(x > y)
fun (>)( x : bool, y : bool) : bool   -> (x && !y)
fun (>=)( x : bool, y : bool) : bool  -> !(x < y)

fun compare( x : bool, y : bool) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// ----------------------------------------------------------------------------
// Integers
// ----------------------------------------------------------------------------

extern inline (==) : (int,int) -> bool  { inline "(#1 == #2)"; js inline "#1 === #2" }
extern inline (!=) : (int,int) -> bool  { inline "(#1 != #2)"; js inline "#1 !== #2" }
extern inline (<=) : (int,int) -> bool  { inline "(#1 <= #2)"; js inline "#1 <= #2" }
extern inline (>=) : (int,int) -> bool  { inline "(#1 >= #2)"; js inline "#1 >= #2"}
extern inline (<)  : (int,int) -> bool  { inline "(#1 < #2)"; js inline "#1 < #2" }
extern inline (>)  : (int,int) -> bool  { inline "(#1 > #2)"; js inline "#1 > #2" }

extern inline (+) : (int,int) -> int { cs inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
extern inline (-) : (int,int) -> int { cs inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }


// Do not use. Used by the compiler for platforms that do not support 32 bit multiplication natively.
extern int-multiply : (int,int) -> int {
  cs inline "(#1 * #2)"
  js "_int32_multiply"
}

extern inline (*) : (int,int) -> int { 
  cs inline "(#1 * #2)"; 
  js "$std_core.int_multiply"
}

fun compare( x : int, y : int) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// Return the absolute value of an integer.
// Raises an exception if the `:int` is `minInt`
// (since the negation of `minInt` equals itself and is still negative)
fun abs( i : int ) : exn int
{
  if (i >= 0) return i
  if (i > min-int) return negate(i)
  error( "system.core.abs: cannot make minInt into a positive int without overflow" )
}

// Take the bitwise _and_ of two `:int`s
extern inline (&) : (int,int) -> int { 
  cs inline "((#1)&(#2))";
  js inline "((#1)&(#2))"
}
  
// Take the bitwise _or_ of two `:int`s
extern inline (|) : (int,int) -> int { 
  cs inline "((#1)|(#2))";
  js inline "((#1)|(#2))";
}

// Take the bitwise _xor_ of two `:int`s
extern inline (^) : (int,int) -> int { 
  cs inline "((#1)^(#2))";
  js inline "((#1)^(#2))"
}

// Bitwise _not_ of an `:int`, i.e. flips all bits.
extern inline not : ( i : int ) -> int  { 
  cs inline "(~(#1))";
  js inline "(~(#1))"
}

private extern inline prim-shl : (int,int) -> int { 
  cs inline "((#1)<<(#2))";
  js inline "((#1)<<(#2))";
}

private extern inline prim-shr : (int,int) -> int { 
  cs inline "(int)((uint)(#1)>>(#2))";
  js inline "((#1)>>>(#2))";
}

private extern inline prim-sar : (int,int) -> int { 
  cs inline "((#1)>>(#2))";
  js inline "((#1)>>(#2))";
}

// Bitwise rotate an `:int` `n` bits to the left.
// Does not rotate when `n` is negative.
fun rotl( i : int, n : int ) : int {
  if (n < 0) return i;
  val r = n & (bits-int - 1)  // modulo bits in an int
  (prim-shl(i,r) | prim-shr(i,bits-int - r))
}

// Bitwise rotate an `:int` `n` bits to the right.
// Does not rotate when `n` is negative.
fun rotr( i : int, n : int ) : int {
  if (n < 0) return i;
  val r = n & (bits-int - 1)  // modulo bits in an int
  (prim-shr(i,r) | prim-shl(i,bits-int - r))
}

// Shift an `:int` `i` to the left by `n` bits.
// Returns `i` when `n` is negative, or `0` when `n >= bitsInt`.
fun shl( i : int, n : int ) {
  if (n < 0) return i
  if (n >= bits-int) return 0
  prim-shl(i,n)
}

// Logical shift an `:int` to the right by `n` bits. Shift in zeros from the left.
// Returns `i` when `n` is negative, or `0` when `n >= bitsInt`.
fun shr( i : int, n : int ) {
  if (n < 0) return i;
  if (n >= bits-int) return 0;
  prim-shr(i,n);
}

// Arithmetic shift an `:int` to the right by `n` bits. Preserves the sign bit.
// Returns `i` when `n` is negative. When `n >= bitsInt` returns either `-1` when `i<0`, or `0` when `i>=0`.
fun sar( i : int, n : int ) {
  if (n < 0) return i;
  if (n >= bits-int) return (if (i < 0) then (0 - 1) else 0);
  prim-sar(i,n);
}



// Return the minimum of two integers
fun min( i : int, j : int ) : int  -> if (i <= j) then i else j
// Return the maximum of two integers
fun max( i : int, j : int ) : int  -> if (i >= j) then i else j


// Returns the smallest element of a list of integers (or `0` for the empty list)
fun minimum( xs : list<int> ) : int
{
  match(xs) {
    Nil -> 0 
    Cons(x,xx) -> xx.foldl( x, min )
  }
}

// Returns the largest element of a list of integers (or `0` for the empty list)
fun maximum( xs : list<int> ) : int
{
  match(xs) {
    Nil -> 0 
    Cons(x,xx) -> xx.foldl( x, max )
  }
}

// Do not use. Used by the compiler for platforms that do not support 32 bit truncated division natively.
extern int-cdiv : (int,int) -> exn int {
  cs inline "(#1 / #2)"
  js "_int32_cdiv"
}

// Do not use. Used by the compiler for platforms that do not support 32 bit truncated modulus natively.
extern int-cmod : (int,int) -> exn int {
  cs inline "(#1 % #2)"
  js "_int32_cmod"
}

// Truncated division (as in C). See also `(/):(x : int, y : int) -> int`.
extern inline cdiv : (int,int) -> exn int { 
  cs inline "(#1/#2)";
  js "$std_core.int_cdiv"
}

// Truncated modulus (as in C). See also `(%):(x : int, y : int) -> int`.
extern inline cmod : (int,int) -> exn int { 
  cs inline "(#1 % #2)";
  js "$std_core.int_cmod"
}

// Truncated division (as in C). See also `(/):(x : int, y : int) -> int`.
private extern inline unsafe-cdiv : (int,int) -> int {
  cs inline "(#1/#2)";
  js inline "((#1/#2)|0)"
}

// Truncated modulus (as in C). See also `(%):(x : int, y : int) -> int`.
private extern inline unsafe-cmod : (int,int) -> int { 
  cs inline "(#1 % #2)";
  js inline "((#1 % #2)|0)"
}

// Convert an integer to a double.
extern inline double : (int) -> double { cs inline "(double)(#1)"; js inline "(#1)" }

// Negate an integer
fun negate( i : int ) {
  return (0 - i)
}

// Negate an integer
fun (~)(i : int) : total int {
  return (0 - i)
}

// The maximal integer value before overflow happens
val max-int = 0x7FFFFFFF

// The minimal integer value before underflow happens
val min-int = 0 - max-int - 1

// The number of bits in an `:int` (always 32)
val bits-int = 32

// Euclidean-0 division.
// Euclidean division is defined as: For any `D`  and `d`  where `d!=0` , we have:
//
// 1. `D == d*(D/d) + (D%d)` 
// 2. `D%d`  is always positive where `0 <= D%d < abs(d)` 
//
// Moreover, Euclidean-0 is a total function, for the case where `d==0`  we have
// that `D%0 == D`  and `D/0 == 0` . So property (1) still holds, but not property (2).
// 
// Useful laws that hold for Euclidean-0 division:
//
// * `D/(-d) == -(D/d)` 
// * `D%(-d) == D%d` 
// * `D/(2^n) == sar(D,n)         `  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
// * `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
//
// Note that an interesting edge case is `minInt / -1` which equals `minInt` since in modulo 32-bit 
// arithmetic `minInt == -1 * minInt == -1 * (minInt / -1) + (minInt % -1)` satisfying property (1).
// Of course `(minInt + 1) / -1` is again positive (namely `maxInt`).  
// 
// See also _Division and modulus for computer scientists, Daan Leijen, 2001_ for further information
// available at: <http://research.microsoft.com/pubs/151917/divmodnote.pdf> .
fun (/)( x : int, y : int ) : int
{
  if (y == 0) then return 0
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if (r >= 0) then q
  elif (y > 0) then q - 1
  else q+1
}

// Euclidean-0 modulus. See `(/):(x : int, y : int) -> int` division for more information.
fun (%)( x : int, y : int ) : int
{
  if (y == 0) then return x
  val r = unsafe-cmod(x,y)
  if (r >= 0) then r
  elif (y > 0) then (r+y)
  else (r - y)
}

// Returns `true` if the integer `i`  is an odd number.
fun odd( i ) {
  return (i&1==1);
}

// Returns `true` if the integer `i`  is an even number.
fun even( i ) {
  return (i&1==0);
}

// Return the sum of a list of integers
fun sum( xs : list<int> ) : int
{
  xs.foldl( 0, fun(x,y) { x + y } )
}

// Parse an integer using `parseInt`. If an illegal digit character is encountered the 
// `default` value is returned. An empty string will also result in `default`.
fun parse-int-default( s : string, default : int = 0, hex : bool = False ) : int {
  if (s.empty?) then default else {
    match( s.parse-int(hex) ) {
      Nothing -> default
      Just(i) -> i
    }
  }
}

// Parse an integer after trimming whitespace. 
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string will result in `Just(0)`.
// A string can start with a `-` sign for negative numbers, 
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
fun parse-int( s : string, hex : bool = False) : maybe<int> {
  val base = if (hex) then 16 else 10
  match(s.trim.list) {
    Nil -> Just(0)
    Cons(c,cc) | c == '-' -> {
      match(parse-pos-int(cc,base)) {
        Nothing -> Nothing
        Just(i) -> Just(0 - i)
      }
    }
    cs -> parse-pos-int(cs,base)     
  }
}

private fun parse-pos-int( s : list<char>, base : int = 10 ) : maybe<int> {
  match(s) {
    Cons(c1,Cons(c2,cs)) | c1 == '0' && (c2 == 'x' || c2== 'X' )
      -> parse-digits(cs,16)
    _ -> parse-digits(s,base)
  }
}

// Parse digits in a `base` between 2 and 36 (default 10) given an initial value `acc` (default 0)
// Returns `acc` on the empty string, and `Nothing` if an invalid digit is encountered.
private fun parse-digits( cs : list<char>, base : int = 10, acc : int = 0 ) : maybe<int> {  
  match(cs) {
    Nil -> Just(acc)
    Cons(c,cc) -> {
      val d = if (c >= '0' && c <= (if (base >= 10) then '9' else ('0' + (base - 1).char))) then {
                (c - '0').int
              }
              elif (base > 10 && base <= 36 && c >= 'a' && c <= ('a' + (base - 11).char)) then { 
                (c - 'a').int + 10
              }
              elif (base > 10 && base <= 36 && c >= 'A' && c <= ('A' + (base - 11).char)) then { 
                (c - 'A').int + 10
              }
              else return Nothing   
      parse-digits(cc, base, base*acc + d)
    }
  }
}

public fun parse-fixed( s : string ) : maybe<double> {
  match(s.split(".",2)) {
    Cons(pre,Cons(post,Nil)) -> {
      match(pre.parse-int) {
        Nothing -> Nothing
        Just(d) -> match(post.parse-int) {
          Nothing -> Nothing
          Just(f) -> {
            if (f < 0) then Nothing
            elif (f == 0) then Just(d.double)
            else Just( d.double + (10.0^(post.length.double) / f.double) )
          }
        }
      }
    }
    _ -> match(s.parse-int) {
      Nothing -> Nothing
      Just(i) -> Just(i.double)
    }
  }
}

public fun parse-fixed-default( s : string, default : double = 0.0 ) : double {
  s.parse-fixed.maybe(default)
}


// ----------------------------------------------------------------------------
// Doubles
// ----------------------------------------------------------------------------

extern inline (==) : (double,double) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
extern inline (!=) : (double,double) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
extern inline (<=) : (double,double) -> bool  { inline "(#1 <= #2)" }
extern inline (>=) : (double,double) -> bool  { inline "(#1 >= #2)" }
extern inline (<)  : (double,double) -> bool  { inline "(#1 < #2)" }
extern inline (>)  : (double,double) -> bool  { inline "(#1 > #2)" }

extern inline (+) : (double,double) -> double { inline "(#1 + #2)" }
extern inline (-) : (double,double) -> double { inline "(#1 - #2)" }
extern inline (*) : (double,double) -> double { inline "(#1 * #2)" }
extern inline (/) : (double,double) -> double { inline "(#1 / #2)" }
extern inline (%) : (double,double) -> double { inline "(#1 % #2)" }

fun compare( x : double, y : double) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// Convert a `:double` to `:int` using `round` to round the double to its nearest integer.
// if the double is larger than `maxInt` or smaller than `minInt` just the lower 32 bits are returned.
extern inline unsafe-int : (double) -> int { cs inline "(int)(#1)"; js inline "(#1 | 0)" }

private extern inline getNan : () -> double { 
  cs inline "double.NaN";
  js inline "NaN"
}

private extern inline getNegInf : () -> double { 
  cs inline "double.NegativeInfinity";
  js inline "(- Infinity)"
}

private extern inline getPosInf : () -> double { 
  cs inline "double.PositiveInfinity";
  js inline "Infinity"
}

private extern inline getMaxDouble : () -> double { 
  cs inline "double.MaxValue";
  js inline "(1.7976931348623157e+308)"
}

private extern inline getMinDouble : () -> double { 
  cs inline "double.MinValue";
  js inline "-(1.7976931348623157e+308)"
}

private extern inline getEpsilon : () -> double { 
  cs inline "double.Epsilon";
  js inline "(4.94065645841247e-324)"
}

// convert a `:double` to an `:int` using `round` to round to its nearest integer.
// If the double is larger than `maxInt` or smaller than `minInt` this returns `default` (=0)
public fun int(d : double, default : int = 0 ) : int {
  if (d > max-int.double || d < min-int.double)
   then default
   else d.unsafe-int
}

// Represents a value that is _not a number_ (NaN)
val nan : double = getNan()

// Positive infinity
val pos-inf : double = getPosInf()

// Negative infinity
val neg-inf : double = getNegInf()

// The maximal double value
val max-double : double = getMaxDouble()

// The minimal double value
val min-double : double = getMinDouble()

// The number of bits in a `:double` (64)
val bits-double = 64

// Is this value equal to NaN ?
extern inline nan? : (double) -> bool {
  cs "double.IsNaN"
  js "isNaN"
} 

// Is this value equal to negative or positive infinity ?
extern inf? : (double) -> bool {
  cs "double.IsInfinity"
  js inline "((#1) === Number.POSITIVE_INFINITY || (#1) === Number.NEGATIVE_INFINITY)"
}

// Is this value equal to positive infinity ?
extern inline posinf? : (double) -> bool {
  cs "double.IsPositiveInfinity"
  js inline "((#1) === Number.POSITIVE_INFINITY)"
}


// Is this value equal to negative infinity ?
extern inline neginf? : (double) -> bool {
  cs "double.IsNegativeInfinity"
  js inline "((#1) === Number.NEGATIVE_INFINITY)"
}


// Returns the value `d`  raised to the power `p` .
extern inline (^) : (d:double,p:double) -> double {
  cs "Math.Pow"
  js inline "((#1) ^ (#2))"
}

// Return the square root of a value `d` 
// Returns `nan` if `d == nan`  or if `d`  is negative.
// Returns `inf` if `d == inf` .
extern inline sqrt : (d:double) -> double {
  cs "Math.Sqrt"
  js "Math.sqrt"
}

// Return the absolute value of a `:double` `d` 
extern inline abs : (d:double) -> double  { 
  cs "Math.Abs"
  js "Math.abs" 
}

// Return the largest integer equal or less than `d` 
extern inline floor : (d:double) -> double {
  cs "Math.Floor"
  js "Math.floor"
}

// Return the smallest integer equal or larger than `d` 
extern inline ceiling : (d:double) -> double {
  cs "Math.Ceiling"
  js "Math.ceiling"
}

// Return the integral part of a `:double` `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
extern inline trunc : (d:double) -> double {
  cs "Math.Truncate"
  js "Math.trunc"
}

// Round a double to its nearest integral value.
// If the value is halfway between two integers, the value is rounded to the even one.
extern inline round : (d:double) -> double {
  cs "Math.Round"
  js "Math.round"
}

// Return the sum of a list of doubles
fun sum( xs : list<double> ) : double
{
  xs.foldl( 0.0, (+))
}

// Returns the smallest of two doubles
fun min( x : double, y : double ) : double  -> if (x <= y) then x else y

// Returns the largest of two doubles
fun max( x : double, y : double ) : double  -> if (x >= y) then x else y

// Returns the smallest element of a list of doubles (or `0` for the empty list)
fun minimum( xs : list<double> ) : double
{
  match(xs) {
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, min )
  }
}

// Returns the largest element of a list of doubles (or `0` for the empty list)
fun maximum( xs : list<double> ) : double
{
  match(xs) {
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, max )
  }
}


// ----------------------------------------------------------------------------
// Strings
// ----------------------------------------------------------------------------


// A `:sslice` represents a sub-slice of string and
// has a specific start position and character count. It is used
// instead of string indices to make the actual internal representation
// of strings abstract (like UTF-8 or UTF-16). String slices are 
// returned by functions that find sub strings or patterns in
// in strings. Use `string:(slice : sslice) -> string` to 
// create a fresh substring from a slice. 
abstract struct sslice( str : string, start : int, len : int )

// Internal export for the regex module
fun ".new-sslice"( str :string, start: int, len : int ) {
  Sslice(str,start,len)
}

// Convert a character to a string
external string : ( c : char ) -> string  { 
  cs inline "Primitive.CharToString(#1)";
  js inline "_char_to_string(#1)"
}

// Convert a vector of characters to a string.
extern string : (vector<char>) -> string { 
  cs inline "Primitive.CharsToString(#1)";
  js inline "_chars_to_string(#1)"
}

// Convert a string to a vector of characters.
extern vector : ( s : string ) -> vector<char> { 
  cs inline "Primitive.StringToChars(#1)";
  js inline "_string_to_chars(#1)"
}


// Convert a string to a list of characters
extern list( s : string ) : total list<char> {
  cs inline "Primitive.StringToList(#1)"
  js inline "_string_to_list(#1)"
}

// Convert a list of characters to a string
extern string( cs : list<char> ) : total string {
  cs inline "Primitive.ListToString(#1)"
  js inline "_list_to_string(#1)"
}


// Are two strings equal? 
// Uses exact equality between character codes.
extern inline (==) : (string,string) -> bool {
  inline "(#1 == #2)"
  js inline "(#1 === #2)"
}

// Are two strings not equal?
extern inline (!=) : (string,string) -> bool {
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"
}

private extern string-compare : ( x : string, y : string ) -> int
{
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"
}

// Compare two strings.
// Uses the character codes directly for comparison
fun compare( x : string, y : string) : order
{
  string-compare(x,y).order;
}

fun (>=)( x : string, y : string ) : bool  -> compare(x,y) > Lt
fun (<=)( x : string, y : string ) : bool  -> compare(x,y) < Gt
fun (>)( x : string, y : string ) : bool   -> compare(x,y) == Gt
fun (<)( x : string, y : string ) : bool   -> compare(x,y) == Lt

// Append two strings.
extern inline (+) : (string,string) -> string { inline "(#1 + #2)" }

// Choose a non-empty string
fun (||)( x : string, y : string ) : string {
  if (x.empty?) then y else x 
}

// Length returns the length in the platform specific encoding (and should not be exported)
private extern inline length( s : string ) : int {
  cs inline "#1.Length"
  js inline "#1.length"
}

// O(n). Return the number of characters in a string.
extern count( s : string ) : int {
  cs inline "Primitive.StringCount(#1)"
  js inline "_string_count(#1)"
}

// O(n). Return the number of characters in a string slice
extern count( slice : sslice ) : int {
  cs inline "Primitive.SliceCount(#1)"
  js inline "_sslice_count(#1)"
}

// O(1). The first position in a string
extern first(s : string) : sslice { 
  cs inline "Primitive.SliceFirst(#1)"
  js inline "_sslice_first(#1)"
}

// O(1). The last position in a string
extern last(s : string) : sslice { 
  cs inline "Primitive.SliceLast(#1)"
  js inline "_sslice_last(#1)"
}

// O(1). The entire string as a slice
fun slice( s : string ) : sslice {
  Sslice(s,0,s.length)
}

// An empty slice
val empty = Sslice("",0,0)

// Is a slice empty?
fun empty?( slice : sslice ) : bool  -> (slice.len <= 0)

// Is a slice not empty?
fun notempty?( slice : sslice ) : bool  -> (slice.len > 0)

// Return the first character of a string (or the empty string)
fun head( s : string ) : string {
  s.first.string
}

// Return the tail of a string (or the empty string)
fun tail( s : string ) : string {
  s.first.after.string
}


// O(`count`). Advance the start position of a string slice by `count` characters
// up to the end of the string.
// A negative `count` advances the start position backwards upto the first position
// in a string.
// Maintains the character count of the original slice upto the end of the string.
// For example:
//
// * `"abc".first.advance(1).string == "b"`,
// * `"abc".first.advance(3).string == ""`,
// * `"abc".last.advance(~1).string == "bc"`.
// 
public extern advance( slice : sslice, count : int ) : sslice {
  cs inline "Primitive.SliceAdvance(#1,#2)"
  js inline "_sslice_advance(#1,#2)"
}

// O(`count`). Extend a string slice by `count` characters up to the end of the string.
// A negative `count` shrinks the slice up to the empty slice.
// For example:
//
// * `"abc".first.extend(1).string == "ab"`
// * `"abc".last.extend(~1).string == ""`
// 
extern extend( slice : sslice, count : int ) : sslice {
  cs inline "Primitive.SliceExtend(#1,#2)"
  js inline "_sslice_extend(#1,#2)"
}


// O(1). Return the string slice from the start of a string up to the
// start of `slice` argument.
fun before(slice : sslice) : sslice {
  val Sslice(s,start,_len) = slice
  Sslice(s,0,start)
}

// O(1). Return the string slice from the end of `slice` argument
// to the end of the string.
fun after(slice : sslice) : sslice {
  val Sslice(s,start,len) = slice
  Sslice(s,start+len,s.length - (start+len))
}

// O(n). Copy the `slice` argument into a fresh string.
extern inline string( slice : sslice ) : string {
  cs inline "Primitive.SliceToString(#1)"
  js inline "(function(_s){ return _s.str.substr(_s.start,_s.len) })(#1)"
}

extern next( slice : sslice ) : maybe<(char,sslice)> {
  js "_sslice_next"
}


private fun foreach-while-indexed-acc( slice : sslice, action : (c : char, i : int) -> e maybe<a>, i : int ) : e maybe<a> {
  match(slice.next) {
    Nothing -> Nothing
    Just((c,rest)) -> {
      match(action(c,i)) {
        Nothing -> foreach-while-indexed-acc(unsafe-decreasing(rest),action,i+1)
        res     -> res
      }
    }
  }
}

fun foreach-while-indexed( slice : sslice, action : (c : char, i : int) -> e maybe<a> ) : e maybe<a> {
  foreach-while-indexed-acc( slice, action, 0 ) 
}

fun foreach-while( slice : sslice, action : (c:char) -> e maybe<a>) : e maybe<a> {
  foreach-while-indexed(slice, fun(c,i) { action(c) })
}


fun foreach-indexed( slice : sslice, action : (c:char, i : int) -> e ()) : e () {
  foreach-while-indexed(slice, fun(c,i) { action(c,i); Nothing })
  ()
}

fun foreach( slice : sslice, action : (c:char) -> e ()) : e () {
  foreach-while-indexed(slice, fun(c,i) { action(c); Nothing })
  ()
}

// Does string `s`  contain the string `sub`  ?
private extern inline xindex-of(s : string, sub : string ) : int { 
  cs inline "((#1).IndexOf(#2))";
  js inline "((#1).indexOf(#2))";
}

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with 
// the position just following the substring `sub`. 
fun find( s : string, sub : string ) : maybe<sslice> {
  val i = s.xindex-of(sub)
  if (i>=0) then Just(Sslice(s,i,sub.length)) else Nothing
}

// Does string `s`  contain the string `sub`  ?
private extern inline xlast-index-of(s : string, sub : string ) : int { 
  cs inline "(#1.LastIndexOf(#2))";
  js inline "((#1).lastIndexOf(#2))";
}

// Return the last index of substring `sub` in `s` if it occurs.
fun find-last( s : string, sub : string ) : maybe<sslice> {
  val i = s.xlast-index-of(sub)
  if (i>=0) then Just(Sslice(s,i,sub.length)) else Nothing
}

private extern inline xstarts-with: (s : string, pre : string ) -> bool { 
  cs inline "#1.StartsWith(#2)";
  js inline "(#1.substr(0,#2.length) === #2)"
}


// Is `pre`  a prefix of `s`? If so, returns a slice
// of `s` following `pre` up to the end of `s`.
fun starts-with( s : string, pre : string ) : maybe<sslice> {
  if (xstarts-with(s,pre))
   then Just(Sslice(s,pre.length,s.length - pre.length))
   else Nothing
}


private extern xends-with: (s : string, post : string ) -> bool { 
  cs inline "#1.EndsWith(#2)";
  js inline "((#1).indexOf(#2, (#1).length - (#2).length) !== -1)"
}

// Does string `s`  end with `post`?
// If so, returns a slice of `s` from the start up to the `post` string at the end.
fun ends-with( s : string, post : string ) : maybe<sslice> {
  if (xends-with(s,post))
   then Just(Sslice(s,0,s.length - post.length))
   else Nothing
}

// Does string `s`  contain the string `sub`  ?
extern inline contains: (s : string, sub : string ) -> bool { 
  cs inline "((#1).Contains(#2))";
  js inline "((#1).indexOf(#2) >= 0)"
}

// Concatenate a vector of strings
extern inline join: (v : vector<string> ) -> string { 
  cs "String.Concat";
  js inline "((#1).join(''))"
}

// Concatenate a vector of strings with a separator `sep`
extern inline join: (v : vector<string>, sep : string ) -> total string { 
  cs "Primitive.Concat";
  js inline "((#1).join(#2))"
}

// Truncate a string to `count` characters.
fun truncate( s : string, count : int ) : string  {
  s.first.extend(count - 1).string
}

// Trim off a substring `sub` if `s` starts with that string.
fun trim-left( s : string, sub : string ) : string {
  match(s.starts-with(sub)) {
    Just(slice) -> slice.string
    Nothing -> s
  }
}

// Trim off a substring `sub` if `s` ends with that string.
fun trim-right( s : string, sub : string ) : string {
  match(s.ends-with(sub)) {
    Just(slice) -> slice.string
    Nothing -> s
  }
}

// Repeat a string `n` times 
extern repeat( s : string, n : int ) : string {
  cs inline "Primitive.Repeat(#1,#2)"
  js inline "(#2 <= 0 ? '' : new Array(#2 +1).join(#1))"
}

// Convert a `:maybe` string to a string using the empty sting for `Nothing`
fun string( ms : maybe<string> ) : string {
  match(ms) {
    Nothing -> ""
    Just(s) -> s
  }
}

// Is a string empty?
fun empty?( s : string ) : bool {
  s == ""
}

// Is a string not empty?
fun notempty?( s : string ) : bool {
  s != ""
}


private extern inline splitv : ( s : string,  sep : string ) -> vector<string> { 
  cs inline "(#1.Split(new String[]{#2}, StringSplitOptions.None))";
  js inline "((#1).split(#2))"
}

private extern inline splitv : ( s : string,  sep : string, n : int ) -> vector<string> { 
  cs inline "#1.Split(new String[]{#2},#3, StringSplitOptions.None)";
  js inline "(#1).split(#2, #3)"
}

// Split a string into parts that were delimited by `sep`. The delimeters are not included in the results.
// For example: `split("1,,2",",") == ["1","","2]`
fun split( s : string, sep : string ) : list<string>
{
  splitv(s,sep).list
}

// Split a string into at most `n` parts that were delimited by a string `sep`. The delimeters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
fun split( s : string, sep: string, n : int) : list<string>
{
  splitv(s,sep,n).list
}

// Replace every occurrence of `pattern` to `repl` in a string.
public extern inline replace-all( s : string, pattern : string, repl : string ) : string { 
  cs inline "(#1).Replace(#2,#3)";
  js inline @"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)";  
}

// Count occurences of `pattern` in a string.
public extern inline count( s : string, pattern : string ) : int { 
  cs inline "Primitive.Count(#1,#2)";
  js inline @"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)";  
}


// Apply a function `f` to each character in a string
fun map( s : string, f : char -> e char ) : e string
{
  s.list.map(f).string // todo: optimize
}

fun foreach-while-indexed( s : string, action : (c:char,i:int) -> e maybe<a> ) : e maybe<a> {
  s.slice.foreach-while-indexed(action)
}

fun foreach-while( s : string, action : (c:char) -> e maybe<a> ) : e maybe<a> {
  s.slice.foreach-while(action)
}

fun foreach-indexed( s : string, action : (c:char,i:int) -> e () ) : e () {
  s.slice.foreach-indexed(action)
}

// Invoke a function for each character in a string
fun foreach( s : string, action : (c:char) -> e () ) : e () {
  s.slice.foreach(action)
}



// Invoke a function for each character in a string
//fun foreach( s : string, f : char -> e () ) : e () {
//  s.list.foreach(f)  // todo: optimize
//}

// Count the number of times a predicate is true for each character in a string
fun count( s : string, pred : (char) -> bool ) : int
{
  var cnt := 0
  s.foreach fun(c) {
    if (pred(c)) then cnt := cnt+1
  }
  cnt
}

fun lines( s : string ) : list<string>
{
  s.split("\n")
}

fun unlines( xs : list<string> ) : string
{
  xs.join("\n") 
}

extern to-upper : (s : string) -> string { 
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()" 
}

extern to-lower : (s : string) -> string { 
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()" 
}

// If the first character of a string is ASCII `lower?` convert it to uppercase.
fun capitalize( s : string ) : string {
  s.first.string.to-upper + s.first.after.string
}

// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
fun pad-left( s : string, width : int, fill : string = " ") : string
{
  val n = s.length
  if (n >= width)
   then s
   else fill.repeat(width - n / fill.length) + s
}

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
fun pad-right( s : string, width : int, fill : string = " ") : string
{
  val n = s.length
  if (n >= width)
   then s
   else s + fill.repeat(width - n / fill.length)
}

// Trim whitespace on the left and right side of a string
fun trim( s : string ) : string {
  s.trim-left.trim-right
}

// Trim the starting white space of a string 
extern inline trim-left( s : string ) : string {
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"
}

// Trim the ending white space of a string.
extern inline trim-right( s : string ) : string {
  cs inline "(#1).TrimEnd()"
  js inline "((#1).replace(/\\s+$/,''))"
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

private extern inline unsafe-assign : forall<a> ( v : vector<a>, i : int, x : a ) -> total () {
  cs inline "(#1)[#2] = #3";
  js inline "(#1)[#2] = #3";
}

private extern inline unsafe-vector : forall<a> ( n : int ) -> total vector<a> {
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"
}

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
extern inline [] : forall<a> ( v : vector<a>, index : int ) -> exn a  { 
  inline "(#1)[#2]" 
}

// Return the length of a vector.
extern inline length : forall<a> (v : vector<a> ) -> int  { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length";
}

// Create a new vector of length `n`  with initial elements `default` .
public fun vector(n : int, default : a) : vector<a> {
  vector-init(n, fun(_i){ default } )
} 

// Create a new vector of length `n`  with initial elements given by function `f` .
public extern vector-init(n : int, f : int -> a) : vector<a> {
  cs inline "Primitive.NewArray<##1>(#1,#2)"
  js inline "_vector(#1,#2)"
} 

// Create an empty vector.
extern inline vector : forall<a> () -> vector<a> {
  cs inline "new ##1[0]"
  js inline "[]"
}

// Invoke a function `f` for each element in a vector `v`
fun foreach( v : vector<a>, f : (a) -> e () ) : e () {
  v.foreach-indexed( fun(x,i) { f(x) })
}

// Invoke a function `f` for each element in a vector `v`
fun foreach-indexed( v : vector<a>, f : (a,int) -> e () ) : e () {
  for( 0, v.length - 1 ) fun(i) {
    f(v.unsafe-idx(i),i)
  }
}

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b> {
  for-while( 0, v.length - 1 ) fun(i) {
    f(v.unsafe-idx(i))
  }
}

// Apply a total function `f` to each element in a vector `v`
fun map( v : vector<a>, f : a -> e b ) : e vector<b> {
  val w = unsafe-vector(v.length)
  v.foreach-indexed fun(x,i) {
    unsafe-assign(w,i,f(x))
  }
  w
}

// Convert a vector to a list.
fun list( v : vector<a> ) : list<a> {
  v.vlist
}

// Convert a vector to a list with an optional tail.
extern vlist( v : vector<a>, tail : list<a> = [] ) : list<a> {
  cs inline "Primitive.VList<##1>(#1,#2)"
  js inline "_vlist(#1,#2)"
}

// Convert a list to a vector.
extern vector( xs : list<a> ) : vector<a> {
  cs inline "Primitive.UnVList<##1>(#1)"
  js inline "_unvlist(#1)"
}


// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic show: shows the internal representation of an object as a string  
// Note: this breaks parametricity so it should not be public
private extern gshow : forall<a> a -> string { 
  cs inline "#1.ToString()";
  js inline "(#1).toString()"
}

// Generic formatting
private extern inline gformat : forall<a> (value:a,format:string) -> string { 
  cs inline "#1.ToString(#2)";
  js inline "_gformat(#1, #2)";
}

// Convert an `:int` to a string
fun show( i : int ) : string 
{ 
  i.gshow()  
}

// Show an `:int` as a hexadecimal value.
// The `width`  parameter specifies how wide the hex value is where `0`  is used to align
// Uses capital letters for hexadecimal digits by default.
fun show-hex( i : int, width : int = 1, use-capitals : bool = True )
{
  val w = if (width < 0) then 0 else width
  val x = if (use-capitals) then "X" else "x"
  gformat(i, x + w.show)
}

// Show a `:double` as a string with a default `precision`  of `0` .
// Pass a precision of `0`  (or less) to display numbers precisely.
// Uses either `showFixed` or `showExp` depending on what is the most compact representation
fun show( d : double, precision : int = 6 ) : string
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"g" + show(p))
}

fun show-fixed( d : double, precision : int = 6 )
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"F" + show(p))
}

// Show a `:double` in exponential (scientific) notation 
fun show-exp( d : double, precision : int = 6 )
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"e" + show(p))
}

// Show a character as a string
private fun show-char( c : char ) : string
{
  if (c < ' ' || c > '~') {
    if (c == '\n') then "\\n"
    elif (c == '\r') then "\\r"
    elif (c == '\t') then "\\t"
    elif (c.int <= 0xFF) then "\\x" + c.int.show-hex(2)
    elif (c.int <= 0xFFFF) then "\\u" + c.int.show-hex(4)
    else "\\U" + c.int.show-hex(6)
  }
  else {
    if (c == '\'') then "\\'"
    elif (c == '\"') then "\\\""
    elif (c == '\\') then "\\\\"
    else c.string
  }
}

// Show a `:char` as a character literal
function show( c : char ) : string
{
  "'" + c.show-char + "'"
}


// Show a string as a string literal
fun show( s : string ) : string
{
  "\"" + s.list.map(show-char).join + "\""
}

// Convert a `:bool` to a string
fun show( b : bool ) : string  -> if (b) then "True" else "False"

// Convert a unit value `()` to a string
fun show( u : () ) : string  -> "()"

// Show an `:sslice` as a string literal
fun show( s : sslice ) : string  -> s.string.show

// Convert a list to a string
fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
{
  "[" + xs.map(show-elem).join(",") + "]"
}


fun show( xs : list<string> ) : string {
  show-list(xs,show)
}

fun show( xs : list<int> ) : string {
  show-list(xs,show)
}

fun show( xs : list<bool> ) : string {
  show-list(xs,show)
}

fun show-tuple( x : (a,b), showfst : a -> string, showsnd : b -> string ) : string {
  "(" + x.fst.showfst + "," + x.snd.showsnd + ")"
}

// ----------------------------------------------------------------------------
// Print to the console
// ----------------------------------------------------------------------------

// Generic print routine: prints the internal representation as a string to the console, including a final newline character.  
// Note: this breaks parametricity so it should not be public
private fun gprintln( x : a ) : console () {
  println( x.gshow )
}

// Generic print routine: prints the internal representation as a string to the console, 
// including a final newline character.  
// Note: this breaks parametricity so it should not be public
private fun gprint( x : a ) : console () {
  print( x.gshow )
}


private val redirect : ref<global,maybe<(string) -> console ()>> = unsafe-total { ref(Nothing) }

// redirect `print` and `println` calls to a specified function.
public fun print-redirect( print : (msg : string) -> console () ) : io ()
{
  redirect := Just(print);
}

// Print a string to the console, including a final newline character.
private extern xprintsln : string -> console ()  { 
  cs "Console.WriteLine";
  js "_println"
}

// Print a string to the console
private extern xprints : string -> console ()  { 
  cs "Console.Write";
  js "_print"
}

private fun prints( s : string ) : console () {
  unsafe-total { 
    match(!redirect) {
      Nothing -> xprints(s)
      Just(f) -> f(s)
    }
  }
}

private fun printsln( s : string ) : console () {
  unsafe-total { 
    match(!redirect) {
      Nothing -> xprintsln(s)
      Just(f) -> f(s + "\n")
    }
  }
}

// Print a string to the console.
fun print(s : string)  { prints(s) }

// Print an integer to the console.
fun print(i : int)     { prints(show(i)) }

// Print a double to the console.
fun print(d : double)  { prints(show(d)) }

// Print a boolean to the console
fun print( b : bool )  { prints(show(b)) }

// Print a character to the console.
fun print(c : char)    { prints(c.string) }

// Print a unit value to the console
fun print( u : () )  { prints(show(())) }


// Print a string to the console, including a final newline character.
fun println(s : string)  { printsln(s) }

// Print an integer to the console, including a final newline character.
fun println(i : int)     { printsln(show(i)) }

// Print a double to the console, including a final newline character.
fun println(d : double)  { printsln(show(d)) }

// Print a boolean to the console, including a final newline character
fun println( b : bool )  { printsln(show(b)) }

// Print a character to the console, including a final newline character.
function println(c : char)    { printsln(c.string) }

// Print a unit value to the console, including a final newline character
fun println( u : () )  { printsln(show(())) }


private extern xtrace : ( message : string ) -> ()  {
  cs "Primitive.Trace"
  js "_trace"
}

private val trace-enabled : ref<global,bool> = unsafe-total { ref(True) }

// Trace a message used for debug purposes. 
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default. 
// Disabled if `notrace` is called.
public fun trace( message : string ) : () {
  unsafe-total {
    if (!trace-enabled) then xtrace(message) 
  }    
}

// Disable tracing completely.
public fun notrace() : st<global> () {
  trace-enabled := False
}

// Used by the compiler to wrap main console applications
extern main-console : forall<a,e> ( main : () -> e a ) -> e a {
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"
}


// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

// The predicate `:hdiv<h,a,e>` signifies that if the type `:a` contains a reference to `:h`,
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// when writing to the heap (see `(!)`) and is necessary to catch cases where code can diverge
// by storing selfReferential funs in the heap.
type hdiv :: (H,V,E) -> P

// A reference `:ref<h,a>` points to a value of type `:a` in heap `:h`.
type ref :: (H,V) -> V

// Allocate a fresh reference with an initial value.
extern inline ref : forall<h,a> (value:a) -> alloc<h> ref<h,a> { 
  cs inline "new Ref<##1,##2>(#1)";
  js inline "{ value: #1 }"
}

// Assign a new value to a reference.
extern inline set  : forall<h,a> (ref: ref<h,a>, assigned: a) -> <write<h>> () { 
  cs inline "#1.Set(#2)";
  js inline "((#1).value = #2)"
}

// Read the value of reference.
extern inline (!) : forall<h,a,e> (ref: ref<h,a>) -> <read<h>|e> a with(hdiv<h,a,e>)  { 
  cs inline "#1.Value";
  js inline "((#1).value)";
}

// If a heap effect is unobservable, the heap effect can be erased by using the `run` fun.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
extern inline run : forall<e,a> ( action : forall<h> () -> <st<h> | e> a ) -> e a  { 
  cs inline "Primitive.Run<##2>(#1)";
  js inline "((#1)())";
}


// ----------------------------------------------------------------------------
// Unsafe
// ----------------------------------------------------------------------------

private extern inline unsafe-total-cast : forall<e,a> ( action : () -> e a ) -> (() -> total a) { inline "#1" }

// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
fun unsafe-total( action : () -> e a ) : total a
{
  unsafe-total-cast( action )()
}


// _Unsafe_. This function removes the exception effect (`:exn`) from the effect of an action
fun unsafe-noexn( action : () -> <exn|e> a ) : e a
{
  unsafe-total( action )
}

// _Unsafe_. This function removes the non-termination effect (`:div`) from the effect of an action
fun unsafe-nodiv( action : () -> <div|e> a ) : e a
{
  unsafe-total( action )
}

// _Unsafe_. Mark a function parameter as decreasing to suppress the non-termination effect (`:div`).
extern inline unsafe-decreasing( x : a ) : a { inline "#1" }

// ----------------------------------------------------------------------------
// Non determinism
// ----------------------------------------------------------------------------

private val unique-count : ref<global,int> = unsafe-total { ref(0) }

// Returns a unique integer (modulo 32-bits).
fun unique() : ndet int
{
  unsafe-total {
    val u = !unique-count
    unique-count := u+1
    u
  }
}

// Returns one of its arguments `x`  or `y`  based on a non-deterministic choice.
fun choose( x: a, y: a) : ndet a {
  if (random-bool()) then x else y
}

// Return a random number equal or larger than 0.0 and smaller than 1.0
extern random : () -> ndet double   { 
  cs "Primitive.RandomDouble"
  js "Math.random"
}

// Return a positive random integer (including 0)
extern random-int: () -> ndet int  { 
  cs "Primitive.RandomInt"
  js inline "Math.floor(Math.random()*2147483647)"
}

// Return a random boolean
fun random-bool() : ndet bool {
  odd(random-int())
}


// ----------------------------------------------------------------------------
// Control
// ----------------------------------------------------------------------------

// The `while` fun executes `action`  as long as `pred`  is `true`.
fun while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
{
  if (predicate()) {
    action()
    while(predicate, action)
  }
}

// The `repeat` fun executes `action`  `n`  times.
fun repeat( n : int, action : () -> e () ) : e ()
{
  for(1,n) fun(i) { action() }
}

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
fun for( start: int, end : int, action : (int) -> e () ) : e ()
{
  fun rep( i : int ) {
    if (i <= end) {
      inject-div(action)(i)
      rep(i+1)
    }
  }
  unsafe-nodiv { rep(start) }
}

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
fun for-while( start: int, end : int, action : (int) -> e maybe<a> ) : e maybe<a>
{
  fun rep( i : int ) {
    if (i <= end) {
      match(inject-div(action)(i)) {
        Nothing -> rep(i+1)
        Just(x) -> Just(x)
      }
    }
    else Nothing
  }
  unsafe-nodiv { rep(start) }
}

// Return the host environment: `dotnet`, `browser`, `webworker`, or `node`. 
extern host() : ndet string {
  cs inline "\"dotnet\""
  js inline "_host"
}




// ----------------------------------------------------------------------------
// Handled effects
// ----------------------------------------------------------------------------

// Handled effects are lifted to an atomic effect using the `:handled` type constructor
type handled    :: HX -> X;

// Linear effects are lifted to an atomic effect using the `:handled1` type constructor
type handled1   :: HX1 -> X;

// Effect operations with result type `:a`
type open operation<a> 

// Cps effect. This is used by the compiler internally to ensure certain funs
// are compiled with a cps translation
effect cps {
  cps() : ()
}

// Handle the `:cps` effect. 
val uncps = handler {
  return x -> x
  cps()    -> resume(())
}

// inject the `:cps` effect into a non-cps function
extern incps( action: () -> e a ) : <cps|e> a { 
  js inline "(action.length === 1 ? action(_k) : _k(action()))" 
}

// inject the `:cps` effect into a non-cps function with one argument
extern incps1( action: a -> e b, x : a ) : <cps|e> b { 
  js inline "(action.length === 2 ? action(x,_k) : _k(action(x)))" 
}

// inject the `:cps` effect into a non-cps function with two arguments
extern incps2( action: (a,b) -> e c, x : a, y : b ) : <cps|e> c { 
  js inline "(action.length === 3 ? action(x,y,_k) : _k(action(x,y)))" 
}


// Continuation alias. Used internally to do special checks on cps converted code.
alias cont<a,e,b> = a -> e b

// ----------------------------------------------------------------------------
// Handling exceptions when in cps / effect handling mode.
// These are called transparently from the primcatch/primfinally funs
// ----------------------------------------------------------------------------

private effect hexn {
  hthrow(exn : exception) : void  // cannot resume
}

private fun hcatch( action, ehandler ) {
  handle(action) {
    return x    -> x
    hthrow(exn) -> ehandler(exn)
  }
}

private fun hfinally( action, fhandler ) {
  handle(action) {
    return x    -> { 
      fhandler() 
      x
    }
    hthrow(exn) -> {
      fhandler()
      throw(exn)
    }
  }
}

// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// ----------------------------------------------------------------------------

// Asynchronous operations have the `:async` effect.
abstract effect async {
  async-yield() : ()
}

fun ".async-handle"(action) {
  handle(action) {
    return x -> x
    async-yield() -> () // todo: do a zero timeout?
  }
}

// Wait (asynchronously) for `ms` milliseconds. Use `wait(0)` to yield to other asynchronous operations.
extern wait( ms : int ) : async () {
  js inline "(function(){ setTimeout( _async_callback0(_k), #1 ); return async_yield(); })()"
}

fun async-all( actions : list<() -> <async|e> a> ) : <async|e> list<a> {
  async-all-prim( actions.vector ).list;
}

private extern async-all-prim( actions : vector<() -> <async|e> a> ) : <async|e> vector<a>  {
  js inline "$asyncAll(actions,_k)"
}

extern ".async-callback0"( action: () -> b ) : (() -> b) {
  js inline "$asyncCallback0(#1)"
}

extern ".async-callback1"( action: a -> b ) : (a -> b) {
  js inline "$asyncCallback1(#1)"
}

extern ".async-callback-err0"( action: () -> b ) : (() -> b) {
  js inline "$asyncCallbackErr0(#1)"
}

extern ".async-callback-err1"( action: a -> b ) : (a -> b) {
  js inline "$asyncCallbackErr1(#1)"
}

extern ".async-callback-err2"( action: (a1,a2) -> b ) : ((a1,a2) -> b) {
  js inline "$asyncCallbackErr2(#1)"
}

// ----------------------------------------------------------------------------
// Handler primitives. 
// These are hidden from normal usage but generated by the compiler since
// the continuations are not safely typed in plain Koka
// ----------------------------------------------------------------------------

// The Yield monad
type yld<a> {
  Yield<b>( op: operation<b>, cont : b -> yld<a> )
  Result( result: a ) 
}

extern inline ".yieldop"( op : operation<a> ) : a {
  js inline "$std_core.Yield(#1,_k)" 
}

extern ".yieldop1"( efftag: string, op : a ) : b {
  js inline "$yield_linearx(#1,#2)" 
}

/*
extern ".yieldop1"( op : operation<a> ) : a {
  js inline "$yield_linear(#1)" 
}
*/
  
extern inline ".toany"( x : a ) : any { 
  inline "#1" 
} 

extern inline ".applyK"(k : a -> e b, x : a) : e b {
  js inline "(#1 || $std_core.id)(#2)"
}

extern ".htop"() : any {
  js inline "$htop"
}

extern ".hstack"() : any {
  js inline "$hstack"
}

extern ".makeHandler0"( optag:string, // opmatch : operation<c> -> opmatch<e1,e,o>, 
                          ret : a -> <cps|e> b,  
                          ops : (l1:any, l2:int, cont: (c) -> yld<a>, op:o, 
                                 resume:(l1:any,l2:int, cont:(c) -> yld<a>,c) -> <cps|e> b) -> <cps|e> b  
                        ) : total ((action : () -> e1 a) -> e b) { 
  js inline "$makeGenOptHandler0(optag,false,ret,ops)"
}

extern ".makeHandlerRet0"( optag:string, ret : a -> <cps|e> b, ops : int ) : total ((action : () -> e a) -> e b) {
  js inline "$makeGenOptHandler0(optag,false,ret,ops)"
}

extern ".makeShallowHandler0"( optag:string, // opmatch : operation<c> -> opmatch<e1,e,o>, 
                          ret : a -> e b,  
                          ops : (l1:any, l2:int, cont: (c) -> yld<a>, op:o, 
                                resume:(l1:any,l2:int, cont:(c) -> yld<a>, c) -> e a) -> e b  
                        ) : total ((action : () -> e a) -> e b) { 
  js inline "$makeGenOptHandler0(optag,true,ret,ops)"
}


extern ".makeHandler1"( optag : string, // opmatch : operation<c> -> opmatch<e1,e,o>, 
                          ret : (d,a) -> <cps|e> b, 
                          ops : (l1:any, l2:int, cont:(c) -> yld<a>, op:o, 
                                  resume: ((l1:any, l2:int, cont:(c) -> yld<a>, loc: d, x: c) -> <cps|e> b), 
                                  loc:d) -> <cps|e> b  
                        ) :  total ((loc:d, action: () -> e1 a) -> e b) {
  js inline "$makeGenOptHandler1(optag,false,ret,ops)"
}


/*
// Abstract operation matching
abstract type opmatch<e1::E,e2::E,a> {
  OpMatch( : a )
  OpNoMatch
} 

fun ".conOpMatch"(x)  { OpMatch(x) }
fun ".conOpNoMatch"() { OpNoMatch  }

fun ".makeHandler0"( optag:string, matchop : operation<c> -> opmatch<e1,e,o>, 
                          res : a -> <cps|e> b, ops : (op:o, resume:(bool,c) -> <cps|e> b) -> <cps|e> b  ) : total ((action : () -> e1 a) -> e b) {
  fun hndler(yld) {
    match(yld) {
      Yield(op,f) -> {
        match(matchop(op)) {
          OpMatch(eop) -> ops(eop,fun(_,x) { hndler(f(x))}) 
          OpNoMatch    -> reyield(op,fun(x) { hndler(f(x))})
        }
      }
      Result(x) -> res(x)
    }
  }
  unsafeMakeHandler0(hndler)
}

fun ".makeHandler1"( matchop : operation<c> -> opmatch<e1,e,o>, res : (a,d) -> <cps|e> b, ops : (op:o, cont: ((c,d) -> <cps|e> b), loc:d) -> <cps|e> b  ) :  total ((action: () -> e1 a, loc:d) -> e b) {
  fun hndler(yld,loc) {
    match(yld) {
      Yield(op,f) -> {      
        match(matchop(op)) {
          OpMatch(eop) -> ops(eop, fun(x,loc0) { hndler(f(x),loc0) }, loc)
          OpNoMatch    -> reyield(op, fun(x) { hndler(f(x),loc) }) 
        }
      }
      Result(x) -> res(x,loc)          
    }
  }
  unsafeMakeHandler1(hndler)
}

private extern unsafeMakeHandler0( hndler : yld<a> -> <cps|e> b ) : total ((action : () -> e1 a) -> e b) {
  js inline "fun(_action,_k){ _k = _k || id; return hndler(_action(Result),_k); }"
}

private extern unsafeMakeHandler1( hndler : (yld<a>,d) -> <cps|e> b ) : total ((action : () -> e1 a, loc : d) -> e b) {
  js inline "fun(_action,_loc,_k){ _k = _k || id; return hndler(_action(Result),_loc,_k); }"
}

private extern reyield( op: operation<b>, f : b -> <cps|e> a ) : <cps|e> a {
  js inline "Yield(op,fun(_x) { return f(_x,_k); } )"
}
*/




