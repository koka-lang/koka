/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Core functions.

   This module is implicitly imported and all functions and types
   are always available.
   Some types and operations are required to be defined for the compiler
   to work correctly (i.e. types like `:exn` or `:list`)
*/
public module std/core

public import std/core/types
public import std/core/hnd

infixr 80  (^)
infixl 70  (*), (%), (/), cdiv, cmod
infixr 60  (++)
infixl 60  (+), (-)
infix  40  (!=), (==), (<=), (>=), (<), (>)

// prefix     (!), (-) 

extern import {
  c  file "core/core-inline"
  cs file "core/core-inline.cs"
  js file "core/core-inline.js"
}

extern import {
  js file "core/core-integer-inline.js"
}
extern import {
  js file "core/core-string-inline.js"
}
extern import {
  js file "core/core-console-inline.js"
}


// ----------------------------------------------------------------------------
// Builtin effects
// ----------------------------------------------------------------------------

// An alias for the empty effect.
alias total = <>

// The console effect signifies that a function may write to the console.
type console :: X

// An alias for pure effects: a pure function always returns the same result
// when called with the same arguments but may not terminate or raise an exception.
alias pure = <exn,div>

// The `:global-scope` is a special type constant to denote the global scope
type global-scope :: S

// The `:net` effect signifies a function may access the network
type net :: X

// The `:fsys` effect signifies a function may access the file system
type fsys :: X

// The `:ui` effect signifies a function may access the graphics system
type ui :: X

// The `:blocking` effect signifies that a function may block
type blocking :: X

// The `:io-total` effect is used for functions that perform arbitrary I/O operations, but are terminating without raising exceptions.
alias io-total = <ndet,console,net,fsys,ui,st<global>>

// The `:io-noexn` effect is used for functions that perform arbitrary I/O operations, but raise no exceptions
alias io-noexn = <div,io-total>

// The `:io` effect is used for functions that perform arbitrary I/O operations.
alias io = <exn,io-noexn>

// ----------------------------------------------------------------------------
// Masking
// ----------------------------------------------------------------------------

// Add the state effect to a function effect.
// inline extern inject-st : forall<a,b,h,e> ((a) -> e b) -> total ((a) -> <st<h>|e> b)   { inline "#1" }

// Add the state effect to a function effect.
inline extern inject-st : forall<a,h,e> (() -> e a) -> total (() -> <st<h>|e> a)  { inline "#1" }


// ----------------------------------------------------------------------------
// Standard Functions
// ----------------------------------------------------------------------------


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
  (fn(x){ f(g(x)) })
}

// The `ignore` function ignores its argument.
fun ignore( x : a ) : () {
  ()
}

// Return a 'constant' function that ignores its argument and always returns the same result
fun const( default : a ) : total (( x : b ) -> a) {
  (fn(_){ default })
}


// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------


fun int( b : bool ) : int  { if (b) then 1 else 0 }

fun mbint( m : maybe<int> ) : int {
  match(m) {
    Nothing -> 0
    Just(i) -> i
  }
}

// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
fun bool( i : int ) : bool { i!=0 }

// Convert a `:maybe` type to a boolean using `False` for `Nothing` and `True` for `Just`.
fun bool( m : maybe<a> ) : bool {
  match(m) {
    Nothing -> False
    _       -> True
  }
}

// Convert a string to a boolean, using `False` for the empty string and `True` otherwise.
fun bool( s : string ) : bool { s!="" }

// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
fun maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
{
  match(m) {
    Nothing -> onNothing
    Just(x) -> onJust(x)
  }
}

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
// This is an alias for `default`.
fun maybe( m : maybe<a>, nothing : a ) : a {
  default(m,nothing)
}

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
fun default( m : maybe<a>, nothing : a ) : a {
  match(m) {
    Nothing -> nothing
    Just(x) -> x
  }
}

// Get the value of the `Just` constructor or raise an exception
fun unjust( m : maybe<a> ) : exn a {
  match(m) {
    Just(x) -> x
    Nothing -> throw("unexpected Nothing in std/core/unjust")
  }
}

fun map( m : maybe<a>, f : a -> e b ) : e maybe<b> {
  match(m) {
    Nothing -> Nothing
    Just(x) -> Just(f(x))
  }
}

fun (||)( m1 : maybe<a>, m2: maybe<a> ) : maybe<a> {
  match(m1) {
    Nothing -> m2
    _       -> m1
  }
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

// Map over the `Right` component of an `:either` type.
fun map( e : either<a,b>, f : b -> e c  ) : e either<a,c> {
  match(e) {
    Right(x) -> Right(f(x))
    Left(x)  -> Left(x)
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

// Return the tail of list. Returns the empty list if `xs` is empty.
fun tail( xs : list<a> ) : list<a> {
  match(xs) {
    Cons(_,xx) -> xx
    _          -> []
  }
}

// Is the list empty?
fun is-empty( xs : list<a> ) : bool {
  match(xs) {
    Nil -> True
    _   -> False
  }
}

// A `:stream` is a co-inductive type representing an infinite list of elements.
co type stream<a> {
  con Next(head:a, tail: stream<a> )
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
  if (i < 0) then Lt
  elif (i > 0) then Gt
  else Eq
}

fun (==)( x : order, y : order ) : bool { x.int == y.int }
fun (!=)( x : order, y : order ) : bool { x.int != y.int }
fun (>=)( x : order, y : order ) : bool { x.int >= y.int }
fun (<=)( x : order, y : order ) : bool { x.int <= y.int }
fun (>)( x : order, y : order ) : bool  { x.int > y.int }
fun (<)( x : order, y : order ) : bool  { x.int < y.int }

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
fun length(xs) {
  fun len(ys,acc) {
    match(ys) {
      Cons(_,yy) -> yy.len(acc+1)
      Nil        -> acc
    }
  }
  xs.len(0)
}

// Returns an integer list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
fun list( lo: int, hi: int ) : total list<int> {
  if (lo <= hi) 
    then Cons( lo, list( unsafe-decreasing(lo.inc), hi ) )
    else Nil
}

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
fun list( lo: int, hi: int, stride : int ) : total list<int> {
  if (lo <= hi) 
    then Cons( lo, list(  unsafe-decreasing(lo + stride), hi, stride ))
    else Nil  
}


// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
fun list( lo: int, hi: int, f : int -> e a ) : e list<a> {
  if (lo <= hi) 
    then Cons( f(lo), list( unsafe-decreasing(lo.inc), hi, f ))
    else Nil  
}

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
fun list( lo: int, hi: int, stride : int, f : int -> e a ) : e list<a> {
  if (lo <= hi) 
    then Cons( f(lo), list(  unsafe-decreasing(lo + stride), hi, stride, f ))
    else Nil  
}

// Create a list of characters from `lo`  to `hi`  (inclusive).
fun list( lo : char, hi : char ) : total list<char> {
  list(lo.int, hi.int).map( char )
}

private val maxListStack = 50


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

// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
fun zip( xs : list<a>, ys : list<b> ) : list<(a,b)> {
  match(xs) {
    Cons(x,xx) -> match(ys) {
      Cons(y,yy) -> Cons((x,y),zip(xx,yy))
      Nil        -> Nil
    }
    Nil -> Nil
  }
}

// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
fun zipwith( xs : list<a>, ys :list<b>, f : (a,b) -> e c ) : e list<c> {
  match(xs) {
    Cons(x,xx) -> match(ys) {
      Cons(y,yy) -> Cons(f(x,y),zipwith(xx,yy,f))
      Nil -> Nil
    }
    _ -> Nil
  }
}

// Zip two lists together by apply a function `f` to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
fun zipwith-indexed( xs0 : list<a>, ys0 :list<b>, f : (int,a,b) -> e c ) : e list<c> {
  fun zipwith-iter( i, xs, ys ) {
    match(xs) {
      Cons(x,xx) -> match(ys) {
        Cons(y,yy) -> Cons(f(i,x,y),zipwith-iter(i+1,xx,yy))
        Nil -> Nil
      }
      Nil -> Nil
    }
  }
  zipwith-iter(0,xs0,ys0)
}

// Unzip a list of pairs into two lists
fun unzip( xs : list<(a,b)> ) : (list<a>,list<b>) {
  // todo: implement TRMC for multiple results
  fun iter( ys, acc1, acc2 ) {
    match(ys) {
      Cons((x,y),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2))
      Nil            -> (reverse(acc1),reverse(acc2))
    }
  }
  iter(xs,[],[])
}

// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
fun take( xs : list<a>, n : int ) : list<a> {
  match(xs) {
    Cons(x,xx) | n > 0 -> Cons(x, take(xx, n - 1))
    _ -> Nil
  }
}

// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)
fun drop( xs : list<a>, n : int ) : list<a> {
  match(xs) {
    Cons(_,xx) | n > 0 -> drop(xx, n - 1)
    _ -> xs
  }
}

// Apply a function `f`  to each element of the input list in sequence.
fun map(xs : list<a>, f : a -> e b) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x), xx.map(f))
    Nil -> Nil
  }
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
fun map-indexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b> {
  fun map-idx(ys,i) {
    match(ys) {
      Cons(y,yy) -> Cons(f(i,y), map-idx(yy,i+1))
      Nil -> Nil
    }
  }
  map-idx(xs,0)
}

// Apply a function `f`  to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
fun map-peek(xs : list<a>, f : (value : a, rest : list<a>) -> e b) : e list<b> {
  fun mappeek(ys) {
    match(ys) {
      Cons(y,yy) -> Cons(f(y,yy), yy.mappeek)
      Nil -> Nil
    }
  }
  mappeek(xs)
}



// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun map-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  fun mapidx(ys,i) {
    match(ys) {
      Cons(y,yy) -> Cons(f(i,y,yy), mapidx(yy,i+1))
      Nil -> Nil
    }
  }
  mapidx(xs,0)
}

// Reverse a list.
fun reverse(xs : list<a>) : list<a> {
  reverse-append( xs, Nil )
}

// Efficiently reverse a list `xs` and append it to `tl`:
// `reverse-append(xs,tl) == reserve(xs) ++ tl
fun reverse-append( xs : list<a>, tl : list<a> ) : list<a> {
  fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a> {
    match(ys) {
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc
    }
  }
  reverse-acc(tl,xs)
}

// Append two lists.
fun (++)(xs : list<a>, ys : list<a> ) : list<a> {
  append(xs,ys)
}

// Append two lists.
fun append(xs : list<a>, ys : list<a> ) : list<a> {
  match(xs) {
    Cons(x,xx) -> Cons(x,append(xx,ys))
    Nil -> ys
  }
}

// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)`
// Note, `foldr` is less efficient than `foldl` as it reverses the list first.
fun foldr(xs,z,f) {
  xs.reverse.foldl(z,fn(x,y){f(y,x)})
}

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
fun foldl(xs,z,f) {
  match(xs) {
    Cons(x,xx) -> foldl(xx,f(z,x),f)
    Nil        -> z
  }
}

fun foldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a {
  match(xs) {
    Cons(x,xx) -> xx.foldl(x,f)
    Nil        -> throw("unexpected Nil in std/core/foldl1")
  }
}

fun foldr1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a {
  xs.reverse.foldl1(f)
}

// Create a list of `n`  repeated elementes `x`
fun replicate( x : a, n : int ) : list<a> {
  if (n > 0)
   then Cons(x, replicate(x,unsafe-decreasing(n.dec)))
   else Nil
}

// split a list at position `n`
fun split( xs : list<a>, n : int ) : (list<a>, list<a>) {
  (xs.take(n), xs.drop(n))
}


fun span( xs : list<a>, predicate : a -> e bool ) : e (list<a>,list<a>)
{
  // todo: implement TRMC with multiple results to avoid the reverse
  fun span-acc( ys, acc) {
    match(ys) {
      Cons(y,yy) -> if (y.predicate) then yy.span-acc(Cons(y,acc)) else (acc.reverse,ys)
      _ -> (acc.reverse, ys)
    }
  }
  xs.span-acc( [] )
}

// Keep only those initial elements that satisfy `predicate`
fun take-while( xs : list<a>, predicate : a -> e bool ) : e list<a> {
  match(xs) {
    Cons(x,xx) -> if (x.predicate) then Cons(x, xx.take-while(predicate) ) else Nil
    Nil -> Nil
  }
}

// Drop all initial elements that satisfy `predicate`
fun drop-while( xs : list<a>, predicate : a -> e bool ) : e list<a> {
  match(xs) {
    Cons(x,xx) -> if (x.predicate) then xx.drop-while(predicate) else xs
    Nil -> Nil
  }
}

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
fun filter( xs : list<a>, pred : a -> e bool ) : e list<a> {
  match(xs) {
    Cons(x,xx) -> if (pred(x)) then Cons(x,xx.filter(pred)) else xx.filter(pred)
    Nil -> Nil
  }
}


// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd?) == [2]`
fun remove( xs : list<a>, pred : a -> bool ) : list<a> {
  xs.filter( fn(x) { !pred(x) } )
}

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd?) == ([1,3],[2])`
fun partition( xs : list<a>, pred : a -> e bool ) : e (list<a>,list<a>) {
  partition-acc( xs, pred, Nil, Nil)
}

private fun partition-acc( xs : list<a>, pred : a -> e bool, acc1 : list<a>, acc2 : list<a> ) : e (list<a>, list<a>) {
  match(xs) {
    Nil -> (acc1.reverse, acc2.reverse)
    Cons(x,xx) -> if (pred(x))
                    then partition-acc(xx,pred,Cons(x,acc1),acc2)
                    else partition-acc(xx,pred,acc1,Cons(x,acc2))
  }
}


// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`
fun filter-map( xs : list<a>, pred : a -> e maybe<b> ) : e list<b> {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> match(pred(x)) {
      Nothing -> xx.filter-map(pred)
      Just(y) -> Cons(y,xx.filter-map(pred))
    }
  }
}

// Find the first element satisfying some predicate
fun find( xs : list<a>, pred : a -> bool ) : maybe<a> {
  xs.foreach-while fn(x) {
    if (pred(x)) then Just(x) else Nothing
  }
}

// Find the first element satisfying some predicate and return it.
fun find-maybe( xs : list<a>, pred : a -> maybe<b> ) : maybe<b>
{
  xs.foreach-while(pred)
}

// Lookup the first element satisfying some predicate
fun lookup( xs : list<(a,b)>, pred : a -> bool ) : maybe<b>
{
  xs.foreach-while fn(kv) {
    if (pred(kv.fst)) then Just(kv.snd) else Nothing
  }
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
fun foreach-while( xs : list<a>, action : (a) -> e maybe<b> ) : e maybe<b>
{
  match(xs) {
    Nil        -> Nothing
    Cons(x,xx) -> {
      match(action(x)) {
        Nothing -> xx.foreach-while(action)
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

// Invoke `action` for each element of a list, passing also the position of the element.
fun foreach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
{
  var i := 0
  xs.foreach fn(x){
    val j = i; // don't dereference `i` inside the inject
    mask<local>{action(j,x)}
    i := i+1
  }
}

// Insert a separator `sep`  between all elements of a list `xs` .
fun intersperse( xs : list<a>, sep : a ) : list<a> {
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
private fun joinsep( xs : list<string>, sep : string ) : string
{
  fun join-acc( ys : list<string>, acc : string ) {
    match(ys) {
      Cons(y,yy) -> join-acc(yy, acc ++ sep ++ y)  // todo: use string builder
      Nil -> acc
    }
  }
  match(xs) {
    Nil -> ""
    Cons(x,xx) -> join-acc(xx,x)
  }
}

// Concatenate all strings in a list
fun join( xs : list<string> ) : string {
  return xs.joinsep("")
}

// Concatenate all strings in a list using a specific separator
fun join( xs : list<string>, sep : string ) : string {
  return xs.joinsep(sep)
}

// Concatenate all strings in a list in reverse order
fun reverse-join( xs : list<string> ) : string {
  return xs.reverse.joinsep("")  
}

// Append `end` to each string in the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
fun join-end( xs : list<string>, end : string) : string {
  match(xs) {
    Nil -> ""
    _   -> xs.joinsep(end) ++ end
  }
}


// Concatenate all lists in a list (e.g. flatten the list). (tail-recursive)
fun concat( xss : list<list<a>> ) : list<a>
{
  fun concat-pre( ys : list<a>, zss : list<list<a>> ) : list<a> {
    match(ys) {
      Cons(y,yy) -> Cons(y,concat-pre(yy,zss))
      Nil -> match(zss) {
        Cons(zs,zzs) -> concat-pre(zs,zzs)
        Nil -> Nil
      }
    }
  }
  concat-pre([],xss)
}

// Concatenate the result lists from applying a function to all elements.
fun flatmap( xs: list<a>, f : a -> e list<b> ) : e list<b>
{
  fun flatmap-pre( ys, zs ) {
    match(ys) {
      Cons(y,yy) -> Cons(y,flatmap-pre(yy,zs))
      Nil -> match(zs) {
        Cons(z,zz) -> flatmap-pre(f(z),zz)
        Nil -> Nil
      }
    }
  }
  flatmap-pre([],xs)
}

// Concatenate the `Just` result elements from applying a function to all elements.
fun flatmap-maybe( xs : list<a>, f : a -> e maybe<b> ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> match(f(x)) {
      Just(y) -> Cons(y, xx.flatmap-maybe(f))
      Nothing -> xx.flatmap-maybe(f)
    }
    Nil -> Nil
  }
}

// Concatenate a list of `:maybe` values
fun concat-maybe( xs : list<maybe<a>> ) : list<a>
{
  match(xs) {
    Cons(x,xx) -> match(x) {
      Just(y) -> Cons(y, xx.concat-maybe)
      Nothing -> xx.concat-maybe
    }
    Nil -> Nil
  }
}

// Return the last element of a list (or `Nothing` for the empty list)
fun last( xs : list<a> ) : maybe<a>
{
  match(xs) {
    Cons(x,Nil) -> Just(x)
    Cons(_,xx)  -> last(xx)
    Nil         -> Nothing
  }
}

// Return the last element of a list (or `default` for the empty list)
fun last( xs : list<a>, default : a ) : a
{
  match(xs) {
    Cons(x,Nil) -> x
    Cons(_,xx)  -> last(xx,default)
    Nil         -> default
  }
}

// Return the list without its last element.
// Return an empty list for an empty list.
fun init( xs : list<a> ) : list<a> {
  match(xs) {
    Cons(x, xx as Cons) -> Cons(x,init(xx))
    _ -> Nil
  }
}

// Get (zero-based) element `n`  of a list. Return a `:maybe` type.
fun []( xs : list<a>, n : int ) : maybe<a> {
  match(xs) {
    Cons(x,xx) -> if (n>0) then xx[n - 1]
                  elif (n==0) then Just(x)
                  else Nothing
    Nil -> Nothing
  }
}

// Do all elements satisfy a predicate ?
fun all( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Cons(x,xx) -> if (predicate(x)) then xx.all(predicate) else False
    Nil -> True
  }
}


// Are there any elements in a list that satisfy a predicate ?
fun any( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Cons(x,xx) -> if (predicate(x)) then True else xx.any(predicate)
    Nil -> False
  }
}

// ----------------------------------------------------------------------------
// Characters
// ----------------------------------------------------------------------------

inline extern (==) : (char,char) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
inline extern (!=) : (char,char) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
inline extern (<=) : (char,char) -> bool  { inline "(#1 <= #2)" }
inline extern (>=) : (char,char) -> bool  { inline "(#1 >= #2)" }
inline extern (<)  : (char,char) -> bool  { inline "(#1 < #2)" }
inline extern (>)  : (char,char) -> bool  { inline "(#1 > #2)" }

fun compare( x : char, y : char ) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}

// Convert a character to its unicode code point
inline extern int : (char) -> int {
  inline "#1"
  c "kk_integer_from_int"
  cs inline "new BigInteger(#1)"
}

// Convert a unicode code point to a character
inline extern char( i : int) : char {
  inline "(#1)";
  c "kk_integer_clamp32"
  cs inline "Primitive.IntToInt32(#1)"
}

// Add two character code points
fun (+)(c : char, d : char) : total char { (c.int + d.int).char }
// Substract two character codePoints
fun (-)(c : char, d : char) : total char { (c.int - d.int).char }

// Is the character a lower-case ASCII character ?
fun is-lower( c : char ) : bool { c >= 'a' && c <= 'z' }
// Is the character an upper-case ASCII character ?
fun is-upper( c : char ) : bool { c >= 'A' && c <= 'Z' }
// Is the character an ASCII digit ?
fun is-digit( c : char ) : bool { c >= '0' && c <= '9' }
// Is the character an ASCII hexa-decimal digit ?
fun is-hex-digit( c : char ) : bool { c.is-digit || (c >= 'a' && c <= 'f') || (c >= 'A'  && c <= 'F') }
// Is the character an ASCII letter is-
fun is-alpha( c : char ) : bool { c.is-lower || c.is-upper }
// Is the character ASCII letter or digit?
fun is-alpha-num( c : char ) : bool { c.is-alpha || c.is-digit }
// Is the character an ASCII character, e.g. `c <= '\x7F'`  ?
fun is-ascii( c : char )     : bool { c <= '\x7F' }
// Is the character an ASCII control character, e.g. `c < ' '`  ?
fun is-control( c : char )   : bool { c < ' ' }
// Tests if a character is an element of `" \t\n\r"`
fun is-white( c : char )     : bool { c == ' ' || c == '\t' || c == '\n' || c == '\r' }


// ----------------------------------------------------------------------------
// Booleans
// ----------------------------------------------------------------------------

fun (==)( x : bool, y : bool) : bool { if (x) then y else !y }
fun (!=)( x : bool, y : bool) : bool { if (x) then !y else y }

fun (<)( x : bool, y : bool) : bool  { (!x && y) }
fun (<=)( x : bool, y : bool) : bool { !(x > y) }
fun (>)( x : bool, y : bool) : bool  { (x && !y) }
fun (>=)( x : bool, y : bool) : bool { !(x < y) }

fun compare( x : bool, y : bool) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}

// Transform a boolean to a maybe type, using `Nothing` for `False`
fun maybe( b : bool ) : maybe<()> {
    if (b) then Just(()) else Nothing
}

// ----------------------------------------------------------------------------
// Integers
// ----------------------------------------------------------------------------

// Compare two integers
inline extern compare(^x : int, ^y : int) : order  {
  c inline "kk_int_as_order(kk_integer_cmp_borrow(#1,#2,kk_context()),kk_context())"
  cs "Primitive.IntCompare"
  js "$std_core._int_compare"
}


// Are two integers equal?
inline extern (==)(^x : int, ^y : int) : bool  {
  c  "kk_integer_eq_borrow"
  cs inline "(#1 == #2)"
  js "$std_core._int_eq"
}

// Are two integers not equal?
inline extern (!=)(^x : int, ^y : int) : bool  {
  c  "kk_integer_neq_borrow"
  cs inline "(#1 != #2)"
  js "$std_core._int_ne"
}

// Is the first integer smaller or equal to the second?
inline extern (<=)(^x : int, ^y : int) : bool  {
  c  "kk_integer_lte_borrow"
  cs inline "(#1 <= #2)"
  js "$std_core._int_le"
}

// Is the first integer greater or equal to the second?
inline extern (>=)(^x : int, ^y : int) : bool  {
  c  "kk_integer_gte_borrow"
  cs inline "(#1 >= #2)"
  js "$std_core._int_ge"
}

// Is the first integer smaller than the second?
inline extern (<)(^x : int, ^y : int) : bool  {
  c  "kk_integer_lt_borrow"
  cs inline "(#1 < #2)"
  js "$std_core._int_lt"
}

// Is the first integer greater than the second?
inline extern (>)(^x : int, ^y : int) : bool  {
  c  "kk_integer_gt_borrow"
  cs inline "(#1 > #2)"
  js "$std_core._int_gt"
}

private inline extern int-add : (int,int) -> int {
  c  "kk_integer_add"
  cs inline "(#1 + #2)"
  js "$std_core._int_add"
}

// Add two integers.
fun (+)(x : int, y : int ) : int 
  int-add(x,y)

private inline extern int-sub : (int,int) -> int {
  c  "kk_integer_sub"
  cs inline "(#1 - #2)"
  js "$std_core._int_sub"
}

// Substract two integers.
fun (-)(x : int, y : int ) : int 
  int-sub(x,y)

// Multiply two integers.
inline extern (*) : (int,int) -> int {
  c  "kk_integer_mul"
  cs inline "(#1 * #2)"
  js "$std_core._int_mul"
}

// Euclidean-0 division of two integers. See also `divmod:(x : int, y : int) -> (int,int)`.
inline extern (/)(x:int,y:int) : int {
  c  "kk_integer_div"
  cs "Primitive.IntDiv"
  js "$std_core._int_div"
}
// Euclidean modulus of two integers; always a non-negative number. See also `divmod:(x : int, y : int) -> (int,int)`.
inline extern (%)  : (int,int) -> int {
  c  "kk_integer_mod"
  cs "Primitive.IntMod"
  js "$std_core._int_mod"
}

// Euclidean-0 division & modulus.
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
// See also _Division and modulus for computer scientists, Daan Leijen, 2001_ for further information
// available at: <http://research.microsoft.com/pubs/151917/divmodnote.pdf> .
inline extern divmod(x:int,y:int) : (int,int) {
  c  "kk_integer_div_mod_tuple"
  cs "Primitive.IntDivMod"
  js "$std_core._int_divmod"
}

fun negate(i : int) : int {
  ~i
}

// Negate an integer.
inline extern (~)(i:int) : int {
  c  "kk_integer_neg"
  cs inline "(-#1)"
  js "$std_core._int_negate"
}

// Convert an integer to a `:double`. May return `nan` if the integer is too large to represent as a `:double`.
inline extern double( i : int) : double  {
  c  "kk_integer_as_double"
  cs "Primitive.IntToDouble"
  js "$std_core._int_to_double"
}

// Is this an odd integer?
inline extern is-odd   : ( int ) -> bool  {
  c  "kk_integer_is_odd"
  cs inline "!(#1.IsEven)"
  js "$std_core._int_isodd"
}

// Is this equal to zero?
inline extern is-zero( ^x : int) : bool  {
  c  inline "kk_integer_is_zero_borrow(#1)"
  cs inline "(#1.IsZero)"
  js "$std_core._int_iszero"
}


// Return the absolute value of an integer.
inline extern abs(i : int) : int {
  c  "kk_integer_abs"
  cs "BigInteger.Abs"
  js "$std_core._int_abs"
}

fun inc( i : int ) : int {
  i + 1
}

fun dec( i : int ) : int {
  i - 1
}

// Calculate `10^exp`
fun exp10( exp : int ) : int {
  1.mul-exp10(exp)
}

// Raise an integer `i` to the power of `exp`.
extern pow( i : int, exp : int ) : int {
  c  "kk_integer_pow"
  cs "Primitive.IntPow"
  js "_int_pow"
}

// Raise an integer `i` to the power of `exp`.
fun (^)(i : int, exp : int ) : int {
  pow(i,exp);
}

// Calculate `2^exp`.
fun exp2( exp : int ) : int {
  pow(2,exp)
}

// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.
extern is-exp10( i : int ) : int {
  c  "kk_integer_ctz"
  cs "Primitive.IntCountPow10"
  js "_int_count_pow10"
}

// Return the number of decimal digits of `i`. Return `0` when `i==0`.
extern count-digits( i : int ) : int {
  c  "kk_integer_count_digits"
  cs "Primitive.IntCountDigits"
  js "_int_count_digits"
}

extern mul-exp10( i : int, n : int ) : int {
  c  "kk_integer_mul_pow10"
  cs "Primitive.IntMulPow10"
  js "_int_mul_pow10"
}

extern cdiv-exp10( i : int, n : int ) : int {
  c  "kk_integer_cdiv_pow10"
  cs "Primitive.IntCDivPow10"
  js "_int_cdiv_pow10"
}

fun cdivmod-exp10( i : int, n : int ) : (int,int) {
  if (n <= 0) return (i,0)
  val cq = i.cdiv-exp10(n)
  val cr = i - cq.mul-exp10(n)
  (cq,cr)
}

fun divmod-exp10( i : int, n : int ) : (int,int) {
  val (cq,cr) = cdivmod-exp10(i,n)
  if (!cr.is-neg) then (cq,cr) else (cq.dec, cr + exp10(n))
}

// Is this an even integer?
fun is-even(i:int) : bool { !is-odd(i) }

// Is the integer positive (stricly greater than zero)
fun is-pos(i : int ) : bool { i.sign == Gt }

// Is the integer negative (stricly smaller than zero)
fun is-neg(i : int ) : bool { i.sign == Lt }

inline extern sign( ^i : int ) : order {
  c  inline "kk_int_as_order(kk_integer_signum_borrow(#1),kk_context())"
  cs "Primitive.IntSign"
  js "$std_core._int_sign"
}

// Return the minimum of two integers
fun min( i : int, j : int ) : int { if (i <= j) then i else j }

// Return the maximum of two integers
fun max( i : int, j : int ) : int { if (i >= j) then i else j }

// Returns the smallest element of a list of integers (or `default` (=`0`) for the empty list)
fun minimum( xs : list<int>, default : int = 0 ) : int
{
  match(xs) {
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, min )
  }
}

// Returns the largest element of a list of integers (or `default` (=`0`) for the empty list)
fun maximum( xs : list<int>, default : int = 0 ) : int
{
  match(xs) {
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, max )
  }
}

// Return the sum of a list of integers
fun sum( xs : list<int> ) : int
{
  xs.foldl( 0, fn(x,y) { x + y } )
}

// Transform an integer to a maybe type, using `Nothing` for `0`
fun maybe( i : int ) : maybe<int> {
  if (i==0) then Nothing else Just(i)
}

// fold over the integers between [`start`,`end`] (inclusive).
fun fold-int( start : int, end : int, init : a, f : (int,a) -> e a ) : e a {
  if (start >= end) then init else
    val x = f(start,init)
    fold-int(unsafe-decreasing(start.inc), end, x, f)
}

// fold over the integers between [0,`upto`)  (not including `upto`).
fun fold-int( upto : int, init : a, f : (int,a) -> e a ) : e a {
  fold-int( 0, upto.dec, init, f )
}



// ----------------------------------------------------------------------------
// 32-bit integers
// Just define the operations needed for defining the std/core interface but
// don't export any definitions here. Full operations are defined in `std/int32`.
// ----------------------------------------------------------------------------

// Convert an `:int32` to an `:int`.
inline extern int( i : int32 ) : int {
  c  "kk_integer_from_int"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"
}

// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.
extern int32( i : int) : int32   {
  c  "kk_integer_clamp32"
  cs "Primitive.IntToInt32"
  js "$std_core._int_clamp32"
}


// Minimal set of operations that we need in `std/core`.
private inline extern (<=) : (int32,int32) -> bool  { inline "(#1 <= #2)"; js inline "(#1 <= #2)" }
private inline extern (<)  : (int32,int32) -> bool  { inline "(#1 < #2)"; js inline "(#1 < #2)" }
private inline extern (+)  : (int32,int32) -> int32 { inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
private inline extern (-)  : (int32,int32) -> int32 { inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }
private inline extern is-pos( i : int32 ) : bool { inline "(#1>0)" }
private inline extern is-neg( i : int32 ) : bool { inline "(#1<0)" }

private fun incr( i : int32 ) : int32 {
  i + 1.int32
}

private fun decr( i : int32 ) : int32 {
  i - 1.int32
}

// ----------------------------------------------------------------------------
// N-bit ssize_t
// Just define the operations needed for defining the std/core interface but
// don't export any definitions here.
// ----------------------------------------------------------------------------

// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
extern ssize_t( i : int) : ssize_t   {
  c  "kk_integer_clamp_ssize_t" 
  cs "Primitive.IntToInt32"
  js "$std_core._int_clamp32"
}

// Convert an `:ssize_t` to an `:int`.
inline extern int( i : ssize_t ) : int {
  c  "kk_integer_from_ssize_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"
}


// Minimal set of operations that we need in `std/core`.
private inline extern (<=) : (ssize_t,ssize_t) -> bool   { inline "(#1 <= #2)" }
private inline extern (>=) : (ssize_t,ssize_t) -> bool   { inline "(#1 >= #2)" }
private inline extern (<)  : (ssize_t,ssize_t) -> bool   { inline "(#1 < #2)" }
private inline extern (+)  : (ssize_t,ssize_t) -> ssize_t { inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
private inline extern (-)  : (ssize_t,ssize_t) -> ssize_t { inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }
private inline extern is-pos( i : ssize_t ) : bool { inline "(#1 > 0)" }
private inline extern is-neg( i : ssize_t ) : bool { inline "(#1 < 0)" }
private extern is-zero( i : ssize_t ) : bool { inline "(#1 == 0)"; js inline "(#1 === 0)" }
private extern decr(i : ssize_t ) : ssize_t { inline "(#1 - 1)" }
private extern incr(i : ssize_t ) : ssize_t { inline "(#1 + 1)" }



// ----------------------------------------------------------------------------
// int64
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:int64_t`.
extern int64( i : int) : int64 {
  c  "kk_integer_clamp64"
  cs "Primitive.IntToInt64"
  js "$std_core._int_clamp64"
}

// Convert an `:int64_t` to an `:int`.
inline extern int( i : int64 ) : int {
  c  "kk_integer_from_int64"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int64"
}

// ----------------------------------------------------------------------------
// intptr_t
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:intptr_t`. 
extern intptr_t( i : int) : intptr_t {
  c  "kk_integer_clamp_intptr_t"
  cs "Primitive.IntToInt64"
  js "$std_core._int_clamp64"
}

// Convert an `:intptr_t` to an `:int`.
inline extern int( i : intptr_t ) : int {
  c  "kk_integer_from_intptr_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int64"
}

// ----------------------------------------------------------------------------
// Byte
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in a `:byte`.
extern byte( i : int) : byte {
  c  "kk_integer_clamp_byte"
  cs inline "Primitive.IntToByte"
  js "$std_core._int_clamp_byte"
}

// Convert an `:byte` to an `:int`.
inline extern int( i : byte ) : int {
  c  "kk_integer_from_byte"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"
}

// ----------------------------------------------------------------------------
// Parse numbers
// ----------------------------------------------------------------------------

// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.
fun parse-int-default( s : string, default : int = 0, hex : bool = False ) : int {
  if (s.is-empty) then default else s.parse-int(hex).maybe(default) }

// Parse an integer after trimming whitespace.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string will result in `Just(0)`.
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
fun parse-int( s : string, hex : bool = False) : maybe<int> {
  s.trim.xparse-int(hex)
}

private extern xparse-int( s : string, hex : bool ) : maybe<int> {
  c  "kk_integer_xparse"
  cs "Primitive.IntParse"
  js "_int_parse"
}


// ----------------------------------------------------------------------------
// Doubles 
// ----------------------------------------------------------------------------

inline extern (==) : (double,double) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
inline extern (!=) : (double,double) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
inline extern (<=) : (double,double) -> bool  { inline "(#1 <= #2)" }
inline extern (>=) : (double,double) -> bool  { inline "(#1 >= #2)" }
inline extern (<)  : (double,double) -> bool  { inline "(#1 < #2)" }
inline extern (>)  : (double,double) -> bool  { inline "(#1 > #2)" }

inline extern (+) : (double,double) -> double { inline "(#1 + #2)" }
inline extern (-) : (double,double) -> double { inline "(#1 - #2)" }
inline extern (*) : (double,double) -> double { inline "(#1 * #2)" }
inline extern (/) : (double,double) -> double { inline "(#1 / #2)" }
inline extern (%) : (double,double) -> double { c inline "fmod(#1,#2)"; inline "(#1 % #2)" }

fun compare( x : double, y : double) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// Is the value negative?
fun is-neg( d : double ) : bool {
  d < 0.0
}

// Is the value positive?
fun is-pos( d : double ) : bool {
  d > 0.0
}

// Is the value zero?
fun is-zero( d : double ) : bool {
  d == 0.0
}

fun sign( d : double ) : order {
  if (d<0.0) then Lt elif (d>0.0) then Gt else Eq
}

// Negate a `:double`.
inline extern (~)(d : double ) : double {
  inline "(-#1)"  // inline so `~0.0` becomes negative zero
}


// convert a `:double` to an `:int` using `round` to round to its nearest integer.
// (rounding to an even number on a tie)
// Returns `0` if the argument is not `finite?`.
inline extern int(d : double ) : int {
  c  "kk_integer_from_double"
  cs "Primitive.IntDouble"
  js "$std_core._int_double"
}


// Returns the value `d`  raised to the power `p` .
inline extern (^) : (d:double,p:double) -> double {
  c inline "pow(#1,#2)"
  cs "Math.Pow"
  js "Math.pow"
}

// Return the absolute value of a `:double` `d`
inline extern abs : (d:double) -> double  {
  c inline "kk_double_abs(#1)"
  cs "Math.Abs"
  js "Math.abs"
}

// Returns the smallest of two doubles
fun min( x : double, y : double ) : double { if (x <= y) then x else y }

// Returns the largest of two doubles
fun max( x : double, y : double ) : double { if (x >= y) then x else y }

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
abstract struct sslice( str : string, start : ssize_t, len : ssize_t )

// Internal export for the regex module
fun ".new-sslice"( str :string, start: ssize_t, len : ssize_t ) {
  Sslice(str,start,len)
}

// Convert a character to a string
extern string : ( c : char ) -> string  {
  c  "kk_string_from_char"
  cs inline "Primitive.CharToString(#1)";
  js inline "_char_to_string(#1)"
}

// Convert a vector of characters to a string.
extern string : (vector<char>) -> string {
  c  "kk_string_from_chars"
  cs inline "Primitive.CharsToString(#1)";
  js inline "_chars_to_string(#1)"
}

// Convert a string to a vector of characters.
extern vector : ( s : string ) -> vector<char> {
  c  "kk_string_to_chars"
  cs inline "Primitive.StringToChars(#1)";
  js inline "_string_to_chars(#1)"
}


// Convert a string to a list of characters
extern list( s : string ) : total list<char> {
  c  "kk_string_to_list"
  cs inline "Primitive.StringToList(#1)"
  js inline "_string_to_list(#1)"
}

// Convert a list of characters to a string
extern string( cs : list<char> ) : total string {
  c  "kk_string_from_list"
  cs inline "Primitive.ListToString(#1)"
  js inline "_list_to_string(#1)"
}


// Are two strings equal?
// Uses exact equality between character codes.
inline extern (==) : (string,string) -> bool {
  c  "kk_string_is_eq"
  cs inline "(#1 == #2)"
  js inline "(#1 === #2)"
}

// Are two strings not equal?
inline extern (!=) : (string,string) -> bool {
  c  "kk_string_is_neq"
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"
}

private extern string-compare : ( x : string, y : string ) -> int
{
  c  "kk_string_cmp_int"
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"
}

// Compare two strings.
// Uses the character codes directly for comparison
fun compare( x : string, y : string) : order
{
  string-compare(x,y).order;
}

fun (>=)( x : string, y : string ) : bool { compare(x,y) > Lt }
fun (<=)( x : string, y : string ) : bool { compare(x,y) < Gt }
fun (>)( x : string, y : string ) : bool  { compare(x,y) == Gt }
fun (<)( x : string, y : string ) : bool  { compare(x,y) == Lt }

// Append two strings.
extern (++)(x : string, y : string) : string {
  c "kk_string_cat"
  inline "(#1 + #2)"
}

// Choose a non-empty string
fun (||)( x : string, y : string ) : string {
  if (x.is-empty) then y else x
}

// Length returns the length in the platform specific encoding (and should not be exported)
private inline extern length( s : string ) : ssize_t {
  c inline "kk_string_len(#1,kk_context())"
  cs inline "#1.Length"
  js inline "#1.length"
}

// O(n). Return the number of characters in a string.
extern count( s : string ) : int {
  c  "kk_string_count_int"
  cs "Primitive.StringCount"
  js "_string_count"
}

// O(n). Return the number of characters in a string slice
extern count( slice : sslice ) : int {
  c  "kk_slice_count"
  cs "Primitive.SliceCount"
  js "_sslice_count"
}

private extern first1(s : string ) : sslice {
  c  "kk_slice_first"
  cs "Primitive.SliceFirst"
  js "_sslice_first"
}

private extern last1(s : string ) : sslice {
  c  "kk_slice_last"
  cs "Primitive.SliceLast"
  js "_sslice_last"
}

// O(`n`). The first `n` (default = `1`) characters in a string.
fun first(s : string, n : int = 1) : sslice {
  val slice = s.first1
  if (n==1) then slice else slice.extend( n - 1 )
}

// O(`n`). The last `n` (default = `1`) characters in a string
fun last(s : string, n : int = 1) : sslice {
  val slice = s.last1
  if (n==1) then slice
            else slice.advance(1 - n).extend(n - 1)
}

// O(1). The entire string as a slice
fun slice( s : string ) : sslice {
  Sslice(s,0.ssize_t,s.length)
}

// An empty slice
val empty = Sslice("",0.ssize_t,0.ssize_t)

// Is a slice empty?
fun is-empty( slice : sslice ) : bool {
  !slice.len.is-pos
}

// An invalid slice
val invalid = Sslice("",(-1).ssize_t,0.ssize_t)

// Is a slice invalid?
fun is-valid( slice : sslice ) : bool {
  slice.start >= 0.ssize_t
}

// Is a slice not empty?
fun is-notempty( slice : sslice ) : bool {
  slice.len.is-pos
}

// Return the first character of a string as a string (or the empty string)
fun head( s : string ) : string {
  s.first.string
}

// Return the tail of a string (or the empty string)
fun tail( s : string ) : string {
  s.first.after.string
}

// Return the common prefix of two strings (upto `upto` characters (default is minimum length of the two strings))
extern common-prefix(s : string, t : string, ^upto : int = -1 ) : sslice {
  c  "kk_slice_common_prefix_borrow"
  cs "Primitive.SliceCommonPrefix"
  js "_sslice_common_prefix"
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
// * `"abc".last.advance(-1).string == "b"`.
//
extern advance( slice : sslice, ^count : int ) : sslice {
  c  "kk_slice_advance_borrow"
  cs "Primitive.SliceAdvance"
  js "_sslice_advance"
}

// O(`count`). Extend a string slice by `count` characters up to the end of the string.
// A negative `count` shrinks the slice up to the empty slice.
// For example:
//
// * `"abc".first.extend(1).string == "ab"`
// * `"abc".last.extend(-1).string == ""`
//
extern extend( slice : sslice, ^count : int ) : sslice {
  c  "kk_slice_extend_borrow"
  cs "Primitive.SliceExtend"
  js "_sslice_extend"
}


// O(1). Return the string slice from the start of a string up to the
// start of `slice` argument.
fun before(slice : sslice) : sslice {
  val Sslice(s,start,_len) = slice
  Sslice(s,0.ssize_t,start)
}

// O(1). Return the string slice from the end of `slice` argument
// to the end of the string.
fun after(slice : sslice) : sslice {
  val Sslice(s,start,len) = slice
  Sslice(s,start+len,s.length - (start+len))
}

// O(n). Copy the `slice` argument into a fresh string.
// Takes O(1) time if the slice covers the entire string.
extern string( slice : sslice ) : string {
  c  "kk_slice_to_string"
  cs "Primitive.SliceToString"
  js "_slice_to_string"
}

// If the slice is not empty,
// return the first character, and a new slice that is advanced by 1.
extern next( slice : sslice ) : maybe<(char,sslice)> {
  c  "kk_slice_next"
  cs "Primitive.SliceNext"
  js "_sslice_next"
}

// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.
fun foreach-while( slice : sslice, action : (c : char) -> e maybe<a> ) : e maybe<a> {
  match(slice.next) {
    Nothing -> Nothing
    Just((c,rest)) -> {
      match(action(c)) {
        Nothing -> foreach-while(unsafe-decreasing(rest),action)
        res     -> res
      }
    }
  }
}

// Apply a function for each character in a string slice.
fun foreach( slice : sslice, action : (c:char) -> e ()) : e () 
  foreach-while( slice ) fn(c)
    action(c)
    Nothing
  ()


// Does string `s`  contain the string `sub`  ?
private inline extern xindex-of(s : string, sub : string ) : ssize_t {
  c inline "kk_string_index_of1(#1,#2,kk_context())"
  cs inline "((#1).IndexOf(#2) + 1)";
  js inline "((#1).indexOf(#2) + 1)";
}

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.
fun find( s : string, sub : string ) : maybe<sslice> {
  val i = s.xindex-of(sub)
  if (i.is-zero) then Nothing else Just(Sslice(s,i.decr,sub.length))
}

// Does string `s`  contain the string `sub`  ?
private inline extern xlast-index-of(s : string, sub : string ) : ssize_t {
  c inline "kk_string_last_index_of1(#1,#2,kk_context())"
  cs inline "(#1.LastIndexOf(#2) + 1)";
  js inline "((#1).lastIndexOf(#2) + 1)";
}

// Return the last index of substring `sub` in `s` if it occurs.
fun find-last( s : string, sub : string ) : maybe<sslice> {
  val i = s.xlast-index-of(sub)
  if (i.is-zero) then Nothing else Just(Sslice(s,i.decr,sub.length))
}

private inline extern xstarts-with: (s : string, pre : string ) -> bool {
  c  "kk_string_starts_with"
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
  c  "kk_string_ends_with"
  cs inline "#1.EndsWith(#2)";
  js inline "((#1).indexOf(#2, (#1).length - (#2).length) !== -1)"
}

// Does string `s`  end with `post`?
// If so, returns a slice of `s` from the start up to the `post` string at the end.
fun ends-with( s : string, post : string ) : maybe<sslice> {
  if (xends-with(s,post))
   then Just(Sslice(s,0.ssize_t,s.length - post.length))
   else Nothing
}

// Does string `s`  contain the string `sub`  ?
inline extern contains: (s : string, sub : string ) -> bool {
  c  "kk_string_contains"
  cs inline "((#1).Contains(#2))";
  js inline "((#1).indexOf(#2) >= 0)"
}

// Concatenate a vector of strings
inline extern join: (v : vector<string> ) -> string {
  c  "kk_string_join"
  cs "String.Concat";
  js inline "((#1).join(''))"
}

// Concatenate a vector of strings with a separator `sep`
inline extern join: (v : vector<string>, sep : string ) -> total string {
  c  "kk_string_join_with"
  cs "Primitive.Concat";
  js inline "((#1).join(#2))"
}

// Truncate a string to `count` characters.
fun truncate( s : string, count : int ) : string  {
  s.first.extend(count - 1).string
}

// Trim off a substring `sub` while `s` starts with that string.
fun trim-left( s : string, sub : string ) : string {
  if (sub.is-empty) return s
  match(s.starts-with(sub)) {
    Just(slice) -> trim-left(unsafe-decreasing(slice.string),sub)
    Nothing -> s
  }
}

// Trim off a substring `sub` while `s` ends with that string.
fun trim-right( s : string, sub : string ) : string {
  if (sub.is-empty) return s
  match(s.ends-with(sub)) {
    Just(slice) -> trim-right(unsafe-decreasing(slice.string),sub)
    Nothing -> s
  }
}

// Repeat a string `n` times
fun repeat( s : string, ^n : int ) : string {
  repeatz(s,n.ssize_t)
}

private extern repeatz( s : string, n : ssize_t ) : string {
  c  "kk_string_repeat"
  cs "Primitive.Repeat"
  js "_string_repeat"
}

// Convert a `:maybe` string to a string using the empty sting for `Nothing`
fun string( ms : maybe<string> ) : string {
  match(ms) {
    Nothing -> ""
    Just(s) -> s
  }
}

// Is a string empty?
fun is-empty( s : string ) : bool {
  s == ""
}

// Is a string not empty?
fun is-notempty( s : string ) : bool {
  s != ""
}

// Transform a string to a maybe type, using `Nothing` for an empty string
fun maybe( s : string ) : maybe<string> {
    if (s.is-empty) then Nothing else Just(s)
}


private inline extern splitv( s : string,  sep : string ) : vector<string> {
  c  "kk_string_splitv"
  cs inline "(#1.Split(new String[]{#2}, StringSplitOptions.None))";
  js inline "((#1).split(#2))"
}

private inline extern splitv( s : string,  sep : string, n : ssize_t ) : vector<string> {
  c  "kk_string_splitv_atmost"
  cs inline "#1.Split(new String[]{#2},#3, StringSplitOptions.None)";
  js inline "(#1).split(#2, #3)"
}

// Split a string into parts that were delimited by `sep`. The delimeters are not included in the results.
// For example: `split("1,,2",",") == ["1","","2]`
fun split( s : string, sep : string ) : list<string> {
  splitv(s,sep).list
}

// Split a string into at most `n` parts that were delimited by a string `sep`. The delimeters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
fun split( s : string, sep: string, ^n : int) : list<string> {
  splitv(s,sep,n.ssize_t).list
}

// Replace every occurrence of `pattern` to `repl` in a string.
inline extern replace-all( s : string, pattern : string, repl : string ) : string {
  c  "kk_string_replace_all"
  cs inline "(#1).Replace(#2,#3)";
  js inline @"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)";
}

// Count occurences of `pattern` in a string.
inline extern count( s : string, pattern : string ) : int {
  c  "kk_string_count_pattern"
  cs inline "Primitive.Count(#1,#2)";
  js inline @"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)";
}


// Apply a function `f` to each character in a string
fun map( s : string, f : char -> e char ) : e string {
  s.list.map(f).string // todo: optimize
}

// Invoke a function for each character in a string.
// If `action` returns `Just`, the function returns immediately with that result.
fun foreach-while( s : string, action : (c:char) -> e maybe<a> ) : e maybe<a> {
  s.slice.foreach-while(action)
}

// Invoke a function for each character in a string
fun foreach( s : string, action : (c:char) -> e () ) : e () {
  s.slice.foreach(action)
}

// Return the first character of a string (or `Nothing` for the empty string).
fun head-char( s : string ) : maybe<char> {
  s.foreach-while( Just )
}

// Invoke a function for each character in a string
//fun foreach( s : string, f : char -> e () ) : e () {
//  s.list.foreach(f)  // todo: optimize
//}

// Count the number of times a predicate is true for each character in a string
fun count( s : string, pred : (char) -> bool ) : int
{
  var cnt := 0
  s.foreach fn(c) {
    if (pred(c)) then cnt := cnt+1
  }
  cnt
}

// Split a string into a list of lines
fun lines( s : string ) : list<string> {
  s.split("\n")
}

// Join a list of strings with newlines
fun unlines( xs : list<string> ) : string {
  xs.join("\n")
}

// Convert a string to upper-case
extern to-upper : (s : string) -> string {
  c "kk_string_to_upper"
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()"
}

// Convert a string to lower-case
extern to-lower : (s : string) -> string {
  c "kk_string_to_lower"
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()"
}

// Convert the first character of a string to uppercase.
fun capitalize( s : string ) : string {
  s.first.string.to-upper ++ s.first.after.string
}

// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
fun pad-left( s : string, ^width : int, fill : char = ' ') : string
{
  val w = width.ssize_t
  val n = s.length
  if (w <= n)
   then s
   else fill.string.repeatz( w - n ) ++ s
}

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
fun pad-right( s : string, ^width : int, fill : char = ' ') : string
{
  val w = width.ssize_t
  val n = s.length
  if (w <= n)
   then s
   else s ++ fill.string.repeatz(w - n)
}

// Trim whitespace on the left and right side of a string
fun trim( s : string ) : string {
  s.trim-left.trim-right
}

// Trim the starting white space of a string
inline extern trim-left( s : string ) : string {
  c  "kk_string_trim_left"
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"
}

// Trim the ending white space of a string.
inline extern trim-right( s : string ) : string {
  c  "kk_string_trim_right"
  cs inline "(#1).TrimEnd()"
  js inline "((#1).replace(/\\s+$/,''))"
}



// ----------------------------------------------------------------------------
//  Vectors
// ----------------------------------------------------------------------------

// Return the element at position `index`  in vector `v`  without bounds check!
private inline extern unsafe-idx( ^v : vector<a>, index : ssize_t ) : total a {
  c  inline "kk_vector_at_borrow(#1,#2)"
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

private inline extern unsafe-assign : forall<a> ( v : vector<a>, i : ssize_t, x : a ) -> total () {
  c "kk_vector_unsafe_assign"
  cs inline "(#1)[#2] = #3";
  js inline "(#1)[#2] = #3";
}

private inline extern unsafe-vector : forall<a> ( n : ssize_t ) -> total vector<a> {
  c  inline "kk_vector_alloc_uninit(#1,NULL,kk_context())"
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"
}

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
inline extern []( ^v : vector<a>, ^index : int ) : exn a  {
  c "kk_vector_at_int_borrow"
  cs inline "(#1)[Primitive.IntToInt32(#2)]"
  js inline "$std_core._vector_at(#1,#2)"
}

// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds
fun at( ^v : vector<a>, ^index : int ) : maybe<a> {
  val idx = index.ssize_t
  if (v.lengthz <= idx) then Nothing else Just(unsafe-idx(v,idx))
}

// Return the length of a vector.
fun length( ^v : vector<a> ) : int {
  v.lengthz.int
}

private inline extern lengthz( ^v : vector<a> ) : ssize_t {
  c  inline "kk_vector_len_borrow(#1)"
  cs inline "((#1).Length)";
  js inline "((#1).length)";
}

// Create a new vector of length `n`  with initial elements `default` .
fun vector( ^n : int, default : a) : vector<a> {
  vector-initz(n.ssize_t, fn(_i){ default } )
}

// Create a new vector of length `n`  with initial elements given by function `f` .
fun vector-init( ^n : int, f : int -> a ) : vector<a> {
  vector-initz( n.ssize_t, fn(i) { f(i.int) } )
}

// Create a new vector of length `n`  with initial elements given by function `f` .
private extern vector-initz(n : ssize_t, f : ssize_t -> a) : vector<a> {
  c "kk_vector_init"
  cs inline "Primitive.NewArray<##1>(#1,#2)"
  js inline "_vector(#1,#2)"
}

// Create an empty vector.
inline extern vector : forall<a> () -> vector<a> {
  c inline "kk_vector_empty()"
  cs inline "new ##1[0]"
  js inline "[]"
}

// Invoke a function `f` for each element in a vector `v`
fun foreach( v : vector<a>, f : (a) -> e () ) : e () {
  v.foreach-indexedz( fn(_,x) { f(x) })
}

// Invoke a function `f` for each element in a vector `v`
fun foreach-indexed( v : vector<a>, f : (int,a) -> e () ) : e () {
  foreach-indexedz( v, fn(i,x) { f(i.int,x) } )
}

private fun foreach-indexedz( v : vector<a>, f : (ssize_t,a) -> e () ) : e () {
  forz( 0.ssize_t, v.lengthz.decr ) fn(i) {
    f(i,v.unsafe-idx(i))
  }
}

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b> {
  for-whilez( 0.ssize_t, v.lengthz.decr ) fn(i) {
    f(v.unsafe-idx(i))
  }
}

// Apply a total function `f` to each element in a vector `v`
fun map( v : vector<a>, f : a -> e b ) : e vector<b> {
  val w = unsafe-vector(v.length.ssize_t)
  v.foreach-indexedz fn(i,x) {
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
  c  "kk_vector_to_list"
  cs inline "Primitive.VList<##1>(#1,#2)"
  js inline "_vlist(#1,#2)"
}

// Convert a list to a vector.
fun vector( xs : list<a> ) : vector<a> {
  xs.unvlist
}

extern unvlist( xs : list<a> ) : vector<a> {
  c  "kk_list_to_vector"
  cs inline "Primitive.UnVList<##1>(#1)"
  js inline "_unvlist(#1)"
}





// ----------------------------------------------------------------------------
// Delayed values
// ----------------------------------------------------------------------------

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract type delayed<e,a> {
  con Delay( dref : ref<global,either<() -> e a,a>> )
}

// Create a new `:delayed` value.
fun delay( action : () -> e a ) : delayed<e,a> {
  unsafe-total{
    Delay(ref(Left(action)))
  }
}

// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
fun force( delayed : delayed<e,a> ) : e a {
  unsafe-total {
    val r = delayed.dref
    match(!r) {
      Right(x) -> x
      Left(action) -> {
        val x = (inject-st{mask<div>(action)})()
        r := Right(x)
        x
      }
    }
  }
}


// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic show: shows the internal representation of an object as a string
// Note: this breaks parametricity so it should not be public
private extern gshow : forall<a> a -> string {
  c "kk_show_any"
  cs inline "#1.ToString()";
  js inline "#1.toString()"
}

// Convert an `:int` to a string
extern show( i : int ) : string {
  c "kk_integer_to_string"
  cs inline "#1.ToString()";
  js inline "#1.toString()"
}


private extern int-show-hex(i:int,use-capitals:bool) : string {
  c "kk_integer_to_hex_string"
  cs "Primitive.IntShowHex"
  js "_int_showhex"
}

// Show an `:int` as a hexadecimal value.\
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
fun show-hex( i : int, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) {
  (if (i<0) then "-" else "") ++ pre ++ int-show-hex(i.abs,use-capitals).pad-left(width,'0')
}
 
// Show a `:double` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.
fun show( d : double, precision : int = -17 ) : string {
  val dabs = d.abs
  if (dabs >= 1.0e-5 && dabs < 1.0e+21)
   then show-fixed(d,precision)
   else show-exp(d,precision)
}

// Show a `:double` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.
fun show-fixed( d : double, precision : int = -2 ) : string {
  val dabs = d.abs
  if (dabs < 1.0e-15 || dabs > 1.0e+21)
   then show-exp(d,precision)
   else show-fixedx(d, precision.int32)
}

private extern show-fixedx( d : double, prec : int32 ) : string {
  c  "kk_double_show_fixed"
  cs "Primitive.DoubleShowFixed"
  js "_double_show_fixed"
}

// Show a `:double` in exponential (scientific) notation.
// The optional `precision` (= `-17`) specifies the precision.
// If `>=0` it specifies the number of digits behind the dot (up to `17` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
fun show-exp( d : double, precision : int = -17 ) {
  show-expx(d,precision.int32)
}

private extern show-expx( d : double, prec : int32 ) : string {
  c  "kk_double_show_exp"
  cs "Primitive.DoubleShowExp"
  js "_double_show_exp"
}

// Show a character as a string
private fun show-char( c : char ) : string
{
  if (c < ' ' || c > '~') then {
    if (c == '\n') then "\\n"
    elif (c == '\r') then "\\r"
    elif (c == '\t') then "\\t"
    elif (c.int <= 0xFF) then "\\x" ++ c.int.show-hex(2,pre="")
    elif (c.int <= 0xFFFF) then "\\u" ++ c.int.show-hex(4,pre="")
    else "\\U" ++ c.int.show-hex(6,pre="")
  }
  else {
    if (c == '\'') then "\\'"
    elif (c == '\"') then "\\\""
    elif (c == '\\') then "\\\\"
    else c.string
  }
}

// Show a `:char` as a character literal
fun show( c : char ) : string
{
  "'" ++ c.show-char ++ "'"
}


// Show a string as a string literal
noinline fun show( s : string ) : string 
{
  "\"" ++ s.list.map(show-char).join ++ "\""
}

// Convert a `:bool` to a string
fun show( b : bool ) : string { if (b) then "True" else "False" }

// Convert a unit value `()` to a string
fun show( u : () ) : string  { "()" }

// Show an `:sslice` as a string literal
fun show( s : sslice ) : string { s.string.show }

// Convert a list to a string
fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
{
  "[" ++ xs.map(show-elem).join(",") ++ "]"
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
  "(" ++ x.fst.showfst ++ "," ++ x.snd.showsnd ++ ")"
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

private noinline val redirect : ref<global,maybe<(string) -> console ()>> = unsafe-total { ref(Nothing) }

// redirect `print` and `println` calls to a specified function.
noinline fun print-redirect( print : (msg : string) -> console () ) : io ()
{
  redirect := Just(print);
}

// Print a string to the console, including a final newline character.
private extern xprintsln(s : string) : console ()  {
  c  "kk_println"
  cs "Console.WriteLine"
  js "_println"
}

// Print a string to the console
private extern xprints( s : string) : console ()  {
  c  "kk_print"
  cs "Console.Write"
  js "_print"
}

// _Unsafe_. This function removes the non-termination effect (`:div`) from the effect of an action
inline extern unsafe-nostate( action : () -> <st<h>,console> a ) : (() -> console a) { inline "#1"}

private noinline fun prints( s : string ) : console () {
  (unsafe-nostate {
    match(!redirect) {
      Nothing -> xprints(s)
      Just(f) -> f(s)
    }
  })()
}

private noinline fun printsln( s : string ) : console () {
  (unsafe-nostate {
    match(!redirect) {
      Nothing -> xprintsln(s)
      Just(f) -> f(s ++ "\n")
    }
  })()
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
fun println(c : char)    { printsln(c.string) }

// Print a unit value to the console, including a final newline character
fun println( u : () )  { printsln(show(())) }

// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------


private extern xtrace : ( message : string ) -> ()  {
  c  "kk_trace"
  cs "Primitive.Trace"
  js "_trace"
}

private extern xtrace-any : forall<a> ( message: string, x : a ) -> ()  {
  c  "kk_trace_any"
  cs "Primitive.TraceAny"
  js "_trace_any"
}

private val trace-enabled : ref<global,bool> = unsafe-total { ref(True) }

// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.
fun trace( message : string ) : () {
  unsafe-total {
    if (!trace-enabled) then xtrace(message)
  }
}

fun trace-any( message : string, x : a ) : () {
  unsafe-total {
    if (!trace-enabled) then xtrace-any(message,x)
  }
}
// Disable tracing completely.
noinline fun notrace() : st<global> () {
  trace-enabled := False
}

private extern unsafe-assert-fail( msg : string ) : () {
  c "kk_assert_fail"
  js inline "function() { throw new Error(\"assertion failed: \" + #1) }()"
}

fun assert( message : string, condition : bool ) : () {  // Compiler removes assert calls in optimized builds
  if (!condition) then unsafe-assert-fail(message)  
}


// ----------------------------------------------------------------------------
// Exceptions
// ----------------------------------------------------------------------------

// The `:named` effect is the default umbrella effect for named effects
type nmd :: X

type scope :: S -> X

// Exceptions
effect exn {
  // Throw an exception
  brk throw-exn( exn : exception ) : a
}

fun ".default-exn"(action) {
  handle( action ) {
    brk throw-exn( exn : exception ) {
      "uncaught exception: ".prints
      show(exn).println
    }
  }
}

// The exception data type
struct exception( message :string, info :exception-info )

// Exception information
open type exception-info {
  ExnError      // Generic error
  ExnAssert
  ExnTodo
  ExnRange
  ExnPattern( location : string, definition : string )
  ExnSystem( errno : int )
  ExnInternal( name : string )
}


// Show the exception message
fun show( exn : exception ) : string {
  exn.message
}

// Throw an exception with a specified message.
fun throw( message: string, info : exception-info = ExnError ) : exn a {
  throw-exn(Exception(message,info))
}


// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
fun error-pattern(location : string, definition : string) : exn a {
  throw(location ++ (if (definition.is-empty) then "" else (": " ++ definition)) ++ ": pattern match failure",
         ExnPattern(location,definition))
}

// Catch any exception raised in `action` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
fun try( action : () -> <exn|e> a, hndl: exception -> e a ) : e a {
  with brk throw-exn(exn){ hndl(exn) }
  action()
}

// _Deprecated_; use `try` instead. Catch an exception raised by `throw` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
fun catch( action : () -> <exn|e> a, hndl: exception -> e a) : e a {
  try(action,hndl)
}

// An `:error` type represents a first-class exception result.
type error<a> {
  Error( exception : exception )
  Ok( result : a )
}


// Transform an exception effect to an  `:error` type.
fun try( action : () -> <exn|e> a ) : e error<a> {
  try({ Ok(action()) }, fn(exn){ Error(exn) })
}

// Return a default value when an exception is raised
fun try-default( value : a , action : () -> <exn|e> a ) : e a {
  try( action, fn(_) { value })
}

// Transform an `:error` type back to an `exn` effect.
fun untry( err : error<a> ) : exn a {
  match(err) {
    Error(exn) -> throw-exn(exn)
    Ok(x)      -> x
  }
}

// Transform an `:error` type back to an `exn` effect.
fun exn( err : error<a> ) : exn a {
  untry(err)
}

// Use default value `def` in case of an error.
fun default( t : error<a>, def : a ) : a {
  match(t) {
    Error  -> def
    Ok(x)  -> x
  }
}

// Transform an `:error` type to a `:maybe` value.
fun maybe( t : error<a> ) : maybe<a> {
  match(t) {
    Error  -> Nothing
    Ok(x)  -> Just(x)
  }
}

// Transform an `:error` type to an `:either` value.
fun either( t : error<a> ) : either<exception,a> {
  match(t) {
    Error(exn) -> Left(exn)
    Ok(x)      -> Right(x)
  }
}

/*
// Set a `hndler` that is called only when an exception is raised in the `action` block.
fun on-exn( hndler : exception -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a {
  // prim-try-some( action, fn(exn){ hndler(exn); throw(exn) })
  match(mask<exn>{ try(action) }) {
    TryOk(x)    { x }
    TryExn(exn) { hndler(exn); throw(exn) }
  }
}
*/
// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
fun on-exit( hndler : () -> e (), action : () -> e a ) : e a {
  finally(hndler,action)
}


// ----------------------------------------------------------------------------
// Null is used for external interfaces
// ----------------------------------------------------------------------------

// Abstract type used for passing `null` values to external functions
type null<a>

// Unsafe: transform any type to a `null` type; used internally by the compiler.
extern ".null-any"(x : a) : null<a> {
  c  inline "((#1).box == kk_box_null.box ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox(#1))"
  cs inline "#1"
  js inline "(#1==null ? null : #1)"  // undefined -> null
}

// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).
extern null(x : maybe<a>) : null<a> {
  c  inline "(kk_std_core_types__is_Nothing(#1) ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox((#1)._cons.Just.value) /* kk_datatype_unbox(kk_datatype_unjust(#1,kk_context())) */ )"
  cs inline "(#1.tag_ == __std_core._maybe_Tag.Nothing ? default(##1) : #1.@value)"
  js inline "(#1==null ? null : #1.value)"
}

// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).
extern maybe( n : null<a> ) : maybe<a> {
  c  inline "(kk_datatype_as_ptr(#1) == NULL ? kk_std_core_types__new_Nothing(kk_context()) : kk_std_core_types__new_Just(kk_datatype_box(#1),kk_context()))"
  cs inline "(EqualityComparer<##1>.Default.Equals(#1,default(##1)) ? __std_core._maybe<##1>.Nothing_ : new __std_core._maybe<##1>(#1))"
  js inline "(#1==null ? $std_core_types.Nothing : $std_core_types.Just(#1))"
}

// Cast a integer that is zero to a null
fun null( i : int ) : null<int>  {
    i.maybe.null
}

// Cast an empty string a null
fun null( s : string ) : null<string>  {
    s.maybe.null
}

// Cast a boolean `False` to null
fun null( b : bool ) : null<()> {
    b.maybe.null
}

// val null-const : forall<a> null<a> = null(Nothing)



// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

// Used by the compiler to wrap main console applications
extern main-console : forall<a,e> ( main : () -> e a ) -> e a {
  c "kk_main_console"
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"
}


// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------


inline extern inject-local<a,h,e>( action : () -> e a ) : total (() -> <local<h>|e> a) { inline "#1" }

// Assign to an entry in a local `:vector` variable.
inline extern []( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn|e> () {
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"
  js inline "(#1)[#2] = #3"
}

// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
fun once( calc : () -> a ) : (() -> a) {
  unsafe-total {
    val r = ref(Nothing)
    return fn() {
      unsafe-total {
        match(!r) {
          Just(x) -> x
          Nothing -> {
            val x = calc()
            r := Just(x)
            x
          }
        }
      }
    }
  }
}


// ----------------------------------------------------------------------------
// Unsafe
// ----------------------------------------------------------------------------



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


// ----------------------------------------------------------------------------
// Non determinism
// ----------------------------------------------------------------------------

private noinline val unique-count : ref<global,int> = unsafe-total { ref(0) }

// Returns a unique integer (modulo 32-bits).
fun unique() : ndet int
{
  unsafe-total {
    val u = !unique-count
    unique-count := u+1
    u
  }
}


// ----------------------------------------------------------------------------
// Control
// ----------------------------------------------------------------------------

// The `while` fun executes `action`  as long as `pred`  is `true`.
fun while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
{
  if (predicate()) then {
    action()
    while(predicate, action)
  }
}

// The `repeat` fun executes `action`  `n`  times.
fun repeat( ^n : int, action : () -> e () ) : e ()
{
  for(1,n) fn(i) { action() }
}

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
fun for( ^start: int, end : int, action : (int) -> e () ) : e ()
{
  fun rep( ^i : int ) {
    if (i <= end) then {
      action(i)
      rep(unsafe-decreasing(i.inc))
    }
  }
  rep(start)
}


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
private fun forz( start: ssize_t, end : ssize_t, action : (ssize_t) -> e () ) : e ()
{
  fun rep( i : ssize_t ) {
    if (i <= end) then {
      action(i)
      rep(unsafe-decreasing(i.incr))
    }
  }
  rep(start)
}


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
fun for-while( start: int, end : int, action : (int) -> e maybe<a> ) : e maybe<a>
{
  fun rep( i : int ) {
    if (i <= end) then {
      match(action(i)) {
        Nothing -> rep(unsafe-decreasing(i.inc))
        Just(x) -> Just(x)
      }
    }
    else Nothing
  }
  rep(start)
}


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
private fun for-whilez( start: ssize_t, end : ssize_t, action : (ssize_t) -> e maybe<a> ) : e maybe<a>
{
  fun rep( i : ssize_t ) {
    if (i <= end) then {
      match(action(i)) {
        Nothing -> rep(unsafe-decreasing(i.incr))
        Just(x) -> Just(x)
      }
    }
    else Nothing
  }
  rep(start)
}

// Return the host environment: `dotnet`, `browser`, `webworker`, `node`, or `libc`.
extern host() : ndet string {
  c "kk_get_host"
  cs inline "\"dotnet\""
  js inline "_host"
}


// .open is used in the compiler to open up closed effects.
// Usually simplified away but still present if --no-simplify flag is given
inline extern ".open"<e1 :: E, e2 :: E, a, b>( x : a ) : total b {
  inline "#1"
}

// Magic casting to the `:any` type.
inline extern ".toany"( x : a ) : any {
  inline "#1"
  cs inline "(object)(#1)"
}

/*
// TODO: revisit mask
inline extern ".inject-effect"<a::V,e1::E,e2::E>( effect-name : string, level : int, action : () -> e1 a ) : e2 a {
  js inline "$std_core._throw_exception('Core.inject-effect is not implemented')"
}
*/

// TODO: revisit value codegen
alias value<a> = a

extern phantom<a>() : a {
  c inline "kk_box_null"
  inline "undefined"
}

