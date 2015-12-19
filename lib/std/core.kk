/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module defines standard operations that are always available.

   Some types and operations are required to be defined for the compiler
   to work correctly (i.e. types like ":int" or ":list")
*/
public module std/core

infixr 80  (^)
infixl 70  (*), (%), (/), cdiv, cmod
infixr 60  (+), (-)
infix  40  (!=), (==), (<=), (>=), (<), (>)
infixr 30  (&&)
infixr 20  (||)

// prefix     (!), (-) 

external include {
  cs file "core-inline.cs"
  js file "core-inline.js"
}

// ----------------------------------------------------------------------------
// Core types
// ----------------------------------------------------------------------------

// A 32-bit signed integer (using two's complement to represent negative numbers).
type int

// A unicode character
type char

// A unicode string
type string

// A 64-bit IEEE 754 floating point value
type double

// An any type. Used for external calls
type any

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

// The alloc effect signifies a function may allocate in a heap ":h"
type alloc :: H -> X

// The read effect: signifies that a function may read from from heap ":h".
type read :: H -> X

// The write effect: signifies that a function may write to heap ":h".
type write :: H -> X

// The console effect signifies that a function may read or write to the console.
type console :: X

// An alias for the empty effect.
alias total = <>

// An alias for pure effects: a pure function always returns the same result
// when called with the same arguments but may not terminate or raise an exception.
alias pure = <exn,div>

// Stateful functions can manipulate heap ":h" using allocations, reads and writes.
alias st<h> = <read<h>, write<h>, alloc<h>>

// The ":global" heap is a special heap constant to denote the global shared heap
type global :: H

// The ":net" effect signifies a function may access the network
type net :: X

// The ":file" effect signifies a function may access the file system
type file :: X

// The ":ui" effect signifies a function may access the graphics system
type ui :: X

// The ":blocking" effect signifies that a function may block
type blocking :: X

// The ":io" effect is used for functions that perform arbitrary I/O operations.
alias io = <exn,div,ndet,console,net,file,ui,st<global>>

// ----------------------------------------------------------------------------
// Injection
// ----------------------------------------------------------------------------

// Add the divergent effect to a function effect.
external inline injectDiv : forall<a,b,e> (a -> e b) -> (a -> <div|e> b) = inline "#1"

// Add the exception effect to a function effect.
external inline injectExn : forall<a,b,e> (a -> e b) -> (a -> <exn|e> b) = inline "#1"

// Add the state effect to a function effect.
external inline injectSt : forall<a,b,h,e> ((a) -> e b) -> total ((a) -> <st<h>|e> b)  = inline "#1"

// Add the "read" effect to a function effect.
external inline injectRead : forall<a,b,h,e> ((a) -> e b) -> total ((a) -> <read<h>|e> b)  = inline "#1"

// Add the "write" effect to a function effect.
external inline injectWrite : forall<a,b,h,e> ((a) -> e b) -> total ((a) -> <write<h>|e> b)  = inline "#1"

// Add the "ndet" effect to a function effect.
external inline injectNdet : forall<a,b,e> ((a) -> e b) -> total ((a) -> <ndet|e> b)  = inline "#1"

// Add the "io" effect to a function effect.
external inline injectIo : forall<a,b,e> ((a) -> e b) -> total ((a) -> <io|e> b)  = inline "#1"



// ----------------------------------------------------------------------------
// Exceptions
// ----------------------------------------------------------------------------

// Exceptions
type exception

// Return a system dependent description of an exception
external show( exn : exception ) : string {
  cs inline "#1.ToString()"
  js inline "(#1 ? #1.toString() : 'unknown exception')"
}

// Catch an exception raised by "error" and handle it.
// Use "onExn" or "onFail" when appropiate.
external catch : forall<e,a> ( action : () -> <exn|e> a, handler: exception -> e a) -> e a {
  cs inline "Primitive.Catch<##2>(#1,#2)"
  js "$primcatch"
}
  
// Execute a "handler" no matter what exception was raised in "action".
// Use of "onExit" is preferred (which has its arguments swapped).
external inline finally : forall<e,a> (action: () -> e a, handler: () -> e ()) -> e a  {
  cs inline "Primitive.Finally<##2>(#1,#2)"
  js "$finally"
}

// Raise an exception with a specified message.
external error : forall<a> string -> exn a { 
    cs "Primitive.Error";
    js "$error"
}

// Throw an exception
external throw : forall<a> exception -> exn a {
  cs inline "Primitive.Throw<##1>(#1)"
  js "error(#1)"
}

// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
external patternMatchError : forall<a> (range : string, def : string) -> exn a { 
    cs "Primitive.PatternMatchError"
    js "$pattern_match_error"
}

// Set a "handler" that is called when an exception is raised in the "action" block.
function onFail( handler : () -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a
{
  catchOnFail( action, fun(exn){ handler(); throw(exn) })
}

// Return a default value when an exception is raised
function onExn( value : a , action : () -> <exn|e> a ) : e a {
  catch( action, fun(_) { value })
}

private external catchOnFail : forall<e,a> ( action : () -> e a, handler: exception -> e a) -> e a {
  cs inline "Primitive.Catch<##2>(#1,#2)"
  js "$catch"
}


// Set a "handler" that is always called when the "action" finishes (either normally or with an exception).
function onExit( handler : () -> e (), action : () -> e a ) : e a
{
  finally(action,handler)  
}

// Set a "handler" that is called if "action" returns without raising an exception.
function onSuccess( handler : () -> e (), action : () -> e a ) : e a
{
  val x = action()
  handler()
  return x
}

// Ignore any Exceptions
function ignore( action : () -> <exn|e> () ) : e () {
  catch( action, fun(_) { () })
}


// ----------------------------------------------------------------------------
// Internal types
// ----------------------------------------------------------------------------

// Optional is used internally by the compiler to pass optional arguments.
// It is usually displayed as ":?a" for some type ":a".
type optional<a> {
  // The "Optional" constructor is used when an optional argument is given.
  con Optional(value:a)
  // "None" is used when an optional argument is not provided.
  con None
}


type primarray<a>

external conslist( elems: primarray<a>, tail : list<a> ) : total list<a> {
  js "$list"
}

// ----------------------------------------------------------------------------
// Standard functions
// ----------------------------------------------------------------------------

// The identify function returns its argument unchanged.
function id(x) {
  x;
}

// The "const" functions returns its first argument and ignores the second.
function const(x,y) {
  x;
}

// Apply a function "f" to a specified argument "x".
function apply(f,x) {
  f(x);
}

// Compose two functions "f" and "g".
function o(f,g) {
  (fun(x){ f(g(x)) })
}


// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

// The ":void" type is empty and has no constructors.
// See also the ":()" unit type and the ":bool" type.
type void

// The type of booleans has two inhabitants: "True" and "False".
type bool {
  con False
  con True
}


function int( b : bool ) : int = if (b) then 1 else 0

function mbint( m : maybe<int> ) : int {
  match(m) {
    Nothing -> 0
    Just(i) -> i
  }
}

function bool( i : int ) : bool = (i!=0)

function bool( m : maybe<a> ) : bool {
  match(m) {
    Nothing -> False
    _       -> True
  }
}

function bool( s : string ) : bool = (s!="")

// The unit type ":()" is inhabited by just a single value, namely "()".
// See also the ":void" type and the ":bool" type.
struct ()

// A pair of values ":a" and ":b".
struct (,)<a,b>(fst:a,snd:b)

// A triple of values.
struct (,,)<a,b,c>(fst:a,snd:b,thd:c)

// A quadruple of values.
struct (,,,)<a,b,c,d>(fst:a,snd:b,thd:c,field4:d)

// A quintuple of values.
struct (,,,,)<a,b,c,d,e>(fst:a,snd:b,thd:c,field4:d,field5:e)

// The ":maybe" type is used to represent either a value ("Just(x)") or "Nothing".
// This type is often used to represent values that can be _null_.
type maybe<a> {
  con Nothing
  con Just( unJust : a )
}

// Match a ":maybe" value and either return a default value on "Nothing" or apply a function to the value on "Just"
function maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
{
  match(m) {
    Nothing -> onNothing
    Just(x) -> onJust(x)
  }
}

// The choice type represents one of two possible types ":a" or ":b".
type either<a,b> {
  con Left( left : a )
  con Right( left : b )
}

function maybe( e : either<a,b> ) : maybe<b> 
{
  match(e) {
    Left -> Nothing
    Right(x) -> Just(x)
  }
}

// The type of lists, which can be either empty ("Nil") or an element followed
// by a list ("Cons").
type list<a> {
  // The empty list.
  con Nil
  // A `head`  element followed by the `tail`  of the list.
  con Cons(head:a, tail : list<a> )
}

// A ":stream" is a co-inductive type reprenting an infinite list of elements.
cotype stream<a> {
  con Next(head:a, tail: stream<a> )
}

// An enumeration to represent order
type order {
  Lt
  Eq
  Gt
}

function int( x : order ) : int
{
  match(x) {
    Lt -> 0-1
    Eq -> 0
    Gt -> 1
  }
}

function order( i : int ) : order
{
  if (i < 0) Lt
  elif (i > 0) Gt
  else Eq
}

function (==)( x : order, y : order ) : bool = (x.int == y.int)
function (!=)( x : order, y : order ) : bool = (x.int != y.int)
function (>=)( x : order, y : order ) : bool = (x.int >= y.int)
function (<=)( x : order, y : order ) : bool = (x.int <= y.int)
function (>)( x : order, y : order ) : bool = (x.int > y.int)
function (<)( x : order, y : order ) : bool = (x.int < y.int)

function map( t : (a,a), f : a -> e b ) : e (b, b) {
  (t.fst.f, t.snd.f)
}

function map( t : (a,a,a), f : a -> e b ) : e (b, b, b) {
  (t.fst.f, t.snd.f, t.thd.f)
}

function map( t : (a,a,a,a), f : a -> e b ) : e (b,b,b,b) {
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f)
}

// ----------------------------------------------------------------------------
// List functions
// ----------------------------------------------------------------------------

// Returns a singleton list.
function single(x) {
  return Cons(x,Nil)
}

// Returns the length of a list.
function length(xs)
{
  function len(acc,ys) {
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
function list( lo: int, hi: int ) : total list<int>
{
  function enumerate( low:int, high:int, acc: list<int> ) : div list<int>
  {
    if (low > high)
     then acc
     else enumerate(low, high-1, Cons(high,acc))
  }

  unsafeNoDiv { enumerate(lo,hi,[]) }
}

// Create a list of characters from `lo`  to `hi`  (inclusive).
function list( lo : char, hi : char ) : total list<char>
{
  list(lo.int, hi.int).map( char )
}

private val maxListStack = 200


private function zipWithAcc( f, i, acc, xs, ys )
{
  match(xs) {
    Nil -> reverse(acc)
    Cons(x,xx) ->
      match(ys) {
        Nil -> reverse(acc)
        Cons(y,yy) -> zipWithAcc(f, i+1, Cons( f(i,x,y), acc), xx, yy)
      }
  }
}

private function zipWithIter( f, i, xs, ys )
{
  // recurse for the first "maxListStack" elements over the stack (to avoid extra heap allocation)
  if (i > maxListStack) then zipWithAcc( f, i, Nil, xs, ys )
  else {
    match(xs) {
      Nil -> Nil
      Cons(x,xx) ->
        match(ys) {
          Nil -> Nil
          Cons(y,yy) -> Cons( f(i,x,y), zipWithIter(f,i+1,xx,yy))
        }
    }
  }
}

// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
function zip( xs : list<a>, ys : list<b> ) : list<(a,b)>
{
  zipWithIndexed( xs, ys, fun(i,x,y) { (x,y) } )
}

// Zip two lists together by apply a function "f" to all corresponding elements.
// The returned list is only as long as the smallest input list.
function zipWith( xs : list<a>, ys :list<b>, f : (a,b) -> e c ) : e list<c>
{
  zipWithIndexed(xs,ys,fun(i,x,y) { f(x,y) })
}

// Zip two lists together by apply a function "f" to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
function zipWithIndexed( xs : list<a>, ys :list<b>, f : (int,a,b) -> e c ) : e list<c>
{
  zipWithIter(f,0,xs,ys)
}

// Unzip a list of pairs into two lists
function unzip( xs : list<(a,b)> ) : (list<a>,list<b>)
{
  function iter( ys, acc1, acc2 )
  {
    match(ys) {
      Cons((x,y),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2))
      Nil            -> (reverse(acc1),reverse(acc2))
    }
  }
  
  iter(xs,[],[])
}

// Take the first "n" elements of a list (or fewer if the list is shorter than "n")
function take( xs : list<a>, n : int ) : list<a>
{
  if (n <= 0) return Nil
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(x,take(xx,n-1))
  }
}

// Drop the first "n" elements of a list (or fewer if the list is shorter than "n")
function drop( xs : list<a>, n : int ) : list<a>
{
  if (n <= 0) return xs
  match(xs) {
    Nil -> Nil
    Cons(_,xx) -> drop(xx,n-1)
  }
}



// Apply a function `f`  to each element of the input list in sequence.
function map(xs : list<a>, f : a -> e b) : e list<b>
{
  xs.mapIndexedPeek( fun(i,x,xx) { f(x) } )
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
function mapIndexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b>
{
  xs.mapIndexedPeek( fun(i,x,xx) { f(i,x) })
}

// Apply a function `f`  to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
function mapPeek(xs : list<a>, f : (value : a, rest : list<a>) -> e b) : e list<b>
{
  xs.mapIndexedPeek( fun(i,x,xx) { f(x,xx) })
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
function mapIndexedPeek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  // recurse using an accumulator using constant heap space
  function mapAcc(g : (int,a,list<a>) -> e b , n : int, acc : list<b>, ys : list<a>) : e list<b> {

    match(ys) {
      Cons(x,xx) -> mapAcc(g,n+1,Cons(g(n,x,xx),acc),xx)
      Nil -> reverse(acc)
    }
  }

  // recurse for the first "maxListStack" elements over the stack (to avoid extra heap allocation)
  function mapIter(g : (int,a,list<a>) -> e b, n : int, ys : list<a>) : e list<b> {
    if (n > maxListStack) {
      mapAcc(g,n,Nil,ys)
    }
    else {
      match(ys) {
        Cons(x,xx) -> Cons(g(n,x,xx),mapIter(g,n+1,xx))
        Nil        -> Nil
      }
    }
  }

  return mapIter(f,0,xs)
}

// Reverse a list.
function reverse(xs : list<a>) : list<a>
{
  function reverseAcc(acc : list<a>, ys : list<a> ) : list<a> {
    match(ys) {
      Cons(x,xx) -> reverseAcc(Cons(x,acc),xx)
      _          -> acc
    }
  }
  reverseAcc(Nil,xs)
}

// Append two lists.
function (+)(xs : list<a>, ys : list<a> ) : list<a>
{
  // append using _constant_ stack space (by reversing the argument list)
  function revAppend(xx,yy) {
    match(xx) {
      Cons(z,zz) -> revAppend(zz,Cons(z,yy))
      _          -> yy
    }
  }
  // append for the first "maxListStack" elements over the stack
  function append(n,xx,yy) {
    if (n > maxListStack) {
      revAppend(reverse(xx),yy)
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
// Note, "foldr" is less efficient than "foldl" as it reverses the list first. 
function foldr(xs,z,f)
{
  xs.reverse.foldl(z,fun(x,y){f(y,x)}) 
}

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2` 
// Since "foldl" is tail recursive, it is preferred over "foldr" when using an associative function `f` 
function foldl(xs,z,f)
{
  match(xs) {
    Cons(x,xx) -> foldl(xx,f(z,x),f)
    Nil        -> z
  }
}

function foldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
{
  match(xs) {
    Cons(x,xx) -> xx.foldl(x,f)
  }
}

function foldr1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
{
  match(xs) {
    Cons(x,Nil) -> x
    Cons(x,xx)  -> f(x,xx.foldr1(f))
  }
}

// Create a list of `n`  repeated elementes `x` 
function replicate( x : a, n : int ) : list<a>
{
  function enumerate( i : int, acc : list<_a> ) {
    if (i <= 0) then acc else enumerate(i-1,Cons(x,acc))
  }

  unsafeNoDiv{ enumerate(n,[]) }
}

// split a list at position `n` 
function split( xs : list<a>, n : int ) : (list<a>, list<a>)
{
  (xs.take(n), xs.drop(n))
}


function span( xs : list<a>, predicate : a -> e bool ) : e (list<a>,list<a>)
{
  function spanAcc( ys, acc)
  {
    match(ys) {
      Cons(y,yy) -> if (y.predicate) then yy.spanAcc(Cons(y,acc)) else (acc.reverse,ys)
      _ -> (acc.reverse, ys)
    }
  }

  xs.spanAcc( [] )
}

// Keep only those initial elements that satisfy `predicate` 
function takeWhile( xs : list<a>, predicate : a -> e bool ) : e list<a>
{
  xs.span(predicate).fst
}

// Drop all initial elements that satisfy `predicate` 
function dropWhile( xs : list<a>, predicate : a -> e bool ) : e list<a>
{
  xs.span(predicate).snd
}

// Retain only those elements of a list that satisfy the given predicate "pred".
// For example: "filter([1,2,3],odd) == [1,3]"
function filter( xs : list<a>, pred : a -> e bool ) : e list<a>
{
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> if (pred(x)) then Cons(x,xx.filter(pred)) else xx.filter(pred)
  }
}


// Remove those elements of a list that satisfy the given predicate "pred".
// For example: "remove([1,2,3],odd) == [2]"
function remove( xs : list<a>, pred : a -> bool ) : list<a> {
  xs.filter( fun(x) { !pred(x) } )
}

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate "pred".
// For example: "partition([1,2,3],odd) == ([1,3],[2])"
function partition( xs : list<a>, pred : a -> bool ) : (list<a>,list<a>)
{
  partitionAcc( xs, pred, Nil, Nil)
}

private function partitionAcc( xs : list<a>, pred : a -> bool, acc1 : list<a>, acc2 : list<a> ) : (list<a>, list<a>)
{
  match(xs) {
    Nil -> (acc1.reverse, acc2.reverse)
    Cons(x,xx) -> if (pred(x)) 
                    then partitionAcc(xx,pred,Cons(x,acc1),acc2)
                    else partitionAcc(xx,pred,acc1,Cons(x,acc2))
  }
}


// Retain only those elements of a list that satisfy the given predicate "pred".
// For example: "filterMap([1,2,3],fun(i) { if (odd(i)) then Nothing else Just(i*i) }) == [4]"
function filterMap( xs : list<a>, pred : a -> e maybe<b> ) : e list<b>
{
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> match(pred(x)) {
      Nothing -> xx.filterMap(pred)
      Just(y) -> Cons(y,xx.filterMap(pred))
    }
  }
}

// Find the first element satisfying some predicate
function find( xs : list<a>, pred : a -> bool ) : maybe<a>
{
  xs.filter(pred).maybe
}

// Convert a list to a ":maybe" type, using "Nothing" for an empty list, and otherwise "Just" on the head element.
function maybe( xs : list<a> ) : maybe<a>
{
  match(xs) {
    Nil -> Nothing
    Cons(x,_) -> Just(x)
  }
}

// Convert a ":maybe" type to a list type.
function list( m : maybe<a> ) : list<a> 
{
  match(m) {
    Nothing -> Nil
    Just(x) -> Cons(x,Nil)
  }
}

private function indexOfAcc( xs : list<a>, pred : a -> bool, idx : int ) : int
{
  match(xs) {
    Nil -> 0-1
    Cons(x,xx) -> if (pred(x)) then idx else indexOfAcc(xx,pred,idx+1)
  }
}

// Returns the index of the first element where "pred" holds, or "-1" if no such element exists.
function indexOf( xs : list<a>, pred : a -> bool ) : int
{
  indexOfAcc( xs, pred, 0 )
}

// Invoke "action" for each element of a list
function foreach( xs : list<a>, action : (a) -> e () ) : e ()
{
  match(xs) {
    Nil        -> return ()
    Cons(x,xx) -> { action(x); xx.foreach(action) }
  }
}

// Invoke "action" for each element of a list while "action" return "Nothing"
function foreachUntil( xs : list<a>, action : (a) -> e maybe<b> ) : e maybe<b>
{
  match(xs) {
    Nil        -> Nothing
    Cons(x,xx) -> { 
      match(action(x)) {
        Nothing -> xx.foreachUntil(action) 
        just    -> just
      }
    }
  }
}

// Invoke "action" on each element of a list while "action" returns "Just"
function mapWhile( xs : list<a>, action : (a) -> e maybe<b> ) : e list<b>
{
  match(xs) {
    Nil        -> Nil
    Cons(x,xx) -> { 
      match(action(x)) {
        Just(y) -> Cons(y,xx.mapWhile(action))
        Nothing -> Nil
      }
    }
  }
}

private external inline injectSt : forall<a,b,c,h,e> ((a,b) -> e c) -> total ((a,b) -> <st<h>|e> c)  = inline "#1"

// Invoke "action" for each element of a list, passing also the position of the element.
function foreachIndexed( xs : list<a>, action : (int,a) -> e () ) : e ()
{
  var i := 0
  xs.foreach fun(x) {
    injectSt(action)(i,x)
    i := i+1
  }
}

// Insert a separator `sep`  between all elements of a list `xs` .
function intersperse( xs : list<a>, sep : a ) : list<a>
{
  //TODO: make tail recursive
  function before(ys,s) {
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
function join( xs : list<string> ) : string
{
  function joinAcc( ys : list<string>, acc : string ) {
    match(ys) {
      Cons(y,yy) -> joinAcc(yy, acc+y)
      Nil -> acc
    }
  }
  
  match(xs) {
    Nil -> ""
    Cons(x,xx) -> match(xx) {
      Nil -> x
      _   -> joinAcc(xx,x)
    }
  }
  //xs.foldl( "", function(x,y){ x + y } )  // TODO: should use a string builder
}

// Concatenate all strings in a list using a specific separator
function join( xs : list<string>, sep : string ) : string
{
  return xs.intersperse(sep).join()
}

// Concatenate all lists in a list (e.g. flatten the list)
function concat( xs : list<list<a>> ) : list<a>
{
  xs.foldr( [], (+) )
}

// Concatenate the result lists from applying a function to all elements
function concat( xs: list<a>, f : a -> e list<b> ) : e list<b>
{
  xs.map(f).concat()
}

// Concatenate a list of ":maybe" values
function concatMaybe( xs : list<maybe<a>> ) : list<a>
{
  xs.map(list).concat()
}

// Return the last element of a list. Raise an exception if the empty list is passed.
function last( xs : list<a> ) : exn a
{
  match(xs) {
    Cons(x,Nil) -> x
    Cons(_,xx)  -> last(xx)
  }
}

// Return the list without its last element.
// Return an empty list for an empty list.
function init( xs : list<a> ) : list<a>
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
function []( xs : list<a>, n : int ) : maybe<a>
{
  match(xs) {
    Cons(x,xx) -> if (n==0) then Just(x) else xx[n-1]
    Nil -> Nothing
  }
}

// Do all elements satisfy a predicate ?
function all( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Nil -> True
    Cons(x,xx) -> if (predicate(x)) then xx.all(predicate) else False
  }
}


// Are there any elements in a list that satisfy a predicate ?
function any( xs : list<a>, predicate : a -> e bool ) : e bool
{
  match(xs) {
    Nil -> False
    Cons(x,xx) -> if (predicate(x)) then True else xx.any(predicate)
  }
}

// ----------------------------------------------------------------------------
// Characters
// ----------------------------------------------------------------------------

external inline (==) : (char,char) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
external inline (!=) : (char,char) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
external inline (<=) : (char,char) -> bool  = inline "(#1 <= #2)"
external inline (>=) : (char,char) -> bool  = inline "(#1 >= #2)"
external inline (<)  : (char,char) -> bool  = inline "(#1 < #2)"
external inline (>)  : (char,char) -> bool  = inline "(#1 > #2)"

function compare( x : char, y : char ) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}

// Convert a character to its unicode code point
external inline int : (char) -> int { cs inline "(int)(#1)"; js inline "(#1).charCodeAt(0)" }

// Convert a unicode code point to a character
external inline char : (int) -> char { cs inline "(char)(#1)"; js inline "String.fromCharCode(#1)" }

// Add two character code points
function (+)(c : char, d : char) : total char  = (c.int + d.int).char
// Substract two character codePoints
function (-)(c : char, d : char) : total char  = (c.int - d.int).char

// Is the character a lower-case ASCII character ?
function isLower( c : char ) : bool = (c >= 'a' && c <= 'z')
// Is the character an upper-case ASCII character ?
function isUpper( c : char ) : bool = (c >= 'A' && c <= 'Z')
// Is the character an ASCII digit ?
function isDigit( c : char ) : bool = (c >= '0' && c <= '9')
// Is the character an ASCII hexa-decimal digit ?
function isHexDigit( c : char ) : bool = (c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A'  && c <= 'F'))
// Is the character an ASCII letter ?
function isAlpha( c : char ) : bool = (isLower(c) || isUpper(c))
// Is the character ASCII letter or digit?
function isAlphaNum( c : char ) : bool = (isAlpha(c) || isDigit(c))
// Is the character an ASCII character, e.g. `c <= '\x7F'`  ?
function isAscii( c : char )     : bool = (c <= '\x7F')
// Is the character an ASCII control character, e.g. `c < ' '`  ?
function isControl( c : char )   : bool = (c < ' ')
// Tests if a character is an element of `" \t\n\r"` 
function isWhite( c : char )     : bool = (c == ' ' || c == '\t' || c == '\n' || c == '\r')



// ----------------------------------------------------------------------------
// Booleans
// ----------------------------------------------------------------------------

// For short-circuiting we use external here
// function (&&)( x : bool, y : bool) : bool = if (x) then y else false
// function (||)( x : bool, y : bool) : bool = if (x) then true else y

external inline (&&) : (bool,bool) -> bool  = inline "(#1 && #2)"
external inline (||) : (bool,bool) -> bool  = inline "(#1 || #2)"

// for efficiency we use external here
// function (!)( b : bool ) : bool = if (x) then false else true
// function not( b : bool ) : bool = if (x) then false else true

external inline not  : (bool) -> bool  = inline "!(#1)"
external inline (!)  : (bool) -> bool  = inline "!(#1)"

function (==)( x : bool, y : bool) : bool = if (x) then y else !y
function (!=)( x : bool, y : bool) : bool = if (x) then !y else y

function (<)( x : bool, y : bool) : bool  = (!x && y)
function (<=)( x : bool, y : bool) : bool = !(x > y)
function (>)( x : bool, y : bool) : bool  = (x && !y)
function (>=)( x : bool, y : bool) : bool = !(x < y)

function compare( x : bool, y : bool) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// ----------------------------------------------------------------------------
// Integers
// ----------------------------------------------------------------------------

external inline (==) : (int,int) -> bool  { inline "(#1 == #2)"; js inline "#1 === #2" }
external inline (!=) : (int,int) -> bool  { inline "(#1 != #2)"; js inline "#1 !== #2" }
external inline (<=) : (int,int) -> bool  { inline "(#1 <= #2)"; js inline "#1 <= #2" }
external inline (>=) : (int,int) -> bool  { inline "(#1 >= #2)"; js inline "#1 >= #2"}
external inline (<)  : (int,int) -> bool  { inline "(#1 < #2)"; js inline "#1 < #2" }
external inline (>)  : (int,int) -> bool  { inline "(#1 > #2)"; js inline "#1 > #2" }

external inline (+) : (int,int) -> int { cs inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
external inline (-) : (int,int) -> int { cs inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }


// Do not use. Used by the compiler for platforms that do not support 32 bit multiplication natively.
external intMultiply : (int,int) -> int {
  cs inline "(#1 * #2)"
  js "$int32_multiply"
}

external inline (*) : (int,int) -> int { 
  cs inline "(#1 * #2)"; 
  js "$std_core.intMultiply" 
}

function compare( x : int, y : int) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// Return the absolute value of an integer.
// Raises an exception if the ":int" is "minInt"
// (since the negation of "minInt" equals itself and is still negative)
function abs( i : int ) : exn int
{
  if (i >= 0) return i
  if (i > minInt) return negate(i)
  error( "system.core.abs: cannot make minInt into a positive int without overflow" )
}

// Take the bitwise _and_ of two ":int"s
external inline (&) : (int,int) -> int { 
  cs inline "((#1)&(#2))";
  js inline "((#1)&(#2))"
}
  
// Take the bitwise _or_ of two ":int"s
external inline (|) : (int,int) -> int { 
  cs inline "((#1)|(#2))";
  js inline "((#1)|(#2))";
}

// Take the bitwise _xor_ of two ":int"s
external inline (^) : (int,int) -> int { 
  cs inline "((#1)^(#2))";
  js inline "((#1)^(#2))"
}

// Bitwise _not_ of an ":int", i.e. flips all bits.
external inline not : ( i : int ) -> int  { 
  cs inline "(~(#1))";
  js inline "(~(#1))"
}

private external inline primShl : (int,int) -> int { 
  cs inline "((#1)<<(#2))";
  js inline "((#1)<<(#2))";
}

private external inline primShr : (int,int) -> int { 
  cs inline "(int)((uint)(#1)>>(#2))";
  js inline "((#1)>>>(#2))";
}

private external inline primSar : (int,int) -> int { 
  cs inline "((#1)>>(#2))";
  js inline "((#1)>>(#2))";
}

// Bitwise rotate an ":int" "n" bits to the left.
// Does not rotate when "n" is negative.
function rotl( i : int, n : int ) : int {
  if (n < 0) return i;
  val r = n & (bitsInt - 1)  // modulo bits in an int
  (primShl(i,r) | primShr(i,bitsInt - r))
}

// Bitwise rotate an ":int" "n" bits to the right.
// Does not rotate when "n" is negative.
function rotr( i : int, n : int ) : int {
  if (n < 0) return i;
  val r = n & (bitsInt - 1)  // modulo bits in an int
  (primShr(i,r) | primShl(i,bitsInt - r))
}

// Shift an ":int" "i" to the left by "n" bits.
// Returns "i" when "n" is negative, or "0" when "n >= bitsInt".
function shl( i : int, n : int ) {
  if (n < 0) return i
  if (n >= bitsInt) return 0
  primShl(i,n)
}

// Logical shift an ":int" to the right by "n" bits. Shift in zeros from the left.
// Returns "i" when "n" is negative, or "0" when "n >= bitsInt".
function shr( i : int, n : int ) {
  if (n < 0) return i;
  if (n >= bitsInt) return 0;
  primShr(i,n);
}

// Arithmetic shift an ":int" to the right by "n" bits. Preserves the sign bit.
// Returns "i" when "n" is negative. When "n >= bitsInt" returns either "-1" when "i<0", or "0" when "i>=0".
function sar( i : int, n : int ) {
  if (n < 0) return i;
  if (n >= bitsInt) return (if (i < 0) then (0-1) else 0);
  primSar(i,n);
}



// Return the minimum of two integers
function min( i : int, j : int ) : int =  if (i <= j) then i else j
// Return the maximum of two integers
function max( i : int, j : int ) : int =  if (i >= j) then i else j


// Returns the smallest element of a list of integers (or "0" for the empty list)
function minimum( xs : list<int> ) : int
{
  match(xs) {
    Nil -> 0 
    Cons(x,xx) -> xx.foldl( x, min )
  }
}

// Returns the largest element of a list of integers (or "0" for the empty list)
function maximum( xs : list<int> ) : int
{
  match(xs) {
    Nil -> 0 
    Cons(x,xx) -> xx.foldl( x, max )
  }
}

// Do not use. Used by the compiler for platforms that do not support 32 bit truncated division natively.
external intCdiv : (int,int) -> exn int {
  cs inline "(#1 / #2)"
  js "$int32_cdiv"
}

// Do not use. Used by the compiler for platforms that do not support 32 bit truncated modulus natively.
external intCmod : (int,int) -> exn int {
  cs inline "(#1 % #2)"
  js "$int32_cmod"
}

// Truncated division (as in C). See also "(/):(x : int, y : int) -> int".
external inline cdiv : (int,int) -> exn int { 
  cs inline "(#1/#2)";
  js "$std_core.intCdiv"
}

// Truncated modulus (as in C). See also "(%):(x : int, y : int) -> int".
external inline cmod : (int,int) -> exn int { 
  cs inline "(#1 % #2)";
  js "$std_core.intCmod"
}

// Truncated division (as in C). See also "(/):(x : int, y : int) -> int".
private external inline unsafeCdiv : (int,int) -> int {
  cs inline "(#1/#2)";
  js inline "((#1/#2)|0)"
}

// Truncated modulus (as in C). See also "(%):(x : int, y : int) -> int".
private external inline unsafeCmod : (int,int) -> int { 
  cs inline "(#1 % #2)";
  js inline "((#1 % #2)|0)"
}

// Convert an integer to a double.
external inline double : (int) -> double { cs inline "(double)(#1)"; js inline "(#1)" }

// Negate an integer
function negate( i : int ) {
  return (0 - i)
}

// Negate an integer
function (~)(i : int) : total int {
  return (0 - i)
}

// The maximal integer value before overflow happens
val maxInt = 0x7FFFFFFF

// The minimal integer value before underflow happens
val minInt = 0 - maxInt - 1

// The number of bits in an ":int" (always 32)
val bitsInt = 32

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
// * ``D/(2^n) == D `sar` n        ``  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
// * `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
//
// Note that an interesting edge case is "minInt / -1" which equals "minInt" since in modulo 32-bit 
// arithmetic "minInt == -1 * minInt == -1 * (minInt / -1) + (minInt % -1)" satisfying property (1).
// Of course "(minInt + 1) / -1" is again positive (namely "maxInt").  
// 
// See also _Division and modulus for computer scientists, Daan Leijen, 2001_ for further information
// available at: <http://research.microsoft.com/pubs/151917/divmodnote.pdf> .
function (/)( x : int, y : int ) : int
{
  if (y == 0) then return 0
  val q = unsafeCdiv(x,y)
  val r = unsafeCmod(x,y)
  if (r >= 0) then q
  elif (y > 0) then q-1
  else q+1
}

// Euclidean-0 modulus. See "(/):(x : int, y : int) -> int" division for more information.
function (%)( x : int, y : int ) : int
{
  if (y == 0) then return x
  val r = unsafeCmod(x,y)
  if (r >= 0) then r
  elif (y > 0) then (r+y)
  else (r - y)
}

// Returns "true" if the integer `i`  is an odd number.
function odd( i ) {
  return (i&1==1);
}

// Returns "true" if the integer `i`  is an even number.
function even( i ) {
  return (i&1==0);
}

// Return the sum of a list of integers
function sum( xs : list<int> ) : int
{
  xs.foldl( 0, fun(x,y) { x + y } )
}

// Parse an integer using `parseInt`. If an illegal digit character is encountered the 
// `default` value is returned. An empty string will also result in `default`.
function parseIntDefault( s : string, default : int = 0, hex : bool = False ) : int {
  if (s.isEmpty) then default else {
    match( s.parseInt(hex) ) {
      Nothing -> default
      Just(i) -> i
    }
  }
}

// Parse an integer. If an illegal digit character is encountered "Nothing" is returned.
// An empty string will result in "Just(0)".
// A string can start with a `-` sign for negative numbers, 
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
function parseInt( s : string, hex : bool = False) : maybe<int> {
  val base = if (hex) then 16 else 10
  match(s.list.dropWhile(isWhite)) {
    Nil -> Just(0)
    Cons(c,cc) | c=='-' -> {
      match(parsePosInt(cc,base)) {
        Nothing -> Nothing
        Just(i) -> Just(0-i)
      }
    }
    cs -> parsePosInt(cs,base)     
  }
}

private function parsePosInt( s : list<char>, base : int = 10 ) : maybe<int> {
  match(s) {
    Cons(c1,Cons(c2,cs)) | c1=='0' && (c2=='x' || c2=='X')
      -> parseDigits(cs,16)
    _ -> parseDigits(s,base)
  }
}

// Parse digits in a "base" between 2 and 36 (default 10) given an initial value "acc" (default 0)
// Returns "acc" on the empty string, and "Nothing" if an invalid digit is encountered.
private function parseDigits( cs : list<char>, base : int = 10, acc : int = 0 ) : maybe<int> {
  match(cs) {
    Nil -> Just(acc)
    Cons(c,cc) -> {
      val d = if (c >= '0' && c <= (if (base >= 10) then '9' else ('0' + (base-1).char))) then {
                (c - '0').int
              }
              elif (base > 10 && base <= 36 && c >= 'a' && c <= ('a' + (base-11).char)) then { 
                (c - 'a').int + 10
              }
              elif (base > 10 && base <= 36 && c >= 'A' && c <= ('A' + (base-11).char)) then { 
                (c - 'A').int + 10
              }
              else return Nothing   
      parseDigits(cc, base, base*acc + d)
    }
  }
}


public function parseFixed( s : string ) : maybe<double> {
  match(s.split(".",2).list) {
    Cons(pre,Cons(post,Nil)) -> {
      match(pre.parseInt) {
        Nothing -> Nothing
        Just(d) -> match(post.parseInt) {
          Nothing -> Nothing
          Just(f) -> {
            if (f < 0) then Nothing
            elif (f == 0) then Just(d.double)
            else Just( d.double + (10.0^(post.length.double) / f.double) )
          }
        }
      }
    }
    _ -> match(s.parseInt) {
      Nothing -> Nothing
      Just(i) -> Just(i.double)
    }
  }
}

public function parseFixedDefault( s : string, default : double = 0.0 ) : double {
  match(s.parseFixed) {
    Nothing -> default
    Just(d) -> d
  }
}


// ----------------------------------------------------------------------------
// Doubles
// ----------------------------------------------------------------------------

external inline (==) : (double,double) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
external inline (!=) : (double,double) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
external inline (<=) : (double,double) -> bool  = inline "(#1 <= #2)"
external inline (>=) : (double,double) -> bool  = inline "(#1 >= #2)"
external inline (<)  : (double,double) -> bool  = inline "(#1 < #2)"
external inline (>)  : (double,double) -> bool  = inline "(#1 > #2)"

external inline (+) : (double,double) -> double = inline "(#1 + #2)"
external inline (-) : (double,double) -> double = inline "(#1 - #2)"
external inline (*) : (double,double) -> double = inline "(#1 * #2)"
external inline (/) : (double,double) -> double = inline "(#1 / #2)"
external inline (%) : (double,double) -> double = inline "(#1 % #2)"

function compare( x : double, y : double) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}


// Convert a ":double" to ":int" using "round" to round the double to its nearest integer.
// if the double is larger than "maxInt" or smaller than "minInt" just the lower 32 bits are returned.
external inline unsafeInt : (double) -> int { cs inline "(int)(#1)"; js inline "(#1 | 0)" }

private external inline getNan : () -> double { 
  cs inline "double.NaN";
  js inline "NaN"
}

private external inline getNegInf : () -> double { 
  cs inline "double.NegativeInfinity";
  js inline "(- Infinity)"
}

private external inline getPosInf : () -> double { 
  cs inline "double.PositiveInfinity";
  js inline "Infinity"
}

private external inline getMaxDouble : () -> double { 
  cs inline "double.MaxValue";
  js inline "(1.7976931348623157e+308)"
}

private external inline getMinDouble : () -> double { 
  cs inline "double.MinValue";
  js inline "-(1.7976931348623157e+308)"
}

private external inline getEpsilon : () -> double { 
  cs inline "double.Epsilon";
  js inline "(4.94065645841247e-324)"
}

// convert a ":double" to an ":int" using "round" to round to its nearest integer.
// If the double is larger than "maxInt" or smaller than "minInt" this returns "default" (=0)
public function int(d : double, default : int = 0 ) : int {
  if (d > maxInt.double || d < minInt.double)
   then default
   else d.unsafeInt
}

// Represents a value that is _not a number_ (NaN)
val nan : double = getNan()

// Positive infinity
val posInf : double = getPosInf()

// Negative infinity
val negInf : double = getNegInf()

// The maximal double value
val maxDouble : double = getMaxDouble()

// The minimal double value
val minDouble : double = getMinDouble()

// The number of bits in a ":double" (64)
val bitsDouble = 64

// Is this value equal to NaN ?
external inline isNan : (double) -> bool {
  cs "double.IsNaN"
  js "isNaN"
} 

// Is this value equal to negative or positive infinity ?
external isInf : (double) -> bool {
  cs "double.IsInfinity"
  js inline "((#1) === Number.POSITIVE_INFINITY || (#1) === Number.NEGATIVE_INFINITY)"
}

// Is this value equal to positive infinity ?
external inline isPosInf : (double) -> bool {
  cs "double.IsPositiveInfinity"
  js inline "((#1) === Number.POSITIVE_INFINITY)"
}


// Is this value equal to negative infinity ?
external inline isNegInf : (double) -> bool {
  cs "double.IsNegativeInfinity"
  js inline "((#1) === Number.NEGATIVE_INFINITY)"
}


// Returns the value `d`  raised to the power `p` .
external inline (^) : (d:double,p:double) -> double {
  cs "Math.Pow"
  js inline "((#1) ^ (#2))"
}

// Return the square root of a value `d` 
// Returns "nan" if `d == nan`  or if `d`  is negative.
// Returns "inf" if `d == inf` .
external inline sqrt : (d:double) -> double {
  cs "Math.Sqrt"
  js "Math.sqrt"
}

// Return the absolute value of a ":double" `d` 
external inline abs : (d:double) -> double  { 
  cs "Math.Abs"
  js "Math.abs" 
}

// Return the largest integer equal or less than `d` 
external inline floor : (d:double) -> double {
  cs "Math.Floor"
  js "Math.floor"
}

// Return the smallest integer equal or larger than `d` 
external inline ceiling : (d:double) -> double {
  cs "Math.Ceiling"
  js "Math.ceiling"
}

// Return the integral part of a ":double" `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
external inline trunc : (d:double) -> double {
  cs "Math.Truncate"
  js "Math.trunc"
}

// Round a double to its nearest integral value.
// If the value is halfway between two integers, the value is rounded to the even one.
external inline round : (d:double) -> double {
  cs "Math.Round"
  js "Math.round"
}

// Return the sum of a list of doubles
function sum( xs : list<double> ) : double
{
  xs.foldl( 0.0, (+))
}

// Returns the smallest of two doubles
function min( x : double, y : double ) : double = if (x <= y) then x else y

// Returns the largest of two doubles
function max( x : double, y : double ) : double = if (x >= y) then x else y

// Returns the smallest element of a list of doubles (or "0" for the empty list)
function minimum( xs : list<double> ) : double
{
  match(xs) {
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, min )
  }
}

// Returns the largest element of a list of doubles (or "0" for the empty list)
function maximum( xs : list<double> ) : double
{
  match(xs) {
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, max )
  }
}

// ----------------------------------------------------------------------------
// Arrays
// ----------------------------------------------------------------------------

// The type of mutable arrays. See also ":vector" for immutable arrays.
type array :: (H,V) -> V

// used internally to have non-bound-checked access
private external inline unsafeIdx : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>|e> a with(hdiv<h,a,e>) {
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

private external inline unsafeAssign : forall<h,a> ( a : array<h,a>, i : int, x : a ) -> write<h> () {
  cs inline "(#1)[#2] = #3";
  js inline "(#1)[#2] = #3";
}

// Return the "index" element of an array. Raises an exception for out-of-bounds access.
external inline [] : forall<h,a,e> ( self : array<h,a>, index : int ) -> <read<h>,exn|e> a with(hdiv<h,a,e>) 
  = inline "(#1)[#2]"

// Return the length of an ":array".
external inline length : forall<h,a> ( self : array<h,a> ) -> int { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length"
}

// Assign to element `i`  in an array `a` . May raise an out-of-bounds exception
external inline [] : forall<h,a> ( a : array<h,a>, i : int, assigned : a ) -> <write<h>,exn> ()  
  = inline "(#1)[#2] = #3"

// Create a new array of length `n`  with initial elements `a` .
external inline array  : forall<h,a> (n : int, default : a) -> array<h,a> { 
  cs inline "Primitive.NewArray<##2>(#1,#2)";
  js inline "Array(#2)"
}
 
// Create a zero length array
external inline array : forall<h,a> () -> array<h,a> = cs inline "new ##2[0]"

private external inline unsafeArray  : forall<h,a> (n : int) -> array<h,a> { 
  cs inline "new ##2[#1]";
  js inline "Array(#1)";
}

private external inline arrayCopy    : forall<h,a> ( src : array<h,a>, dest : array<h,a>, len : int ) -> read<h> ()  { 
  cs inline "Array.Copy(#1,#2,#3)";
  js inline "error('arrayCopy')"
}

private external inline unsafeFreeze : forall<h,a> ( self : array<h,a> ) -> read<h> vector<a> { 
  cs inline "(#1)";
  js inline "(#1)";
}

// Copy an array
function copy( self : array<h,a> ) : read<h> array<h,a> {
  val dest = unsafeArray(self.length)
  arrayCopy(self,dest,self.length)
  return dest
}

// Convert an array to a vector (to guarantee safety, a copy is made)
function vector( self : array<h,a> ) : read<h> vector<a>
{
  copy(self).unsafeFreeze()
}


// Convert a list to an array.
function array( xs : list<a> ) : array<h,a>
{
  unsafeTotal {
    val n = xs.length
    val a = unsafeArray(n)
    xs.foreachIndexed fun(i,x) {
      a[i] := x
    }
    a
  }
}

// Convert an array to a list
function list( a : array<h,a> ) : read<h> list<a>
{
  unsafeTotal {
    val len = a.length()
    function build( i : int, acc : list<_a> )
    {
      if (i >= 0) then build(i-1, Cons(a[i],acc)) else acc
    }

    build(len-1,Nil)
  }
}

// Invoke a function "f" for each element in a an array `a`.   
// Note: this can diverge by storing self referential functions in the array
function foreach( a : array<h,a>, f : a -> <read<h>,div|e> () ) : <read<h>,div|e> ()
{
  for( 0, a.length-1 ) fun(i) {
    f(a.unsafeIdx(i))
  }
}

// Destructively apply function `f`  to each element in an array `a`.   
// Note: this can diverge by storing self referential functions in the array
function map( a : array<h,a>, f : a -> <st<h>,div|e> a ) : <st<h>,div|e> array<h,a>
{
  for( 0, a.length-1 ) fun(i) {
    a.unsafeAssign( i, f(a.unsafeIdx(i)) )
  }
  a
}

// ----------------------------------------------------------------------------
//  Vectors
// ----------------------------------------------------------------------------

// The type of immutable arrays is called ":vector".
type vector<a>

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
private external inline unsafeIdx : forall<a> ( v : vector<a>, index : int ) -> total a { 
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
external inline [] : forall<a> ( v : vector<a>, index : int ) -> exn a  = inline "(#1)[#2]"

// Return the length of a vector.
external inline length : forall<a> (v : vector<a> ) -> int  { 
  cs inline "(#1 == null ? 0 : (#1).Length)";
  js inline "(#1).length";
}

// Create a new vector of length `n`  with initial elements `default` .
public function vector(n : int, default : a) : vector<a> {
  list(1,n).map( fun(i){ default }).vector
} 

// Create an empty vector.
external inline vector : forall<a> () -> vector<a> {
  cs inline "new ##1[0]"
  js inline "[]"
}

private external inline unsafeUnfreeze : forall<h,a> (v : vector<a>) -> array<h,a> = inline "(#1)"

// Invoke a function "f" for each element in a vector "v"
function foreach( v : vector<a>, f : a -> e () ) : e ()
{
  for( 0, v.length-1 ) fun(i) {
    f(v.unsafeIdx(i))
  }
}

// Invoke a function "f" for each element in a vector "v".
// If "f" returns "Just", the iteration is stopped early and the result is returned.
function foreachWhile( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b>
{
  forWhile( 0, v.length-1 ) fun(i) {
    f(v.unsafeIdx(i))
  }
}

// Apply function "f" to each element in a vector "v"Ju
function map( v : vector<a>, f : a -> e b ) : e vector<b>
{
  val n = v.length
  val a = unsafeArray(n)
  for( 0, v.length-1 ) fun(i) {
    a.unsafeAssign( i, injectSt(f)(v.unsafeIdx(i)) )
  }
  a.unsafeFreeze
}

// Convert a vector to a list.
function list( v : vector<a> ) : list<a>
{
  unsafeTotal{
    v.unsafeUnfreeze.list
  }
}

// Convert a list to a vector.
function vector( xs : list<a> ) : vector<a>
{
  xs.array.unsafeFreeze
}

// Convert a vector to an array (by making a copy)
external inline array : forall<h,a> ( v : vector<a> ) -> array<h,a> { 
  cs inline "(##2[])Array.Clone(#1)";
  js inline "#1"
}


// ----------------------------------------------------------------------------
// Strings
// ----------------------------------------------------------------------------

// Return the `n` th character in a string `s` .
private external inline unsafeIdx : ( s : string, n : int ) -> char  = inline "(#1)[#2]"

// Are two strings equal?
external inline (==) : (string,string) -> bool {
  inline "(#1 == #2)"
  js inline "(#1 === #2)"
}

// Are two strings not equal?
external inline (!=) : (string,string) -> bool {
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"
}

private external stringCompare : ( x : string, y : string ) -> int
{
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"
}

// Compare two strings
function compare( x : string, y : string) : order
{
  stringCompare(x,y).order;
}

function (>=)( x : string, y : string ) : bool = compare(x,y) > Lt
function (<=)( x : string, y : string ) : bool = compare(x,y) < Gt
function (>)( x : string, y : string ) : bool  = compare(x,y) == Gt
function (<)( x : string, y : string ) : bool  = compare(x,y) == Lt

// Append two strings.
external inline (+) : (string,string) -> string = inline "(#1 + #2)"

// Choose a non-empty string
function (||)( x : string, y : string ) : string {
  if (x.isEmpty) then y else x 
}

// Return the `n` th character in a string `s` . 
// Raise an out-of-bounds exception if "n < 0" or "n >= s.length".
external inline [] : ( s : string, n : int ) -> exn char  = inline "(#1)[#2]"

// Return the length of a string.
external inline length : ( s : string ) -> int  { 
  cs inline "(#1).Length";
  js inline "(#1).length";
}

// Does string `s`  contain the character `c`  ?
external inline indexOf : (s : string, c : char ) -> int { 
  cs inline "(#1.IndexOf(#2))";
  js inline "((#1).indexOf(#2))";
}

// Does string `s`  contain the character `c`  ?
external inline lastIndexOf : (s : string, c : char ) -> int { 
  cs inline "(#1.LastIndexOf(#2))";
  js inline "((#1).lastIndexOf(#2))";
}

// Does string `s`  contain the string `sub`  ?
external inline indexOf : (s : string, sub : string ) -> int { 
  cs inline "(#1.IndexOf(#2))";
  js inline "((#1).indexOf(#2))";
}

// Does string `s`  contain the string `sub`  ?
external inline lastIndexOf : (s : string, sub : string ) -> int { 
  cs inline "(#1.LastIndexOf(#2))";
  js inline "((#1).lastIndexOf(#2))";
}

// Does string `s`  contain the character `c`  ?
external inline contains : (s : string, c : char ) -> bool { 
  cs inline "(#1.IndexOf(#2) >= 0)";
  js inline "((#1).indexOf(#2) >= 0)";
}

// Is `pre`  a prefix of `s`  ?
external startsWith: (s : string, pre : string ) -> bool { 
  cs inline "#1.StartsWith(#2)";
  js inline "#1.substr(0,#2.length) === #2"
}

// Does string `s`  end with `post`  ?
external endsWith: (s : string, post : string ) -> bool { 
  cs inline "#1.EndsWith(#2)";
  js inline "((#1).indexOf(#2, (#1).length - (#2).length) !== -1)"
}

// Does string `s`  contain the string `sub`  ?
external inline contains: (s : string, sub : string ) -> bool { 
  cs inline "((#1).Contains(#2))";
  js inline "((#1).indexOf(#2) >= 0)"
}

// Concatenate a vector of strings
external inline join: (v : vector<string> ) -> string { 
  cs "String.Concat";
  js inline "((#1).join(''))"
}

// Concatenate a vector of strings with a separator "sep"
external inline join: (v : vector<string>, sep : string ) -> total string { 
  cs "Primitive.Concat";
  js inline "((#1).join(#2))"
}

// Concatenate an array of strings
external inline join: forall<h> (a : array<h,string> ) -> read<h> string { 
  cs inline "String.Concat(#1)";
  js inline "((#1).join(''))";
}

// Concatenate an array of strings with a separator "sep"
external join: forall<h> (v : array<h,string>, sep : string ) -> read<h> string { 
  cs inline "Primitive.Concat(#1,#2)";
  js inline "(#1).join(#2)"
}

public external inline substr : (s : string, start : int ) -> string { 
  cs inline "((#1).Substring(#2))";
  js inline "((#1).substr(#2))";
}


private external inline substr2 : (s : string, start : int, len : int ) -> string { 
  cs inline "#1.Substring(#2,#3)";
  js inline "#1.substr(#2, #3)"
}

private external inline makeString : (n : int, c : char ) -> string { 
  cs inline "new String(#2,#1)";
  js inline "Array(#1 + 1).join(#2)";
}

// Construct a string of `n`  characters `c`  (or the empty string if `n <= 0` )
function string( n : int, c : char ) : string 
{
  if (n <= 0) then "" else makeString(n,c)
}

// Convert a ":maybe" string to a string using the empty sting for "Nothing"
function mbstring( ms : maybe<string> ) : string {
  match(ms) {
    Nothing -> ""
    Just(s) -> s
  }
}

// Is a string empty?
function isEmpty( s : string ) : bool {
  s == ""
}

// Is a string not empty?
function notEmpty( s : string ) : bool {
  s != ""
}

/*
function substr1( s : string, start : int, len : int = maxInt   )
{
  if (len <= 0) then ""
   else substr2(s, if (start <= 0) then 1 else start, len)
}
*/

public external inline substr1 : (s : string, start : int ) -> string { 
  cs inline "((#1).Substring(#2))";
  js inline "((#1).substr(#2 >=1 ? #2 : 1))";
}


// Return a substring from start position "start" up to either the end of the string, 
// or of at most length "len"
function substr( s : string, start : int, len : int   )
{
  if (len <= 0) then ""
   else substr2(s,start,len)
}

private external inline splitv : ( s : string,  sep : string ) -> vector<string> { 
  cs inline "#1.Split(new String[]{#2}, StringSplitOptions.None)";
  js inline "(#1).split(#2)"
}

private external inline splitv : ( s : string,  sep : string, n : int ) -> vector<string> { 
  cs inline "#1.Split(new String[]{#2},#3, StringSplitOptions.None)";
  js inline "(#1).split(#2, #3)"
}

// Split a string into parts that were delimited by "sep". The delimeters are not included in the results.
// For example: "split(""1,,2"","","") == [""1"","""",""2""]"

function split( s : string, sep : string ) : vector<string>
{
  splitv(s,sep)
}

// Split a string into at most "n" parts that were delimited by a string "sep". The delimeters are not included in the results (except for possibly the final part).
// For example: "split(""1,2,3"","","",2) == [""1"",""2,3""]"
function split( s : string, sep: string, n : int) : vector<string>
{
  splitv(s,sep,n)
}

// Replace every occurrence of "pattern" to "repl" in a string.
public external inline replaceAll( s : string, pattern : string, repl : string ) : string { 
  cs inline "(#1).Replace(#2,#3)";
  js inline @"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)";  
}

// Count occurences of "pattern" in a string.
public external inline count( s : string, pattern : string ) : int { 
  cs inline "Primitive.Count(#1,#2)";
  js inline @"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)";  
}

// Convert a string to a vector of characters.
external inline vector : ( s : string ) -> vector<char> { 
  cs inline "#1.ToCharArray()";
  js inline "#1"
}

// Convert a string to an array of characters.
external inline array : forall<h> ( s : string ) -> array<h,char> { 
  cs inline "#1.ToCharArray()";
  js inline "#1"
}

// Convert an array of characters to a string.
external inline string : forall<h> (array<h,char>) -> read<h> string { 
  cs inline "new String(#1)";
  js inline "#1"
}

// Convert a vector of characters to a string.
external inline string : (vector<char>) -> string { 
  cs inline "new String(#1)";
  js inline "#1"
}

// Convert a string to a list of characters
function list( s : string ) : total list<char>
{
  list(s.vector())
}

// Convert a list of characters to a string
function string( cs : list<char> ) : total string
{
  cs.map( fun(c){ c.string } ).join()
}

// Apply a function "f" to each character in a string
function map( s : string, f : char -> e char ) : e string
{
  s.list().map(f).string()
}

// Invoke a function for each character in a string
function foreach( s : string, f : char -> e () ) : e ()
{
  for( 0, s.length-1 ) fun(i) {
    f(s.unsafeIdx(i))
  }
}

// Count the number of occurrences of a character in a string
function count( s : string, c : char ) : int
{
  var cnt := 0
  s.foreach fun(d) {
    if (c == d) then cnt := cnt+1
  }
  cnt
}

// Count the number of times a predicate is true for each character in a string
function count( s : string, pred : (char) -> bool ) : int
{
  var cnt := 0
  s.foreach fun(c) {
    if (pred(c)) then cnt := cnt+1
  }
  cnt
}


function lines( s : string ) : vector<string>
{
  s.split("\n")
}

function unlines( xs : vector<string> ) : string
{
  xs.join("\n") 
}

external toUpper : (s : string) -> string { 
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()" 
}

external toLower : (s : string) -> string { 
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()" 
}

function capitalize( s : string ) : string {
  s.substr(0,1).toUpper + s.substr(1)
}




// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic print routine: prints the internal representation as a string to the console, including a final newline character.  
// Note: this breaks parametricity so it should not be public
private function gprintln( x : a ) : console () {
  println( x.gshow )
}

// Generic print routine: prints the internal representation as a string to the console, 
// including a final newline character.  
// Note: this breaks parametricity so it should not be public
private function gprint( x : a ) : console () {
  print( x.gshow )
}


// Generic show: shows the internal representation of an object as a string  
// Note: this breaks parametricity so it should not be public
private external gshow : forall<a> a -> string { 
  cs inline "#1.ToString()";
  js inline "(#1).toString()"
}

// Generic formatting
private external inline gformat : forall<a> (value:a,format:string) -> string { 
  cs inline "#1.ToString(#2)";
  js inline "$gformat(#1, #2)";
}

// Convert an ":int" to a string
function show( i : int ) : string 
{ 
  i.gshow()  
}

// Show an ":int" as a hexadecimal value.
// The `width`  parameter specifies how wide the hex value is where `0`  is used to align
// Uses capital letters for hexadecimal digits by default.
function showHex( i : int, width : int = 1, useCapitals : bool = True )
{
  val w = if (width < 0) then 0 else width
  val x = if (useCapitals) then "X" else "x"
  gformat(i, x + w.show)
}

// Show a ":double" as a string with a default `precision`  of `0` .
// Pass a precision of `0`  (or less) to display numbers precisely.
// Uses either "showFixed" or "showExp" depending on what is the most compact representation
function show( d : double, precision : int = 6 ) : string
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"g" + show(p))
}

function showFixed( d : double, precision : int = 6 )
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"F" + show(p))
}

// Show a ":double" in exponential (scientific) notation 
function showExp( d : double, precision : int = 6 )
{
  val p = if (precision < 0) then 0 else precision
  gformat(d,"e" + show(p))
}

// Convert a character to a oneElement string
external inline string : ( c : char ) -> string  { 
  cs inline "(new String(#1,1))";
  js inline "(#1)"
}

private function showChar( c : char ) : string
{
  if (c < ' ' || c > '~') {
    if (c == '\n') then "\\n"
    elif (c == '\r') then "\\r"
    elif (c == '\t') then "\\t"
    elif (c.int <= 0xFF) then "\\x" + c.int.showHex(2)
    elif (c.int <= 0xFFFF) then "\\u" + c.int.showHex(4)
    else "\\U" + c.int.showHex(6)
  }
  else {
    if (c == '\'') then "\\'"
    elif (c == '\"') then "\\\""
    elif (c == '\\') then "\\\\"
    else c.string
  }
}

// Show a ":char" as a char literal
function show( c : char ) : string
{
  "'" + c.showChar + "'"
}

// Show a string as a string literal
function show( s : string ) : string
{
  "\"" + s.list.map(showChar).join + "\""
}

private val redirect : ref<global,maybe<(string) -> console ()>> = unsafeTotal { ref(Nothing) }

// redirect "print" and "println" calls to a specified function.
public function printRedirect( print : (msg : string) -> console () ) : io ()
{
  redirect := Just(print);
}

// Print a string to the console, including a final newline character.
private external xprintsln : string -> console ()  { 
  cs "Console.WriteLine";
  js "$println"
}

// Print a string to the console
private external xprints : string -> console ()  { 
  cs "Console.Write";
  js "$print"
}

private function prints( s : string ) : console () {
  unsafeTotal { 
    match(!redirect) {
      Nothing -> xprints(s)
      Just(f) -> f(s)
    }
  }
}

private function printsln( s : string ) : console () {
  unsafeTotal { 
    match(!redirect) {
      Nothing -> xprintsln(s)
      Just(f) -> f(s + "\n")
    }
  }
}

// Print a string to the console.
function print(s : string)  { prints(s) }

// Print an integer to the console.
function print(i : int)     { prints(show(i)) }

// Print a character to the console.
function print(c : char)    { prints(string(c)) }

// Print a double to the console.
function print(d : double)  { prints(show(d)) }

// Print a boolean to the console
function print( b : bool )  { prints(show(b)) }

// Print a unit value to the console
function print( u : () )  { prints(show(())) }


// Print a string to the console, including a final newline character.
function println(s : string)  { printsln(s) }

// Print an integer to the console, including a final newline character.
function println(i : int)     { printsln(show(i)) }

// Print a character to the console, including a final newline character.
function println(c : char)    { printsln(string(c)) }

// Print a double to the console, including a final newline character.
function println(d : double)  { printsln(show(d)) }

// Print a boolean to the console, including a final newline character
function println( b : bool )  { printsln(show(b)) }

// Print a unit value to the console, including a final newline character
function println( u : () )  { printsln(show(())) }


private external xtrace : ( message : string ) -> ()  {
  cs "Primitive.Trace"
  js "$trace"
}

private val traceEnabled : ref<global,bool> = unsafeTotal { ref(True) }

// Trace a message used for debug purposes. 
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default. 
// Disabled if "notrace" is called.
public function trace( message : string ) : () {
  unsafeTotal {
    if (!traceEnabled) then xtrace(message) 
  }    
}

// Disable tracing completely.
public function notrace() : st<global> () {
  traceEnabled := False
}

// Convert a ":bool" to a string
function show( b : bool ) : string =  if (b) then "True" else "False"

// Convert a unit value "()" to a string
function show( u : () ) : string =  "()"

// Convert a list to a string
function show( xs : list<a>, showElem : (a) -> string ) : string
{
  "[" + xs.map(showElem).join(",") + "]"
}


// Right-align a string to width `width`  using `char`  (default is a space) to fill from the left.
function align( s : string, width : int, char : char = ' ') : string
{
  val n = s.length
  if (n >= width)
   then s
   else string(width - n, char) + s
}

// Left-align a string to width `width`  using `char`  (default is a space) to fill on the right.
function fill( s : string, width : int, char : char = ' ') : string
{
  val n = s.length
  if (n >= width)
   then s
   else s + string(width - n, char)
}

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

// Represents values that are computed asynchronously
type async<a>

// When an asynchronous value is computed, call the specified "handler" (at most once).
external on : forall<a,b,e> ( async : async<a>, handler : (value : a) -> e b ) -> e async<b> {
  js "async_on"
  cs inline "#1.On<##2>(#2)"
}

// When an asynchronous value computation fails, call the specified "handler" (at most once).
external onExn : forall<a,e> ( async : async<a>, handler : (exn : exception) -> e a) -> e async<a> {
  js "async_on_exn"
  cs inline "#1.OnExn<##1>(#2)"
}

// Has an asynchronous value been computed?
external isDone : forall<a> ( async : async<a> ) -> bool {
  js "async_is_done"
  cs inline "#1.IsDone"
}

// Read a line from the console.
external readln : forall<e> () -> <console|e> async<string>  { 
  js "$read_line"
  cs inline "Primitive.ReadLine()"
}

// Read a line from the console and immediately supply a handler 
// For convenience, returns a unit value. Use an explicit "on" if
// the returned ":async" is needed.
function readln( handler : (string) -> <console|e> a ) : <console|e> () 
{
  readln().on(handler)
  return ()
}

// Displays "query" to the user (with a space attached) and invokes "handler" with 
// the user's response. 
function question( query : string, handler : (input : string) -> <console|e> a ) : <console|e> ()
{
  print(query + " ")
  readln( handler )
}

 
// Used by the compiler to wrap main console applications
external mainConsole : forall<a,e> ( main : () -> e a ) -> e a {
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"
}

// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

// The predicate ":hdiv<h,a,e>" signifies that if the type ":a" contains a reference to ":h",
// then the effect ":e" must contain the divergent effect (":div"). This constraint is generated
// when writing to the heap (see "(!)") and is necessary to catch cases where code can diverge
// by storing selfReferential functions in the heap.
type hdiv :: (H,V,E) -> P

// A reference ":ref<h,a>" points to a value of type ":a" in heap ":h".
type ref :: (H,V) -> V

// Allocate a fresh reference with an initial value.
external inline ref : forall<h,a> (value:a) -> alloc<h> ref<h,a> { 
  cs inline "new Ref<##1,##2>(#1)";
  js inline "{ value: #1 }"
}

// Assign a new value to a reference.
external inline set  : forall<h,a> (ref: ref<h,a>, assigned: a) -> <write<h>> () { 
  cs inline "#1.Set(#2)";
  js inline "((#1).value = #2)"
}

// Read the value of reference.
external inline (!) : forall<h,a,e> (ref: ref<h,a>) -> <read<h>|e> a with(hdiv<h,a,e>)  { 
  cs inline "#1.Value";
  js inline "((#1).value)";
}

// If a heap effect is unobservable, the heap effect can be erased by using the "run" function.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
external inline run : forall<e,a> ( action : forall<h> () -> <st<h> | e> a ) -> e a  { 
  cs inline "Primitive.Run<##2>(#1)";
  js inline "((#1)())";
}


// ----------------------------------------------------------------------------
// Unsafe
// ----------------------------------------------------------------------------

private external inline unsafeTotalCast : forall<e,a> ( action : () -> e a ) -> (() -> total a) = inline "#1"

// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
function unsafeTotal( action : () -> e a ) : total a
{
  unsafeTotalCast( action )()
}


// _Unsafe_. This function removes the exception effect (":exn") from the effect of an action
function unsafeNoExn( action : () -> <exn|e> a ) : e a
{
  unsafeTotal( action )
}

// _Unsafe_. This function removes the non-termination effect (":div") from the effect of an action
function unsafeNoDiv( action : () -> <div|e> a ) : e a
{
  unsafeTotal( action )
}

// ----------------------------------------------------------------------------
// Non determinism
// ----------------------------------------------------------------------------

private val uniqueCount : ref<global,int> = unsafeTotal { ref(0) }

// Returns a unique integer (modulo 32-bits).
function unique() : ndet int
{
  unsafeTotal {
    val u = !uniqueCount
    uniqueCount := u+1
    u
  }
}

// Returns one of its arguments `x`  or `y`  based on a non-deterministic choice.
function choose( x: a, y: a) : ndet a {
  if (odd(randomInt())) then x else y
}

// Return a random number equal or larger than 0.0 and smaller than 1.0
external random : () -> ndet double   { 
  cs "Primitive.RandomDouble"
  js "Math.random"
}

// Return a positive random integer (including 0)
external randomInt: () -> ndet int  { 
  cs "Primitive.RandomInt"
  js inline "Math.floor(Math.random()*2147483647)"
}

// ----------------------------------------------------------------------------
// Control
// ----------------------------------------------------------------------------

// The "while" function executes `action`  as long as `pred`  is "true".
function while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
{
  if (predicate()) {
    action()
    while(predicate, action)
  }
}

// The "repeat" function executes `action`  `n`  times.
function repeat( n : int, action : () -> e () ) : e ()
{
  for(1,n) fun(i) { action() }
}

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
function for( start: int, end : int, action : (int) -> e () ) : e ()
{
  function rep( i : int ) {
    if (i <= end) {
      injectDiv(action)(i)
      rep(i+1)
    }
  }
  unsafeNoDiv { rep(start) }
}

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns "Just", the iteration is stopped and the result returned
function forWhile( start: int, end : int, action : (int) -> e maybe<a> ) : e maybe<a>
{
  function rep( i : int ) {
    if (i <= end) {
      match(injectDiv(action)(i)) {
        Nothing -> rep(i+1)
        Just(x) -> Just(x)
      }
    }
    else Nothing
  }
  unsafeNoDiv { rep(start) }
}

// Return the host environment: `dotnet`, `browser`, (web)`worker`, or `nodejs`. 
external getHost() : ndet string {
  cs inline "\"dotnet\""
  js inline "($inBrowser ? 'browser' : ($inWebWorker ? 'worker' : 'nodejs'))"
}

