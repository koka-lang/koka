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
infixl 60  (+), (-)
infix  40  (!=), (==), (<=), (>=), (<), (>)
infixr 30  (&&)
infixr 20  (||)

// prefix     (!), (-)



extern include {
  js file "core-handlers-inline.js" // must come first due to dependencies
}

extern include {
  cs file "core-inline.cs"
  js file "core-inline.js"
}

extern include {
  cs file "core-handlers-inline.cs"
}

extern include {
  js file "core-string-inline.js"
}

extern include {
  js file "core-console-inline.js"
}

extern include {
  js file "../../contrib/biginteger.js"
}


// ----------------------------------------------------------------------------
// Core types
// ----------------------------------------------------------------------------

// An arbitrary precision signed integer.
type int

// A string is a sequence of unicode character points (`char`).
// The encoding of a string is internal and there
// is no constant-time indexing of characters in a string.
// Use the `:sslice` type for efficient matching and retrieving
// sub-strings from string.\
// See also the [``std/text/string``](std_text_string.html) module.
type string

// A 64-bit IEEE 754 floating point value.
// See also `std/num/double` for more operations on `:double`s.
type double

// An any type. Used for extern calls
type any

// A unicode character.
// Characters are unicode _codepoint_\/s.
// This is different from a unicode _grapheme_ which represents a single displayed
// symbol and can consists of multiple codepoints due to combining characters and marks.
// (see also the [``std/text/unicode``](std_text_unicode.html) module.)
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

// The `:global-scope` is a special type constant to denote the global scope
type global-scope :: S

// The `:net` effect signifies a function may access the network
type net :: X

// The `:file` effect signifies a function may access the file system
type file :: X

// The `:ui` effect signifies a function may access the graphics system
type ui :: X

// The `:blocking` effect signifies that a function may block
type blocking :: X

// The `:io-total` effect is used for functions that perform arbitrary I/O operations, but are terminating without raising exceptions.
alias io-total = <ndet,console,net,file,ui,st<global>>

// The `:io-noexn` effect is used for functions that perform arbitrary I/O operations, but raise no exceptions
alias io-noexn = <div,io-total>

// The `:io` effect is used for functions that perform arbitrary I/O operations.
alias io = <exn,io-noexn>

// ----------------------------------------------------------------------------
// Injection
// ----------------------------------------------------------------------------

// Add the divergent effect to a function effect.
extern inline inject-div : forall<a,e> (() -> e a) -> (() -> <div|e> a) { inline "#1" }

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

type open exception-info {
  Error
  Assert
  Todo
  Range
  Finalize
  Pattern( location : string, definition : string )
  System( errno : string )
  Internal( name : string )
}

extern info( exn : exception ) : exception-info {
  cs "Primitive.ExnInfo"
  js "exn_info"
}

extern stack-trace( exn : exception ) : string {
  cs "Primitive.ExnStackTrace"
  js "exn_stacktrace"
}

extern exception( message : string, info : exception-info ) : exception {
  cs "Primitive.ExnException"
  js "exn_exception"
}

extern throw( exn : exception ) : exn a {
  cs "Primitive.Throw"
  js "exn_throw"
}

// Return the message associated with an exception
extern message( exn : exception ) : string {
  cs "Primitive.ExnMessage"
  js "exn_message"
}

// Show the exception message and its stack trace.
fun show( exn : exception ) : string {
  exn.stack-trace
}

// Raise an exception with a specified message.
fun error( message : string ) : exn a {
  throw(message,Error)
}

// Throw an exception
fun throw( message : string, err : exception-info = Error) : exn a {
  throw( exception(message,err) )
}

// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
extern error-pattern(location : string, definition : string) : exn a {
  // needs to be extern since the compiler generates calls and dependency resolution does not work on it
  cs "Primitive.ExnErrorPattern"
  js "exn_error_pattern"
}

// Execute a `hndler` no matter what exception was raised in `action`.
extern finally : forall<e,a> (action: () -> e a, hndl: () -> e ()) -> e a  {
  cs inline "Eff.Op.HandleFinally<##2>(#1,#2)"
  js inline "$std_core._handle_finally(#1,#2)"
}

// Catch any exception raised in `action` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
extern try( action : () -> <exn|e> a, hndl: exception -> e a ) : e a {
  cs inline "Eff.Op.HandleCatch<##1>(#1,#2, false)"
  js inline "$std_core._handle_catch(#1, #2, false)"
}

private extern prim-try-some( action : () -> <exn|e> a, hndl: exception -> <exn|e> a) : <exn|e> a {
  cs inline "Eff.Op.HandleCatch<##1>(#1,#2, false)"
  js inline "$std_core._handle_catch(#1, #2, false)"
}

// _Deprecated_; use `try` instead. Catch an exception raised by `throw` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
fun catch( action : () -> <exn|e> a, hndl: exception -> e a) : e a {
  try(action,hndl)
}

// Set a `hndler` that is called only when an exception is raised in the `action` block.
fun on-exn( hndler : exception -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a {
  prim-try-some( action, fun(exn){ hndler(exn); throw(exn) })
}

// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
// Note that `on-exit(handler,action) == finally(action,handler)`.
fun on-exit( hndler : () -> e (), action : () -> e a ) : e a {
  finally(action,hndler)
}


// A `:try` type represents a first-class exception result.
type try<a> {
  Exn( exception : exception )
  Ok( result : a )
}

// Transform an exception effect to a `:try` type.
fun try( action : () -> <exn|e> a ) : e try<a> {
  try({ Ok(action()) },Exn)
}

// Return a default value when an exception is raised
fun try-default( value : a , action : () -> <exn|e> a ) : e a {
  try( action, fun(_) { value })
}

// Transform an `:try` type back to an `exn` effect.
fun untry( ex : try<a> ) : exn a {
  match(ex) {
    Exn(exn) -> throw(exn)
    Ok(x)    -> x
  }
}

// Transform a `:try` type to a `:maybe` value.
fun maybe( t : try<a> ) : maybe<exception> {
    match(t) {
        Exn(exn) -> Just(exn)
        Ok -> Nothing
    }
}
// ----------------------------------------------------------------------------
// Cancelation
// Internally implemented using an (uncatchable) exception
// ----------------------------------------------------------------------------

type extend exception-info {
  private con Cancel
}

// Was this a cancelation exception?
fun cancel?( exn : exception ) {
  match(exn.info) {
    Cancel -> True
    _      -> False
  }
}

// Was this a finalization exception?
fun finalize?(exn : exception) {
    match(exn.info) {
        Finalize -> True
        _ -> False
    }
}

// _unsafe_. The cancelation exception. User code should never throw
// this exception as it cannot be caught (but it is respected by `finally` blocks).
// It is used internally to `finalize` effect handlers that do not resume.
fun unsafe-cancel-exn() : exception {
  exception("computation is canceled",Cancel)
}

private extern prim-try-all( action : () -> <exn|e> a, hndl: exception -> e a ) : e a {
  cs inline "Eff.Op.HandleCatch<##1>(#1,#2,true)"
  js inline "$std_core._handle_catch(#1,#2,true)"
}

// _unsafe_. Catch any exception, including a possible cancelation.
// Unsafe to use in general as you must guarantee to later use `untry` to re-throw
// at least a cancelation exception.
fun unsafe-try-all( action : () -> <exn|e> a) : e try<a> {
  prim-try-all({Ok(action())},Exn);
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
// Null is used for external interfaces
// ----------------------------------------------------------------------------

// Abstract type used for passing `null` values to external functions
type null<a>

// Unsafe: transform any type to a `null` type; used internally by the compiler.
external ".null-any"(x : a) : null<a> {
  cs inline "#1"
  js inline "(#1==null ? null : #1)"  // undefined -> null
}

// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).
external null(x : maybe<a>) : null<a> {
  cs inline "(#1.tag_ == __std_core._maybe_Tag.Nothing ? default(##1) : #1.@value)"
  js inline "(#1==null ? null : #1.value)"
}

// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).
external maybe( n : null<a> ) : maybe<a> {
  cs inline "(EqualityComparer<##1>.Default.Equals(#1,default(##1)) ? __std_core._maybe<##1>.Nothing_ : new __std_core._maybe<##1>(#1))"
  js inline "(#1==null ? $std_core.Nothing : $std_core.Just(#1))"
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

val null-const : forall<a> null<a> = null(Nothing)



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

// The `ignore` function ignores its argument.
fun ignore( x : a ) : () {
  ()
}

// Return a 'constant' function that ignores its argument and always returns the same result
fun const( default : a ) : total (( x : b ) -> a) {
  (fun(_){ default })
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
fun list( lo : char, hi : char ) : total list<char>
{
  list(lo.int, hi.int).map( char )
}

private val maxListStack = 100


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


// recurse using an accumulator using constant heap space
private fun map-acc(g : (int,a,list<a>) -> e b , n : int, acc : list<b>, ys : list<a>) : e list<b> {
  match(ys) {
    Cons(x,xx) -> map-acc(g,n+1,Cons(g(n,x,xx),acc),xx)
    Nil -> reverse(acc)
  }
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun map-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
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
  reverse-append( xs, Nil )
}

// Efficiently reverse a list `xs` and append it to `tl`:\
// `reverse-append(xs,tl) == reserve(xs) + tl
fun reverse-append( xs : list<a>, tl : list<a> ) : list<a>
{
  fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a> {
    match(ys) {
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc
    }
  }
  reverse-acc(tl,xs)
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
    if (i <= 0) then acc else enumerate(unsafe-decreasing(i.dec),Cons(x,acc))
  }

  enumerate(n,[])
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
// For example: `filter([1,2,3],odd?) == [1,3]`
fun filter( xs : list<a>, pred : a -> e bool ) : e list<a>
{
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> if (pred(x)) then Cons(x,xx.filter(pred)) else xx.filter(pred)
  }
}


// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd?) == [2]`
fun remove( xs : list<a>, pred : a -> bool ) : list<a> {
  xs.filter( fun(x) { !pred(x) } )
}

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd?) == ([1,3],[2])`
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
// For example: `filterMap([1,2,3],fun(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`
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
  xs.foreach-while fun(x) {
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
  xs.foreach-while fun(kv) {
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
private fun joinsep( xs : list<string>, sep : string ) : string
{
  fun join-acc( ys : list<string>, acc : string ) {
    match(ys) {
      Cons(y,yy) -> join-acc(yy, acc + sep + y)  // todo: use string builder
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


// Append `end` to each string in the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
public fun join-end( xs : list<string>, end : string) : string {
  match(xs) {
    Nil -> ""
    _   -> xs.joinsep(end) + end
  }
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
extern inline int : (char) -> int { inline "#1"; cs inline "new BigInteger(#1)" }

// Convert a unicode code point to a character
extern inline char : (int) -> char { inline "(#1)"; cs inline "Primitive.IntToInt32(#1)" }

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

// Transform a boolean to a maybe type, using `Nothing` for `False`
fun maybe( b : bool ) : maybe<()> {
    if (b) then Just(()) else Nothing
}

// ----------------------------------------------------------------------------
// Integers
// ----------------------------------------------------------------------------

// Compare two integers
extern inline compare : (int,int) -> order  {
  cs "Primitive.IntCompare"
  js "$std_core._int_compare"
}


// Are two integers equal?
extern inline (==) : (int,int) -> bool  {
  cs inline "(#1 == #2)"
  js "$std_core._int_eq"
}

// Are two integers not equal?
extern inline (!=) : (int,int) -> bool  {
  cs inline "(#1 != #2)"
  js "$std_core._int_ne"
}

// Is the first integer smaller or equal to the second?
extern inline (<=) : (int,int) -> bool  {
  cs inline "(#1 <= #2)"
  js "$std_core._int_le"
}

// Is the first integer greater or equal to the second?
extern inline (>=) : (int,int) -> bool  {
  cs inline "(#1 >= #2)"
  js "$std_core._int_ge"
}

// Is the first integer smaller than the second?
extern inline (<)  : (int,int) -> bool  {
  cs inline "(#1 < #2)"
  js "$std_core._int_lt"
}

// Is the first integer greater than the second?
extern inline (>)  : (int,int) -> bool  {
  cs inline "(#1 > #2)"
  js "$std_core._int_gt"
}

// Add two integers.
extern inline (+) : (int,int) -> int {
  cs inline "(#1 + #2)"
  js "$std_core._int_add"
}

// Substract two integers.
extern inline (-) : (int,int) -> int {
  cs inline "(#1 - #2)"
  js "$std_core._int_sub"
}

// Multiply two integers.
extern inline (*) : (int,int) -> int {
  cs inline "(#1 * #2)"
  js "$std_core._int_mul"
}

// Euclidean-0 division of two integers. See also `divmod:(x : int, y : int) -> (int,int)`.
extern inline (/)(x:int,y:int) : int {
  cs "Primitive.IntDiv"
  js "$std_core._int_div"
}
// Euclidean modulus of two integers; always a non-negative number. See also `divmod:(x : int, y : int) -> (int,int)`.
extern inline (%)  : (int,int) -> int {
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
extern inline divmod(x:int,y:int) : (int,int) {
  cs "Primitive.IntDivMod"
  js "$std_core._int_divmod"
}

fun negate(i : int) : int {
  ~i
}

// Negate an integer.
extern inline (~)(i:int) : int {
  cs inline "(-#1)"
  js "$std_core._int_negate"
}

// Convert an integer to a `:double`. May return `nan` if the integer is too large to represent as a `:double`.
extern inline double : (int) -> double  {
  cs "Primitive.IntToDouble"
  js "$std_core._int_to_double"
}

// Is this an odd integer?
extern inline odd?   : ( int ) -> bool  {
  cs inline "!(#1.IsEven)"
  js "$std_core._int_isodd"
}

// Is this equal to zero?
extern inline zero?  : ( int ) -> bool  {
  cs inline "(#1.IsZero)"
  js "$std_core._int_iszero"
}


// Return the absolute value of an integer.
extern inline abs(i : int) : int {
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
extern exp10?( i : int ) : int {
  cs "Primitive.IntCountPow10"
  js "_int_count_pow10"
}

// Return the number of decimal digits of `i`. Return `0` when `i==0`.
extern count-digits( i : int ) : int {
  cs "Primitive.IntCountDigits"
  js "_int_count_digits"
}

extern mul-exp10( i : int, n : int ) : int {
  cs "Primitive.IntMulPow10"
  js "_int_mul_pow10"
}

extern cdiv-exp10( i : int, n : int ) : int {
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
  if (!cr.neg?) then (cq,cr) else (cq.dec, cr + exp10(n))
}

// Is this an even integer?
fun even?(i:int) : bool { !odd?(i) }

// Is the integer positive (stricly greater than zero)
fun pos?(i : int ) : bool { i.sign == Gt }

// Is the integer negative (stricly smaller than zero)
fun neg?(i : int ) : bool { i.sign == Lt }

extern inline sign(i : int ) : order {
  cs "Primitive.IntSign"
  js "$std_core._int_sign"
}

// Return the minimum of two integers
fun min( i : int, j : int ) : int  -> if (i <= j) then i else j

// Return the maximum of two integers
fun max( i : int, j : int ) : int  -> if (i >= j) then i else j

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
  xs.foldl( 0, fun(x,y) { x + y } )
}

// Transform an integer to a maybe type, using `Nothing` for `0`
fun maybe( i : int ) : maybe<int> {
    if (i==0) then Nothing else Just(i)
}

// ----------------------------------------------------------------------------
// 32-bit integers
// Just define the operations needed for defining the std/core interface but
// don't export any definitions here. Full operations are defined in `std/int32`.
// ----------------------------------------------------------------------------

// Type of a 32-bit signed integer.
// See the [``std/int32``](std_int32.html) module for operations on 32-bit integers.
type int32

// Convert an `:int32` to an `:int`.
extern inline int( i : int32 ) : int {
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_double"
}

// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.
extern inline int32  : (int) -> int32   {
  cs "Primitive.IntToInt32"
  js "$std_core._int_to_int32"
}


// Minimal set of operations that we need in `std/core`.
private extern inline (<=) : (int32,int32) -> bool  { inline "(#1 <= #2)"; js inline "(#1 <= #2)" }
private extern inline (<)  : (int32,int32) -> bool  { inline "(#1 < #2)"; js inline "(#1 < #2)" }
private extern inline (+)  : (int32,int32) -> int32 { inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
private extern inline (-)  : (int32,int32) -> int32 { inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }
private extern inline pos?( i : int32 ) : bool { inline "(#1>0)" }
private extern inline neg?( i : int32 ) : bool { inline "(#1<0)" }

private fun incr( i : int32 ) : int32 {
  i + 1.int32
}

private fun decr( i : int32 ) : int32 {
  i - 1.int32
}


// ----------------------------------------------------------------------------
// Parse numbers
// ----------------------------------------------------------------------------

// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.
fun parse-int-default( s : string, default : int = 0, hex : bool = False ) : int {
  if (s.empty?) then default else s.parse-int(hex).maybe(default)
}

// Parse an integer after trimming whitespace.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string will result in `Just(0)`.
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
fun parse-int( s : string, hex : bool = False) : maybe<int> {
  s.trim.xparse-int(hex)
}

private extern xparse-int( s : string, hex : bool ) : maybe<int> {
  cs "Primitive.IntParse"
  js "_int_parse"
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


// Is the value negative?
fun neg?( d : double ) : bool {
  d < 0.0
}

// Is the value positive?
fun pos?( d : double ) : bool {
  d > 0.0
}

// Is the value zero?
fun zero?( d : double ) : bool {
  d == 0.0
}

fun sign( d : double ) : order {
  if (d<0.0) then Lt elif (d>0.0) then Gt else Eq
}

// Negate a `:double`.
extern inline (~)(d : double ) : double {
  inline "(-#1)"  // inline so `~0.0` becomes negative zero
}


// convert a `:double` to an `:int` using `round` to round to its nearest integer.
// (rounding to an even number on a tie)
// Returns `0` if the argument is not `finite?`.
extern inline int(d : double ) : int {
  cs "Primitive.IntDouble"
  js "$std_core._int_double"
}


// Returns the value `d`  raised to the power `p` .
extern inline (^) : (d:double,p:double) -> double {
  cs "Math.Pow"
  js "Math.pow"
}

// Return the absolute value of a `:double` `d`
extern inline abs : (d:double) -> double  {
  cs "Math.Abs"
  js "Math.abs"
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
abstract struct sslice( str : string, start : int32, len : int32 )

// Internal export for the regex module
fun ".new-sslice"( str :string, start: int32, len : int32 ) {
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
private extern inline length( s : string ) : int32 {
  cs inline "#1.Length"
  js inline "#1.length"
}

// O(n). Return the number of characters in a string.
extern count( s : string ) : int {
  cs "Primitive.StringCount"
  js "_string_count"
}

// O(n). Return the number of characters in a string slice
extern count( slice : sslice ) : int {
  cs "Primitive.SliceCount"
  js "_sslice_count"
}

private extern inline first1(s : string ) : sslice {
  cs "Primitive.SliceFirst"
  js "_sslice_first"
}

private extern inline last1(s : string ) : sslice {
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
  Sslice(s,0.int32,s.length)
}

// An empty slice
val empty = Sslice("",0.int32,0.int32)

// Is a slice empty?
fun empty?( slice : sslice ) : bool {
  !slice.len.pos?
}

// Is a slice not empty?
fun notempty?( slice : sslice ) : bool {
  slice.len.pos?
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
extern common-prefix(s : string, t : string, upto : int = -1 ) : sslice {
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
public extern advance( slice : sslice, count : int ) : sslice {
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
extern extend( slice : sslice, count : int ) : sslice {
  cs "Primitive.SliceExtend"
  js "_sslice_extend"
}


// O(1). Return the string slice from the start of a string up to the
// start of `slice` argument.
fun before(slice : sslice) : sslice {
  val Sslice(s,start,_len) = slice
  Sslice(s,0.int32,start)
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
  cs "Primitive.SliceToString"
  js "_slice_to_string"
}

// If the slice is not empty,
// return the first character, and a new slice that is advanced by 1.
extern next( slice : sslice ) : maybe<(char,sslice)> {
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
fun foreach( slice : sslice, action : (c:char) -> e ()) : e () {
  foreach-while(slice, fun(c) { action(c); Nothing })
  ()
}

// Does string `s`  contain the string `sub`  ?
private extern inline xindex-of(s : string, sub : string ) : int32 {
  cs inline "((#1).IndexOf(#2))";
  js inline "((#1).indexOf(#2))";
}

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.
fun find( s : string, sub : string ) : maybe<sslice> {
  val i = s.xindex-of(sub)
  if (i.neg?) then Nothing else Just(Sslice(s,i,sub.length))
}

// Does string `s`  contain the string `sub`  ?
private extern inline xlast-index-of(s : string, sub : string ) : int32 {
  cs inline "(#1.LastIndexOf(#2))";
  js inline "((#1).lastIndexOf(#2))";
}

// Return the last index of substring `sub` in `s` if it occurs.
fun find-last( s : string, sub : string ) : maybe<sslice> {
  val i = s.xlast-index-of(sub)
  if (i.neg?) then Nothing else Just(Sslice(s,i,sub.length))
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
   then Just(Sslice(s,0.int32,s.length - post.length))
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

// Trim off a substring `sub` while `s` starts with that string.
fun trim-left( s : string, sub : string ) : string {
  if (sub.empty?) return s
  match(s.starts-with(sub)) {
    Just(slice) -> trim-left(unsafe-decreasing(slice.string),sub)
    Nothing -> s
  }
}

// Trim off a substring `sub` while `s` ends with that string.
fun trim-right( s : string, sub : string ) : string {
  if (sub.empty?) return s
  match(s.ends-with(sub)) {
    Just(slice) -> trim-right(unsafe-decreasing(slice.string),sub)
    Nothing -> s
  }
}

// Repeat a string `n` times
fun repeat( s : string, n : int ) : string {
  repeat32(s,n.int32)
}

private extern repeat32( s : string, n : int32 ) : string {
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
fun empty?( s : string ) : bool {
  s == ""
}

// Is a string not empty?
fun notempty?( s : string ) : bool {
  s != ""
}

// Transform a string to a maybe type, using `Nothing` for an empty string
fun maybe( s : string ) : maybe<string> {
    if (s.empty?) then Nothing else Just(s)
}


private extern inline splitv : ( s : string,  sep : string ) -> vector<string> {
  cs inline "(#1.Split(new String[]{#2}, StringSplitOptions.None))";
  js inline "((#1).split(#2))"
}

private extern inline splitv : ( s : string,  sep : string, n : int32 ) -> vector<string> {
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
fun split( s : string, sep: string, n : int) : list<string> {
  splitv(s,sep,n.int32).list
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
  s.foreach fun(c) {
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
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()"
}

// Convert a string to lower-case
extern to-lower : (s : string) -> string {
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()"
}

// Convert the first character of a string to uppercase.
fun capitalize( s : string ) : string {
  s.first.string.to-upper + s.first.after.string
}

// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
fun pad-left( s : string, width : int, fill : char = ' ') : string
{
  val w = width.int32
  val n = s.length
  if (w <= n)
   then s
   else fill.string.repeat32( w - n ) + s
}

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
fun pad-right( s : string, width : int, fill : char = ' ') : string
{
  val w = width.int32
  val n = s.length
  if (w <= n)
   then s
   else s + fill.string.repeat32(w - n)
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
private extern inline unsafe-idx : forall<a> ( v : vector<a>, index : int32 ) -> total a {
  cs inline "(#1)[#2]";
  js inline "(#1)[#2]";
}

private extern inline unsafe-assign : forall<a> ( v : vector<a>, i : int32, x : a ) -> total () {
  cs inline "(#1)[#2] = #3";
  js inline "(#1)[#2] = #3";
}

private extern inline unsafe-vector : forall<a> ( n : int32 ) -> total vector<a> {
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"
}

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
extern inline [] : forall<a> ( v : vector<a>, index : int ) -> exn a  {
  cs inline "(#1)[Primitive.IntToInt32(#2)]"
  js inline "$std_core._vector_at(#1,#2)"
}

// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds
fun at( v  : vector<a>, index : int ) : maybe<a> {
  val idx = index.int32
  if (idx < 0.int32 || v.length32 <= idx) then Nothing else Just(unsafe-idx(v,idx))
}

// Return the length of a vector.
fun length( v : vector<a> ) : int {
  v.length32.int
}

private extern inline length32 : forall<a> (v : vector<a> ) -> int32  {
  cs inline "((#1).Length)";
  js inline "((#1).length)";
}

// Create a new vector of length `n`  with initial elements `default` .
fun vector(n : int, default : a) : vector<a> {
  vector-init32(n.int32, fun(_i){ default } )
}

// Create a new vector of length `n`  with initial elements given by function `f` .
fun vector-init( n : int, f : int -> a ) : vector<a> {
  vector-init32( n.int32, fun(i) { f(i.int) } )
}

// Create a new vector of length `n`  with initial elements given by function `f` .
private extern vector-init32(n : int32, f : int32 -> a) : vector<a> {
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
  v.foreach-indexed32( fun(x,_) { f(x) })
}

// Invoke a function `f` for each element in a vector `v`
fun foreach-indexed( v : vector<a>, f : (a,int) -> e () ) : e () {
  foreach-indexed32( v, fun(x,i) { f(x,i.int) } )
}

private fun foreach-indexed32( v : vector<a>, f : (a,int32) -> e () ) : e () {
  for32( 0.int32, v.length32.decr ) fun(i) {
    f(v.unsafe-idx(i),i)
  }
}

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b> {
  for-while32( 0.int32, v.length32.decr ) fun(i) {
    f(v.unsafe-idx(i))
  }
}

// Apply a total function `f` to each element in a vector `v`
fun map( v : vector<a>, f : a -> e b ) : e vector<b> {
  val w = unsafe-vector(v.length.int32)
  v.foreach-indexed32 fun(x,i) {
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
// Delayed values
// ----------------------------------------------------------------------------

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract type delayed<e,a> {
  con Delay( dref : ref<global,either<() -> e a,a>> )
}

// Create a new `:delayed` value.
public fun delay( action : () -> e a ) : delayed<e,a> {
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
        val x = inject-st(inject-div(action))()
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
  cs inline "#1.ToString()";
  js inline "#1.toString()"
}

// Generic formatting
private extern inline gformat : forall<a> (value:a,format:string) -> string {
  cs inline "#1.ToString(#2,CultureInfo.InvariantCulture)";
  js inline "_gformat(#1, #2)";
}

// Convert an `:int` to a string
fun show( i : int ) : string {
  i.gshow()
}


private extern int-show-hex(i:int,use-capitals:bool) : string {
  cs "Primitive.IntShowHex"
  js "_int_showhex"
}

// Show an `:int` as a hexadecimal value.\
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
fun show-hex( i : int, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) {
  (if (i<0) then "-" else "") + pre + int-show-hex(i.abs,use-capitals).pad-left(width,'0')
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
  cs "Primitive.DoubleShowExp"
  js "_double_show_exp"
}

// Show a character as a string
private fun show-char( c : char ) : string
{
  if (c < ' ' || c > '~') {
    if (c == '\n') then "\\n"
    elif (c == '\r') then "\\r"
    elif (c == '\t') then "\\t"
    elif (c.int <= 0xFF) then "\\x" + c.int.show-hex(2,pre="")
    elif (c.int <= 0xFFFF) then "\\u" + c.int.show-hex(4,pre="")
    else "\\U" + c.int.show-hex(6,pre="")
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
fun println(c : char)    { printsln(c.string) }

// Print a unit value to the console, including a final newline character
fun println( u : () )  { printsln(show(())) }

// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------


private extern xtrace : ( message : string ) -> ()  {
  cs "Primitive.Trace"
  js "_trace"
}

private extern xtrace-any : forall<a>( message: string, x : a ) -> ()  {
  cs "Primitive.TraceAny"
  js "_trace_any"
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

public fun trace-any( message : string, x : a ) : () {
  unsafe-total {
    if (!trace-enabled) then xtrace-any(message,x)
  }
}
// Disable tracing completely.
public fun notrace() : st<global> () {
  trace-enabled := False
}


fun assert( message : string, condition : bool ) : () {
  if (!condition) unsafe-noexn{ throw(message,Assert) }
}

fun todo( message : string ) : () {
  unsafe-noexn{ throw(message,Todo) }
}



// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

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
// by storing self referential functions in the heap.
type hdiv :: (H,V,E) -> P

// The predicate `:ediv<x,a,e>` signifies that if the type `:a` contains a reference to effect constant `:x",
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// for operations on first-class effects where code can diverge through operations.
type ediv :: (X,V,E) -> P

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

// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
fun once( calc : () -> a ) : (() -> a) {
  unsafe-total {
    var value := Nothing
    return fun() {
      unsafe-total {
        match(value) {
          Just(x) -> x
          Nothing -> {
            val x = calc()
            value := Just(x)
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
  js inline "Math.floor(Math.random()*_max_precise)"
}

// Return a random boolean
fun random-bool() : ndet bool {
  (random() >= 0.5)
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
      action(i)
      rep(unsafe-decreasing(i.inc))
    }
  }
  rep(start)
}


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
private fun for32( start: int32, end : int32, action : (int32) -> e () ) : e ()
{
  fun rep( i : int32 ) {
    if (i <= end) {
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
    if (i <= end) {
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
private fun for-while32( start: int32, end : int32, action : (int32) -> e maybe<a> ) : e maybe<a>
{
  fun rep( i : int32 ) {
    if (i <= end) {
      match(action(i)) {
        Nothing -> rep(unsafe-decreasing(i.incr))
        Just(x) -> Just(x)
      }
    }
    else Nothing
  }
  rep(start)
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


// ----------------------------------------------------------------------------
// Handler primitives.
// These are hidden from normal usage but generated by the compiler since
// the continuations are not safely typed in plain Koka
// ----------------------------------------------------------------------------


extern inject-exn(action : () -> e a) : <exn|e> a {
  js "$std_core._handle_inject_exn"
}

extern inline ".inject-effect"<a::V,e1::E,e2::E>( effect-name : string, action : () -> e1 a ) : e2 a {
  js inline "$std_core._handle_inject(#1,0,#2)"
}

extern inline ".inject-resource"<a::V,e::E>( effect-name : string, resource-tag : int, action : () -> e a ) : e a {
  js inline "$std_core._handle_inject(#1,#2,#3)"
}

// Yield alias
alias yld<a> = a

// Resources identify specific handlers
type resource<e::HX> {
  con ".Resource"(:int)
}


extern inline bind( x : yld<a>, next : a -> e yld<b> ) : e yld<b> {
  cs inline "Eff.Op.Bind<##1,##2>(#1,#2)"
  js "$std_core._bind"
}

extern ".yieldop"( effect-name : string, op-name : string, op-resource : int, op-idx : int, op : a ) : yld<b> {
  cs inline "Eff.Op.YieldOp<##1,##2>(#1,#2,(int)#4,#5)"
  js inline "$std_core._yield_op(#1,#2,#5,#4,#3)"
}

extern ".yieldop-x1"<a,b,x>( effect-name : string, op-name : string, op-resource : int, op-idx : int, op : a, def : maybe<x> /* just to ensure `x` is passed as a type parameter */) : yld<b> {
  cs inline "Eff.Op.YieldOpX1<##1,##2,##3>(#1,#2,(int)#4,#5)"
  js inline "$std_core._yield_op(#1,#2,#5,#4,#3)"
}

type resume-context<a,e::E,b>
type resume-context1<a,e::E,b,l>

extern resume( context : resume-context<a,e,b>, result : a ) : e b {
  js inline "#1.resume(#2,undefined)"
}

extern resume( context : resume-context1<a,e,b,l>, result : a, local : l ) : e b {
  js inline "#1.resume(#2,#3)"
}

extern finalize( context : resume-context<a,e,b>, result : c ) : e c {
  js inline "#1.finalize(#2)"
}

extern finalize( context : resume-context1<a,e,b,l>, result : c ) : e c {
  js inline "#1.finalize(#2)"
}

type handler-branch0<e::E,a>
type handler-branch1<e::E,l,a>

extern ".makeHandlerBranch0"( resume-kind : int, op-name : string,
                              branch : (resume-context<a,e,b>, op : c) -> e b
                            ) : total handler-branch0<e,b> {
  cs inline "new Eff.Branch<##3,##1,##2>((Eff.ResumeKind)((int)#1),#2,#3)"
  js "$std_core._new_branch"
}

extern ".makeHandlerBranch1"( resume-kind : int, op-name : string,
                              branch : (resume-context1<a,e,b,l>, op : c, local : l) -> e b
                            ) : total handler-branch1<e,l,b> {
  cs inline "new Eff.Branch1<##5,##3,##1,##2>((Eff.ResumeKind)((int)#1),#2,#3)"
  js "$std_core._new_branch1"
}

extern ".makeHandlerBranch0-x1"( resume-kind : int, op-name : string,
                                branch : any // forall<x>(resume-context<a<x>,e,b>, op : c<x>) -> e b
                              ) : total handler-branch0<e,b> {
  cs inline "new Eff.BranchX1<##1>((Eff.ResumeKind)((int)#1),#2,(TypeFun1)#3)"
  js "$std_core._new_branch"
}

extern ".makeHandlerBranch1-x1"( resume-kind : int, op-name : string,
                                 branch : any // forall<x>(resume-context1<a<x>,e,b,l>, op : c<x>, local : l) -> e b
                               ) : total handler-branch1<e,l,b> {
  cs inline "new Eff.Branch1X1<##3,##1>((Eff.ResumeKind)((int)#1),#2,(TypeFun1)#3)"
  js "$std_core._new_branch1"
}

val null-return : forall<a,e> null<a -> e a>    = null-const
val null-return1 : forall<a,l,e> null<(a,l) -> e a> = null-const


extern ".makeHandler0"( effect-name : string,
                        reinit : null<() -> e ()>,
                        ret : null<a -> e b>,
                        final : null<() -> e ()>,
                        branches : vector<handler-branch0<e,b>>,
                        handler-kind : int
                      ) : total ((action : () -> e1 a) -> e b) {
  cs inline "Eff.Handler0<##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_handler"
}

extern ".makeHandler1"( effect-name : string,
                        reinit : null<(local : l) -> e l>,
                        ret : null<(result : a, local : l) -> e b>,
                        final : null<(local : l) -> e ()>,
                        branches : vector<handler-branch1<e,l,b>>,
                        handler-kind : int
                      ) : total ((initial-local : l, action : () -> e1 a) -> e b) {
  cs inline "Eff.Handler1<##4,##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_handler1"
}

extern ".makeResourceHandler0"( effect-name : string,
                        reinit : null<() -> e ()>,
                        ret : null<a -> e b>,
                        final : null<() -> e ()>,
                        branches : vector<handler-branch0<e,b>>,
                        handler-kind : int,
                        resource-tag : int
                      ) : total ((action : () -> e1 a) -> e b) {
  cs inline "Eff.Handler0<##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_resource_handler"
}

extern ".makeResourceHandler1"( effect-name : string,
                        reinit : null<(local : l) -> e l>,
                        ret : null<(result : a, local : l) -> e b>,
                        final : null<(local : l) -> e ()>,
                        branches : vector<handler-branch1<e,l,b>>,
                        handler-kind : int,
                        resource-tag : int
                      ) : total ((initial-local : l, action : () -> e1 a) -> e b) {
  cs inline "Eff.Handler1<##4,##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_resource_handler1"
}

extern ".makeFreshResourceHandler0"( effect-name : string,
                        reinit : null<() -> e ()>,
                        ret : null<a -> e b>,
                        final : null<() -> e ()>,
                        branches : vector<handler-branch0<e,b>>,
                        handler-kind : int,
                        resource-tag : int,
                        resource-wrap: int -> c
                      ) : total ((action : (c) -> e1 a) -> e b) {
  cs inline "Eff.Handler0<##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_resource_handler"
}

extern ".makeFreshResourceHandler1"( effect-name : string,
                        reinit : null<(local : l) -> e l>,
                        ret : null<(result : a, local : l) -> e b>,
                        final : null<(local : l) -> e ()>,
                        branches : vector<handler-branch1<e,l,b>>,
                        handler-kind : int,
                        resource-tag : int,
                        resource-wrap: int -> c
                      ) : total ((initial-local : l, action : (c) -> e1 a) -> e b) {
  cs inline "Eff.Handler1<##4,##1,##2>.Create(#1,#2,#3,(Eff.HandlerKind)((int)#4))"
  js "$std_core._new_resource_handler1"
}

// Used for handlers with just a `return` branch
extern ".makeHandlerRet0"(  ignored-effect-name : string,
                         reinit : null<() -> e ()>,
                         ret : null<(result:a) -> e b>,
                         final : null<() -> e ()>,
                         ignored-branches : int,
                         ignored-kind : int
                       ) : total ((action : () -> e a) -> e b) {
  js inline "$std_core._new_empty_handler(#2,#3,#4)"
}

// Used for handlers with just a `return` branch
extern ".makeHandlerRet1"(  ignored-effect-name : string,
                         reinit : null<(local:l) -> e l>,
                         ret : null<(result:a, local:l) -> e b>,
                         final : null<(local:l) -> e ()>,
                         ignored-branches : int,
                         ignored-kind : int
                       ) : total ((local : l, action : () -> e a) -> e b) {
  js inline "$std_core._new_empty_handler1(#2,#3,#4)"
}


// .open is used in the compiler to open up closed effects.
// Usually simplified away but still present if --no-simplify flag is given
extern inline ".open"<e1 :: E, e2 :: E, a, b>( x : a ) : total b {
  inline "#1"
}

// Magic casting to the `:any` type.
extern inline ".toany"( x : a ) : any {
  inline "#1"
  cs inline "(object)(#1)"
}
