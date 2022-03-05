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
module std/core

pub import std/core/types
pub import std/core/hnd

pub infixr 80  (^)
pub infixl 70  (*), (%), (/), cdiv, cmod
pub infixr 60  (++)
pub infixl 60  (+), (-)
pub infix  40  (!=), (==), (<=), (>=), (<), (>)
// prefix     (!), (-) 

extern import
  c  file "core/core-inline"
  cs file "core/core-inline.cs"
  js file "core/core-inline.js"

extern import
  js file "core/core-integer-inline.js"

extern import
  js file "core/core-string-inline.js"

extern import
  js file "core/core-console-inline.js"

// ----------------------------------------------------------------------------
// Builtin effects
// ----------------------------------------------------------------------------

// An alias for the empty effect.
pub alias total = <>

// The console effect signifies that a function may write to the console.
pub type console :: X

// An alias for pure effects: a pure function always returns the same result
// when called with the same arguments but may not terminate or raise an exception.
pub alias pure = <exn,div>

// The `:global-scope` is a special type constant to denote the global scope
pub type global-scope :: S

// The `:net` effect signifies a function may access the network
pub type net :: X

// The `:fsys` effect signifies a function may access the file system
pub type fsys :: X

// The `:ui` effect signifies a function may access the graphics system
pub type ui :: X

// The `:blocking` effect signifies that a function may block
pub type blocking :: X

// The `:io-total` effect is used for functions that perform arbitrary I/O operations, but are terminating without raising exceptions.
pub alias io-total = <ndet,console,net,fsys,ui,st<global>>

// The `:io-noexn` effect is used for functions that perform arbitrary I/O operations, but raise no exceptions
pub alias io-noexn = <div,io-total>

// The `:io` effect is used for functions that perform arbitrary I/O operations.
pub alias io = <exn,io-noexn>

// ----------------------------------------------------------------------------
// Masking
// ----------------------------------------------------------------------------

// Add the state effect to a function effect.
pub inline extern inject-st : forall<a,h,e> (() -> e a) -> total (() -> <st<h>|e> a)  
  inline "#1"

// ----------------------------------------------------------------------------
// Standard Functions
// ----------------------------------------------------------------------------

// The `const` funs returns its first argument and ignores the second.
pub fun const(x,y)
  x

// Apply a function `f` to a specified argument `x`.
pub fun apply(f,x)
  f(x)

// Compose two funs `f` and `g`.
pub fun o(f,g)
  fn(x) f(g(x))

// The `ignore` function ignores its argument.
pub fun ignore( x : a ) : ()
  ()

// Return a 'constant' function that ignores its argument and always returns the same result
pub fun const( default : a ) : total (( x : b ) -> a)
  fn(_) default 

// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

pub fun int( b : bool ) : int  
  if b then 1 else 0

pub fun mbint( m : maybe<int> ) : int
  match m
    Nothing -> 0
    Just(i) -> i

// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
pub fun bool( i : int ) : bool 
  i != 0

// Convert a `:maybe` type to a boolean using `False` for `Nothing` and `True` for `Just`.
pub fun bool( m : maybe<a> ) : bool
  match m
    Nothing -> False
    _       -> True

// Convert a string to a boolean, using `False` for the empty string and `True` otherwise.
pub fun bool( s : string ) : bool  
  s != ""

// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
pub fun maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
  match m
    Nothing -> onNothing
    Just(x) -> onJust(x)

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
// This is an alias for `default`.
pub fun maybe( m : maybe<a>, nothing : a ) : a
  default(m,nothing)

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
pub fun default( m : maybe<a>, nothing : a ) : a
  match m
    Nothing -> nothing
    Just(x) -> x

// Get the value of the `Just` constructor or raise an exception
pub fun unjust( m : maybe<a> ) : exn a
  match m
    Just(x) -> x
    Nothing -> throw("unexpected Nothing in std/core/unjust")

pub fun map( m : maybe<a>, f : a -> e b ) : e maybe<b>
  match m
    Nothing -> Nothing
    Just(x) -> Just(f(x))

pub fun (||)( m1 : maybe<a>, m2: maybe<a> ) : maybe<a>
  match m1
    Nothing -> m2
    _       -> m1

// Convert a `:either` to a `:maybe` type discarding the value of the `Left` constructor
// and using `Just` for the `Right` constructor.
pub fun maybe( e : either<a,b> ) : maybe<b>
  match e
    Left -> Nothing
    Right(x) -> Just(x)

// Map over the `Right` component of an `:either` type.
pub fun map( e : either<a,b>, f : b -> e c  ) : e either<a,c>
  match e
    Right(x) -> Right(f(x))
    Left(x)  -> Left(x)

// The type of lists, which can be either empty (`Nil`) or an element followed
// by a list (`Cons`).
pub type list<a>
  // The empty list.
  con Nil
  // A ``head``  element followed by the ``tail``  of the list.
  con Cons(head:a, tail : list<a> )

// Return the head of list if the list is not empty.
pub fun head( xs : list<a> ) : maybe<a>
  match xs
    Cons(x) -> Just(x)
    _       -> Nothing

// Return the head of list if the list is not empty, or use `default` otherwise
pub fun head( xs : list<a>, default : a ) : a
  match xs
    Cons(x) -> x
    _       -> default

// Return the tail of list. Returns the empty list if `xs` is empty.
pub fun tail( xs : list<a> ) : list<a>
  match xs
    Cons(_,xx) -> xx
    _          -> []

// Is the list empty?
pub fun is-empty( xs : list<a> ) : bool
  match xs
    Nil -> True
    _   -> False

// A `:stream` is a co-inductive type representing an infinite list of elements.
pub co type stream<a>
  con Next(head:a, tail: stream<a> )

pub fun int( x : order ) : int
  match x
    Lt -> -1
    Eq -> 0
    Gt -> 1

pub fun order( i : int ) : order
  if i < 0 then Lt
  elif i > 0 then Gt
  else Eq

pub fun (==)( x : order, y : order ) : bool 
  x.int == y.int 

pub fun (!=)( x : order, y : order ) : bool 
  x.int != y.int 

pub fun (>=)( x : order, y : order ) : bool 
  x.int >= y.int 

pub fun (<=)( x : order, y : order ) : bool 
  x.int <= y.int

pub fun (>)( x : order, y : order ) : bool  
  x.int > y.int

pub fun (<)( x : order, y : order ) : bool  
  x.int < y.int
  
pub fun map( t : (a,a), f : a -> e b ) : e (b, b)
  (t.fst.f, t.snd.f)

pub fun map( t : (a,a,a), f : a -> e b ) : e (b, b, b)
  (t.fst.f, t.snd.f, t.thd.f)

pub fun map( t : (a,a,a,a), f : a -> e b ) : e (b,b,b,b)
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f)

// ----------------------------------------------------------------------------
// List functions
// ----------------------------------------------------------------------------

// Returns a singleton list.
pub fun single(x)
  [x]

// Returns the length of a list.
pub fun length(xs)
  fun len(ys,acc)
    match ys
      Cons(_,yy) -> yy.len(acc+1)
      Nil        -> acc
  xs.len(0)

// Returns an integer list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
pub fun list( lo: int, hi: int ) : total list<int>
  if lo <= hi
    then Cons( lo, list( unsafe-decreasing(lo.inc), hi ) )
    else Nil

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
pub fun list( lo: int, hi: int, stride : int ) : total list<int>
  if lo <= hi
    then Cons( lo, list(  unsafe-decreasing(lo + stride), hi, stride ))
    else Nil  

// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
pub fun list( lo: int, hi: int, f : int -> e a ) : e list<a>
  if lo <= hi
    then Cons( f(lo), list( unsafe-decreasing(lo.inc), hi, f ))
    else Nil  

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
pub fun list( lo: int, hi: int, stride : int, f : int -> e a ) : e list<a>
  if lo <= hi
    then Cons( f(lo), list(  unsafe-decreasing(lo + stride), hi, stride, f ))
    else Nil  

// Create a list of characters from `lo`  to `hi`  (inclusive).
pub fun list( lo : char, hi : char ) : total list<char>
  list(lo.int, hi.int).map( char )

// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
pub fun zip( xs : list<a>, ys : list<b> ) : list<(a,b)>
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> Cons((x,y),zip(xx,yy))
      Nil        -> Nil
    Nil -> Nil

// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
pub fun zipwith( xs : list<a>, ys :list<b>, f : (a,b) -> e c ) : e list<c>
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> Cons(f(x,y),zipwith(xx,yy,f))
      Nil -> Nil
    _ -> Nil

// Zip two lists together by apply a function `f` to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
pub fun zipwith-indexed( xs0 : list<a>, ys0 :list<b>, f : (int,a,b) -> e c ) : e list<c>
  fun zipwith-iter( i, xs, ys )
    match xs
      Cons(x,xx) -> match ys
        Cons(y,yy) -> Cons(f(i,x,y),zipwith-iter(i+1,xx,yy))
        Nil -> Nil
      Nil -> Nil
  zipwith-iter(0,xs0,ys0)

// Unzip a list of pairs into two lists
pub fun unzip( xs : list<(a,b)> ) : (list<a>,list<b>)
  // todo: implement TRMC for multiple results
  fun iter( ys, acc1, acc2 )
    match ys
      Cons((x,y),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2))
      Nil            -> (reverse(acc1),reverse(acc2))
  iter(xs,[],[])

// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
pub fun take( xs : list<a>, n : int ) : list<a>
  match xs
    Cons(x,xx) | n > 0 -> Cons(x, take(xx, n - 1))
    _ -> Nil

// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)
pub fun drop( xs : list<a>, n : int ) : list<a>
  match xs
    Cons(_,xx) | n > 0 -> drop(xx, n - 1)
    _ -> xs

// Apply a function `f`  to each element of the input list in sequence.
pub fun map(xs : list<a>, f : a -> e b) : e list<b>
  match xs
    Cons(x,xx) -> Cons(f(x), xx.map(f))
    Nil -> Nil

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
pub fun map-indexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b>
  fun map-idx(ys,i)
    match ys
      Cons(y,yy) -> Cons(f(i,y), map-idx(yy,i+1))
      Nil -> Nil
  map-idx(xs,0)

// Apply a function `f`  to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
pub fun map-peek(xs : list<a>, f : (value : a, rest : list<a>) -> e b) : e list<b>
  fun mappeek(ys)
    match ys
      Cons(y,yy) -> Cons(f(y,yy), yy.mappeek)
      Nil -> Nil
  mappeek(xs)

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
pub fun map-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
  fun mapidx(ys,i)
    match ys
      Cons(y,yy) -> Cons(f(i,y,yy), mapidx(yy,i+1))
      Nil -> Nil
  mapidx(xs,0)

// Reverse a list.
pub fun reverse(xs : list<a>) : list<a>
  reverse-append( xs, Nil )

// Efficiently reverse a list `xs` and append it to `tl`:
// `reverse-append(xs,tl) == reserve(xs) ++ tl
pub fun reverse-append( xs : list<a>, tl : list<a> ) : list<a>
  fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a>
    match ys
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc
  reverse-acc(tl,xs)

// Append two lists.
pub fun (++)(xs : list<a>, ys : list<a> ) : list<a>
  append(xs,ys)

// Append two lists.
pub fun append(xs : list<a>, ys : list<a> ) : list<a>
  match xs
    Cons(x,xx) -> Cons(x,append(xx,ys))
    Nil -> ys

// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)`
// Note, `foldr` is less efficient than `foldl` as it reverses the list first.
pub fun foldr(xs : list<a>, z : b, f : (b, a) -> e b) : e b
  xs.reverse.foldl(z) fn(x,y) f(x,y)

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
pub fun foldl(xs : list<a>, z : b, f : (b, a) -> e b) : e b
  match xs
    Cons(x,xx) -> foldl(xx,f(z,x),f)
    Nil        -> z

pub fun foldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
  match xs
    Cons(x,xx) -> xx.foldl(x,f)
    Nil        -> throw("unexpected Nil in std/core/foldl1")

pub fun foldr1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
  xs.reverse.foldl1(f)

// Create a list of `n`  repeated elementes `x`
pub fun replicate( x : a, n : int ) : list<a>
  if n > 0
   then Cons(x, replicate(x,unsafe-decreasing(n.dec)))
   else Nil

// split a list at position `n`
pub fun split( xs : list<a>, n : int ) : (list<a>, list<a>)
  (xs.take(n), xs.drop(n))

pub fun span( xs : list<a>, predicate : a -> e bool ) : e (list<a>,list<a>)
  // todo: implement TRMC with multiple results to avoid the reverse
  fun span-acc( ys, acc)
    match ys
      Cons(y,yy) -> if y.predicate then yy.span-acc(Cons(y,acc)) else (acc.reverse,ys)
      _ -> (acc.reverse, ys)
  xs.span-acc( [] )

// Keep only those initial elements that satisfy `predicate`
pub fun take-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if x.predicate then Cons(x, xx.take-while(predicate) ) else Nil
    Nil -> Nil

// Drop all initial elements that satisfy `predicate`
pub fun drop-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if x.predicate then xx.drop-while(predicate) else xs
    Nil -> Nil

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
pub fun filter( xs : list<a>, pred : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if pred(x) then Cons(x,xx.filter(pred)) else xx.filter(pred)
    Nil -> Nil

// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd?) == [2]`
pub fun remove( xs : list<a>, pred : a -> bool ) : list<a>
  xs.filter( fn(x) !pred(x) )

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd?) == ([1,3],[2])`
pub fun partition( xs : list<a>, pred : a -> e bool ) : e (list<a>,list<a>)
  partition-acc( xs, pred, Nil, Nil)

fun partition-acc( xs : list<a>, pred : a -> e bool, acc1 : list<a>, acc2 : list<a> ) : e (list<a>, list<a>)
  match xs
    Nil -> (acc1.reverse, acc2.reverse)
    Cons(x,xx) -> if (pred(x))
                    then partition-acc(xx,pred,Cons(x,acc1),acc2)
                    else partition-acc(xx,pred,acc1,Cons(x,acc2))

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if i.odd? then Nothing else Just(i*i) }) == [4]`
pub fun filter-map( xs : list<a>, pred : a -> e maybe<b> ) : e list<b>
  match xs
    Nil -> Nil
    Cons(x,xx) -> match pred(x)
      Nothing -> xx.filter-map(pred)
      Just(y) -> Cons(y,xx.filter-map(pred))

// Find the first element satisfying some predicate
pub fun find( xs : list<a>, pred : a -> bool ) : maybe<a>
  xs.foreach-while fn(x)
    if pred(x) then Just(x) else Nothing

// Find the first element satisfying some predicate and return it.
pub fun find-maybe( xs : list<a>, pred : a -> maybe<b> ) : maybe<b>
  xs.foreach-while(pred)

// Lookup the first element satisfying some predicate
pub fun lookup( xs : list<(a,b)>, pred : a -> e bool ) : e maybe<b>
  xs.foreach-while fn(kv)
    if pred(kv.fst) then Just(kv.snd) else Nothing

// Convert a list to a `:maybe` type, using `Nothing` for an empty list, and otherwise `Just` on the head element.
// Note: this is just `head`.
pub fun maybe( xs : list<a> ) : maybe<a>
  match xs
    Nil -> Nothing
    Cons(x,_) -> Just(x)

// Convert a `:maybe` type to a list type.
pub fun list( m : maybe<a> ) : list<a>
  match m
    Nothing -> Nil
    Just(x) -> Cons(x,Nil)

fun index-of-acc( xs : list<a>, pred : a -> bool, idx : int ) : int
  match xs
    Cons(x,xx) -> if pred(x) then idx else index-of-acc(xx,pred,idx+1)
    Nil        -> -1
    

// Returns the index of the first element where `pred` holds, or `-1` if no such element exists.
pub fun index-of( xs : list<a>, pred : a -> bool ) : int
  index-of-acc( xs, pred, 0 )

// Invoke `action` for each element of a list
pub fun foreach( xs : list<a>, action : (a) -> e () ) : e ()
  match xs
    Cons(x,xx) -> { action(x); xx.foreach(action) }
    Nil        -> ()    

// Invoke `action` for each element of a list while `action` return `Nothing`
pub fun foreach-while( xs : list<a>, action : (a) -> e maybe<b> ) : e maybe<b>
  match xs
    Nil        -> Nothing
    Cons(x,xx) ->
      match action(x)
        Nothing -> xx.foreach-while(action)
        just    -> just      

// Invoke `action` on each element of a list while `action` returns `Just`
pub fun map-while( xs : list<a>, action : (a) -> e maybe<b> ) : e list<b>
  match xs
    Nil        -> Nil
    Cons(x,xx) ->
      match action(x)
        Just(y) -> Cons(y,xx.map-while(action))
        Nothing -> Nil


// Invoke `action` for each element of a list, passing also the position of the element.
pub fun foreach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
  var i := 0
  xs.foreach fn(x)
    val j = i // don't dereference `i` inside the inject
    mask<local>{ action(j,x) }
    i := i+1

// Insert a separator `sep`  between all elements of a list `xs` .
pub fun intersperse( xs : list<a>, sep : a ) : list<a>
  fun before(ys)
    match ys
      Cons(y,yy) -> Cons(sep,Cons(y,before(yy)))
      Nil        -> Nil
  match xs
    Cons(x,xx)  -> Cons(x, xx.before)
    Nil         -> Nil

// Concatenate all strings in a list
fun joinsep( xs : list<string>, sep : string ) : string
  fun join-acc( ys : list<string>, acc : string )
    match ys
      Cons(y,yy) -> join-acc(yy, acc ++ sep ++ y)  // todo: use string builder
      Nil -> acc
  match xs
    Nil -> ""
    Cons(x,xx) -> join-acc(xx,x)

// Concatenate all strings in a list
pub fun join( xs : list<string> ) : string
  xs.joinsep("")

// Concatenate all strings in a list using a specific separator
pub fun join( xs : list<string>, sep : string ) : string
  xs.joinsep(sep)

// Concatenate all strings in a list in reverse order
pub fun reverse-join( xs : list<string> ) : string
  xs.reverse.joinsep("")  

// Append `end` to each string in the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
pub fun join-end( xs : list<string>, end : string) : string
  match xs
    Nil -> ""
    _   -> xs.joinsep(end) ++ end

// Concatenate all lists in a list (e.g. flatten the list). (tail-recursive)
pub fun concat( xss : list<list<a>> ) : list<a>
  fun concat-pre( ys : list<a>, zss : list<list<a>> ) : list<a>
    match ys
      Cons(y,yy) -> Cons(y,concat-pre(yy,zss))
      Nil -> match zss
        Cons(zs,zzs) -> concat-pre(zs,zzs)
        Nil -> Nil
  concat-pre([],xss)

// Concatenate the result lists from applying a function to all elements.
pub fun flatmap( xs: list<a>, f : a -> e list<b> ) : e list<b>
  fun flatmap-pre( ys, zs )
    match ys
      Cons(y,yy) -> Cons(y,flatmap-pre(yy,zs))
      Nil -> match zs
        Cons(z,zz) -> flatmap-pre(f(z),zz)
        Nil -> Nil
  flatmap-pre([],xs)

// Concatenate the `Just` result elements from applying a function to all elements.
pub fun flatmap-maybe( xs : list<a>, f : a -> e maybe<b> ) : e list<b>
  match xs
    Cons(x,xx) -> match f(x)
      Just(y) -> Cons(y, xx.flatmap-maybe(f))
      Nothing -> xx.flatmap-maybe(f)
    Nil -> Nil

// Concatenate a list of `:maybe` values
pub fun concat-maybe( xs : list<maybe<a>> ) : list<a>
  match xs
    Cons(x,xx) -> match x
      Just(y) -> Cons(y, xx.concat-maybe)
      Nothing -> xx.concat-maybe
    Nil -> Nil

// Return the last element of a list (or `Nothing` for the empty list)
pub fun last( xs : list<a> ) : maybe<a>
  match xs
    Cons(x,Nil) -> Just(x)
    Cons(_,xx)  -> last(xx)
    Nil         -> Nothing

// Return the last element of a list (or `default` for the empty list)
pub fun last( xs : list<a>, default : a ) : a
  match xs
    Cons(x,Nil) -> x
    Cons(_,xx)  -> last(xx,default)
    Nil         -> default

// Return the list without its last element.
// Return an empty list for an empty list.
pub fun init( xs : list<a> ) : list<a>
  match xs
    Cons(x, xx as Cons) -> Cons(x,init(xx))
    _ -> Nil

// Get (zero-based) element `n`  of a list. Return a `:maybe` type.
pub fun []( xs : list<a>, n : int ) : maybe<a>
  match xs
    Cons(x,xx) -> if n>0 then xx[n - 1]
                  elif n==0 then Just(x)
                  else Nothing
    Nil -> Nothing

// Do all elements satisfy a predicate ?
pub fun all( xs : list<a>, predicate : a -> e bool ) : e bool
  match xs
    Cons(x,xx) -> if predicate(x) then xx.all(predicate) else False
    Nil -> True

// Are there any elements in a list that satisfy a predicate ?
pub fun any( xs : list<a>, predicate : a -> e bool ) : e bool
  match xs
    Cons(x,xx) -> if predicate(x) then True else xx.any(predicate)
    Nil -> False

// ----------------------------------------------------------------------------
// Characters
// ----------------------------------------------------------------------------

pub inline extern (==) : (char,char) -> bool  
  inline "(#1 == #2)"
  js inline "(#1 === #2)" 

pub inline extern (!=) : (char,char) -> bool  
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

pub inline extern (<=) : (char,char) -> bool 
  inline "(#1 <= #2)"

pub inline extern (>=) : (char,char) -> bool 
  inline "(#1 >= #2)"

pub inline extern (<)  : (char,char) -> bool 
  inline "(#1 < #2)"

pub inline extern (>)  : (char,char) -> bool 
  inline "(#1 > #2)"

pub fun compare( x : char, y : char ) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq

// Convert a character to its unicode code point
pub inline extern int : (char) -> int
  inline "#1"
  c "kk_integer_from_int"
  cs inline "new BigInteger(#1)"

// Convert a unicode code point to a character
pub inline extern char( i : int) : char
  inline "(#1)"
  c "kk_integer_clamp32"
  cs inline "Primitive.IntToInt32(#1)"

// Add two character code points
pub fun (+)(c : char, d : char) : total char 
  (c.int + d.int).char

// Substract two character codePoints
pub fun (-)(c : char, d : char) : total char 
  (c.int - d.int).char

// Is the character a lower-case ASCII character ?
pub fun is-lower( c : char ) : bool 
  c >= 'a' && c <= 'z'

// Is the character an upper-case ASCII character ?
pub fun is-upper( c : char ) : bool 
  c >= 'A' && c <= 'Z'

// Is the character an ASCII digit ?
pub fun is-digit( c : char ) : bool 
  c >= '0' && c <= '9'

// Is the character an ASCII hexa-decimal digit ?
pub fun is-hex-digit( c : char ) : bool 
  c.is-digit || (c >= 'a' && c <= 'f') || (c >= 'A'  && c <= 'F')

// Is the character an ASCII letter is-
pub fun is-alpha( c : char ) : bool 
  c.is-lower || c.is-upper

// Is the character ASCII letter or digit?
pub fun is-alpha-num( c : char ) : bool 
  c.is-alpha || c.is-digit

// Is the character an ASCII character, e.g. `c <= '\x7F'`  ?
pub fun is-ascii( c : char )     : bool 
  c <= '\x7F'

// Is the character an ASCII control character, e.g. `c < ' '`  ?
pub fun is-control( c : char )   : bool 
  c < ' '

// Tests if a character is an element of `" \t\n\r"`
pub fun is-white( c : char )     : bool 
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

// ----------------------------------------------------------------------------
// Booleans
// ----------------------------------------------------------------------------

pub fun (==)( x : bool, y : bool) : bool 
  if x then y else !y

pub fun (!=)( x : bool, y : bool) : bool 
  if x then !y else y

pub fun (<)( x : bool, y : bool) : bool  
  (!x && y)

pub fun (<=)( x : bool, y : bool) : bool 
  !(x > y)

pub fun (>)( x : bool, y : bool) : bool  
  (x && !y)

pub fun (>=)( x : bool, y : bool) : bool 
  !(x < y)

pub fun compare( x : bool, y : bool) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq

// Transform a boolean to a maybe type, using `Nothing` for `False`
pub fun maybe( b : bool ) : maybe<()>
  if b then Just(()) else Nothing


// ----------------------------------------------------------------------------
// Integers
// ----------------------------------------------------------------------------

// Compare two integers
pub inline extern compare(^x : int, ^y : int) : order
  c inline "kk_int_as_order(kk_integer_cmp_borrow(#1,#2,kk_context()),kk_context())"
  cs "Primitive.IntCompare"
  js "$std_core._int_compare"

// Are two integers equal?
pub inline extern (==)(^x : int, ^y : int) : bool
  c  "kk_integer_eq_borrow"
  cs inline "(#1 == #2)"
  js "$std_core._int_eq"

// Are two integers not equal?
pub inline extern (!=)(^x : int, ^y : int) : bool
  c  "kk_integer_neq_borrow"
  cs inline "(#1 != #2)"
  js "$std_core._int_ne"

// Is the first integer smaller or equal to the second?
pub inline extern (<=)(^x : int, ^y : int) : bool
  c  "kk_integer_lte_borrow"
  cs inline "(#1 <= #2)"
  js "$std_core._int_le"

// Is the first integer greater or equal to the second?
pub inline extern (>=)(^x : int, ^y : int) : bool
  c  "kk_integer_gte_borrow"
  cs inline "(#1 >= #2)"
  js "$std_core._int_ge"

// Is the first integer smaller than the second?
pub inline extern (<)(^x : int, ^y : int) : bool
  c  "kk_integer_lt_borrow"
  cs inline "(#1 < #2)"
  js "$std_core._int_lt"

// Is the first integer greater than the second?
pub inline extern (>)(^x : int, ^y : int) : bool
  c  "kk_integer_gt_borrow"
  cs inline "(#1 > #2)"
  js "$std_core._int_gt"

inline extern int-add : (int,int) -> int
  c  "kk_integer_add"
  cs inline "(#1 + #2)"
  js "$std_core._int_add"

// Add two integers.
pub fun (+)(x : int, y : int ) : int 
  int-add(x,y)

inline extern int-sub : (int,int) -> int
  c  "kk_integer_sub"
  cs inline "(#1 - #2)"
  js "$std_core._int_sub"

// Substract two integers.
pub fun (-)(x : int, y : int ) : int 
  int-sub(x,y)

// Multiply two integers.
pub inline extern (*) : (int,int) -> int
  c  "kk_integer_mul"
  cs inline "(#1 * #2)"
  js "$std_core._int_mul"

// Euclidean-0 division of two integers. See also `divmod:(x : int, y : int) -> (int,int)`.
pub inline extern (/)(x:int,y:int) : int
  c  "kk_integer_div"
  cs "Primitive.IntDiv"
  js "$std_core._int_div"

// Euclidean modulus of two integers; always a non-negative number. See also `divmod:(x : int, y : int) -> (int,int)`.
pub inline extern (%)  : (int,int) -> int
  c  "kk_integer_mod"
  cs "Primitive.IntMod"
  js "$std_core._int_mod"

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
pub inline extern divmod(x:int,y:int) : (int,int)
  c  "kk_integer_div_mod_tuple"
  cs "Primitive.IntDivMod"
  js "$std_core._int_divmod"

pub fun negate(i : int) : int
  ~i

// Negate an integer.
pub inline extern (~)(i:int) : int
  c  "kk_integer_neg"
  cs inline "(-#1)"
  js "$std_core._int_negate"

// Convert an integer to a `:float64`. May return `nan` if the integer is too large to represent as a `:float64`.
pub inline extern float64( i : int) : float64
  c  "kk_integer_as_double"
  cs "Primitive.IntToDouble"
  js "$std_core._int_to_double"

// Convert an integer to a `:float32`. May return `nan` if the integer is too large to represent as a `:float32`.
pub inline extern float32( i : int) : float32
  c  "kk_integer_as_float"
  cs "Primitive.IntToFloat"
  js "$std_core._int_to_float"  

// Is this an odd integer?
pub inline extern is-odd( i : int ) : bool
  c  "kk_integer_is_odd"
  cs inline "!(#1.IsEven)"
  js "$std_core._int_isodd"

// Is this equal to zero?
pub inline extern is-zero( ^x : int) : bool
  c  inline "kk_integer_is_zero_borrow(#1)"
  cs inline "(#1.IsZero)"
  js "$std_core._int_iszero"

// Return the absolute value of an integer.
pub inline extern abs(i : int) : int
  c  "kk_integer_abs"
  cs "BigInteger.Abs"
  js "$std_core._int_abs"

pub fun inc( i : int ) : int
  i + 1

pub fun dec( i : int ) : int
  i - 1

// Calculate `10^exp`
pub fun exp10( exp : int ) : int
  1.mul-exp10(exp)

// Raise an integer `i` to the power of `exp`.
pub extern pow( i : int, exp : int ) : int
  c  "kk_integer_pow"
  cs "Primitive.IntPow"
  js "_int_pow"

// Raise an integer `i` to the power of `exp`.
pub fun (^)(i : int, exp : int ) : int
  pow(i,exp)

// Calculate `2^exp`.
pub fun exp2( exp : int ) : int
  pow(2,exp)

// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.
pub extern is-exp10( i : int ) : int
  c  "kk_integer_ctz"
  cs "Primitive.IntCountPow10"
  js "_int_count_pow10"

// Return the number of decimal digits of `i`. Return `0` when `i==0`.
pub extern count-digits( i : int ) : int
  c  "kk_integer_count_digits"
  cs "Primitive.IntCountDigits"
  js "_int_count_digits"

pub extern mul-exp10( i : int, n : int ) : int
  c  "kk_integer_mul_pow10"
  cs "Primitive.IntMulPow10"
  js "_int_mul_pow10"

pub extern cdiv-exp10( i : int, n : int ) : int
  c  "kk_integer_cdiv_pow10"
  cs "Primitive.IntCDivPow10"
  js "_int_cdiv_pow10"

pub fun cdivmod-exp10( i : int, n : int ) : (int,int)
  if n <= 0 return (i,0)
  val cq = i.cdiv-exp10(n)
  val cr = i - cq.mul-exp10(n)
  (cq,cr)

pub fun divmod-exp10( i : int, n : int ) : (int,int)
  val (cq,cr) = cdivmod-exp10(i,n)
  if !cr.is-neg then (cq,cr) else (cq.dec, cr + exp10(n))

// Is this an even integer?
pub fun is-even(i:int) : bool 
  !is-odd(i)

// Is the integer positive (stricly greater than zero)
pub fun is-pos(i : int ) : bool 
  i.sign == Gt

// Is the integer negative (stricly smaller than zero)
pub fun is-neg(i : int ) : bool 
  i.sign == Lt

pub inline extern sign( ^i : int ) : order
  c  inline "kk_int_as_order(kk_integer_signum_borrow(#1),kk_context())"
  cs "Primitive.IntSign"
  js "$std_core._int_sign"

// Return the minimum of two integers
pub fun min( i : int, j : int ) : int 
  if i <= j then i else j

// Return the maximum of two integers
pub fun max( i : int, j : int ) : int 
  if i >= j then i else j

// Returns the smallest element of a list of integers (or `default` (=`0`) for the empty list)
pub fun minimum( xs : list<int>, default : int = 0 ) : int
  match xs
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, min )

// Returns the largest element of a list of integers (or `default` (=`0`) for the empty list)
pub fun maximum( xs : list<int>, default : int = 0 ) : int
  match xs
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, max )

// Return the sum of a list of integers
pub fun sum( xs : list<int> ) : int
  xs.foldl( 0, fn(x,y) { x + y } )

// Transform an integer to a maybe type, using `Nothing` for `0`
pub fun maybe( i : int ) : maybe<int>
  if i==0 then Nothing else Just(i)

// fold over the integers between [`start`,`end`] (inclusive).
pub fun fold-int( start : int, end : int, init : a, f : (int,a) -> e a ) : e a
  if start >= end then init else
    val x = f(start,init)
    fold-int(unsafe-decreasing(start.inc), end, x, f)

// fold over the integers between [0,`upto`)  (not including `upto`).
pub fun fold-int( upto : int, init : a, f : (int,a) -> e a ) : e a
  fold-int( 0, upto.dec, init, f )


pub fun fold-while-int( start : int, end : int, init : a, f : (int,a) -> e maybe<a> ) : e a
  if start >= end then init else
    match f(start,init)
      Just(x) -> fold-while-int(unsafe-decreasing(start.inc), end, x, f)
      Nothing -> init

// ----------------------------------------------------------------------------
// 32-bit integers
// Just define the operations needed for defining the std/core interface but
// don't export any definitions here. Full operations are defined in `std/int32`.
// ----------------------------------------------------------------------------

// Convert an `:int32` to an `:int`.
pub inline extern int( i : int32 ) : int
  c  "kk_integer_from_int"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"

// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.
pub extern int32( i : int) : int32
  c  "kk_integer_clamp32"
  cs "Primitive.IntToInt32"
  js "$std_core._int_clamp32"

// Convenient shorthand to `int32`, e.g. `1234.i32`
pub inline fun i32( i : int ) : int32
  i.int32



// Minimal set of operations that we need in `std/core`.
inline extern (<=) : (int32,int32) -> bool  
  inline "(#1 <= #2)"
  js inline "(#1 <= #2)"

inline extern (<)  : (int32,int32) -> bool  
  inline "(#1 < #2)"
  js inline "(#1 < #2)"

inline extern (+)  : (int32,int32) -> int32 
  inline "(#1 + #2)"
  js inline "((#1 + #2)|0)"

inline extern (-)  : (int32,int32) -> int32 
  inline "(#1 - #2)"
  js inline "((#1 - #2)|0)"

inline extern is-pos( i : int32 ) : bool 
  inline "(#1>0)"

inline extern is-neg( i : int32 ) : bool 
  inline "(#1<0)"


fun incr( i : int32 ) : int32
  i + 1.int32

fun decr( i : int32 ) : int32
  i - 1.int32

// ----------------------------------------------------------------------------
// N-bit ssize_t
// Just define the operations needed for defining the std/core interface but
// don't export any definitions here.
// ----------------------------------------------------------------------------

// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
pub extern ssize_t( i : int) : ssize_t
  c  "kk_integer_clamp_ssize_t" 
  cs "Primitive.IntToInt32"
  js "$std_core._int_clamp32"

// Convert an `:ssize_t` to an `:int`.
pub inline extern int( i : ssize_t ) : int
  c  "kk_integer_from_ssize_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"

// Minimal set of operations that we need in `std/core`.
inline extern (<=) : (ssize_t,ssize_t) -> bool   
  inline "(#1 <= #2)"

inline extern (>=) : (ssize_t,ssize_t) -> bool   
  inline "(#1 >= #2)"

inline extern (<)  : (ssize_t,ssize_t) -> bool   
  inline "(#1 < #2)"

inline extern (+)  : (ssize_t,ssize_t) -> ssize_t 
  inline "(#1 + #2)"
  js inline "((#1 + #2)|0)"

inline extern (-)  : (ssize_t,ssize_t) -> ssize_t 
  inline "(#1 - #2)"
  js inline "((#1 - #2)|0)"

inline extern is-pos( i : ssize_t ) : bool 
  inline "(#1 > 0)"

inline extern is-neg( i : ssize_t ) : bool 
  inline "(#1 < 0)"

extern is-zero( i : ssize_t ) : bool 
  inline "(#1 == 0)"
  js inline "(#1 === 0)"

extern decr(i : ssize_t ) : ssize_t 
  inline "(#1 - 1)"

extern incr(i : ssize_t ) : ssize_t 
  inline "(#1 + 1)"


// ----------------------------------------------------------------------------
// int8
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:int8`.
pub extern int8( i : int) : int8
  c  "kk_integer_clamp_int8"
  cs "Primitive.IntToInt8"
  js "$std_core._int_clamp8"

// Convert an `:int8` to an `:int`.
pub inline extern int( i : int8 ) : int
  c  "kk_integer_from_int8"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"


// clamp an `:int` to fit in an `:int8` but interpret the `:int` as an unsigned 8-bit value,
// and clamp between 0 and 255.
pub extern uint8( i : int) : int8
  c  "kk_integer_clamp_byte"
  cs "Primitive.IntToUInt8"
  js "$std_core._int_clamp_byte"

// Convert an `:int8` to an `:int` but interpret the `:int8` as an unsigned 8-bit value between 0 and 255.
pub inline extern uint( i : int8 ) : int
  c  "kk_integer_from_uint8"
  cs inline "(new BigInteger(#1 >= 0 ? #1 : 256 + #1))"
  js "$std_core._int_from_int32"


// ----------------------------------------------------------------------------
// int16
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:int16`.
pub extern int16( i : int) : int16
  c  "kk_integer_clamp_int16"
  cs "Primitive.IntToInt16"
  js "$std_core._int_clamp16"

// Convert an `:int16` to an `:int`.
pub inline extern int( i : int16 ) : int 
  c  "kk_integer_from_int16"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int32"


// ----------------------------------------------------------------------------
// int64
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:int64_t`.
pub extern int64( i : int) : int64
  c  "kk_integer_clamp64"
  cs "Primitive.IntToInt64"
  js "$std_core._int_clamp64"

// Convert an `:int64_t` to an `:int`.
pub inline extern int( i : int64 ) : int
  c  "kk_integer_from_int64"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int64"

// Convenient shorthand to `int64`, e.g. `1234.i64`
pub inline fun i64( i : int ) : int64
  i.int64


// ----------------------------------------------------------------------------
// intptr_t
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:intptr_t`. 
pub extern intptr_t( i : int) : intptr_t
  c  "kk_integer_clamp_intptr_t"
  cs "Primitive.IntToInt64"
  js "$std_core._int_clamp64"

// Convert an `:intptr_t` to an `:int`.
pub inline extern int( i : intptr_t ) : int
  c  "kk_integer_from_intptr_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_from_int64"



// ----------------------------------------------------------------------------
// Parse numbers
// ----------------------------------------------------------------------------

// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.
pub fun parse-int-default( s : string, default : int = 0, hex : bool = False ) : int
  if s.is-empty then default else s.parse-int(hex).maybe(default)

// Parse an integer after trimming whitespace.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string will result in `Just(0)`.
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
pub fun parse-int( s : string, hex : bool = False) : maybe<int>
  s.trim.xparse-int(hex)

extern xparse-int( s : string, hex : bool ) : maybe<int>
  c  "kk_integer_xparse"
  cs "Primitive.IntParse"
  js "_int_parse"

// ----------------------------------------------------------------------------
// Floating point
// todo: move to std/num/float64
// ----------------------------------------------------------------------------

pub inline extern (==) : (float64,float64) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
pub inline extern (!=) : (float64,float64) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
pub inline extern (<=) : (float64,float64) -> bool  { inline "(#1 <= #2)" }
pub inline extern (>=) : (float64,float64) -> bool  { inline "(#1 >= #2)" }
pub inline extern (<)  : (float64,float64) -> bool  { inline "(#1 < #2)" }
pub inline extern (>)  : (float64,float64) -> bool  { inline "(#1 > #2)" }
pub inline extern (+) : (float64,float64) -> float64 { inline "(#1 + #2)" }
pub inline extern (-) : (float64,float64) -> float64 { inline "(#1 - #2)" }
pub inline extern (*) : (float64,float64) -> float64 { inline "(#1 * #2)" }
pub inline extern (/) : (float64,float64) -> float64 { inline "(#1 / #2)" }
pub inline extern (%) : (float64,float64) -> float64 { c inline "fmod(#1,#2)"; inline "(#1 % #2)" }


// Is the value negative?
pub fun is-neg( d : float64 ) : bool
  d < 0.0

// Is the value positive?
pub fun is-pos( d : float64 ) : bool
  d > 0.0

// Is the value zero?
pub fun is-zero( d : float64 ) : bool
  d == 0.0

pub fun sign( d : float64 ) : order
  if d < 0.0 then Lt elif d > 0.0 then Gt else Eq

// Negate a `:float64`.
pub inline extern (~)( f : float64 ) : float64
  inline "(-#1)"  // inline so `~0.0` becomes negative zero

// convert a `:float64` to an `:int` using `round` to round to its nearest integer.
// (rounding to an even number on a tie)
// Returns `0` if the argument is not `finite?`.
pub inline extern int( f : float64 ) : int
  c  "kk_integer_from_double"
  cs "Primitive.IntDouble"
  js "$std_core._int_double"

// Returns the value `f`  raised to the power `p` .
pub inline extern (^)( f : float64, p : float64) : float64
  c inline "pow(#1,#2)"
  cs "Math.Pow"
  js "Math.pow"

// Return the absolute value of a `:float64` `f`
pub inline extern abs( f : float64 ) : float64
  c inline "kk_double_abs(#1)"
  cs "Math.Abs"
  js "Math.abs"

// Returns the smallest of two floats
pub fun min( x : float64, y : float64 ) : float64 
  if x <= y then x else y

// Returns the largest of two floats
pub fun max( x : float64, y : float64 ) : float64 
  if x >= y then x else y

// Returns the smallest element of a list of floats (or `0` for the empty list)
pub fun minimum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, min )

// Returns the largest element of a list of floats (or `0` for the empty list)
pub fun maximum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, max )

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
pub fun ".new-sslice"( str :string, start: ssize_t, len : ssize_t )
  Sslice(str,start,len)

// Convert a character to a string
pub extern string : ( c : char ) -> string
  c  "kk_string_from_char"
  cs inline "Primitive.CharToString(#1)"
  js inline "_char_to_string(#1)"

// Convert a vector of characters to a string.
pub extern string : (vector<char>) -> string
  c  "kk_string_from_chars"
  cs inline "Primitive.CharsToString(#1)"
  js inline "_chars_to_string(#1)"

// Convert a string to a vector of characters.
pub extern vector : ( s : string ) -> vector<char>
  c  "kk_string_to_chars"
  cs inline "Primitive.StringToChars(#1)"
  js inline "_string_to_chars(#1)"

// Convert a string to a list of characters
pub extern list( s : string ) : total list<char>
  c  "kk_string_to_list"
  cs inline "Primitive.StringToList(#1)"
  js inline "_string_to_list(#1)"

// Convert a list of characters to a string
pub extern string( cs : list<char> ) : total string
  c  "kk_string_from_list"
  cs inline "Primitive.ListToString(#1)"
  js inline "_list_to_string(#1)"

// Are two strings equal?
// Uses exact equality between character codes.
pub inline extern (==) : (string,string) -> bool
  c  "kk_string_is_eq"
  cs inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two strings not equal?
pub inline extern (!=) : (string,string) -> bool
  c  "kk_string_is_neq"
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

pub inline fun (==)(l1: sslice, l2: sslice ): bool
  match (l1, l2)
    (Sslice(s1, i1, f1), Sslice(s2, i2, f2)) ->
      if f1.int != f2.int then False
      elif i1.int != i2.int then False
      else s1 == s2

pub inline fun (!=)(l1: sslice, l2: sslice ): bool
  match (l1, l2)
    (Sslice(s1, i1, f1), Sslice(s2, i2, f2)) ->
      if f1.int != f2.int then True
      elif i1.int != i2.int then True
      else s1 != s2

extern string-compare : ( x : string, y : string ) -> int
  c  "kk_string_cmp_int"
  cs inline "String.Compare(#1,#2)"
  js inline "(#1===#2 ? 0 : (#1 > #2 ? 1 : -1))"

// Compare two strings.
// Uses the character codes directly for comparison
pub fun compare( x : string, y : string) : order
  string-compare(x,y).order

pub fun (>=)( x : string, y : string ) : bool { compare(x,y) > Lt }
pub fun (<=)( x : string, y : string ) : bool { compare(x,y) < Gt }
pub fun (>)( x : string, y : string ) : bool  { compare(x,y) == Gt }
pub fun (<)( x : string, y : string ) : bool  { compare(x,y) == Lt }

// Append two strings.
pub extern (++)(x : string, y : string) : string
  c "kk_string_cat"
  inline "(#1 + #2)"

// Choose a non-empty string
pub fun (||)( x : string, y : string ) : string
  if x.is-empty then y else x

// Length returns the length in the platform specific encoding (and should not be exported)
inline extern length( s : string ) : ssize_t
  c inline "kk_string_len(#1,kk_context())"
  cs inline "#1.Length"
  js inline "#1.length"

// O(n). Return the number of characters in a string.
pub extern count( s : string ) : int
  c  "kk_string_count_int"
  cs "Primitive.StringCount"
  js "_string_count"

// O(n). Return the number of characters in a string slice
pub extern count( slice : sslice ) : int
  c  "kk_slice_count"
  cs "Primitive.SliceCount"
  js "_sslice_count"

extern first1(s : string ) : sslice
  c  "kk_slice_first"
  cs "Primitive.SliceFirst"
  js "_sslice_first"

extern last1(s : string ) : sslice
  c  "kk_slice_last"
  cs "Primitive.SliceLast"
  js "_sslice_last"

// O(`n`). The first `n` (default = `1`) characters in a string.
pub fun first(s : string, n : int = 1) : sslice
  val slice = s.first1
  if n==1 then slice else slice.extend( n - 1 )

// O(`n`). The last `n` (default = `1`) characters in a string
pub fun last(s : string, n : int = 1) : sslice
  val slice = s.last1
  if n==1 then slice
            else slice.advance(1 - n).extend(n - 1)

// O(1). The entire string as a slice
pub fun slice( s : string ) : sslice
  Sslice(s,0.ssize_t,s.length)

// An empty slice
val empty = Sslice("",0.ssize_t,0.ssize_t)

// Is a slice empty?
pub fun is-empty( slice : sslice ) : bool
  !slice.len.is-pos

// An invalid slice
val invalid = Sslice("",(-1).ssize_t,0.ssize_t)

// Is a slice invalid?
pub fun is-valid( slice : sslice ) : bool
  slice.start >= 0.ssize_t

// Is a slice not empty?
pub fun is-notempty( slice : sslice ) : bool
  slice.len.is-pos

// Return the first character of a string as a string (or the empty string)
pub fun head( s : string ) : string
  s.first.string

// Return the tail of a string (or the empty string)
pub fun tail( s : string ) : string
  s.first.after.string

// Return the common prefix of two strings (upto `upto` characters (default is minimum length of the two strings))
pub extern common-prefix(s : string, t : string, ^upto : int = -1 ) : sslice
  c  "kk_slice_common_prefix_borrow"
  cs "Primitive.SliceCommonPrefix"
  js "_sslice_common_prefix"

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
pub extern advance( slice : sslice, ^count : int ) : sslice
  c  "kk_slice_advance_borrow"
  cs "Primitive.SliceAdvance"
  js "_sslice_advance"

// O(`count`). Extend a string slice by `count` characters up to the end of the string.
// A negative `count` shrinks the slice up to the empty slice.
// For example:
//
// * `"abc".first.extend(1).string == "ab"`
// * `"abc".last.extend(-1).string == ""`
//
pub extern extend( slice : sslice, ^count : int ) : sslice
  c  "kk_slice_extend_borrow"
  cs "Primitive.SliceExtend"
  js "_sslice_extend"

// O(1). Return the string slice from the start of a string up to the
// start of `slice` argument.
pub fun before(slice : sslice) : sslice
  val Sslice(s,start,_len) = slice
  Sslice(s,0.ssize_t,start)

// O(1). Return the string slice from the end of `slice` argument
// to the end of the string.
pub fun after(slice : sslice) : sslice
  val Sslice(s,start,len) = slice
  Sslice(s,start+len,s.length - (start+len))

// O(n). Copy the `slice` argument into a fresh string.
// Takes O(1) time if the slice covers the entire string.
pub extern string( slice : sslice ) : string
  c  "kk_slice_to_string"
  cs "Primitive.SliceToString"
  js "_slice_to_string"

// If the slice is not empty,
// return the first character, and a new slice that is advanced by 1.
pub extern next( slice : sslice ) : maybe<(char,sslice)>
  c  "kk_slice_next"
  cs "Primitive.SliceNext"
  js "_sslice_next"

pub fun substr( slice: sslice, start : int, end : int ): sslice
  val Sslice(s, st, len) = slice
  val newst = st.int + start
  val newLen = end - start
  if newst + newLen > len.int then
    Sslice(s, newst.ssize_t, (len.int - start).ssize_t)
  else
    Sslice(s, newst.ssize_t, newLen.ssize_t) 

// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.
pub fun foreach-while( slice : sslice, action : (c : char) -> e maybe<a> ) : e maybe<a>
  match slice.next
    Nothing -> Nothing
    Just((c,rest)) ->
      match action(c)
        Nothing -> foreach-while(unsafe-decreasing(rest),action)
        res     -> res


// Apply a function for each character in a string slice.
pub fun foreach( slice : sslice, action : (c:char) -> e ()) : e () 
  foreach-while( slice ) fn(c)
    action(c)
    Nothing
  ()

// Does string `s`  contain the string `sub`  ?
inline extern xindex-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_index_of1(#1,#2,kk_context())"
  cs inline "((#1).IndexOf(#2) + 1)"
  js inline "((#1).indexOf(#2) + 1)"

// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.
pub fun find( s : string, sub : string ) : maybe<sslice>
  val i = s.xindex-of(sub)
  if i.is-zero then Nothing else Just(Sslice(s,i.decr,sub.length))

// Does string `s`  contain the string `sub`  ?
inline extern xlast-index-of(s : string, sub : string ) : ssize_t
  c inline "kk_string_last_index_of1(#1,#2,kk_context())"
  cs inline "(#1.LastIndexOf(#2) + 1)"
  js inline "((#1).lastIndexOf(#2) + 1)"

// Return the last index of substring `sub` in `s` if it occurs.
pub fun find-last( s : string, sub : string ) : maybe<sslice>
  val i = s.xlast-index-of(sub)
  if i.is-zero then Nothing else Just(Sslice(s,i.decr,sub.length))

inline extern xstarts-with: (s : string, pre : string ) -> bool
  c  "kk_string_starts_with"
  cs inline "#1.StartsWith(#2)"
  js inline "(#1.substr(0,#2.length) === #2)"

// Is `pre`  a prefix of `s`? If so, returns a slice
// of `s` following `pre` up to the end of `s`.
pub fun starts-with( s : string, pre : string ) : maybe<sslice>
  if xstarts-with(s,pre)
    then Just(Sslice(s,pre.length,s.length - pre.length))
    else Nothing

extern xends-with: (s : string, post : string ) -> bool
  c  "kk_string_ends_with"
  cs inline "#1.EndsWith(#2)"
  js inline "((#1).indexOf(#2, (#1).length - (#2).length) !== -1)"

// Does string `s`  end with `post`?
// If so, returns a slice of `s` from the start up to the `post` string at the end.
pub fun ends-with( s : string, post : string ) : maybe<sslice>
  if (xends-with(s,post))
    then Just(Sslice(s,0.ssize_t,s.length - post.length))
    else Nothing

// Does string `s`  contain the string `sub`  ?
pub inline extern contains: (s : string, sub : string ) -> bool
  c  "kk_string_contains"
  cs inline "((#1).Contains(#2))"
  js inline "((#1).indexOf(#2) >= 0)"

// Concatenate a vector of strings
pub inline extern join: (v : vector<string> ) -> string
  c  "kk_string_join"
  cs "String.Concat"
  js inline "((#1).join(''))"

// Concatenate a vector of strings with a separator `sep`
pub inline extern join: (v : vector<string>, sep : string ) -> total string
  c  "kk_string_join_with"
  cs "Primitive.Concat"
  js inline "((#1).join(#2))"

// Truncate a string to `count` characters.
pub fun truncate( s : string, count : int ) : string
  s.first.extend(count - 1).string

// Trim off a substring `sub` while `s` starts with that string.
pub fun trim-left( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.starts-with(sub)
    Just(slice) -> trim-left(unsafe-decreasing(slice.string),sub)
    Nothing -> s

// Trim off a substring `sub` while `s` ends with that string.
pub fun trim-right( s : string, sub : string ) : string
  if sub.is-empty return s
  match s.ends-with(sub)
    Just(slice) -> trim-right(unsafe-decreasing(slice.string),sub)
    Nothing -> s

// Repeat a string `n` times
pub fun repeat( s : string, ^n : int ) : string
  repeatz(s,n.ssize_t)

extern repeatz( s : string, n : ssize_t ) : string
  c  "kk_string_repeat"
  cs "Primitive.Repeat"
  js "_string_repeat"

// Convert a `:maybe` string to a string using the empty sting for `Nothing`
pub fun string( ms : maybe<string> ) : string
  match ms
    Nothing -> ""
    Just(s) -> s

// Is a string empty?
pub fun is-empty( s : string ) : bool
  s == ""

// Is a string not empty?
pub fun is-notempty( s : string ) : bool
  s != ""

// Transform a string to a maybe type, using `Nothing` for an empty string
pub fun maybe( s : string ) : maybe<string>
  if s.is-empty then Nothing else Just(s)

inline extern splitv( s : string,  sep : string ) : vector<string>
  c  "kk_string_splitv"
  cs inline "(#1.Split(new String[]{#2}, StringSplitOptions.None))"
  js inline "((#1).split(#2))"

inline extern splitv( s : string,  sep : string, n : ssize_t ) : vector<string>
  c  "kk_string_splitv_atmost"
  cs inline "#1.Split(new String[]{#2},#3, StringSplitOptions.None)"
  js inline "(#1).split(#2, #3)"

// Split a string into parts that were delimited by `sep`. The delimeters are not included in the results.
// For example: `split("1,,2",",") == ["1","","2]`
pub fun split( s : string, sep : string ) : list<string>
  splitv(s,sep).list

// Split a string into at most `n` parts that were delimited by a string `sep`. The delimeters are not included in the results (except for possibly the final part).
// For example: `split("1,2,3",",",2) == ["1","2,3"]`
pub fun split( s : string, sep: string, ^n : int) : list<string>
  splitv(s,sep,n.ssize_t).list

// Replace every occurrence of `pattern` to `repl` in a string.
pub inline extern replace-all( s : string, pattern : string, repl : string ) : string
  c  "kk_string_replace_all"
  cs inline "(#1).Replace(#2,#3)"
  js inline r"(#1).replace(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),#3)"

// Count occurences of `pattern` in a string.
pub inline extern count( s : string, pattern : string ) : int
  c  "kk_string_count_pattern"
  cs inline "Primitive.Count(#1,#2)"
  js inline r"((#2) ? ((#1).match(new RegExp((#2).replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0)"

// Apply a function `f` to each character in a string
pub fun map( s : string, f : char -> e char ) : e string
  s.list.map(f).string // todo: optimize

// Invoke a function for each character in a string.
// If `action` returns `Just`, the function returns immediately with that result.
pub fun foreach-while( s : string, action : (c:char) -> e maybe<a> ) : e maybe<a>
  s.slice.foreach-while(action)

// Invoke a function for each character in a string
pub fun foreach( s : string, action : (c:char) -> e () ) : e ()
  s.slice.foreach(action)

// Return the first character of a string (or `Nothing` for the empty string).
pub fun head-char( s : string ) : maybe<char>
  s.foreach-while( Just )

// Invoke a function for each character in a string
//fun foreach( s : string, f : char -> e () ) : e ()
//  s.list.foreach(f)  // todo: optimize
//

// Count the number of times a predicate is true for each character in a string
pub fun count( s : string, pred : (char) -> bool ) : int
  var cnt := 0
  s.foreach fn(c)
    if pred(c) then cnt := cnt+1
  cnt

// Split a string into a list of lines
pub fun lines( s : string ) : list<string>
  s.split("\n")

// Join a list of strings with newlines
pub fun unlines( xs : list<string> ) : string
  xs.join("\n")

// Convert a string to upper-case
pub extern to-upper : (s : string) -> string
  c "kk_string_to_upper"
  cs inline "(#1).ToUpper()"
  js inline "(#1).toUpperCase()"

// Convert a string to lower-case
pub extern to-lower : (s : string) -> string
  c "kk_string_to_lower"
  cs inline "(#1).ToLower()"
  js inline "(#1).toLowerCase()"

// Convert the first character of a string to uppercase.
pub fun capitalize( s : string ) : string
  s.first.string.to-upper ++ s.first.after.string

// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.
pub fun pad-left( s : string, ^width : int, fill : char = ' ') : string
  val w = width.ssize_t
  val n = s.length
  if w <= n
    then s
    else fill.string.repeatz( w - n ) ++ s

// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.
pub fun pad-right( s : string, ^width : int, fill : char = ' ') : string
  val w = width.ssize_t
  val n = s.length
  if w <= n
    then s
    else s ++ fill.string.repeatz(w - n)

// Trim whitespace on the left and right side of a string
pub fun trim( s : string ) : string
  s.trim-left.trim-right

// Trim the starting white space of a string
pub inline extern trim-left( s : string ) : string
  c  "kk_string_trim_left"
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"

// Trim the ending white space of a string.
pub inline extern trim-right( s : string ) : string
  c  "kk_string_trim_right"
  cs inline "(#1).TrimEnd()"
  js inline "((#1).replace(/\\s+$/,''))"


// ----------------------------------------------------------------------------
//  Vectors
// ----------------------------------------------------------------------------

// Return the element at position `index`  in vector `v`  without bounds check!
inline extern unsafe-idx( ^v : vector<a>, index : ssize_t ) : total a
  c  inline "kk_vector_at_borrow(#1,#2)"
  cs inline "(#1)[#2]"
  js inline "(#1)[#2]"

inline extern unsafe-assign : forall<a> ( v : vector<a>, i : ssize_t, x : a ) -> total ()
  c "kk_vector_unsafe_assign"
  cs inline "(#1)[#2] = #3"
  js inline "(#1)[#2] = #3"

inline extern unsafe-vector : forall<a> ( n : ssize_t ) -> total vector<a>
  c  inline "kk_vector_alloc_uninit(#1,NULL,kk_context())"
  cs inline "(new ##1[#1])"
  js inline "Array(#1)"

// Return the element at position `index`  in vector `v` . Raise an out of bounds exception if `index < 0`  or `index >= v.length` .
pub inline extern []( ^v : vector<a>, ^index : int ) : exn a
  c "kk_vector_at_int_borrow"
  cs inline "(#1)[Primitive.IntToInt32(#2)]"
  js inline "$std_core._vector_at(#1,#2)"

// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds
pub fun at( ^v : vector<a>, ^index : int ) : maybe<a>
  val idx = index.ssize_t
  if v.lengthz <= idx then Nothing else Just(unsafe-idx(v,idx))

// Return the length of a vector.
pub fun length( ^v : vector<a> ) : int
  v.lengthz.int

inline extern lengthz( ^v : vector<a> ) : ssize_t
  c  inline "kk_vector_len_borrow(#1)"
  cs inline "((#1).Length)"
  js inline "((#1).length)"

// Create a new vector of length `n`  with initial elements `default` .
pub fun vector( ^n : int, default : a) : vector<a>
  vector-initz(n.ssize_t, fn(_i){ default } )

// Create a new vector of length `n`  with initial elements given by function `f` .
pub fun vector-init( ^n : int, f : int -> a ) : vector<a>
  vector-initz( n.ssize_t, fn(i) { f(i.int) } )

// Create a new vector of length `n`  with initial elements given by function `f` .
extern vector-initz(n : ssize_t, f : ssize_t -> a) : vector<a>
  c "kk_vector_init"
  cs inline "Primitive.NewArray<##1>(#1,#2)"
  js inline "_vector(#1,#2)"

// Create an empty vector.
pub inline extern vector : forall<a> () -> vector<a>
  c inline "kk_vector_empty()"
  cs inline "new ##1[0]"
  js inline "[]"

// Invoke a function `f` for each element in a vector `v`
pub fun foreach( v : vector<a>, f : (a) -> e () ) : e ()
  v.foreach-indexedz( fn(_,x) { f(x) })

// Invoke a function `f` for each element in a vector `v`
pub fun foreach-indexed( v : vector<a>, f : (int,a) -> e () ) : e ()
  foreach-indexedz( v, fn(i,x) { f(i.int,x) } )

fun foreach-indexedz( v : vector<a>, f : (ssize_t,a) -> e () ) : e ()
  forz( 0.ssize_t, v.lengthz.decr ) fn(i)
    f(i,v.unsafe-idx(i))

// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.
pub fun foreach-while( v : vector<a>, f : a -> e maybe<b> ) : e maybe<b>
  for-whilez( 0.ssize_t, v.lengthz.decr ) fn(i)
    f(v.unsafe-idx(i))

// Apply a total function `f` to each element in a vector `v`
pub fun map( v : vector<a>, f : a -> e b ) : e vector<b>
  val w = unsafe-vector(v.length.ssize_t)
  v.foreach-indexedz fn(i,x)
    unsafe-assign(w,i,f(x))
  w

// Convert a vector to a list.
pub fun list( v : vector<a> ) : list<a>
  v.vlist

// Convert a vector to a list with an optional tail.
pub extern vlist( v : vector<a>, tail : list<a> = [] ) : list<a>
  c  "kk_vector_to_list"
  cs inline "Primitive.VList<##1>(#1,#2)"
  js inline "_vlist(#1,#2)"

// Convert a list to a vector.
pub fun vector( xs : list<a> ) : vector<a>
  xs.unvlist

pub extern unvlist( xs : list<a> ) : vector<a>
  c  "kk_list_to_vector"
  cs inline "Primitive.UnVList<##1>(#1)"
  js inline "_unvlist(#1)"


// ----------------------------------------------------------------------------
// Delayed values
// ----------------------------------------------------------------------------

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract type delayed<e,a>
  con Delay( dref : ref<global,either<() -> e a,a>> )

// Create a new `:delayed` value.
pub fun delay( action : () -> e a ) : delayed<e,a>
  unsafe-total
    Delay(ref(Left(action)))

// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
pub fun force( delayed : delayed<e,a> ) : e a
  unsafe-total
    val r = delayed.dref
    match !r
      Right(x) -> x
      Left(action) ->
        val x = (inject-st{mask<div>(action)})()
        r := Right(x)
        x

// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic show: shows the internal representation of an object as a string
// Note: this breaks parametricity so it should not be pub
extern gshow : forall<a> a -> string
  c "kk_show_any"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

// Convert an `:int` to a string
pub extern show( i : int ) : string
  c "kk_integer_to_string"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

extern int-show-hex(i:int,use-capitals:bool) : string
  c "kk_integer_to_hex_string"
  cs "Primitive.IntShowHex"
  js "_int_showhex"

// Show an `:int` as a hexadecimal value.\
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
pub fun show-hex( i : int, width : int = 1, use-capitals : bool = True, pre : string = "0x" )
  (if i<0 then "-" else "") ++ pre ++ int-show-hex(i.abs,use-capitals).pad-left(width,'0')

 
// Show a `:float64` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.
pub fun show( d : float64, precision : int = -17 ) : string
  val dabs = d.abs
  if dabs >= 1.0e-5 && dabs < 1.0e+21
    then show-fixed(d,precision)
    else show-exp(d,precision)

// Show a `:float64` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.
pub fun show-fixed( d : float64, precision : int = -2 ) : string
  val dabs = d.abs
  if dabs < 1.0e-15 || dabs > 1.0e+21
    then show-exp(d,precision)
    else show-fixedx(d, precision.int32)

extern show-fixedx( d : float64, prec : int32 ) : string
  c  "kk_double_show_fixed"
  cs "Primitive.DoubleShowFixed"
  js "_double_show_fixed"

// Show a `:float64` in exponential (scientific) notation.
// The optional `precision` (= `-17`) specifies the precision.
// If `>=0` it specifies the number of digits behind the dot (up to `17` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
pub fun show-exp( d : float64, precision : int = -17 )
  show-expx(d,precision.int32)

extern show-expx( d : float64, prec : int32 ) : string
  c  "kk_double_show_exp"
  cs "Primitive.DoubleShowExp"
  js "_double_show_exp"

// Show a character as a string
fun show-char( c : char ) : string

  if c < ' ' || c > '~' then
    if c == '\n' then "\\n"
    elif c == '\r' then "\\r"
    elif c == '\t' then "\\t"
    elif c.int <= 0xFF then "\\x" ++ c.int.show-hex(2,pre="")
    elif c.int <= 0xFFFF then "\\u" ++ c.int.show-hex(4,pre="")
    else "\\U" ++ c.int.show-hex(6,pre="")
  else
    if c == '\'' then "\\'"
    elif c == '\"' then "\\\""
    elif c == '\\' then "\\\\"
    else c.string

// Show a `:char` as a character literal
pub fun show( c : char ) : string
  "'" ++ c.show-char ++ "'"

// Show a string as a string literal
pub noinline fun show( s : string ) : string 
  "\"" ++ s.list.map(show-char).join ++ "\""

// Convert a `:bool` to a string
pub fun show( b : bool ) : string 
  if b then "True" else "False"

// Convert a unit value `()` to a string
pub fun show( u : () ) : string  
  "()"

// Show an `:sslice` as a string literal
pub fun show( s : sslice ) : string 
  s.string.show

// Convert a list to a string
pub fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
  "[" ++ xs.map(show-elem).join(",") ++ "]"

pub fun show( xs : list<string> ) : string
  show-list(xs,show)

pub fun show( xs : list<int> ) : string
  show-list(xs,show)

pub fun show( xs : list<bool> ) : string
  show-list(xs,show)

pub fun show-tuple( x : (a,b), showfst : a -> string, showsnd : b -> string ) : string
  "(" ++ x.fst.showfst ++ "," ++ x.snd.showsnd ++ ")"


// ----------------------------------------------------------------------------
// Print to the console
// ----------------------------------------------------------------------------

// Generic print routine: prints the internal representation as a string to the console, including a final newline character.
// Note: this breaks parametricity so it should not be pub
fun gprintln( x : a ) : console ()
  println( x.gshow )

// Generic print routine: prints the internal representation as a string to the console,
// including a final newline character.
// Note: this breaks parametricity so it should not be pub
fun gprint( x : a ) : console ()
  print( x.gshow )

noinline val redirect : ref<global,maybe<(string) -> console ()>> =
  unsafe-total { ref(Nothing) }

// redirect `print` and `println` calls to a specified function.
noinline fun print-redirect( print : (msg : string) -> console () ) : io ()
  redirect := Just(print)

// Print a string to the console, including a final newline character.
extern xprintsln(s : string) : console ()
  c  "kk_println"
  cs "Console.WriteLine"
  js "_println"

// Print a string to the console
extern xprints( s : string) : console ()
  c  "kk_print"
  cs "Console.Write"
  js "_print"

// _Unsafe_. This function removes the non-termination effect (`:div`) from the effect of an action
pub inline extern unsafe-nostate( action : () -> <st<h>,console> a ) : (() -> console a) 
  inline "#1"

noinline fun prints( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprints(s)
      Just(f) -> f(s)
  )()

noinline fun printsln( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprintsln(s)
      Just(f) -> f(s ++ "\n")
  )()

// Print a string to the console.
pub fun print(s : string)  
  prints(s)

// Print an integer to the console.
pub fun print(i : int)     
  prints(show(i))

// Print a float64 to the console.
pub fun print(d : float64)  
  prints(show(d))

// Print a boolean to the console
pub fun print( b : bool )  
  prints(show(b))

// Print a character to the console.
pub fun print(c : char)    
  prints(c.string)

// Print a unit value to the console
pub fun print( u : () )  
  prints(show(()))

// Print a string to the console, including a final newline character.
pub fun println(s : string)  
  printsln(s)

// Print an integer to the console, including a final newline character.
pub fun println(i : int)     
  printsln(show(i))

// Print a float64 to the console, including a final newline character.
pub fun println(d : float64)  
  printsln(show(d))

// Print a boolean to the console, including a final newline character
pub fun println( b : bool )  
  printsln(show(b))

// Print a character to the console, including a final newline character.
pub fun println(c : char)    
  printsln(c.string)

// Print a unit value to the console, including a final newline character
pub fun println( u : () )  
  printsln(show(()))


// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------

extern xtrace : ( message : string ) -> ()
  c  "kk_trace"
  cs "Primitive.Trace"
  js "_trace"

extern xtrace-any : forall<a> ( message: string, x : a ) -> ()
  c  "kk_trace_any"
  cs "Primitive.TraceAny"
  js "_trace_any"

val trace-enabled : ref<global,bool> = unsafe-total{ ref(True) }

// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.
pub fun trace( message : string ) : ()
  unsafe-total
    if !trace-enabled then xtrace(message)

pub fun trace-any( message : string, x : a ) : ()
  unsafe-total
    if !trace-enabled then xtrace-any(message,x)

// Disable tracing completely.
noinline fun notrace() : st<global> ()
  trace-enabled := False

extern unsafe-assert-fail( msg : string ) : ()
  c "kk_assert_fail"
  js inline "function() { throw new Error(\"assertion failed: \" + #1) }()"

pub fun assert( message : string, condition : bool ) : ()   // Compiler removes assert calls in optimized builds
  if !condition then unsafe-assert-fail(message)  


// ----------------------------------------------------------------------------
// Exceptions
// ----------------------------------------------------------------------------

// The `:named` effect is the default umbrella effect for named effects
pub type nmd :: X

pub type scope :: S -> X

// Exceptions
pub effect exn
  // Throw an exception
  ctl throw-exn( exn : exception ) : a

pub fun ".default-exn"(action)
  handle( action )
    final ctl throw-exn( exn : exception )
      "uncaught exception: ".prints
      show(exn).println

// The exception data type
pub struct exception( message :string, info :exception-info )

// Exception information
pub open type exception-info
  ExnError      // Generic error
  ExnAssert
  ExnTodo
  ExnRange
  ExnPattern( location : string, definition : string )
  ExnSystem( errno : int )
  ExnInternal( name : string )

// Show the exception message
pub fun show( exn : exception ) : string
  exn.message

// Throw an exception with a specified message.
pub fun throw( message: string, info : exception-info = ExnError ) : exn a
  throw-exn(Exception(message,info))

// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
pub fun error-pattern(location : string, definition : string) : exn a
  throw(location ++ (if definition.is-empty then "" else (": " ++ definition)) ++ ": pattern match failure",
         ExnPattern(location,definition))

// Catch any exception raised in `action` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
pub fun try( action : () -> <exn|e> a, hndl: exception -> e a ) : e a
  with final ctl throw-exn(exn) hndl(exn)
  action()

// _Deprecated_; use `try` instead. Catch an exception raised by `throw` and handle it.
// Use `on-exn` or `on-exit` when appropiate.
pub fun catch( action : () -> <exn|e> a, hndl: exception -> e a) : e a
  try(action,hndl)

// An `:error` type represents a first-class exception result.
pub type error<a>
  Error( exception : exception )
  Ok( result : a )

// Transform an exception effect to an  `:error` type.
pub fun try( action : () -> <exn|e> a ) : e error<a>
  try({ Ok(action()) }, fn(exn){ Error(exn) })

// Return a default value when an exception is raised
pub fun try-default( value : a , action : () -> <exn|e> a ) : e a
  try( action, fn(_) { value })

// Transform an `:error` type back to an `exn` effect.
pub fun untry( err : error<a> ) : exn a
  match err
    Error(exn) -> throw-exn(exn)
    Ok(x)      -> x

// Transform an `:error` type back to an `exn` effect.
pub fun exn( err : error<a> ) : exn a
  untry(err)

// Use default value `def` in case of an error.
pub fun default( t : error<a>, def : a ) : a
  match t
    Error  -> def
    Ok(x)  -> x

// Transform an `:error` type to a `:maybe` value.
pub fun maybe( t : error<a> ) : maybe<a>
  match t
    Error  -> Nothing
    Ok(x)  -> Just(x)

// Transform an `:error` type to an `:either` value.
pub fun either( t : error<a> ) : either<exception,a>
  match t
    Error(exn) -> Left(exn)
    Ok(x)      -> Right(x)

/*
// Set a `hndler` that is called only when an exception is raised in the `action` block.
pub fun on-exn( hndler : exception -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a
  // prim-try-some( action, fn(exn){ hndler(exn); throw(exn) })
  match mask<exn>{ try(action) }
    TryOk(x)    { x
    TryExn(exn) { hndler(exn); throw(exn)

*/
// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
pub fun on-exit( hndler : () -> e (), action : () -> e a ) : e a
  finally(hndler,action)

// ----------------------------------------------------------------------------
// Null is used for external interfaces
// ----------------------------------------------------------------------------

// Abstract type used for passing `null` values to external functions
pub type null<a>

// Unsafe: transform any type to a `null` type; used internally by the compiler.
pub extern ".null-any"(x : a) : null<a>
  c  inline "((#1).box == kk_box_null.box ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox(#1))"
  cs inline "#1"
  js inline "(#1==null ? null : #1)"  // undefined -> null

// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).
pub extern null(x : maybe<a>) : null<a>
  c  inline "(kk_std_core_types__is_Nothing(#1) ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox((#1)._cons.Just.value) /* kk_datatype_unbox(kk_datatype_unjust(#1,kk_context())) */ )"
  cs inline "(#1.tag_ == __std_core._maybe_Tag.Nothing ? default(##1) : #1.@value)"
  js inline "(#1==null ? null : #1.value)"

// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).
pub extern maybe( n : null<a> ) : maybe<a>
  c  inline "(kk_datatype_as_ptr(#1) == NULL ? kk_std_core_types__new_Nothing(kk_context()) : kk_std_core_types__new_Just(kk_datatype_box(#1),kk_context()))"
  cs inline "(EqualityComparer<##1>.Default.Equals(#1,default(##1)) ? __std_core._maybe<##1>.Nothing_ : new __std_core._maybe<##1>(#1))"
  js inline "(#1==null ? $std_core_types.Nothing : $std_core_types.Just(#1))"

// Cast a integer that is zero to a null
pub fun null( i : int ) : null<int>
    i.maybe.null

// Cast an empty string a null
pub fun null( s : string ) : null<string>
    s.maybe.null

// Cast a boolean `False` to null
pub fun null( b : bool ) : null<()>
    b.maybe.null

// val null-const : forall<a> null<a> = null(Nothing)

// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

// Used by the compiler to wrap main console applications
pub extern main-console : forall<a,e> ( main : () -> e a ) -> e a
  c "kk_main_console"
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"

// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

pub inline extern inject-local<a,h,e>( action : () -> e a ) : total (() -> <local<h>|e> a)  
  inline "#1"
  
// Assign to an entry in a local `:vector` variable.
pub inline extern []( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn|e> ()
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"
  js inline "(#1)[#2] = #3"

// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
pub fun once( calc : () -> a ) : (() -> a)
  unsafe-total
    val r = ref(Nothing)
    return fn()
      unsafe-total
        match !r
          Just(x) -> x
          Nothing ->
            val x = calc()
            r := Just(x)
            x


// ----------------------------------------------------------------------------
// Unsafe
// ----------------------------------------------------------------------------

// _Unsafe_. This function removes the exception effect (`:exn`) from the effect of an action
pub fun unsafe-no-exn( action : () -> <exn|e> a ) : e a
  unsafe-total( action )



// ----------------------------------------------------------------------------
// Non determinism
// ----------------------------------------------------------------------------

noinline val unique-count : ref<global,int> = unsafe-total{ ref(0) }

// Returns a unique integer (modulo 32-bits).
pub fun unique() : ndet int
  unsafe-total
    val u = !unique-count
    unique-count := u+1
    u

// ----------------------------------------------------------------------------
// Control
// ----------------------------------------------------------------------------

// The `while` fun executes `action`  as long as `pred`  is `true`.
pub fun while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
  if predicate() then
    action()
    while(predicate, action)

// The `repeat` fun executes `action`  `n`  times.
pub fun repeat( ^n : int, action : () -> e () ) : e ()
  for(1,n) fn(i) 
    action()

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
pub fun for( ^start: int, end : int, action : (int) -> e () ) : e ()
  fun rep( ^i : int )
    if i <= end then
      action(i)
      rep(unsafe-decreasing(i.inc))
  rep(start)

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
fun forz( start: ssize_t, end : ssize_t, action : (ssize_t) -> e () ) : e ()
  fun rep( i : ssize_t )
    if i <= end then
      action(i)
      rep(unsafe-decreasing(i.incr))
  rep(start)

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
pub fun for-while( start: int, end : int, action : (int) -> e maybe<a> ) : e maybe<a>
  fun rep( i : int )
    if i <= end then
      match action(i)
        Nothing -> rep(unsafe-decreasing(i.inc))
        Just(x) -> Just(x)
    else Nothing
  rep(start)

// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
fun for-whilez( start: ssize_t, end : ssize_t, action : (ssize_t) -> e maybe<a> ) : e maybe<a>
  fun rep( i : ssize_t )
    if i <= end then
      match action(i)
        Nothing -> rep(unsafe-decreasing(i.incr))
        Just(x) -> Just(x)
    else Nothing
  rep(start)

// Return the host environment: `dotnet`, `browser`, `webworker`, `node`, or `libc`.
pub extern host() : ndet string
  c "kk_get_host"
  cs inline "\"dotnet\""
  js inline "_host"

/*
// .open is used in the compiler to open up closed effects.
// Usually simplified away but still present if --no-simplify flag is given
pub inline extern ".open"<e1 :: E, e2 :: E, a, b>( x : a ) : total b
  inline "#1"
*/

// Magic casting to the `:any` type.
pub inline extern ".toany"( x : a ) : any
  inline "#1"
  cs inline "(object)(#1)"

// Internal: used for value effects
// TODO: revisit value effects codegen
pub alias value<a> = a

// Internal: used for value effects
// TODO: revisit value effects codegen
pub extern phantom<a>() : a
  c inline "kk_box_null"
  inline "undefined"

