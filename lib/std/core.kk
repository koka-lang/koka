/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

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
pub import std/core/exn
pub import std/core/bool
pub import std/core/order
pub import std/core/char
pub import std/core/int
pub import std/core/float64
pub import std/core/vector
pub import std/core/string
pub import std/core/list
pub import std/core/maybe
pub import std/core/either
pub import std/core/tuple
pub import std/core/show
pub import std/core/debug
pub import std/core/delayed
pub import std/core/console

extern import
  c  file "core/core-inline"
  cs file "core/core-inline.cs"

// ----------------------------------------------------------------------------
// Builtin effects
// ----------------------------------------------------------------------------

// An alias for the empty effect.
// pub alias total = <>

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

// The `:named` effect is the default umbrella effect for named effects
pub type nmd :: X

// The `:scope` effect is used to ensure named effects cannot escape the scope of their handler
pub type scope :: S -> X


// ----------------------------------------------------------------------------
// Standard Functions
// ----------------------------------------------------------------------------

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

// Concise way to ensure two expressions have the same type.
pub fun same-type( x : a, y : a ) : a
  x

// ----------------------------------------------------------------------------
// Control statements
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

// Executes `action`  for each integer between [`start`,`end`]  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
pub fun for( ^start: int, end : int, action : (int) -> e () ) : e ()
  fun rep( ^i : int )
    if i <= end then
      action(i)
      rep(unsafe-decreasing(i.inc))
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


// ----------------------------------------------------------------------------
// Generic equality and comparison
// ----------------------------------------------------------------------------

// Generic inequality
pub fun (!=)(x : a, y : a, ?(==) : (a,a) -> bool ) : bool
  not(x==y)

// Generic equality if `cmp` exists
pub fun cmp/(==)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  match cmp(x,y)
    Eq -> True
    _  -> False

// Generic greater than
pub fun (>)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Gt

// Generic lower than
pub fun (<)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Lt

// Generic greater than or equal
pub fun (>=)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  y < x

// Generic lower than or equal
pub fun (<=)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  y > x


// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

// Used by the compiler to wrap main console applications
pub extern main-console : forall<a,e> ( main : () -> e a ) -> e a
  c "kk_main_console"
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"


// Return the host environment: `dotnet`, `browser`, `webworker`, `node`, or `libc`.
pub extern host() : ndet string
  c "kk_get_host"
  cs inline "\"dotnet\""
  js inline "$std_core_console._host"


// The default exception handler
pub fun "@default-exn"(action : () -> <exn,console|e> () ) : <console|e> ()
  handle( action )
    final ctl throw-exn( exn : exception )
      "uncaught exception: ".print
      exn.println


// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

// Assign to an entry in a local `:vector` variable.
pub inline extern index( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn|e> ()
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"
  js inline "(#1)[#2] = #3"


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
// Stream
// ----------------------------------------------------------------------------

// A `:stream` is a co-inductive type representing an infinite list of elements.
pub co type stream<a>
  con Next(head:a, tail: stream<a> )

