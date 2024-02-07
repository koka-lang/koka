/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:maybe` functions.
module std/core/maybe

import std/core/types
import std/core/hnd
import std/core/exn

// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
pub fun maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
  match m
    Nothing -> onNothing
    Just(x) -> onJust(x)

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
pub fun default( m : maybe<a>, nothing : a ) : a
  match m
    Nothing -> nothing
    Just(x) -> x

// Get the value of the `Just` constructor or raise an exception
pub fun unjust( m : maybe<a> ) : exn a
  match m
    Just(x) -> x
    Nothing -> throw("unexpected Nothing in std/core/maybe/unjust")

pub fun map( m : maybe<a>, f : a -> e b ) : e maybe<b>
  match m
    Nothing -> Nothing
    Just(x) -> Just(f(x))

pub fun (||)( m1 : maybe<a>, m2: maybe<a> ) : maybe<a>
  match m1
    Nothing -> m2
    _       -> m1

// Equality on `:maybe`
pub fun (==)( mb1 : maybe<a>, mb2 : maybe<a>, ?(==) : (a,a) -> bool ) : bool
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> x==y
                  Nothing -> False
    Nothing  -> match mb2
                  Nothing -> True
                  _       -> False


// Order on `:maybe` values
pub fun cmp( mb1 : maybe<a>, mb2 : maybe<a>, ?cmp : (a,a) -> order ) : order
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> cmp(x,y)
                  Nothing -> Gt
    Nothing  -> match mb2
                  Nothing -> Eq
                  _       -> Lt


// Order two `:maybe` values in ascending order
pub fip fun order2( mb1 : maybe<a>, mb2 : maybe<a>, ^?order2 : (a,a) -> order2<a> ) : order2<maybe<a>>
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> match order2(x,y)
                               Eq2(z)   -> Eq2(Just(z))
                               Lt2(l,g) -> Lt2(Just(l),Just(g))
                               Gt2(l,g) -> Gt2(Just(l),Just(g))
                  Nothing -> Gt2(Nothing,Just(x))
    Nothing  -> Lt2(Nothing,mb2)



// Show a `:maybe` type
pub fun show( mb : maybe<a>, ?show : a -> e string ) : e string
  match mb
    Just(x) -> "Just(" ++ x.show ++ ")"
    Nothing -> "Nothing"

// Convert a maybe type to a boolean, equivalent to `is-just`.
pub fun bool( mb : maybe<a> ) : bool
  match mb
    Just    -> True
    Nothing -> False