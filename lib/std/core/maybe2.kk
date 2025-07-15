/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:maybe2` functions which, unlike `:maybe`, can contain 2 values to avoid boxing allocations.
module std/core/maybe2

import std/core/types
import std/core/hnd
import std/core/exn

// Match a `:maybe2` value and either return a default value on `Nothing2` or apply a function to the value on `Just2`
pub fun maybe2( m : maybe2<a,b>, onNothing: c, onJust: (a,b) -> e c ) : e c
  match m
    Nothing2   -> onNothing
    Just2(x,y) -> onJust(x,y)

// Convert a `:maybe2<a,b>` value to `:(a,b)`, using the `nothing` parameter for `Nothing2`.
pub fun default( m : maybe2<a,b>, nothing : (a,b) ) : (a,b)
  match m
    Nothing2   -> nothing
    Just2(x,y) -> (x,y)

// Get the value of the `Just` constructor or raise an exception
pub fun unjust( m : maybe2<a,b>, ?kk-file-line: string) : exn (a,b)
  match m
    Just2(x,y) -> (x,y)
    Nothing2   -> throw("unexpected Nothing2 in " ++ ?kk-file-line)

// Get the value of the `Just` constructor or raise an exception
pub fun expect( m : maybe2<a,b>, error-msg: string) : exn (a,b)
  match m
    Just2(x,y) -> (x,y)
    Nothing2   -> throw(error-msg)

pub fun map( m : maybe2<a,b>, f : (a,b) -> e (c,d) ) : e maybe2<c,d>
  match m
    Nothing2    -> Nothing2
    Just2(x,y)  -> match f(x,y)
                     (fx,fy) -> Just2(fx,fy)

pub fun (||)( m1 : maybe2<a,b>, m2: maybe2<a,b> ) : maybe2<a,b>
  match m1
    Nothing2 -> m2
    _        -> m1

// Equality on `:maybe`
pub fun (==)( mb1 : maybe2<a,b>, mb2 : maybe2<a,b>, ?fst/(==) : (a,a) -> e bool, ?snd/(==) : (b,b) -> e bool ) : e bool
  match mb1
    Just2(x,y) -> match mb2
                    Just2(a,b) -> (x==a) && (y==b)
                    Nothing2   -> False
    Nothing2   -> match mb2
                    Nothing2 -> True
                    _        -> False


// Order on `:maybe` values
pub fun cmp( mb1 : maybe2<a,b>, mb2 : maybe2<a,b>, ?fst/cmp : (a,a) -> e order, ?snd/cmp : (b,b) -> e order) : e order
    match mb1
      Just2(x,y) -> match mb2
                      Just2(a,b) -> match ?fst/cmp(x,a)
                                      Eq -> ?snd/cmp(y,b)
                                      lg -> lg
                      Nothing2   -> Gt
      Nothing2   -> match mb2
                      Nothing2 -> Eq
                      _        -> Lt


// Show a `:maybe2` type
pub fun show( mb : maybe2<a,b>, ?fst/show : a -> e string, ?snd/show : b -> e string ) : e string
  match mb
    Just2(x,y) -> "Just2(" ++ x.show ++ "," ++ y.show ++ ")"
    Nothing2   -> "Nothing2"

// Convert a `:maybe2` type to a boolean, equivalent to `is-just2`.
pub fun bool( mb : maybe2<a,b> ) : bool
  match mb
    Just2    -> True
    Nothing2 -> False
