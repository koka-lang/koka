/*---------------------------------------------------------------------------
  Copyright 2026-2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:result` functions.
module std/core/result

import std/core/types
import std/core/hnd

// Convert a `:result` to a `:maybe` type discarding the value of the `Error` constructor
// and using `Just` for the `Ok` constructor.
pub fun maybe( r : result<a,e> ) : maybe<a>
  match r
    Error -> Nothing
    Ok(x) -> Just(x)

// Convert a `:result` to its `Ok` value by providing a default value in the case of an `Error`.
pub fun default( r : result<a,e>, def : a ) : a
  match r
    Error -> def
    Ok(x) -> x

// Convert a `:result` to an `:eiter` type using `Left` for the `Error` constructor
// and using `Right` for the `Ok` constructor.
pub fun either( r : result<a,e> ) : either<e,a>
  match r
    Error(x) -> Left(x)
    Ok(x)    -> Right(x)

// Map over the `Ok` component of a `:result` type.
pub fun map( e : result<a,b>, f : a -> e c  ) : e result<c,b>
  match e
    Ok(x)    -> Ok(f(x))
    Error(x) -> Error(x)

// Flatten the result of a result.
pub fun flatten( rr : result<result<a,b>,b> ) : e result<a,b>
  match rr
    Ok(r) -> match r
      Ok(x)    -> Ok(x)
      Error(y) -> Error(y)
    Error(z)   -> Error(z)


// Equality on `:result` values
pub fun (==)(e1: result<a,b>, e2: result<a,b>, ?ok/(==): (a, a) -> e bool, ?error/(==): (b, b) -> e bool) : e bool
  match e1
    Error(l1) -> match e2
      Error(l2) -> l1 == l2
      _ -> False
    Ok(r1) -> match e2
      Ok(r2) -> r1 == r2
      _ -> False

// Show an `:result` type
pub fun show( e : result<a,b>, ?ok/show : a -> e string, ?error/show : b -> e string ) : e string
  match e
    Ok(x)    -> "Ok(" ++ x.show ++ ")"
    Error(y) -> "Error(" ++ y.show ++ ")"
