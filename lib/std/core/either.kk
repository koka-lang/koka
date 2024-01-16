/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:either` functions.
module std/core/either

import std/core/types

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

