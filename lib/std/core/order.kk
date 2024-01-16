/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:order` functions.
module std/core/order

import std/core/types
import std/core/int

pub fip fun int( x : order ) : int
  match x
    Lt -> -1
    Eq -> 0
    Gt -> 1

pub fun(==)( x : order, y : order ) : bool
  x.int == y.int

pub fun(!=)( x : order, y : order ) : bool
  x.int != y.int

pub fun(>=)( x : order, y : order ) : bool
  x.int >= y.int

pub fun(<=)( x : order, y : order ) : bool
  x.int <= y.int

pub fun(>)( x : order, y : order ) : bool
  x.int > y.int

pub fun(<)( x : order, y : order ) : bool
  x.int < y.int

