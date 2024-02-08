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

pub fip fun (==)( x : order, y : order ) : bool
  x.int == y.int

pub fip fun (!=)( x : order, y : order ) : bool
  x.int != y.int

pub fip fun (>=)( x : order, y : order ) : bool
  x.int >= y.int

pub fip fun (<=)( x : order, y : order ) : bool
  x.int <= y.int

pub fip fun (>)( x : order, y : order ) : bool
  x.int > y.int

pub fip fun (<)( x : order, y : order ) : bool
  x.int < y.int

// Convert an `:order2` to an `:int` (`-1`, `0`, or `1`)
pub fip fun order2/int( x : order2<a> ) : int
  match x
    Lt2 -> -1
    Eq2 -> 0
    Gt2 -> 1

// Given a comparison function, we can order 2 elements.
pub fun order2( x : a, y : a, ^cmp : (a,a) -> order ) : order2<a>
  match cmp(x,y)
    Eq -> Eq2(x)
    Lt -> Lt2(x,y)
    Gt -> Gt2(y,x)
