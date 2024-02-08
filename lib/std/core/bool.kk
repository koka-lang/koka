/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:bool` functions.
//
// Booleans are either `True` or `False`.
module std/core/bool

import std/core/types

// ----------------------------------------------------------------------------
// Booleans
// ----------------------------------------------------------------------------

pub fip fun (==)( x : bool, y : bool) : bool
  if x then y else !y

pub fip fun (!=)( x : bool, y : bool) : bool
  if x then !y else y

pub fip fun (<)( x : bool, y : bool) : bool
  (!x && y)

pub fip fun (<=)( x : bool, y : bool) : bool
  !(x > y)

pub fip fun (>)( x : bool, y : bool) : bool
  (x && !y)

pub fip fun (>=)( x : bool, y : bool) : bool
  !(x < y)

// Compare two booleans with `False < True`.
pub fip fun cmp( x : bool, y : bool) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq

// Order two booleans in ascending order.
pub fip fun order2( x : bool, y : bool ) : order2<bool>
  if (x==y) then Eq2(x) elif (x < y) then Lt2(x,y) else Gt2(y,x)


// Convert a `:bool` to a string
pub fun show( b : bool ) : string
  if b then "True" else "False"

// Convert a boolean to an `:int`
pub fip fun int( b : bool ) : int
  if b then 1 else 0
