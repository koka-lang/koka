module std/core/bool

import std/core/types
import std/core/hnd

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

pub fip fun cmp( x : bool, y : bool) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq

// Transform a boolean to a maybe type, using `Nothing` for `False`
pub fun maybe( b : bool ) : maybe<()>
  if b then Just(()) else Nothing

