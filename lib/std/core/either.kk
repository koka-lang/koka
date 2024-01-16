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

