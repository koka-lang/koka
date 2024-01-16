/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard tuple functions.
module std/core/tuple

import std/core/types
import std/core/hnd

// Map a function over a tuple of elements of the same type.
pub fun tuple2/map( t : (a,a), f : a -> e b ) : e (b, b)
  (t.fst.f, t.snd.f)

// Map a function over a triple of elements of the same type.
pub fun tuple3/map( t : (a,a,a), f : a -> e b ) : e (b, b, b)
  (t.fst.f, t.snd.f, t.thd.f)

// Map a function over a quadruple of elements of the same type.
pub fun tuple4/map( t : (a,a,a,a), f : a -> e b ) : e (b,b,b,b)
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f)


// Element-wise tuple equality
pub fun tuple2/(==)( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/(==) : (a,a) -> bool, ?snd/(==) : (b,b) -> bool ) : bool
  (x1==x2) && (y1==y2)

// Element-wise triple equality
pub fun tuple3/(==)( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ?fst/(==) : (a,a) -> bool, ?snd/(==) : (b,b) -> bool, ?thd/(==) : (c,c) -> bool ) : bool
  (x1==x2) && (y1==y2) && (z1==z2)


// Order on tuples
pub fun tuple2/cmp( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/cmp : (a,a) -> order, ?snd/cmp : (b,b) -> order ) : order
  match fst/cmp(x1,x2)
    Eq -> snd/cmp(y1,y2)
    lg -> lg

// Order on triples
pub fbip fun tuple3/cmp( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ^?fst/cmp : (a,a) -> order, ^?snd/cmp : (b,b) -> order, ^?thd/cmp : (c,c) -> order ) : order
  match fst/cmp(x1,x2)
    Eq -> match snd/cmp(y1,y2)
            Eq -> thd/cmp(z1,z2)
            lg -> lg
    lg -> lg


// Convert a unit value `()` to a string
pub fun unit/show( u : () ) : string
  "()"

// Show a tuple
pub fun tuple2/show( x : (a,b), ?fst/show : a -> e string, ?snd/show : b -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ ")"

// Show a triple
pub fun tuple3/show( x : (a,b,c), ?fst/show : a -> e string, ?snd/show : b -> e string, ?thd/show : c -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ ")"

// _deprecated_, use `tuple2/show` instead
pub fun show-tuple( x : (a,b), showfst : a -> e string, showsnd : b -> e string ) : e string
  show(x,?fst/show=showfst,?snd/show=showsnd)

