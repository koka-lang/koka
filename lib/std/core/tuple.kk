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

// Map a function over a quintuple of elements of the same type.
pub fun tuple5/map( t : (a,a,a,a,a), f : a -> e b ) : e (b,b,b,b,b)
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f, t.field5.f)

// Compare unit values. Useful to build composite equality for structures containing a unit (e.g. either<string, ()>)
pub inline fun unit/(==)(a: (), b: ()): bool
  True

// Element-wise tuple equality
pub fun tuple2/(==)( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/(==) : (a,a) -> e bool, ?snd/(==) : (b,b) -> e bool ) : e bool
  (x1==x2) && (y1==y2)

// Element-wise triple equality
pub fun tuple3/(==)( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ?fst/(==) : (a,a) -> e bool, ?snd/(==) : (b,b) -> e bool, ?thd/(==) : (c,c) -> e bool ) : e bool
  (x1==x2) && (y1==y2) && (z1==z2)

// Element-wise quadruple equality
pub fun tuple4/(==)( (x1,y1,z1,w1) : (a,b,c,d), (x2,y2,z2,w2) : (a,b,c,d), 
                      ?fst/(==) : (a,a) -> e bool, ?snd/(==) : (b,b) -> e bool, 
                      ?thd/(==) : (c,c) -> e bool, ?field4/(==) : (d,d) -> e bool ) : e bool
  (x1==x2) && (y1==y2) && (z1==z2) && (w1==w2)

// Element-wise quintuple equality
pub fun tuple5/(==)( (x1,y1,z1,w1,v1) : (a,b,c,d,f), (x2,y2,z2,w2,v2) : (a,b,c,d,f), 
                        ?fst/(==) : (a,a) -> e bool, ?snd/(==) : (b,b) -> e bool, ?thd/(==) : (c,c) -> e bool, 
                        ?field4/(==) : (d,d) -> e bool, ?field5/(==) : (f,f) -> e bool ) : e bool
  (x1==x2) && (y1==y2) && (z1==z2) && (w1==w2) && (v1==v2)

// Order on unit
pub inline fun unit/cmp( a: (), b: () ) : order
  Eq

// Order on tuples
pub fun tuple2/cmp( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/cmp : (a,a) -> e order, ?snd/cmp : (b,b) -> e order ) : e order
  match fst/cmp(x1,x2)
    Eq -> snd/cmp(y1,y2)
    lg -> lg

// Order on triples
pub fbip fun tuple3/cmp( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ^?fst/cmp : (a,a) -> e order, ^?snd/cmp : (b,b) -> e order, ^?thd/cmp : (c,c) -> e order ) : e order
  match fst/cmp(x1,x2)
    Eq -> match snd/cmp(y1,y2)
            Eq -> thd/cmp(z1,z2)
            lg -> lg
    lg -> lg

// Order on quadruples
pub fbip fun tuple4/cmp( (x1,y1,z1,w1) : (a,b,c,d), (x2,y2,z2,w2) : (a,b,c,d), 
                            ^?fst/cmp : (a,a) -> e order, ^?snd/cmp : (b,b) -> e order, 
                            ^?thd/cmp : (c,c) -> e order, ^?field4/cmp : (d,d) -> e order ) : e order
  match fst/cmp(x1,x2)
    Eq -> match snd/cmp(y1,y2)
            Eq -> match thd/cmp(z1,z2)
                    Eq -> field4/cmp(w1,w2)
                    lg -> lg
            lg -> lg
    lg -> lg

// Order on quintuples
pub fbip fun tuple5/cmp( (x1,y1,z1,w1,v1) : (a,b,c,d,f), (x2,y2,z2,w2,v2) : (a,b,c,d,f), 
                            ^?fst/cmp : (a,a) -> e order, ^?snd/cmp : (b,b) -> e order, ^?thd/cmp : (c,c) -> e order, 
                            ^?field4/cmp : (d,d) -> e order, ^?field5/cmp : (f,f) -> e order ) : e order
  match fst/cmp(x1,x2)
    Eq -> match snd/cmp(y1,y2)
            Eq -> match thd/cmp(z1,z2)
                    Eq -> match field4/cmp(w1,w2)
                            Eq -> field5/cmp(v1,v2)
                            lg -> lg
                    lg -> lg
            lg -> lg
    lg -> lg

// fip ordering of unit values
pub inline fip fun unit/order2( a: (), b: () ) : order2<()>
  Eq2(a)

// Convert a unit value `()` to a string
pub inline fun unit/show( u : () ) : string
  "()"

// Show a tuple
pub fun tuple2/show( x : (a,b), ?fst/show : a -> e string, ?snd/show : b -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ ")"

// Show a triple
pub fun tuple3/show( x : (a,b,c), ?fst/show : a -> e string, ?snd/show : b -> e string, ?thd/show : c -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ ")"

// Show a quadruple
pub fun tuple4/show( x : (a,b,c,d), ?fst/show : a -> e string, ?snd/show : b -> e string, 
                                    ?thd/show : c -> e string, ?field4/show : d -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ "," ++ x.field4.show ++ ")"

// Show a quintuple
pub fun tuple5/show( x : (a,b,c,d,f), ?fst/show : a -> e string, ?snd/show : b -> e string, ?thd/show : c -> e string, 
                                      ?field4/show : d -> e string, ?field5/show : f -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ "," ++ x.field4.show ++ "," ++ x.field5.show ++ ")"

// _deprecated_, use `tuple2/show` instead
pub fun show-tuple( x : (a,b), showfst : a -> e string, showsnd : b -> e string ) : e string
  show(x,?fst/show=showfst,?snd/show=showsnd)

