module std/core/tuple

import std/core/types

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
