module implicit-stack

import std/core/unsafe
import bench

type digit<a>
  Zero
  One( x : a, p1 : pad, p2 : pad )
  Two( x : a, y : a, p : pad )

reference type pair3<a>
  Pair3( x : a, y : a, p : pad )

lazy type queue<a>
  Shallow( d : digit<a>, p1 : pad, p2 : pad )
  Deep( front : digit<a>, middle : queue<pair3<a>>, rear : digit<a> )
  lazy DeepOne( p : pad, middle : queue<pair3<a>>, rear : digit<a> ) ->
    match uncons(middle)
      Nothing2 -> Shallow( rear, Pad, Pad )
      Just2( Pair3(y, z, p), middle' ) ->
        Deep( Two(y, z, p), middle', rear )
  lazy Snoc( middle : queue<a>, y : a, p : pad ) ->
    match middle
      Shallow(Zero, p1, p2) ->
        Shallow(One(y, Pad, Pad), p1, p2)
      Shallow(One(x, _, p0), p1, p2) ->
        Deep( Two( x, y, p0 ), Shallow( Zero, p1, p2 ), Zero )
      Deep( front, middle', Zero ) ->
        Deep( front, middle', One(y, Pad, Pad) )
      Deep( front, middle', One(x, _, p0) ) ->
        Deep( front, Snoc(middle', Pair3(x, y, p0), Pad), Zero )
      _ -> impossible("Snoc")

val implicit/empty = Shallow(Zero, Pad, Pad)



fun isEmpty( ^q : queue<_> ) : div bool
  match q
    Shallow(Zero) -> True
    _ -> False

fbip fun uncons( q : queue<a> ) : div maybe2<a, queue<a>>
  match queue/lazy-force(q)
    Shallow(Zero) -> Nothing2
    Shallow(One(x), p1, p2) ->
      Just2( x, Shallow(Zero, p1, p2) )
    Deep( One(x), middle, rear ) ->
      Just2( x, DeepOne( Pad, middle, rear ) )
    Deep( Two(x, y, p), middle, rear ) ->
      Just2( x, Deep( One(y, Pad, p), middle, rear ) )
    _ -> impossible("uncons")

fbip(1) fun snoc( q : queue<a>, y : a ) : div queue<a>
  queue/lazy-step(Snoc( q, y, Pad ))


pub fun main()
  benchmain(QueueI(implicit/empty, snoc, uncons))
