module realtime-strict

import bench

type stream<a>
  SNil
  SCons( x : a, xs : stream<a> )

type queue<a>
  Queue( front : stream<a>, rear : list<a>, sched : stream<a> )

val realtime/empty = Queue( SNil, Nil, SNil )

fun isEmpty( ^q : queue<a> ) : div bool
  match q
    Queue( front, _, _ ) -> match front
      SNil -> True
      _ -> False

fun rotate( front : stream<a>, rear : list<a>, sched : stream<a> ) : div stream<a>
  match (front, rear)
    (SNil, Cons(y, _)) -> SCons( y, sched )
    (SCons( x, xs ), Cons(y, ys)) -> SCons( x, rotate( xs, ys, SCons( y, sched )))
    _ -> SNil // cannot happen

fun exec( q : queue<a> ) : div queue<a>
  match q
    Queue( front, rear, sched ) -> match sched
      SCons( _, s ) -> Queue( front, rear, s )
      SNil ->
        val front' = rotate( front, rear, SNil )
        Queue( front', Nil, front' )

fun snoc( q : queue<a>, x : a ) : div queue<a>
  match q
    Queue( front, rear, sched ) ->
      exec( Queue( front, Cons( x, rear ), sched ))

fun uncons( q : queue<a> ) : div maybe2<a,queue<a>>
  match q
    Queue( front, rear, sched ) -> match front
      SNil -> Nothing2
      SCons( x, f ) -> Just2( x, exec( Queue( f, rear, sched )))

pub fun main()
  benchmain(QueueI(realtime/empty, snoc, uncons))