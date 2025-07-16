// This contains a custom implementation where we use a thunk to keep
// a constructor context unique. See hoodmelville-strict.kk for the thunk-free
// implementation given by Okasaki.

module hoodmelville

import bench

lazy type thunk<a>
  Done( done : list<a> )
  lazy Reverse( front : list<a>, front-ctx : ctx<list<a>>, rear : list<a>, rear-rev : list<a> ) ->
    match rear
      Cons(y,r) ->
        match front
          Nil -> Done( front-ctx ++. Cons(y,rear-rev) )
          Cons(x,f) -> Reverse(f, front-ctx ++ ctx Cons(x,hole), r, Cons(y,rear-rev) )
      _ -> impossible("rear must be longer than front")

fbip fun tick( t : thunk<a> ) : thunk<a>
  thunk/lazy-step(t)

fbip fun whnf( t : thunk<a> ) : div list<a>
  match thunk/lazy-force(t)
    Done( d ) -> d

value type state<a>
  Reversing( steps : int, tails : int, t : thunk<a> )
  Dropping( p : pad, tails : int, t : list<a> )
  SDone( p1 : pad, p2 : pad, t : list<a> )
  Idle

type queue<a>
  Queue( lenf : int, front : list<a>, state : state<a>, lenr : int, rear : list<a> )

val hm/empty = Queue( 0, Nil, Idle, 0, Nil )

fip fun isEmpty( ^q : queue<a> ) : bool
  match q
    Queue( 0, _, _, _ ) -> True
    _ -> False

fbip fun exec( s : state<a> ) : div state<a>
  match s
    Reversing( steps, tails, thunk ) ->
      if steps > 0
        then Reversing( steps - 1, tails, thunk.tick() )
        else Dropping( Pad, tails, whnf(thunk) )
    Dropping( p, tails, done ) ->
      if tails > 0
        then Dropping( p, tails - 1, done.tail )
        else SDone( p, Pad, done )
    s -> s

fbip fun invalidate( s : state<a> ) : state<a>
  match s
    Reversing( steps, tails, thunk ) -> Reversing( steps, tails + 1, thunk )
    Dropping( p, tails, done ) -> Dropping( p, tails + 1, done )
    s -> s

fbip fun exec2( q : queue<a> ) : div queue<a>
  match q
    Queue( lenf, front, s, lenr, rear ) ->
      match exec(exec(s))
        SDone( _, _, newf ) -> Queue( lenf, newf, Idle, lenr, rear )
        s' -> Queue( lenf, front, s', lenr, rear )

fun check( q : queue<a> ) : div queue<a>
  match q
    Queue( lenf, front, state, lenr, rear ) ->
      if lenr <= lenf
        then exec2( Queue( lenf, front, state, lenr, rear ) )
        else
          val newstate = Reversing( lenf, 0, Reverse( front, ctx hole, rear, Nil ) )
          exec2( Queue( lenf + lenr, front, newstate, 0, Nil ) )

fun snoc( q : queue<a>, x : a ) : div queue<a>
  match q
    Queue( lenf, front, s, lenr, rear ) ->
      check( Queue( lenf, front, s, lenr + 1, Cons( x, rear ) ) )

fun uncons( q : queue<a> ) : div maybe2<a, queue<a>>
  match q
    Queue( _, Nil, _, _, _ ) -> Nothing2
    Queue( lenf, Cons( x, front ), s, lenr, rear ) ->
      Just2( x, check( Queue( lenf - 1, front, invalidate(s), lenr, rear ) ) )

pub fun main()
  benchmain(QueueI(hm/empty, snoc, uncons))
