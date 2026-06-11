// Realtime queue (see "Purely Functional Data Structures" by Chris Okasaki, Sect. 7.2)
// - worst-case  O(1), persistent O(1)
// - amortized   O(1), persistent O(1)
module realtime

import bench

type stream<a>
  SNil
  SCons( head : a, tail : stream<a> )

  // `SRotate(pre,post,acc) == pre ++ reverse post ++ acc` with `|pre| + 1 == |post|`
  lazy SRotate( pre : stream<a>, post : list<a>, acc : stream<a> ) ->
    match pre
      SCons(x,xx) -> match post
                      Cons(y,yy) -> SCons(x, SRotate(xx, yy, SCons(y,acc)))
                      Nil        -> SCons(x, SNil)  // cannot happen
      SNil        -> match post
                      Cons(y,_) -> SCons(y, acc)
                      Nil       -> SNil          // cannot happen


struct queue<a>
  front : stream<a>
  rear  : list<a>
  sched : stream<a>

val realtime/empty = Queue(SNil,Nil,SNil)

// evaluate one step into the front (by evaluating the schedule one step at a time)
inline fun queue( front : stream<a>, rear : list<a>, sched : stream<a> ) : div queue<a>
  match sched
    SCons(_,s) -> Queue(front,rear,s)
    SNil       -> val f = SRotate(front,rear,SNil ) in Queue(f,Nil,f) // start a fresh schedule (sharing the front)

fun snoc( Queue(front,rear,sched) : queue<a>, x : a ) : div queue<a>
  queue(front, Cons(x,rear), sched)

fun uncons( Queue(front,rear,sched) : queue<a> ) : div maybe2<a,queue<a>>
  match front
    SCons(x,xx) -> Just2(x, queue(xx,rear,sched))
    SNil        -> Nothing2

pub fun main()
  benchmain(QueueI(realtime/empty, snoc, uncons))
