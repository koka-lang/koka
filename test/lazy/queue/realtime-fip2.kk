// Realtime queue (see "Purely Functional Data Structures" by Chris Okasaki, Sect. 7.2)
// - worst-case  O(1), persistent O(1)
// - amortized   O(1), persistent O(1)
module queue/realtime-fip2
import std/core/unsafe
import bench

type stream<a>
  SNil
  SCons( head : a, tail : stream<a> )
  lazy SReverse( xs : stream<a>, acc : stream<a> ) ->
    match xs
      SCons(x, xx) -> SReverse( xx, SCons(x, acc) )
      SNil -> acc
  lazy SAppend( xs : stream<a>, ys : stream<a> ) ->
    // TODO: this fails to compile without unsafe-total here
    val ysx = unsafe-total { stream/lazy-step(ys) }
    match xs
      SCons(x, xx) -> SCons(x, SAppend(xx, ysx))
      SNil         -> ysx

struct queue<a>
  front : stream<a>
  rear  : stream<a>
  sched : stream<a>

val realtime/empty = Queue(SNil,SNil,SNil)

// evaluate one step into the front (by evaluating the schedule one step at a time)
fun schedule( Queue(front,rear,sched) : queue<a> ) : div queue<a>
  match sched
    SCons(_,s) -> Queue(front,rear,s)
    SNil       -> val f = SAppend(front,SReverse(rear, SNil)) in Queue(f,SNil,f) // start a fresh schedule (sharing the front)

fun snoc( Queue(front,rear,sched) : queue<a>, x : a ) : div queue<a>
  Queue(front, SCons(x,rear),sched).schedule

fun uncons( Queue(front,rear,sched) : queue<a> ) : div maybe2<a,queue<a>>
  match front
    SCons(x,xx) -> Just2(x,Queue(xx,rear,sched).schedule)
    SNil        -> Nothing2

pub fun main()
  benchmain(QueueI(realtime/empty, snoc, uncons))
