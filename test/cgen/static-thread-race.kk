// A toplevel constant is a C static that every thread dups and drops.
// Its refcount must not be updated non-atomically from several threads.
import std/os/task

val xs : list<int> = list(1, 100)

noinline fun twice( ys : list<int> ) : (list<int>, list<int>)
  (ys, ys)

fun work() : int
  var n := 0
  for(1, 200000) fn(_)
    n := n + twice(xs).fst.length
  n

fun main()
  val ps = list(1, 8).map(fn(_) task(work))
  println(ps.await.sum)
