import std/os/task

// Capturing `d` in a task marks it thread-shared, including the reference that
// holds its computation. The first `force` stores the computed list into that
// reference, and from then on the eight tasks dup and drop the list at the same
// time, so it must have been marked thread-shared as it was stored; otherwise
// their plain (non-atomic) refcount updates race and free it while in use.
pub fun main()
  val d : delayed<total,list<int>> = delay(fn() list(1, 1000))
  val first = task(fn() d.force.sum).await
  val ps = list(1, 8).map(fn(_) task(fn() list(1, 2000).foldl(0, fn(acc, _) acc + d.force.sum)))
  println(first)
  println(ps.await.sum)
