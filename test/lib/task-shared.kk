import std/os/task

// Scheduling the task marks everything its closure reaches as thread-shared,
// including `xs`, which `main` still holds: the shared refcount must keep both
// references, or the task's drop frees `xs` while `main` still reads it, after
// the allocation of `ys` has reused its cells.
pub fun main()
  val xs = list(1, 100000)
  val p = task(fn() xs.sum)
  val s = p.await
  val ys = list(1, 100000).map(fn(x) 0 - x)
  println(s + xs.sum)
  println(ys.sum)
