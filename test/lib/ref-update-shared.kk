import std/os/task

// Scheduling the task marks everything its closure reaches as thread-shared,
// including `r`: `ref/update` on a thread-shared reference is a get and a set.
pub fun main()
  val r = ref([1, 2, 3])
  val p = task(fn() r)
  val r2 = p.await
  ref/update(r, fn(xs) Cons(0, xs))
  ref/update(r2, fn(xs) xs.map(fn(x) x + 1))
  println(!r)
