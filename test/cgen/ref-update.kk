// `update` passes the current value to a function and stores its result
fun main()
  val r = ref(10)
  r.update(fn(x) x + 1)
  println(!r)
  var xs := [1, 2, 3]
  std/core/types/@byref(xs).update(fn(l) l.map(fn(x) x * 2))
  println(xs)
