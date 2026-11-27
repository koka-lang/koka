// `ref/update` passes the current value to a function and stores its result
fun main()
  val r = ref(10)
  r.update(fn(x) x + 1)
  println(!r)
