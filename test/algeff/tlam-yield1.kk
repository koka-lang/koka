// A type abstraction over a computation that may yield (see `lists`)
effect tick
  ctl tick() : ()

fun lists(f : forall<a> a -> e a) : e (forall<a> list<a> -> e list<a>)
  f(fn(xs) xs)

fun step(x : a) : tick a
  tick()
  x

fun main()
  with ctl tick() { println("tick"); resume(()) }
  val g = lists(step)
  println(g([1,2]).length)
