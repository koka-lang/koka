import std/async

effect ctl op(x : int) : int

fun main()
  with handler
    final ctl op(p)
      trace("optrace: " ++ p.show)
      println("op: " ++ p.show)
  println("Init")
  val xs = interleaved([{println("1"); op(1); println("2"); 1 }, {println("2"); 2}])
  println("Done: " ++ xs.show)
  