import std/async

effect ctl op(x : int) : int

fun main()
  with handler
    final ctl op(p)
      println("op: " ++ p.show)
  println("Init")
  val xs = interleaved( [ { println("1a"); op(1); println("1b"); 1 }, { println("2"); 2 } ])
  println("Done: " ++ xs.show)
  