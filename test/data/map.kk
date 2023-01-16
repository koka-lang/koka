import std/data/map


fun showEntry(i: int, s: string)
  "k: " ++ i.show() ++ " v: " ++ s

fun main()
  with int_compare
  val m = singleton(1, "test")
  val m2 = m.insert(2, "other")
  m2.showTreeWith(True, True, showEntry).println
  val m3 = m.delete(2)
  m3.showTreeWith(True, True, showEntry).println
  println("Done")