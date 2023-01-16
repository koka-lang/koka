import std/data/map


fun main()
  with int_compare
  val m = singleton(1, "test")
  val m2 = m.insert(2, "other")
  val m3 = m.delete(2)
  println("Done")