import std/data/map


fun showEntry(i: int, s: ())
  "(" ++ i.show() ++ ",())"

fun main()
  with int_compare
  var m := singleton(1, ())
  m := m.insert(2, ())
  m := m.insert(3, ())
  m := m.insert(4, ())
  m := m.insert(5, ())
  m.showTreeWith(True, False, showEntry).println

  val m3 = m.delete(2)
  // m3.showTreeWith(True, True, showEntry).println
  println("Done")
  println(m.printmap())

fun printmap(m: map<int, ()>): string
  match m
    Tip -> "Tip"
    Bin(_, k, v, l, r) -> "Bin(" ++ k.show() ++ ", " ++ v.show() ++ ", " ++ printmap(l) ++ ", " ++ printmap(r) ++ ")"