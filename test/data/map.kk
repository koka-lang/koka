import std/data/map


fun showEntry(i: int, s: ())
  "(" ++ i.show() ++ ",())"

fun main()
  with int_compare
  var m: map<int,()> := fromList([(1, ()), (2, ()), (3, ()), (4, ()), (5, ())])
  m.showTreeWith(True, False, showEntry).println

  val m3 = m.delete(2)
  // m3.showTreeWith(True, True, showEntry).println
  println("Done")
  println(m.printmap())

fun printmap(m: map<int, ()>): string
  match m
    Tip -> "Tip"
    Bin(_, k, v, l, r) -> "Bin(" ++ k.show() ++ ", " ++ v.show() ++ ", " ++ printmap(l) ++ ", " ++ printmap(r) ++ ")"