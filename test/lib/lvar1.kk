public module lvar1

public import std/os/lvar

fun is-gte (x : bool, y : bool) : total maybe<bool> {
  Just(x <= y)
}

fun main() {
  val lv = lvar(False, (||), is-gte)
  lv.put(True)
  lv.freeze()
  val t = lv.get(fn(x){ if x == True then Just(x) else Nothing })
  t.println
}
