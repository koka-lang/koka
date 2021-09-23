public module lvar1

public import std/os/task

fun main() {
  val lv = lvar(False, (||), is-gte-bool)
  lv.put(True)
  lv.freeze()
  val t = lv.get(fn(x){ if x == True then Just(x) else Nothing })
  t.println
}

fun is-gte-bool (x : bool, y : bool) : total maybe<bool> {
  Just(x <= y)
}
