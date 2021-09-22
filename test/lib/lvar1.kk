public module lvar1

public import std/os/task

fun main() {
  val lv = lvar(False, (||))
  lv.put(True)
  lv.freeze()
  val t = lv.get(fn(x){ if x == True then x else False })
  t.println
}