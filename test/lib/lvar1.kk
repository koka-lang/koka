public module lvar1

public import std/os/lvar

fun lattice-compare (x : bool, y : bool) : total maybe<order> {
  Just(x.compare(y))
}

fun lattice-join (x : bool, y : bool) : total maybe<bool> {
  Just(x || y)
}

fun main() {
  val lv = lvar(False, lattice-join, lattice-compare)
  lv.put(True)
  lv.freeze().println
  lv.get([True]).println
}
