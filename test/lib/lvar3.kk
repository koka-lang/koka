public module lvar3

public import std/os/lvar
public import std/os/task

fun lattice_join(x : int, y : int) : total maybe<int> {
  Just(max(x, y))
}

fun lattice_compare(x : int, y : int) : total maybe<order> {
  Just(compare(x, y))
}

fun main() {
  val lv    = lvar(0, lattice_join, lattice_compare)
  val add1  = fn () {lv.put(fn (x) { Just(x + 1) })}
  val add10 = fn () {lv.put(fn (x) { Just(x + 10) })}
  val promises = [add1, add1, add1, add10, add10, add10].map(task)
  lv.get([100]).println
  await(promises)
  lv.freeze().println
}
