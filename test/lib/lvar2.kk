public module lvar2

public import std/os/lvar
public import std/os/task

fun is-gte(x : int, y : int) : total maybe<bool> {
  Just(x >= y)
}

fun main() {
  val lv    = lvar(0, max, is-gte)
  val add1  = fn () {lv.put(fn (x) {x + 1})}
  val add10 = fn () {lv.put(fn (x) {x + 10})}
  val promises = [add1, add1, add1, add10, add10, add10].map(task)
  lv.get([0]).println
  lv.get([1]).println
  lv.get([2]).println
  lv.get([3]).println
  lv.get([10]).println
  lv.get([11]).println
  lv.get([12]).println
  lv.get([13]).println
  lv.get([20]).println
  lv.get([21]).println
  lv.get([22]).println
  lv.get([23]).println
  lv.get([30]).println
  lv.get([31]).println
  lv.get([32]).println
  lv.get([33]).println
  await(promises)
  lv.freeze().println
}
