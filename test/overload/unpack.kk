import std/num/float64

struct basic-num<a>
  zero: a

struct num<a>
  base: basic-num<a>
  (+): (a, a) -> a
  (-): (a, a) -> a

fun add(a1: a, a2: a, b1: b, b2: b, .?a/num: num<a>, .?b/num: num<b>, ?a/show: a -> string, ?b/show: b -> string): console ()
  println(b1 + b2)
  println(a1 + a2)
  println(a/num/(+)(a1, a2))

val int/basic-num: basic-num<int> = Basic-num(0)
val float/basic-num: basic-num<float64> = Basic-num(0.0)

fun num(add, sub, ?basic-num)
  Num(basic-num, add, sub)

val int/num : num<int> = num((+), (-))
val float/num : num<float64> = num((+), (-))

fun main()
  add(1, 2, 1.2, 2.3)