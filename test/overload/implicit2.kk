// issue #755
val basic/an-implicit = 3

fun param(a : int = 0, b : int = 0, ?an-implicit: int) : console int
  println(a.show ++ ", " ++ b.show)
  0

fun impl-higher-order(?param : (b: int) -> console int)
  param(b = 1)

fun intparam(b:int) : console int
  b.println
  b

fun test1()
  impl-higher-order(intparam)


fun main()
  impl-higher-order()
