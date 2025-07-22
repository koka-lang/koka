// issue #755 and #758
val basic/an-implicit = 3
fun higher-order(f : (b: int) -> console int)
  f(b = 1)
fun param(a, b, ?an-implicit: int) : console int
  println(a.show ++ ", " ++ b.show)
  0
fun impl-higher-order(?param : (b: int, a: int) -> console int)
  param(1, 2)
fun test()
  impl-higher-order()
  