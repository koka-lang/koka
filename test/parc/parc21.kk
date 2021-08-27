// Check that borrowing keeps evaluation order

noinline fun bo(a : int, ^b : int) {
  a
}

fun print-ret(x : int) {
  print(x)
  x
}

fun test() {
  bo(3, 4)
  bo(print-ret(3), print-ret(4))
}