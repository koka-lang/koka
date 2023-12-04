alias newtest = test;

type test
  Test(nt: newtest)
  Empty

fun test1(n: newtest)
  match n
    Test(nt) -> nt.test1 + 1
    Empty -> 0

fun main()
  test1(Test(Test(Empty))).println
