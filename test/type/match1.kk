type test<a> {
  Test(arg : a)
}

fun xtest( x : test<a> ) : _ a {
  val (test,_) = (x,"hi")
  match(test) {
    Test(argx) -> { println("test"); argx }
  }
}