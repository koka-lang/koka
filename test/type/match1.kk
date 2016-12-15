type test<a> {
  Test(arg : a)
}

function xtest( x : test<a> ) : _ a {
  val (test,_) = (x,"hi")
  match(test) {
    Test(argx) -> { println("test"); argx }
  }
}