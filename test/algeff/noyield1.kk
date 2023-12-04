effect fun foo() : int
effect fun bar() : int

pub fun main()
  test().println

pub fun test() : int
  with fun foo() 42
  with fun bar() foo() + 1  
  foobar()

fun foobar() : <bar> int
  with fun foo() 1
  bar() + foo()