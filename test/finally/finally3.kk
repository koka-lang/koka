// Test finally
effect fun inc() : int

fun bar() {
  println("action: " + inc().show )
  42
}

fun foo()
{
  var i : int := 42
  with return(x) { println("return, local: " + i.show ); x }
  with finally   { println("finally, local: " + i.show) }
  with fun inc() { i := i + 1; i }
  bar()
}

fun main() {
  foo().println
}
