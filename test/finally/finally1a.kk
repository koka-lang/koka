// Test finally

fun bar() {
  println("action")
  throw("ouch")
  42
}

fun foo()
{
  with return(x) { println("return"); x }
  with finally   { println("finally") }
  bar()
}

fun main() {
  foo().println
}
