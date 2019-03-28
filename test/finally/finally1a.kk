// Test finally

fun bar() {
  println("action")
  error("ouch")
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
