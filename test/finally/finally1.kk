// Test finally

fun bar() {
  println("action");
  42
}

fun foo()
{
  with return(x) { println("return"); x }
  with finally   { println("finally") }
  bar()
  /*
  handle(bar) {
    return x -> {
      println("return")
      x
    }
    finally  -> println("finally")
  }
  */
}

fun main() {
  foo().println
}
