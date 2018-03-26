// Test finally

fun bar() {
  println("action");
  42
}

fun foo() 
{
  handle(bar) {
    return x -> { 
      println("return")
      x
    }
    finally  -> println("finally")
  }
}

fun main() {
  foo().println
}