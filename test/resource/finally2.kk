// Test finally
effect state {
  fun inc() : int
}


fun bar() {
  println("action: " + inc().show )
  42
}

fun foo() 
{
  handle(bar) (i : int = 42) {
    return x -> { 
      println("return, local: " + i.show )
      x
    }
    finally  -> {
      println("finally, local: " + i.show)
    }
    inc() -> resume(i,i+1)
  }
}

fun main() {
  foo().println
}