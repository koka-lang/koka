// Test finally
ambient fun inc() : int

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

  /*
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
  */
}

fun main() {
  foo().println
}
