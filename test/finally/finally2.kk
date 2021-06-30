// Test finally
effect state {
  fun inc() : int
}


fun bar() {
  println("action: " ++ inc().show )
  42
}

fun foo() {
  var i := 41
  with handler {
    fun inc(){ i := i+1; i }
    return (x){
      println("return, local: " ++ i.show )
      x
    }
    finally {
      println("finally, local: " ++ i.show)
    }
  }
  bar()
}

fun main() {
  foo().println
}
