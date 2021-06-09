// Test initially

fun bar() {
  println("action");
  42
}

fun foo()
{
  var i := 42
  with handler {
    initially (r){
      println("initially: " ++ r.show)
    }
    return (x){
      println("return, local=" ++ i.show)
      x
    }
    finally {
      println("finally, local=" ++ i.show)
    }
  }
  bar()
}

fun main() {
  foo().println
}
