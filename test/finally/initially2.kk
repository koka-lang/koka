// Test initially
effect amb {
  ctl flip() : bool
}

val amb = handler {
  return  (x)    -> [x]
  ctl flip() -> resume(True) ++ resume(False)
}

fun bar() {
  println("action: " ++ flip().show );
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
  amb(foo).map(show).join(",").println
}
