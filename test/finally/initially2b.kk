// Test initially
effect amb {
  ctl flip() : bool
}

val amb = handler {
  return  (x)    -> [x]
  ctl flip() -> resume(True) ++ resume(False)
}

effect state {
  fun inc() : int
}

fun bar() : _ int {
  println("action: " ++ flip().show );
  inc();
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
    fun inc(){ i := i+1; i }
  }
  bar()
}

fun main() {
  amb(foo).map(show).join(",").println
}
