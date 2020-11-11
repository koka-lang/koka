// Test initially
effect control flip() : bool
effect fun inc() : int

val amb = handler {
  return  (x)    -> [x]
  control flip() -> resume(False) + resume(True)

}

fun bar() {
  val b = flip()
  println("action: " + b.show + ", " + inc().show );
  if (b) throw("ouch")
  42
}

fun foo()
{
  var i := 42
  with handler {
    fun inc(){ i := i+1; i }
    initially (r){
      println("initially: " + r.show)
    }
    return (x){
      println("return, local=" + i.show)
      x
    }
    finally {
      println("finally, local=" + i.show)
    }
  }
  bar()
}

fun main() {
  amb(foo).map(show).join(",").println
}
