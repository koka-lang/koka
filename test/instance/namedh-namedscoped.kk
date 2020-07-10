effect instance reader<s::S,a::V> {
  fun ask() : a
}

fun reader(init : a, action : forall<s> (reader<s,a> -> e b )) : e b {  // rank-2 signature required
  with x = instance {
    fun ask() { init }
  }
  action(x)
}

fun main() {
  with a = reader("hello")
  with b = reader("world")
  println( a.ask() + " " + b.ask() )
}

/* // rejected by the type checker as `s` escapes
fun escape() {
  with a = reader(1) 
  a
}

fun wrong() { 
  val x = escape()
  println( x.ask() )
}
*/
