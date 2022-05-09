effect instance reader<a> {
  fun ask() : a     // reader<a> -> exn a
}

fun reader(init : a, action : reader<a> -> e b) : e b {
  with x = instance {
    fun ask() { init }
  }
  action(x)
}

fun main() {
  with a = reader("hello")
  with b = reader("world")
  println( a.ask() ++ " " ++ b.ask() )
}

fun escape() : reader<int> {
  with a = reader(1) 
  a
}
fun wrong() {  // dynamic exception
  val x = escape()
  println( x.ask() )
}
