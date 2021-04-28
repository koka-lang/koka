
fun foo( x : int, y : int ) {
  val x1 = if (x >= 0) then apply(id,1) else 2
  val y1 = if (y >= 0) then 1 else 2
  x1+y1
}

fun main() {
  foo(1,2).println
}