

fun main()  
  val b1 = "Hello There".bytes
  val new = "World".bytes.slice
  val b2 = b1.assign(6, new)
  println(b2.string) 