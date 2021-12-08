// test contributed by Renzo Duin
struct testing
  p : int

fun main()
  //val a = Testing(99999999) // works on 32-bit
  //val a = Testing(999999999) // breaks on 32-bit
  //val a = Testing(999999999999999999) // works
  val a = Testing(1000000000000000000) // breaks
  println(a.p) 
  val b = -1 % a.p
  println(b) 
  println(a.p)
  val crash = 1 % a.p 
  println("done")
