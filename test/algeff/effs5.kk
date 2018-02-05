// --------------------------------------------------------
// tests code generation for case statements with effect
// operations in the branches 
// --------------------------------------------------------

effect state {
  fun get() : string
  fun append( x : string ) : string
}

val state = handler(s) {
  return x   -> (x,s)
  get()      -> resume(s,s)
  append(s') -> resume("x",s + ", " + s')
}


effect amb {
  fun flip() : bool
}
 
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

fun test() {
  val b = if (flip()) then flip() else False
  val c = if (b) then append("1") else append("2")
  append(c)
}

fun main() {
  state("0"){ amb(test) }.snd
}