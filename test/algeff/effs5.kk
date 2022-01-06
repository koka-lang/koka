// --------------------------------------------------------
// tests code generation for case statements with effect
// operations in the branches 
// --------------------------------------------------------

effect state {
  fun get() : string
  fun append( x : string ) : string
}

fun state(init,action) {
  var s := init
  with {
    return(x){ (x,s) }
    fun get()     { s }
    fun append(s'){ s := s ++ ", " ++ s'; "x" }
  }
  action()
}


effect amb {
  ctl flip() : bool
}
 
val amb = handler {
  return(x)      -> [x]
  ctl flip() -> resume(False) ++ resume(True)
}

fun test() {
  val b = if (flip()) then flip() else False
  val c = if (b) then append("1") else append("2")
  append(c)
}

fun main() {
  state("0"){ amb(test) }.snd.println
}