public module effs1

effect amb {
  fun flip() : bool
}
 
fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}
fun main() {
  amb(xor).show.println
}

