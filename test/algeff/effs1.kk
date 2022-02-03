module effs1

effect amb {
  ctl flip() : bool
}

fun xor() : amb bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}

val amb = handler {
  return(x){ [x] }
  ctl flip(){ resume(False) ++ resume(True) }
}

fun main() {
  amb(xor).show.println
}
