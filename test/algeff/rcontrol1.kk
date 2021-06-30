public module rcontrol1

effect amb {
  control flip() : bool
}

fun xor() : amb bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}

val amb = handler {
  return(x){ [x] }
  rcontrol flip(){ rcontext.resume(False) ++ rcontext.resume(True)  }
}

fun main() {
  amb(xor).show.println
}
