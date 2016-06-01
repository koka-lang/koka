public module effs1

effect amb {
  flip() : bool
}
 
function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

function main() {
  amb(xor).showList(show).println
}

