public module effs1

effect amb {
  flip() : bool
}
 

function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

function amb_handle(action) {
  (handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  })(action)
}

function test() {
  amb_handle(xor)
}