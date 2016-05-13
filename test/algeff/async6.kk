// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
effect amb {
  flip() : bool
}
 
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

function dowait(s : int) {
  val secs = s.show + " seconds"
  println("and waiting at least " + secs)
  wait(s * 1000)
  println("(done waiting " + secs + ")")
  "waited " + secs
}

function test() {
  println("starting")
  function f() {
    println("what is your name?")
    readline()
    error("ouch")
  }
  function g() {
    wait(1000)
    if (flip()) then dowait(2) else dowait(3)
  }
  val xs = asyncAll( [f,g] )
  println("hi " + xs.join(","))  
}

function main() {
  async{ amb(test); () }
}