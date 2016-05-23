// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------

function test() {
  println("starting")
  function f() {
    println("what is your name?")
    readline()
  }
  function g() {
    println("and waiting at least 3 secs")
    wait(3000)
    println("(done waiting)")
    "timing"
  }
  val xs = asyncAll( [f,g] )
  println("hi " + xs.join(","))  
}

function main() {
  test()
}