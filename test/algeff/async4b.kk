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
    println("and reading a file")
    readTextFile("package.json")    
  }
  val xs = asyncAll( [f,g] )
  println("hi " + xs.join(","))  
}

function main() {
  async(test)
}