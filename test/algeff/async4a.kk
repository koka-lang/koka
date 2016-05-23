// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------

function test() {
  println("starting")
  val xs = asyncAll( [
    { println("what is your name?")
      readline()
    },
    { println("and waiting at least 3 secs")
      wait(3000)
      println("(done waiting)")
      "timing"
    }
  ])
  println("hi " + xs.join(","))  
}

function main() {
  test()
}