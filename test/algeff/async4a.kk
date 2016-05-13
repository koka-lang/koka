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
      "done waiting"
    }
  ])
  println("hi " + xs.join(","))  
}

function main() {
  async(test)
}