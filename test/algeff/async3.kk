// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------

function test() {
  println("what is your name?")
  val xs = asyncAll( [
    { print("what is your name?")
      readline()
    },
    { print("what is your age?")
      readline()
    }
  ])
  println("hi " + xs.join(","))  
}

function main() {
  async(test)
}