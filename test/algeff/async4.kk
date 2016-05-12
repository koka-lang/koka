// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------

function test() {
  println("what is your name?")
  val xs = asyncAll( [
    { println("what is your name?")
      readline()
    },
    { println("and reading a text file")
      readTextFile("package.json")
    }
  ])
  println("hi " + xs.join(","))  
}

function main() {
  async(test)
}