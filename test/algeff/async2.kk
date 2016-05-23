// --------------------------------------------------------
// Async demo: combine with ambiguity
// --------------------------------------------------------
effect amb {
  flip() : bool
}
 
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}


function test() {
  println("what is your name?")
  val name = readline()
  if (flip()) then println("and your age?") else println("and your gender?")
  val rest = readline()
  println("hi " + name + ", you are " + rest)  
}

function main() {
  amb(test)
  () 
}