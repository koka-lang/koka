// Calculate the collatz sequence (which should always end in 1)

function collatz(n : int, steps : int = 0) : div int 
{
  if (n == 1)     then steps
  elif (even?(n)) then collatz(n/2, steps+1)
  else collatz(n*3+1, steps+1)
}

public function main() : <div,console> () 
{
  val i = 27
  println("Collatz(" + i.show + ") took " + collatz(i).show + " steps to reach 1.")
}
