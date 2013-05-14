// Calculate the collatz sequence (which should always end in 1)

function collatz(n : int, steps : int = 0) : div maybe<int> 
{
  if (n == 1)    then Just(steps)
  elif (even(n)) then collatz(n/2, steps+1)
  elif (n+1 > maxInt/3) then Nothing
  else collatz(n*3+1, steps+1)
}

public function main() : <div,console> () 
{
  i = 27
  match(collatz(i)) {
    Nothing -> println("Collatz(" + i.show + ") overflowed" )
    Just(n) -> println("Collatz(" + i.show + ") took " + n.show + " steps to reach 1.")
  }
}
