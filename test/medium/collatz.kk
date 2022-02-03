module collatz

fun collatz(n : int, steps : int = 0) : div maybe<int> {
  if (n == 1) then {
    Just(steps)
  }
  elif (n.is-even) then {
    collatz(n/2, steps+1)
  } 
  elif (n+1 > 1000000000) then {
    Nothing
  }
  else {
    collatz(n*3+1, steps+1)
  }
}

pub fun main() : io () {
  match(collatz(27)) {
    Nothing -> println("Overflowed integer range.")
    Just(n) -> println("Took " ++ n.show ++ " steps.")
  }
}
