module collatz

private function collatz(n : int, steps : int = 0) : div maybe<int> {
  if (n == 1) {
    Just(steps)
  }
  elif (even(n)) {
    collatz(n/2, steps+1)
  } 
  elif (n+1 > maxInt/3) {
    Nothing
  }
  else {
    collatz(n*3+1, steps+1)
  }
}

public function main() : io () {
  match(collatz(27)) {
    Nothing -> println("Overflowed integer range.")
    Just(n) -> println("Took " + n.show + " steps.")
  }
}
