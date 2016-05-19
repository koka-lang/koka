module fibonacci

public function main() {
  println("The 10000th fibonacci number is " + fibonacci(10000).show)
}

public function fibonacci(n : int) : div int {
  fibonacci_(n, 0, 1)
}

function fibonacci_(n : int, x1 : int, x2 : int) : div int 
{
  if (n<=0) return x1
  fibonacci_(n - 1, x2, x1+x2)
}

