module fibonacci

public fun main() {
  println("The 10000th fibonacci number is " + fibonacci(10000).show)
}

public fun fibonacci(n : int) : div int {
  fib(n, 0, 1)
}

fun fib(n : int, x1 : int, x2 : int) : div int 
{
  if (n<=0) return x1
  fib(n - 1, x2, x1+x2)
}

