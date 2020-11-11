module fibonacci

public fun main() {
  val n = 10000
  println("The " + n.show + "th fibonacci number is " + fibonacci(10000).show)
}

public fun fibonacci(n : int) : div int {
  fib(n, 0, 1)
}

fun fib(n : int, x1 : int, x2 : int) : div int {
  if (n<=0) then x1 else fib(n - 1, x2, x1+x2)
}
