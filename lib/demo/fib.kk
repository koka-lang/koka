// Example of an imperative version of the fibonacci function;
function main()
{
  val n = 10
  println("fibonacci(" + n.show + ") is " + fib(n).show );
}

// Calculate the n'th fibonacci number
function fib(n)
{
  var x := 0
  var y := 1
  repeat(n) {
    val y0 = y
    y := x+y
    x := y0
  }
  return x
}