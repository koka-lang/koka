module sfib-int

fun sfibx(n : int, x1 : int, x2 : int) : div int
  if n <= 0
    then x1
    else sfibx(n - 1, x2, (x1 + x2) / (x2 - x1))

fun sfib(n : int)
  sfibx(n,2,8)

pub fun main()
  sfib(100000000).println