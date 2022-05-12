module sfib-int

fun sfibx(n : int, x : int) : div int
  if n <= 0 then x
  elif x > 1000000000
    then sfibx(n - 1, 1)
    else sfibx(n - 1, x*x + x + 2)

pub fun sfib(n : int)
  sfibx(n,1)

pub fun test(n : int) 
  val xs = list(1,10000)
  val ssum = fold-int(1,n,0) fn(i,acc)
               acc + xs.sum
  ssum.println

pub fun main()
  test(100000)  