import std/time/timer

effect fun ask1() : int
effect fun ask2() : int

// use noinline to prevent full inlining and optimizations
// use at least 2 effects in order to cause allocation when converting from `:<ask1,ask2,exn>` into `:<ask1,exn>`
noinline fun square( i : int ) : <ask1,exn> int   
  if True then i * i else throw("impossible: " ++ ask1().show)

noinline fun square-ask1() : ask1 int
  ask1() * ask1()

noinline fun square-ask2() : ask2 int
  ask2() * ask2()


val n = 1_000_000

// the current algorithm does not float up the open through the `fold-int`, and thus does N opens around square
// todo: improve the algorithm to float here as well?
fun test-one() : <ask1,ask2,exn> int
  val x1 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x2 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x3 = square(1) + square(1) + square(1) + square(1) + square(1) 
  val x  = x1 + x2 + x3
  val y = square-ask1()
  val z = square-ask2()
  x + y + z

fun test() : <ask1,ask2,exn> int
  repeat(n)  
    test-one()
    ()
  42

// Time both
fun main() : <console,ndet,exn> int
  with fun ask1() 1
  with fun ask2() 2
  print-elapsed( test, "test: " )
  
