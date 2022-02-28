import std/time/timer

effect fun ask1() : int
effect fun ask2() : int

// use noinline to prevent full inlining and optimizations
// use at least 2 effects in order to cause allocation when converting from `:<ask1,ask2,exn>` into `:<ask1,exn>`
// BEGIN: square
noinline fun square( i : int ) : <ask1,exn> int
  if True then i * i else throw("impossible: " ++ ask1().show)
// END: square

// BEGIN: squarex
noinline fun square'( i : int ) : <exn> int
  if True then i * i else throw("impossible: ")
// END: squarex

// BEGIN: square1
noinline fun square-ask1() : ask1 int
  ask1() * ask1()
// END: square1

// BEGIN: square2
noinline fun square-ask2() : ask2 int
  ask2() * ask2()
// END: square2


val n = 100_000

// if open floating  is enabled there will be only one open around all the
// xN evaluations. Without open floating we do 20 separate open calls.
// BEGIN: body
fun test-one() : <ask1,ask2,exn> int
  // call `square(1)` 20 times
  val x1 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x2 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x3 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x4 = square(1) + square(1) + square(1) + square(1) + square(1)
  val x = x1 + x2 + x3 + x4
  val y = square-ask1()
  val z = square-ask2()
  x + y + z
// END: body

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

