import std/time/timer

effect fun ask1() : int
effect fun ask2() : int

// use noinline to prevent full inlining and optimizations
// at least 2 effects to cause allocation when converting from <ask1,ask2,exn> -> <ask1,exn>
noinline fun square( i : int ) : <ask1,exn> int   
  if True then i * i else throw("impossible")

noinline fun square-ask1() : ask1 int
  ask1() * ask1()

noinline fun square-ask2() : ask2 int
  ask2() * ask2()

val n = 10_000_000

// currently does not float up the open through the `fold-int`, and thus does N opens around square
fun test-slow() : <ask1,ask2,exn> int
  val x = fold-int(1,n,1) fn(acc,i) square(acc)
  val y = square-ask1()
  val z = square-ask2()
  x + y + z


// manually lift out the fold, now there is one open for the whole `foldint`
noinline fun foldint() : <ask1,exn> int
  fold-int(1,n,2) fn(acc,i) square(acc)

fun test-fast() : <ask1,ask2,exn> int
  val x = foldint()
  val y = square-ask1()
  val z = square-ask2()
  x + y + z


// Time both
fun main() : <console,ndet,exn> int
  with fun ask1() 1
  with fun ask2() 2
  print-elapsed( test-slow, "test slow" )
  print-elapsed( test-fast, "test fast" )
