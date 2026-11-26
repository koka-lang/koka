// A `max-effects<e,m>` constraint bounds the effect `e` of a function argument by the closed row
// `m`. At each call `e` is the exact effect of the argument, and the call is opened to the effect
// of its context; a function can pass the constraint on to its caller.
fun apply-max(f : () -> e a, ?max-effects : max-effects<e,<div,ndet>>) : e a
  f()

fun twice(f : () -> e int, ?max-effects : max-effects<e,<div,ndet>>) : e int
  apply-max(f) + apply-max(f)

fun spin(n : int) : div int
  if n <= 0 then 0 else spin(n - 1)

pub fun main()
  val a = apply-max(fn() 1)
  val b = apply-max(fn() unique() * 0 + spin(2) + 2)
  val c = twice(fn() 3)
  if a < 0 then throw("negative")
  println([a,b,c])
