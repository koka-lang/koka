// Test inject

fun foo(action : () -> e a ) : <ndet|e> a
{
  val x = inject<ndet>(action)
  if (random-bool()) then foo(action) else x 
}

fun main() {
  foo{42}.println
}