effect output {
  fun msg(s : string) : ()
}

noinline fun calc(i : int) : int {
  i*i
}

fun test1() : <output> int {
  msg("hi")
  val x = calc(2)
  calc(x)
}

fun main() {
  with fun msg(s) { println(s) }
  test1().println
}
