effect input {
  fun ask() : int
}

noinline fun calc(i : int, j) {
  i + j
}

fun test1() {
  // throw("ouch")          // open(〈〉, 〈reader, exn〉)(throw)("ouch")
  calc( ask(), ask() )
}

fun main() {
  with fun ask() { 1 }
  test1().println
}
