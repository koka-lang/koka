fun test() {
  choose( ref(1), error("hi") )
}

fun test2() {
  choose( const((),ref(1)), ref(2).set(3))
}
