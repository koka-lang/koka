function test() {
  choose( ref(1), error("hi") )
}

function test2() {
  choose( const((),ref(1)), ref(2).set(3))
}
