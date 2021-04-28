fun choose(x,y){ if (True) then x else y}

fun test() {
  choose( ref(1), throw("hi") )
}

fun test2() {
  choose( const((),ref(1)), ref(2).set(3))
}
