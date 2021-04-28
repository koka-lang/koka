fun choose(x,y){ if (True) then x else y }

fun apply(f,x) {
  f(x)
}

fun test() {
  fun i(x) { x }
  i(i)
}

fun test2() {
  single(id)
}

fun test3()  { choose(fn(x,y){x},fn(x,y){y}) }
fun test4(f) { choose(id,f) }
