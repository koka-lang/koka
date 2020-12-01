fun id(x) {
  return x;
}

fun apply(f,x) {
  f(x)
}

fun test() {
  fun i(x) { x }
  i(i)
}

fun test2() {
  single(hm1/id)
}

fun test3()  { choose(fn(x,y){x},fn(x,y){y}) }
fun test4(f) { choose(core/id,f) }
