// test duplicate cps translation of polymorphic results

fun compose1(f,g) {
  fun comp(x) {
    f(g(x))
  }
  comp
}


fun compose2(f,g) {
  return fun(x) {
    f(g(x))
  }
}

fun main() {
  println(compose1(inc,inc)(42))
  println(compose2(inc,inc)(42))
}
