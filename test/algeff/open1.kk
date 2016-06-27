// test lifting of cps functions
fun myid() : ((x:a) -> e a) {
  return fun(x){ x }
}

val hs : forall<e> list<int -> e int> = [myid()]

fun bar() {
  head(hs).maybe(id)(10)
}

fun main() {
  bar().show.println
}