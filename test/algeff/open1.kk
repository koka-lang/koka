// test lifting of cps functions
fun myid() : ((x:a) -> e a) {
  return fn(x){ x }
}

val hs : forall<e> list<int -> e int> = [myid()]

fun bar() {
  head(hs).default(id)(10)
}

fun main() {
  bar().show.println
}