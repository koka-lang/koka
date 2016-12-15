
fun map-acc( xs : list<a>, f : a -> e b, acc : list<b> ) : e list<b> {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> xx.map-acc(f, Cons(f(x),acc))
  }
}

fun main() {
  [1,2].map-acc(show,[]).join(",").println
}