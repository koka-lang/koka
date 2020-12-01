
fun mapx( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
    Nil        -> Nil
  }
}

fun main() {
  list(1,10).mapx(fn(i){ i.println; i+1 }).sum.println
}
