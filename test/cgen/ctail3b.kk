effect out {
  fun out(s : string) : ()
}


fun mapx( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
    Nil        -> Nil
  }
}

fun main() {
  with fun out(s:string) { if (s=="9") throw(s); println(s) }
  list(1,10).mapx(fn(i:int){ if (i.is-odd) i.show.out; i+1 }).sum.println
}
