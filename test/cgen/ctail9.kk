
fun mapxx( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapxx(f))
    Nil        -> Nil
  }
}

fun mapx( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Nil  -> Nil
    _    -> xs.mapxx(f)
  }
}

fun test(n : int) {
  val xs = list(1,n)
  val x = fold-int(0,100000000/(if n<=0 then 1 else n),0) fn(i,acc)
            // acc + xs.mapxx(fn(x){ x+1 }).sum
            acc + xs.mapx(fn(x){ x+1 }).sum
  println("total: " ++ x.show)
}

