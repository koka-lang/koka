
public fun f(x : int) : div int {
  if (x==0) then 1 else g(x) + 1;
}
fun g(x : int) : div int {
  g(f(x - 1));
}

private fun const(x,y) {
  x
}

fun apply(f,x) {
  f(x)
}

fun incr(x : int) : int {
  x + 1
}

fun eta-test1() {
  id(id)(1)
}

fun box-test1( x : int ) : int {
  code1/apply(incr,x)
}

fun box-test2( xs : list<int> ) : list<int> {
  xs.map(incr)
}

fun box-test3( xs : list<int> ) : int {
  xs.map(Just).head(Nothing).default(42)
}

fun box-test4( xs : list<int> ) : int {
  match(xs.map(Just)) {
    Cons(Just(x)) -> x
    Cons(Nothing,Cons(Just(y))) -> y
    _ -> 42
  }
}

fun mapx( xs, f ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
  }
}

fun map-acc( xs, f, acc ) {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> xx.map-acc(f,Cons(f(x),acc))
  }
}
