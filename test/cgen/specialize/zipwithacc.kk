// this doesn't specialize correctly
fun zipwith-acc( f, i, acc, xs, ys )
{
  match(xs) {
    Nil -> reverse(acc)
    Cons(x,xx) ->
      match(ys) {
        Nil -> reverse(acc)
        Cons(y,yy) -> zipwith-acc(f, i+1, Cons( f(i,x,y), acc), xx, yy)
      }
  }
}

fun main() {
    val plus : (int, int) -> int = (+)
    val f = fn(i, x, y) { i.plus(x).plus(y) }
    val xs : list<int> = list(1, 10)
    val ys : list<int> = list(20, 10)
    zipwith-acc(f, 0, [], xs, ys).show.println
}