fun xforeach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
{
  var i := 0
  xs.foreach fun(x) {
    val j = i // don't dereference `i` inside the closure
    inject<local>{ action(j,x) }
    i := i+1
  }
}
