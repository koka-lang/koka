// Test inject
fun injectst( action : () -> e a ) : <st<h>|e> a {
  // we can inject using an explicit `some`, or just underscore, or leave out the parameters
  inject<alloc>{inject<write<_h>>{inject<some<h>read<h>>(action)}}
}

fun xforeach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
{
  var i := 0
  xs.foreach fun(x) {
    val j = i;
    injectst{action(j,x)}
    i := i+1
  }
}

fun main() {
  var acc := 0
  [0,41].xforeach-indexed fun(idx,x) {
    acc := acc + idx + x
  }
  println(acc)
}