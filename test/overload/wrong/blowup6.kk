// accepted by the derivation rules but no clear decreasing measure
fun end/even( xs : list<list<int>> ) : string
  show(xs)

fun grow/even( x : a , ?even : list<a> -> string ) : string
  even([x])

fun main()
  even(1)
