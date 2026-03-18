// this is rejected if we treat infinite chains as potential solutions
// (otherwise we accept it)
fun end/even( xs : list<list<int>> ) : string
  show(xs)

fun grow2/even( x : list<a> , ?even : list<list<a>> -> string ) : string
  even([x])

fun grow1/even( x : a , ?even : list<a> -> string ) : string
  even([x])

fun main()
  even(1)
