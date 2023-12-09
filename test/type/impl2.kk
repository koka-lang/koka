fun showx( i : int ) : string
  i.show

fun showx( c : char ) : string
  c.show

fun showx( f : float64 ) : string
  f.show


fun showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(",") ++ "]"

fun foo1()
  [1,2].showx

fun foo2()
  [[1],[2]].showx // this resolves recursively