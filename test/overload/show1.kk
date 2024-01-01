fun int/showx( i : int ) : string
  i.show

fun char/showx( c : char ) : string
  c.show

fun float64/showx( f : float64 ) : string
  f.show

fun showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(",") ++ "]"

fun foo1()
  [1,2].showx

fun foo2()
  [[1],[2]].showx // this resolves recursively

fun foo3()
  [['a','b']].showx