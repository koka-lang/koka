import std/num/float64

fun int/showx( i : int ) : string
  i.show

fun char/showx( c : char ) : string
  c.show

fun float64/showx( f : float64 ) : string
  f.show

fun default/list/showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(",") ++ "]"

fun test1()
  [1,2].showx

fun test2()
  [[1],[2]].showx // this resolves recursively

fun test3()
  [['a','b']].showx  // this prefers `charlist/showx` as `default/list/showx` is in the default namespace

fun charlist/showx( cs : list<char> ) : string
  cs.string.show

fun test4()
  [['a','b']].showx(?showx=charlist/showx)

