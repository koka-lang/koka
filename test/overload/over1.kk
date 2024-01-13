fun int/showx( i : int ) : string
  i.show

fun char/showx( c : char ) : string
  c.show

fun list/showx( xs : list<a>, ?elem/show : a -> string) : string
  xs.show(?show = elem/show)

// test if named parameters can be assigned
fun tuple2/showx( x : (a,b), showfst : a -> e string, ?snd/show : b -> e string ) : e string
  tuple2/show(x, ?fst/show = showfst, ?snd/show = snd/show)

fun test()
  [1,2].println