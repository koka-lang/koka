// encode Haskell show instances where we show list of characters specially

fun showx( i : int ) : string
  i.show

fun showx( c : char ) : string
  c.show

fun showx( f : float64 ) : string
  f.show


// a default showlistx for most lists
fun showlistx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(",") ++ "]"

// a special one for characters -- when resolving an implicit parameter `showlistx` the
// postfixed ones will be preferred over the plain `showlistx`
fun showlistx_chars( cs : list<char> ) : string
  "\"" ++ cs.map(string).join ++ "\""

// instance for showing lists
fun showx( xs : list<a>, ?showlistx : list<a> -> string ) : string
  xs.showlistx


fun test1()
  [1,2].showx

fun test2()
  [[1],[2]].showx // this resolves recursively

fun test3()
  [['a','b']].showx  // resolves recursively and uses `showhlistx_chars`
