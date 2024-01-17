// encode Haskell show instances where we show list of characters specially
import std/num/float64

fun int/showx( i : int ) : string
  i.show

fun char/showx( c : char ) : string
  c.show

fun float/showx( f : float64 ) : string
  f.show

// a default showx for most lists
fun list/showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(",") ++ "]"

fun chars/showx( cs : list<char>, ?showx : char -> string ) : string
  "\"" ++ cs.map(?showx).join ++ "\""

fun test1()
  [1,2].showx


// fun test1a() ['h','i'].showx   // ambiguous now

// fun test1b() [].showx        // ambiguous

fun test2()
  [[1],[2]].showx // this resolves recursively

fun test3()
  [[1,2]].showx  // resolves recursively and uses `list/showx(?showlistx=default/showlistx(?showx=int/showx))`

// fun test3a() [['a','b']].showx  // resolves recursively and uses `list/showx(?showlistx=chars/showlistx)`

fun test4()
  [1,2] < [2 + 1,3]


fun test(xs)
  val ys = [xs,['a','b']].map(length)
  ys.show