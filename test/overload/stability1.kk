// Stability examples
fun id/trans( x : a ) : a
  x

fun int/trans( x : int ) : int
  x + 1

fun bad1( x : a ) : a
  trans(x)

fun bad2( x : a, ?trans : a -> a ) : a
  trans(x)

// // rejected
// fun bad3(x)
//   trans(x)

fun main()
  (bad1(1), bad2(1,id/trans), int/trans(1))
