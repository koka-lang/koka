fun sum1( xs, ?(+), ?zero )
  xs.foldl(zero,(+))

fun sum2( xs : list<a>, ?p1 : (a,a) -> a = (+), ?p2 : (a,a) -> a = (+), ?zero : a ) : a
  xs.foldl(zero,p1)

fun foo1()
  val zero = 0
  [1,2].sum1((+),zero)


fun foo2()
  val zero = 0
  [1,2].sum1



fun test(  x : int, y : int = 3, ?z : int ) : int
  x + y + z

fun foo3()
  val z = 2
  test(1)