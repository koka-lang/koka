fun sum1( xs, ?(+), ?zero )
  xs.foldl(zero,(+))

fun sum2( xs : list<a>, ?p1 : (a,a) -> a = (+), ?p2 : (a,a) -> a = (+), ?zero : a ) : a
  xs.foldl(zero,fn(x,y) p1(x,p2(y,y)))

val zero = 0
val zero_int = 0
val zero_float = 0.0

fun test1()
  val zero = 0
  [1,2].sum1((+),zero)


fun test2()
  [1,2].sum1



fun test(  x : int, y : int = 3, ?z : int ) : int
  x + y + z

fun test3()
  val z = 2
  test(1)

fun test4()
  test(1,?z=1+2,y=2+2)

fun test5()
  [1,2].sum2( ?p2=(*))