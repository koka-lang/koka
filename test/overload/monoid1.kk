value struct monoid<a>
  mzero : a
  mplus : (a,a) -> a

val intplus/monoid = Monoid(0,(+))
val intmul/monoid  = Monoid(1,(*))
val float/monoid   = Monoid(0.0,(+))

fun msum( xs, ?m/monoid )
  xs.foldl(m/monoid.mzero,m/monoid.mplus)


fun list/monoid( ?m/monoid : monoid<a>)  : monoid<list<a>>
  val zero = m/monoid.mzero
  val plus = m/monoid.mplus
  Monoid([zero], fn(xs,ys) zipwith(xs,ys,plus))


fun test1()
  [1.0,2.0].msum

fun test2()
  [[1.0],[2.0]].msum.msum
