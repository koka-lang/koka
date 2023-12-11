value struct monoid<a>
  mzero : a
  mplus : (a,a) -> a

fun zero( ??monoid : monoid<a> ) : a
  mzero

fun plus( x : a, y : a, ??monoid : monoid<a> ) : a
  mplus(x,y)


val monoid_intplus = Monoid(0,(+))
val monoid_intmul  = Monoid(1,(*))
val monoid_float   = Monoid(0.0,(+))

fun msum( xs, ?monoid )
  xs.foldl(zero(),fn(x,y) plus(x,y))
  // note: we need to eta expand `plus` to match the type `(a,a,?monoid:monoid<a>) -> a` to `(a,a) -> a`
  // maybe we can do this kind of conversion automatically in the compiler?

fun msum-expand( xs, ??monoid )
  xs.foldl(mzero,mplus)
  // note: now no eta expansion is needed

fun monoid( ?monoid : monoid<a>)  : monoid<list<a>>
  Monoid([zero()], fn(xs,ys) zipwith(xs,ys,fn(x,y) plus(x,y)))  // again eta expansion

fun monoid-expand( ??monoid )
  Monoid([mzero],fn(xs,ys) zipwith(xs,ys,mplus))


fun test1()
  [1.0,2.0].msum

fun test2()
  [[1.0],[2.0]].msum.msum
