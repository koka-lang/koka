value struct monoid<a>
  mzero : a
  mplus : (a,a) -> a

fun zero( ?monoid : monoid<a> ) : a
  monoid.mzero

fun plus( x : a, y : a, ?monoid : monoid<a> ) : a
  (monoid.mplus)(x,y)


val monoid_intplus = Monoid(0,(+))
val monoid_intmul  = Monoid(1,(*))
val monoid_float   = Monoid(0.0,(+))

fun msum( xs, ?monoid )
  xs.foldl(zero(),fn(x,y) plus(x,y))
  // note: we need to eta expand `plus` to match the type `(a,a,?monoid:monoid<a>) -> a` to `(a,a) -> a`
  // maybe we can do this kind of conversion automatically in the compiler?

fun msum-named( xs, ?monoid )
  xs.foldl(monoid.mzero,monoid.mplus)
  // note: now no eta expansion is needed

fun msum-expand( xs, ?monoid )
  val mzero = monoid.mzero
  val mplus = monoid.mplus
  xs.foldl(mzero,mplus)
  // note: now no eta expansion is needed

fun monoid( ?monoid : monoid<a>)  : monoid<list<a>>
  Monoid([zero()], fn(xs,ys) zipwith(xs,ys,fn(x,y) plus(x,y)))  // again eta expansion

fun monoid-named( ?m : monoid<a> = monoid)  : monoid<list<a>>
  Monoid([m.mzero], fn(xs,ys) zipwith(xs,ys,m.mplus))

fun monoid-expand( ?monoid )
  val mzero = monoid.mzero
  val mplus = monoid.mplus
  Monoid([mzero],fn(xs,ys) zipwith(xs,ys,mplus))


fun test1()
  [1.0,2.0].msum

fun test2()
  [[1.0],[2.0]].msum.msum
