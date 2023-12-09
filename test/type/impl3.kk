value struct monoid<a>
  zero : a
  plus : (a,a) -> a

val zero_int = 0
val zero_float = 0.0

val monoid_intplus = Monoid(0,(+))
val monoid_intmul  = Monoid(1,(*))
val monoid_float   = Monoid(0.0,(+))


fun monoid( ?m : monoid<a> = monoid)  : monoid<list<a>>
  Monoid([m.zero],fn(xs,ys) zipwith(xs,ys,m.plus))

fun sumx( xs : list<a>, ?m : monoid<a> = monoid ) : a
  xs.foldl(m.zero,m.plus)

fun test1()
  [1.0,2.0].sumx

fun test2()
  [[1.0]].sumx
