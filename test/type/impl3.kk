struct monoid<a>
  zero : a
  plus : (a,a) -> a

//val zero = 0
//val zero = 0.0

val monoid = Monoid(0,(+))
//val monoid = Monoid(1,(*))
//val monoid = Monoid(0.0,(+))
val monoid_modint = Monoid(0,(+))

fun monoid( ?m:monoid<a> = monoid)  : monoid<list<a>>
  Monoid([m.zero],fn(xs,ys) zipwith(xs,ys,m.plus))
