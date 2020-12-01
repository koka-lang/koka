fun xauto(f:forall<a> a -> a) {
  f(f)
}

fun auto(f:forall<a> a -> a) : (forall<a> a -> a)
{
  f(f)
}

val test1 = auto(id)
val test2 = apply(auto,id)
val ids   = single(id:forall<a> a -> a)  //rigid annotation
val test3  = ids.map(auto)
val test3a = ids.map(xauto)

fun test4()  { apply(head,ids) }
fun test4a() { apply(tail,ids) }

fun test5()  { apply(fn(xs:list<list<forall<a> a -> a>>){ xs.map(head) },single(ids)) }

fun test6()  { choose(id,auto) }
fun test6a() { choose(auto,id) }
fun test7()  { choose(xauto,xauto) }

fun test8()  { choose(ids,[]) }
fun test8a() { choose([],ids) }
