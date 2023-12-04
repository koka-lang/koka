fun choose(x,y) { 
  if (True) then x else y 
}

fun xauto(f:forall<a> a -> a) {
  f(f)
}

fun auto(f:forall<a> a -> e a) : e (forall<a> a -> e a)
{
  f(f)
}

val test1 = auto(id)
val test2 = apply(auto,id)
val ids   = single(id:forall<a> a -> a)  //rigid annotation
val test3  = ids.map(auto)
val test3a = ids.map(xauto)

fun headlist( xs : list<a> ) : exn a
  head(xs).unjust

fun taillist( xs : list<a> ) : list<a>
  tail(xs)  

fun test4()  { apply(headlist,ids) }
fun test4a() { apply(taillist,ids) }

fun test5()  { apply(fn(xs:list<list<forall<a> a -> a>>){ xs.map(headlist) },single(ids)) }

fun test6()  { choose(id,auto) }
fun test6a() { choose(auto,id) }
fun test7()  { choose(xauto,xauto) }

fun test8()  { choose(ids,[]) }
fun test8a() { choose([],ids) }
