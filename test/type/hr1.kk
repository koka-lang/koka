function xauto(f:forall<a> a -> a) {
  f(f)
}

function auto(f:forall<a> a -> a) : (forall<a> a -> a)
{
  f(f)
}

val test1 = auto(id)
val test2 = apply(auto,id)
val ids   = single(id:forall<a> a -> a)  //rigid annotation
val test3  = ids.map(auto)
val test3a = ids.map(xauto)

function test4()  -> apply(head,ids)
function test4a() -> apply(tail,ids)

function test5()  -> apply(fun(xs:list<list<forall<a> a -> a>>){ xs.map(head) },single(ids))

function test6()  -> choose(id,auto)
function test6a() -> choose(auto,id)
function test7()  -> choose(xauto,xauto)

function test8()  -> choose(ids,[])
function test8a() -> choose([],ids)
