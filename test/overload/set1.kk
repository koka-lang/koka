struct set<s,a>
  empty   : s<a>
  single  : (a) -> s<a>
  union   : (s<a>,s<a>) -> s<a>
  contains: (s<a>,a) -> bool
  elements: (s<a>) -> list<a>

val unit/set : set<maybe,()> =
  fun union(mbx,mby)
    match mbx
      Nothing -> mby
      _       -> mbx
  fun contains(mbx,()) mbx.maybe/bool
  fun elements(mbx)    mbx.maybe/list
  Set(Nothing,Just,union,contains,elements)

fun list/set( ?eq : (a,a) -> bool ) : set<list,a>
  fun single(x)
    [x]
  fun contains(xs,y)
    match xs
      Nil -> False
      Cons(x,xx) -> if eq(x,y) then True else contains(xx,y)
  fun union(xs,ys)
    match xs
      Nil -> ys
      Cons(x,xx) -> if ys.contains(x) then union(xx,ys)
                                      else union(xx,Cons(x,ys))
  Set([],single,union,contains,id)


fun two( x : a, y : a, ??set : set<s,a> ) : s<a>
  union( single(x), single(x) )

fun inttest(??set)
  two(1,2).elements.show-list(show)

fun unittest(??set)
  two((),()).elements.show-list(show)

fun int/eq(x:int,y:int) x==y
fun unit/eq(x:(),y:())  True
fun char/eq(x:char,y:char) x==y

fun test1()
  inttest()

fun test2()
  unittest()  // picks ?set=unit/set as that is shortest..

fun test2a()
  unittest(?set = list/set())