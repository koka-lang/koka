alias container<k,v,c> = list<(k,v,c)>

fun eq/something(?(==) : (k, k) -> e bool): container<k,v,c>
  []
val thing : list<(int,int,char)> = []

fun do-something(l: list<(k,v,c)>, ?something: container<k,v,c>,
    ?k/show: (k) -> e string,
    ?v/show: (v) -> e string,
    ?c/show: (c) -> e string,
    ?k/(==): (k,k) -> e bool): e string
  something.map(fn((k, v, _)) show(k) ++ " -> " ++ show(v)
      ).join("")

fun main(): console ()
  val r = thing.do-something()
  r.println