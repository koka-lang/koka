// polymorphic recursion with an annotation: the recursive call keeps its own type arguments
type nested<a>
  Flat( x : a )
  Nest( n : nested<list<a>> )

fun depth( n : nested<a> ) : int
  match n
    Flat -> 0
    Nest(m) -> 1 + depth(m)

fun main()
  println(depth(Nest(Nest(Flat([[1]])))).show)
