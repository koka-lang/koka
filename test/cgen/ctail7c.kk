// Tail recursion on a field that has a single constructor
type tree
  Tip
  Node( left : ind<tree>, right : ind<tree> )

reference type ind<a>
  Ind( ind : a )

fun make( depth : int ) : console tree
  if depth > 0 then 
    val d = depth.dec
    Node( Ind(make(unsafe-decreasing(d))), Ind(Tip) ) 
  else Node( Ind(Tip), Ind(Tip) )

fun check( t : tree ) : int
  match t
    Tip -> 0
    Node(Ind(l),Ind(r)) -> l.check + r.check + 1

fun main() {
  make(10).check.println
}