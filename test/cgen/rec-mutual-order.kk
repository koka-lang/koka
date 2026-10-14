// Mutually recursive definitions whose annotations quantify in another order than
// their generalized types: `forall<e,t>` against `forall<t,e>`, of different kinds.
// A reference to the other member takes the type arguments of its generalized type,
// which the specialization of the implicit `?t/pretty` substitutes.
type tree<t>
  Leaf( v : t )
  Node( kids : list<tree<t>> )

fun kids/pretty( ks : list<tree<t>>, ?t/pretty : t -> <div|e> string ) : <div|e> string
  ks.map(fn(k) k.tree/pretty).join(",")

fun tree/pretty( x : tree<t>, ?t/pretty : t -> <div|e> string ) : <div|e> string
  match x
    Leaf(v) -> v.t/pretty
    Node(ks) -> "[" ++ ks.kids/pretty ++ "]"

fun main()
  println(Node([Leaf(1), Leaf(2)]).tree/pretty(?t/pretty = fn(i) i.show))
