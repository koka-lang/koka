// Mutually recursive functions that pass a function parameter on (through `map`).
// Specializing `ptree` at the call in `main` used to loop forever: each member's
// specialized body calls the other member, which was specialized again.
type tree<t>
  Leaf( v : t )
  Node( kids : list<tree<t>> )

fun pkids( ks : list<tree<t>>, f : t -> string ) : div string
  ks.map(fn(k) ptree(k, f)).join(",")

fun ptree( x : tree<t>, f : t -> string ) : div string
  match x
    Leaf(v) -> f(v)
    Node(ks) -> "[" ++ pkids(ks, f) ++ "]"

fun main()
  println(Node([Leaf(1), Leaf(2)]).ptree(fn(i) i.show))
