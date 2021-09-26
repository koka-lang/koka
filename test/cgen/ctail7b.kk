type tree
  Tip
  Node( left : tree, right : tree )


// make a perfectly balanced binary tree of `depth`
fun make( depth : int ) : div tree
  if depth > 0
    then Node( make(depth.dec), make(depth.dec) ) 
    else Node( Tip, Tip )

// normal checksum  
fun check( t : tree ) : int
  match t
    Node(l,r) -> l.check + r.check + 1
    Tip       -> 0
  
  
fun main() {
  make(10).check.println
}