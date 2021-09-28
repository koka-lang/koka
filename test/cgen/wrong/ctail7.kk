// Tail recursion on a field in a value type.
type tree
  Tip
  Node( lr : maybe<(tree,tree)> )


fun make( depth : int ) : div tree
  if depth > 0
    then Node( Just((make(depth.dec), make(depth.dec))) ) 
    else Node( Just((Tip, Tip)) )
