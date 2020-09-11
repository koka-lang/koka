type tree {
  Bin( left: tree, right: tree )
  Tip( value : int )
}

type visit {
  Done
  BinL( left:tree, visit: visit )
  BinR( right:tree, visit: visit )
}

type direction {
  Up
  Down
}

fun tmap(f : int -> int, t : tree, visit : visit, d : direction) : div tree {
  if (d.down?) {
    // go down the left spine
    match(t) {
      Bin(l,r) -> tmap(f,l,BinR(r,visit),Down)  // go down the left spine, and remember the right subtrees
      Tip(x)   -> tmap(f,Tip(f(x)),visit,Up)    // now go up through the saved `visit`
    }
  }
  else {
    // go up through the saved visit
    match(visit) {
      Done      -> t                  
      BinR(r,v) -> tmap(f,r,BinL(t,v),Down)   // go down into a right subtree, and remember the current left tree `t`
      BinL(l,v) -> tmap(f,Bin(l,t),v,Up)      // restore a `Bin` node and keep going up
    }
  }
}


fun visitmap(f : int -> int, t : tree, visit : visit) : div tree {
  match(visit) {
    Done      -> t
    BinR(r,v) -> binmap(f,r,BinL(t,v))
    BinL(l,v) -> visitmap(f,Bin(l,t),v)
  }
}

fun binmap( f : int -> int, t : tree, visit : visit) : div tree {
  match(t) {
    Bin(l,r) -> binmap(f,l,BinR(r,visit))  // reuse bin      
    Tip(x)   -> visitmap(f,Tip(f(x)),visit)      
  }
}
