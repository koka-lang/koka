/*
  Benchmark red-black trees. 
  Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean
  Algorithms based on:
out/  
  * Chris Okasaki, \"Red-Black Trees in a Functional Setting\",
    Journal of Functional Programming, 9(4), pp 471-477, July 1999
    <http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#jfp99>

  * Stefan Kahrs, \"Red-black trees with types\",
    Journal of functional programming, 11(04), pp 425-432, July 2001
*/    

//-----------------------------------------------------------------------------
// Red black tree definition
//-----------------------------------------------------------------------------
type color { 
  Red 
  Black 
}

type tree {
  Leaf
  Node(color: color, left: tree, key: int, value: bool, right: tree)
}


fun is-red(t : tree) : bool {
  match(t) {
    Node(Red) -> True
    _         -> False
  }
}


//-----------------------------------------------------------------------------
// Insertion
//-----------------------------------------------------------------------------

fun balance-left(l:tree, k: int, v: bool, r: tree): tree {
  match(l) {
    Leaf -> Leaf
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)
  }
}

fun balance-right(l: tree, k: int, v: bool, r: tree): tree {
  match(r) {
    Leaf -> Leaf
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))
  }
}

fun ins(t: tree, k: int, v: bool): tree {
  match(t) {
    Leaf -> Node(Red, Leaf, k, v, Leaf)
    Node(Red, l, kx, vx, r)
      -> if (k < kx) then Node(Red, ins(l, k, v), kx, vx, r)
         elif (k == kx) then Node(Red, l, k, v, r)
         else Node(Red, l, kx, vx, ins(r, k, v))
    Node(Black, l, kx, vx, r)
      -> if (k < kx) then (if (is-red(l)) then balance-left(ins(l,k,v), kx, vx, r)
                                          else Node(Black, ins(l, k, v), kx, vx, r))
         elif (k == kx) then Node(Black, l, k, v, r)
         elif (is-red(r)) then balance-right(l, kx, vx, ins(r,k,v))
         else Node(Black, l, kx, vx, ins(r, k, v))  
  }
}

fun set-black(t: tree) : tree {
  match(t) {
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t
  }
}

fun insert(t: tree, k: int, v: bool): tree {
  if (is-red(t))
    then ins(t, k, v).set-black
    else ins(t, k, v)
}

//-----------------------------------------------------------------------------
// Deletion
// Adapted from Stefan Kahrs algorithm in "Red black trees with types", JFP, July 2001.
//-----------------------------------------------------------------------------

fun balance( l : tree, k : int, v : bool, r : tree ) : tree {
  match((l,r)) {
    (Node(Red,lx,kx,vx,rx), Node(Red,ly,ky,vy,ry))
      -> Node(Red,Node(Black,lx,kx,vx,rx),k,v,Node(Black,ly,ky,vy,ry))
    (Node(Red,lx,kx,vx,rx), Node(Red,ly,ky,vy,ry)) 
      -> Node(Red,Node(Black,lx,kx,vx,rx),k,v,Node(Black,ly,ky,vy,ry))
    (Node(Red,lx,kx,vx,Node(Red,ly,ky,vy,ry)), _) 
      -> Node(Red,Node(Black,lx,kx,vx,ly),ky,vy,Node (Black,ry,k,v,r))
    (_,Node(Red,lx,kx,vx, Node (Red,ly,ky,vy,ry))) 
      -> Node(Red,Node(Black,l,k,v,lx),kx,vx,Node(Black,ly,ky,vy,ry))
    (_,Node(Red,Node(Red,ly,ky,vy,ry),kx,vx,rx)) 
      -> Node(Red,Node(Black,l,k,v,ly),ky,vy,Node(Black,ry,kx,vx,rx))
    _ -> Node(Black,l,k,v,r)
  } 
}

fun subl( t : tree ) : tree {
  match(t) {
    Node(Black,l,k,v,r) -> Node(Red,l,k,v,r)
    _ -> t
  }
}

fun del-bal-left( l : tree, k : int, v : bool, r : tree ) : tree {
  match((l,r)) {
    (Node(Red,ly,ky,vy,ry),_)   -> Node(Red,Node(Black,ly,ky,vy,ry),k,v,r)
    (_,Node(Black,lx,kx,vx,rx)) -> balance(l,k,v,Node(Red,lx,kx,vx,rx))
    (_,Node(Red,Node(Black,ly,ky,vy,ry),kx,vx,rx))
      -> Node(Red,Node(Black,l,k,v,ly),ky,vy,balance(ry,kx,vx,rx.subl))
    _ -> Node(Black,l,k,v,r) // should not happen
  }
}


fun del-bal-right( l : tree, k : int, v : bool, r : tree ) : tree {
  match((l,r)) {
    (_,Node(Red,ly,ky,vy,ry))   -> Node(Red,l,k,v,Node(Black,ly,ky,vy,ry))
    (Node(Black,lx,kx,vx,rx),_) -> balance(Node(Red,lx,kx,vx,rx),k,v,r)         
    (Node(Red,lx,kx,vx,Node(Black,ly,ky,vy,ry)),_) 
      -> Node(Red,balance(lx.subl,kx,vx,ly),ky,vy,Node(Black,ry,k,v,r))
    _ -> Node(Black,l,k,v,r) // should not happen
  }
}

fun fuse(l : tree, r : tree) : div tree {  // todo: improve exn/div checking for multi patterns
  match((l,r)) {
    (Leaf,_) -> r
    (_,Leaf) -> l
    (Node(Black,lx,kx,vx,rx),Node(Black,ly,ky,vy,ry)) -> match(fuse(rx,ly)) {
      Node(Red,lz,kz,vz,rz) -> Node(Red,Node(Black,lx,kx,vx,lz),kz,vz,Node(Black,rz,ky,vy,ry))
      z -> del-bal-left(lx,kx,vx,Node(Black,z,ky,vy,ry))
    }
    (Node(Red,lx,kx,vx,rx),Node(Red,ly,ky,vy,ry)) -> match(fuse(rx,ly)) {
      Node(Red,lz,kz,vz,rz) -> Node(Red,Node(Red,lx,kx,vx,lz),kz,vz,Node(Red,rz,ky,vy,ry))
      z -> Node(Red,lx,kx,vx,Node(Red,z,ky,vy,ry))
    }
    (_,Node(Red,ly,ky,vy,ry)) -> Node(Red,fuse(l,ly),ky,vy,ry)
    (Node(Red,lx,kx,vx,rx),_) -> Node(Red,lx,kx,vx,fuse(rx,r))
    _ -> Leaf // should not happen
  }
}


fun is-bnode(t : tree) : bool {
  match(t) {
    Node(Black) -> True
    _ -> False
  }
}


fun delete( t : tree, key : int ) : div tree {
  match(t) {
    Node(_,l,k,v,r) -> match(compare(key,k)) {
      Lt -> if (l.is-bnode()) then del-bal-left(l.delete(key),k,v,r)
                              else Node(Red,l.delete(key),k,v,r)
      Gt -> if (r.is-bnode()) then del-bal-right(l,k,v,r.delete(key))
                              else Node(Red,l,k,v,r.delete(key))
      Eq -> fuse(l,r)
    }
    Leaf -> Leaf
  }
}


/*
// From: https://hackage.haskell.org/package/llrbtree-0.1.1/docs/src/Data-Set-RBTree.html#delete
// This one is a tad faster
struct del{ t : tree; b : bool }

fun set-red(t: tree) : tree {
  match(t) {
    Node(_, l, k, v, r) -> Node(Red, l, k, v, r)
    _ -> t
  }
}

fun make-black(t : tree ) : del {
  match(t) {
    Node(Red,l,k,v,r) -> Del(Node(Black,l,k,v,r),False)
    _                 -> Del(t,True) 
  }
}

inline fun unbalance-left( c : color, l : tree, k : int, v : bool, r : tree ) : del {
  match(l) {
    Node(Black) -> Del( balance-left(set-red(l),k,v,r), c.black? )
    Node(Red,lx,kx,vx,rx) -> Del( Node(Black,lx,kx,vx,balance-left(set-red(rx),k,v,r)), False )
    _ -> Del(Node(Black,l,k,v,r),False)  // cannot happen
  }
}

inline fun unbalance-right( c : color, l : tree, k : int, v : bool, r : tree ) : del {
  match(r) {
    Node(Black) -> Del( balance-right(l,k,v,set-red(r)), c.black? )
    Node(Red,lx,kx,vx,rx) -> Del( Node(Black,balance-right(l,k,v,set-red(lx)),kx,vx,rx), False )
    _ -> Del(Node(Black,l,k,v,r),False)  // cannot happen
  }
}

struct delmin{ del : del; key: int; min : bool }

fun delete-min(t : tree) : tree {
  match(t.del-min) {
    Delmin(Del(t')) -> t'.set-black
  }
}


fun del-min( t : tree ) : delmin {
  match(t) {
    Node(Black,Leaf,k,v,r) {
      match(r) {
        Leaf -> Delmin(Del(Leaf,True),k,v)
        _    -> Delmin(Del(r.set-black,False),k,v)
      }
    }
    Node(Red,Leaf,k,v,r) {
      Delmin(Del(r,False),k,v)
    }
    Node(c,l,k,v,r) {
      match(l.del-min) {
        Delmin(Del(lx,True),kx,vx)  -> Delmin(unbalance-right(c,lx,k,v,r),kx,vx)
        Delmin(Del(lx,False),kx,vx) -> Delmin(Del(Node(c,lx,k,v,r),False),kx,vx)
      }
    }
    Leaf -> Delmin(Del(t,False),0,False) // cannot happen    
  }
}

fun del( t : tree, k : int ) : del {
  match(t) {
    Node(cx,lx,kx,vx,rx) -> match(compare(k,kx)) {
      Lt -> match(lx.del(k)) {
        Del(ly,True)  -> unbalance-right(cx,ly,kx,vx,rx)
        Del(ly,False) -> Del(Node(cx,ly,kx,vx,rx),False)
      }
      Gt -> match(rx.del(k)) {
        Del(ry,True)  -> unbalance-left(cx,lx,kx,vx,ry)
        Del(ry,False) -> Del(Node(cx,lx,kx,vx,ry),False)
      }
      Eq -> match(rx) {
        Leaf -> if (cx.black?) then lx.make-black else Del(lx,False)
        _    -> match(rx.del-min) {
          Delmin(Del(ry,True),ky,vy) -> unbalance-left(cx,lx,ky,vy,ry)
          Delmin(Del(ry,False),ky,vy) -> Del(Node(cx,lx,ky,vy,ry),False)
        }
      }
    }
    Leaf -> Del(Leaf,False)    
  }
}

fun delete( t : tree, k : int ) : div tree {
  match(t.del(k)) {
    Del(tx,_) -> tx.set-black
  }
}
*/

//-----------------------------------------------------------------------------
// Benchmark
//-----------------------------------------------------------------------------


fun fold(t: tree, b: a, f: (int, bool, a) -> a) : a {
  match (t) {
    Leaf                -> b
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
  }
}

fun make-tree-aux(total : int, n: int, t: tree): div tree {
  if (n <= 0) then t else {
    val n1 = n - 1
    val t1 = insert(t, n1, n1 % 10 == 0)
    val t2 = if (n1%4 == 0) then delete(t1, n1 + (total - n1)/4) else t1
    make-tree-aux(total, n1, t2)    
  }
}

fun make-tree(n: int): div tree {
  make-tree-aux(n, n, Leaf)
}

fun main()  {
  val t = make-tree(4200000); // todo: read first command line arg for size
  val v = t.fold(0) fn(k,v,r){ if (v) then r + 1 else r }
  v.println
}
