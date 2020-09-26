// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean

import std/num/int32

//-----------------------------------------------------------------------------
// Red black tree definition
//-----------------------------------------------------------------------------
type color { 
  Red 
  Black 
}

type tree {
  Leaf
  Node(color: color, left: tree, key: int32, value: bool, right: tree)
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

fun balance-left(l:tree, k: int32, v: bool, r: tree): tree {
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

fun balance-right(l: tree, k: int32, v: bool, r: tree): tree {
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

fun ins(t: tree, k: int32, v: bool): tree {
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

fun insert(t: tree, k: int32, v: bool): tree {
  if (is-red(t))
    then set-black(ins(t, k, v))
    else ins(t, k, v)
}

//-----------------------------------------------------------------------------
// Deletion
// Adapted from Stefan Kahrs algorithm in "Red black trees with types", JFP, July 2001.
//-----------------------------------------------------------------------------


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

fun balance( l : tree, k : int32, v : bool, r : tree ) : tree {
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
    _ -> t // should not happen
  }
}

fun del-bal-left( l : tree, k : int32, v : bool, r : tree ) : tree {
  match((l,r)) {
    (Node(Red,ly,ky,vy,ry),_)   -> Node(Red,Node(Black,ly,ky,vy,ry),k,v,r)
    (_,Node(Black,lx,kx,vx,rx)) -> balance(l,k,v,Node(Red,lx,kx,vx,rx))
    (_,Node(Red,Node(Black,ly,ky,vy,ry),kx,vx,rx))
      -> Node(Red,Node(Black,l,k,v,ly),ky,vy,balance(ry,kx,vx,rx.subl))
    _ -> Node(Black,l,k,v,r) // should not happen
  }
}


fun del-bal-right( l : tree, k : int32, v : bool, r : tree ) : tree {
  match((l,r)) {
    (_,Node(Red,ly,ky,vy,ry))   -> Node(Red,l,k,v,Node(Black,ly,ky,vy,ry))
    (Node(Black,lx,kx,vx,rx),_) -> balance(Node(Red,lx,kx,vx,rx),k,v,r)         
    (Node(Red,lx,kx,vx,Node(Black,ly,ky,vy,ry)),_) 
      -> Node(Red,balance(lx.subl,kx,vx,ly),ky,vy,Node(Black,ry,k,v,r))
    _ -> Node(Black,l,k,v,r) // should not happen
  }
}

fun is-bnode(t : tree) : bool {
  match(t) {
    Node(Black) -> True
    _ -> False
  }
}

fun delete( t : tree, key : int32 ) : div tree {
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



//-----------------------------------------------------------------------------
// Benchmark
//-----------------------------------------------------------------------------


fun fold(t: tree, b: a, f: (int32, bool, a) -> a): a {
  match (t) {
    Leaf                -> b
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
  }
}

fun make-tree-aux(total : int32, n: int32, t: tree): div tree {
  if (n <= 0.int32) then t else {
    val n1 = n.dec
    val t1 = insert(t, n1, n1 % 10.int32 == zero)
    val t2 = if (n1%4.int32 == zero) then delete(t1, n1 + (total - n1)/4.int32) else t1
    make-tree-aux(total, n1, t2)    
  }
}

fun make-tree(n: int32): div tree {
  make-tree-aux(n, n, Leaf)
}

fun main() {
  val t = make-tree(4200000.int32); // todo: read first command line arg for size
  val v = t.fold(zero) fun(k,v,r:int32){ if (v) then r.inc else r }
  v.show.println
}
