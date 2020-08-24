// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean
import std/num/int32

type color { 
  Red 
  Black 
}

type tree {
  Leaf()
  Node(color: color, lchild: tree, key: int32, value: bool, rchild: tree)
}

fun balance1(kv: int32, vv: bool, t: tree, s: tree): tree {
  match(s) {
    Leaf -> Leaf
    Node(_, Node(Red, l, kx, vx, r1), ky, vy, r2)
      -> Node(Red, Node(Black, l, kx, vx, r1), ky, vy, Node(Black, r2, kv, vv, t))
    Node(_, l1, ky, vy, Node(Red, l2, kx, vx, r))
      -> Node(Red, Node(Black, l1, ky, vy, l2), kx, vx, Node(Black, r, kv, vv, t))
    Node(_, l, ky, vy, r)
      -> Node(Black, Node(Red, l, ky, vy, r), kv, vv, t)
  }
}

fun balance2(t: tree, kv: int32, vv: bool, s: tree): tree {
  match(s) {
    Leaf -> Leaf
    Node(_, Node(Red, l, kx1, vx1, r1), ky, vy, r2)
      -> Node(Red, Node(Black, t, kv, vv, l), kx1, vx1, Node(Black, r1, ky, vy, r2))
    Node(_, l1, ky, vy, Node(Red, l2, kx2, vx2, r2))
      -> Node(Red, Node(Black, t, kv, vv, l1), ky, vy, Node(Black, l2, kx2, vx2, r2))
    Node(_, l, ky, vy, r)
      -> Node(Black, t, kv, vv, Node(Red, l, ky, vy, r))
  }
}

fun is-red(t : tree) : bool {
  match(t) {
    Node(Red) -> True
    _         -> False
  }
}

fun ins(t: tree, kx: int32, vx: bool): tree {
  match(t) {
    Leaf 
      -> Node(Red, Leaf, kx, vx, Leaf)
    Node(Red, a, ky, vy, b)
      -> if (kx < ky) then Node(Red, ins(a, kx, vx), ky, vy, b)
         elif (kx == ky) then Node(Red, a, kx, vx, b)
         else Node(Red, a, ky, vy, ins(b, kx, vx))
    Node(Black, a, ky, vy, b)
      -> if (kx < ky) then (if (is-red(a)) then balance1(ky, vy, b, ins(a, kx, vx))
                                           else Node(Black, ins(a, kx, vx), ky, vy, b))
         elif (kx < ky) then Node(Black, ins(a, kx, vx), ky, vy, b)
         elif (kx == ky) then Node(Black, a, kx, vx, b)
         elif (is-red(b)) then balance2(a, ky, vy, ins(b, kx, vx))
         else Node(Black, a, ky, vy, ins(b, kx, vx))
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

fun fold(t: tree, b: a, f: (int32, bool, a) -> a) : a {
  match (t) {
    Leaf                -> b
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
  }
}

fun make-tree-aux(n: int32, m: tree): div tree {
  if (n <= 0.int32) then m else {
    val n1 = n.dec
    make-tree-aux(n1, insert(m, n1, n1 % 10.int32 == 0.int32))
  }
}

fun make-tree(n: int32): div tree {
  make-tree-aux(n, Leaf)
}

fun main() {
  val m = make-tree(4200000.int32); // todo: read first command line arg for size
  val v = m.fold(0.int32) fun(k, v, r:int32) { if (v) then r.inc else r }
  v.show.println
}
