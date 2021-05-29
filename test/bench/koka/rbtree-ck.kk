// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap-checkpoint.lean
import std/num/int32

type color {
  Red
  Black
}

type tree {
  Leaf()
  Node(color: color, lchild: tree, key: int32, value: bool, rchild: tree)
}

fun is-red( ^t : tree) : bool {
  match(t) {
    Node(Red) -> True
    _         -> False
  }
}

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

fun lookup(^ t: tree, k: int32): maybe<bool> {
  match(t) {
    Leaf -> Nothing
    Node(_, l, kx, vx, r)
      -> if(k < kx) then l.lookup(k)
         elif (k == kx) then Just(vx)
         else r.lookup(k)
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

fun make-tree-aux(freq : int32, n: int32, t: tree, acc : list<tree> ): div list<tree> {
  if (n <= 0.int32) then Cons(t,acc) else {
    val t' = insert(t, n, (n % 10.int32) == 0.int32)
    make-tree-aux(freq, n.dec, t', if ((n%freq)==0.int32) then Cons(t',acc) else acc)
  }
}

fun make-tree(freq: int32, n: int32): div list<tree> {
  make-tree-aux(freq, n, Leaf, [])
}

fun lookup-tree-aux( ^t: tree, n: int32, r: bool): div bool {
  if(n <= 0.int32) then r else {
    val n1 = n.dec
    val l = match(t.lookup(n1)) {
      Just(b) -> b
      Nothing -> False
    }
    lookup-tree-aux(t, n1, (not(r) && l) || (r && not(l)))
  }
}

fun lookup-tree( ^t: tree, n: int32): div bool {
  lookup-tree-aux(t, n, False)
}

fun main() {
  val n = 4200000.int32 // todo: read first command line arg for size
  val m = make-tree(5.int32, n)
  val l = lookup-tree(m.head(Leaf), n)
  val v = m.head(Leaf).fold(0.int32) fn(k, v, r:int32) { if (v) then r.inc else r }
  l.show.println
  v.show.println
}
