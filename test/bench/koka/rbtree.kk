// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean

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


fun balance1(k: int, v: bool, t: tree, s: tree): tree {
  match(s) {
    Leaf -> Leaf
    Node(_, Node(Red, l, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, kx, vx, rx), ky, vy, Node(Black, ry, k, v, t))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, t))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, t)
  }
}

fun balance2(t: tree, k: int, v: bool, s: tree): tree {
  match(s) {
    Leaf -> Leaf
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, t, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, t, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, t, k, v, Node(Red, lx, kx, vx, rx))
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
      -> if (k < kx) then (if (is-red(l)) then balance1(kx, vx, r, ins(l, k, v))
                                          else Node(Black, ins(l, k, v), kx, vx, r))
         elif (k == kx) then Node(Black, l, k, v, r)
         elif (is-red(r)) then balance2(l, kx, vx, ins(r, k, v))
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
    then set-black(ins(t, k, v))
    else ins(t, k, v)
}

fun fold(t: tree, b: a, f: (int, bool, a) -> a): a {
  match (t) {
    Leaf                -> b
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
  }
}

fun make-tree-aux(n: int, m: tree): div tree {
  if (n <= 0) then m else {
    val n1 = n - 1
    make-tree-aux(n1, insert(m, n1, n1 % 10 == 0))
  }
}

fun make-tree(n: int): div tree {
  make-tree-aux(n, Leaf)
}

fun main() {
  val m = make-tree(4200000); // todo: read first command line arg for size
  val v = m.fold(0) fun(k,v,r){ if (v) then r + 1 else r }
  v.println
}
