type color {
  Red
  Black
}

type tree {
  Leaf
  Node(color: color, lchild: tree, key: int, value: bool, rchild: tree)
}

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
