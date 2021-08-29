module splay-trees-fbip

import base

alias elem = int

type tree {
  Node(left : tree, here : elem, right : tree)
  Leaf
}

// We use the following pattern: Every function traverses the tree and creates a 'zipper'
// while going down the splay tree. If the tree is owned this won't allocate, since the
// zipper constructors have the same number of arguments as 'Node'.
// Then we use the 'splay' function to get a tree back where the chosen node is at the top.
type zipper {
  Root
  LeftChild  { parent : elem; up : zipper; right : tree }
  RightChild { parent : elem; up : zipper; left  : tree }
}

type container {
  Con(leaf : tree, here : elem, right : tree)
}

// Insert 'v' into the splay tree and return the position as a zipper
fun insert(v : elem, tree) {
  fun go2(tree, zipper, k) {
    match(tree) {
      Leaf { if(v == k) then splay_(zipper) else splay(Con(Leaf, v, Leaf), zipper) }
      Node(a, b, c) {
        if(v <= b) { go2(a, LeftChild(b, zipper, c), b) }
        else { go2(c, RightChild(b, zipper, a), k) }
      }
    }
  }
  fun go(tree, zipper) {
    match(tree) {
      Leaf { splay(Con(Leaf, v, Leaf), zipper) }
      Node(a, b, c) {
        if(v <= b) { go2(a, LeftChild(b, zipper, c), b) }
        else { go(c, RightChild(b, zipper, a)) }
      }
    }
  }
  go(tree, Root)
}

public fun member(v : elem, tree : tree) : (bool, tree) {
  fun go2(tree, zipper, k) {
    match(tree) {
      Leaf -> (v == k, splay(Con(Leaf, v, Leaf), zipper))
      Node(a, b, c) {
        if(v <= b) { go2(a, LeftChild(b, zipper, c), b) }
        else { go2(c, RightChild(b, zipper, a), k) }
      }
    }
  }
  fun go(tree, zipper) {
    match(tree) {
      Leaf -> (False, splay(Con(Leaf, v, Leaf), zipper))
      Node(a, b, c) {
        if(v <= b) { go2(a, LeftChild(b, zipper, c), b) }
        else { go(c, RightChild(b, zipper, a)) }
      }
    }
  }
  go(tree, Root)
}

// Splay the subtree to the root.
fun splay(c, zipper) {
  match(c) {
    Con(e, f, g) -> match(zipper) {
      Root                                -> Node(e, f, g)
      LeftChild(h, Root, i)               -> Node(e, f, Node(g, h, i))
      LeftChild(h, RightChild(d,z,c), i)  -> splay(Con(Node(c,d,e),f,Node(g,h,i)),z)
      LeftChild(h, LeftChild(j,z,k), i)   -> splay(Con(e,f,Node(g,h,Node(i,j,k))),z)
      RightChild(d, Root, c)              -> Node(Node(c, d, e), f, g)
      RightChild(d, RightChild(b,z,a), c) -> splay(Con(Node(Node(a,b,c),d,e),f,g),z)
      RightChild(d, LeftChild(h,z,i), c)  -> splay(Con(Node(c,d,e),f,Node(g,h,i)),z)
    }
  }
}

fun splay_(zipper) {
  match(zipper) {
    Root -> Leaf
    LeftChild(b, z, c) -> splay(Con(Leaf, b, c), z)
    RightChild(b, z, a) -> splay(Con(a, b, Leaf), z)
  }
}

// Benchmark

fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(x,t))
    }
  }
  go(xs, Leaf)
}

public fun count-splay-trees-fbip(start : int, end : int, xs : list<int>) {
  fun go(i, acc, t) {
    if(i <= end) {
      match(member(i, t)) {
        (True, t') -> go(i+1, acc+1, t')
        (False, t') -> go(i+1, acc, t')
      }
    } else acc
  }
  go(start, 0, from-list(xs))
}