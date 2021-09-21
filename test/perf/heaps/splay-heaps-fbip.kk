module splay-heaps-fbip

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

// Insert 'v' into the splay tree and return the position as a zipper
// This ensures stability (eg. if b is already in the set and v == b
// then we insert v behind b in the ordering). It slightly benefits
// ascending insertions.
fun insert(v : elem, tree) {
  fun go(tree, zipper) {
    match(tree) {
      Leaf { splay(Leaf, v, Leaf, zipper) }
      Node(a, b, c) {
        // if(v == b) { splay(Node(a, b, Leaf), v, c, zipper) }
        if(v < b) { go(a,  LeftChild(b, zipper, c)) }
        else { go(c, RightChild(b, zipper, a)) }
      }
    }
  }
  go(tree, Root)
}

// Splay the subtree to the root.
fun splay(e, f, g, zipper) {
  match(zipper) {
    Root                                -> Node(e, f, g)
    LeftChild(h, Root, i)               -> Node(e, f, Node(g, h, i))
    LeftChild(h, RightChild(d,z,c), i)  -> splay(Node(c,d,e),f,Node(g,h,i),z)
    LeftChild(h, LeftChild(j,z,k), i)   -> splay(e,f,Node(g,h,Node(i,j,k)),z)
    RightChild(d, Root, c)              -> Node(Node(c, d, e), f, g)
    RightChild(d, RightChild(b,z,a), c) -> splay(Node(Node(a,b,c),d,e),f,g,z)
    RightChild(d, LeftChild(h,z,i), c)  -> splay(Node(c,d,e),f,Node(g,h,i),z)
  }
}

fun splay_(zipper) {
  match(zipper) {
    Root -> Leaf
    LeftChild(b, z, c) -> splay(Leaf, b, c, z)
    RightChild(b, z, a) -> splay(a, b, Leaf, z)
  }
}

fun access-min(tree) {
  fun go(c, d, e) {
    match(c) {
      Leaf -> Node(c, d, e)
      Node(a, b, c) -> go(a, b, Node(c, d, e))
    }
  }
  match(tree) {
    Leaf -> tree
    // special case to avoid allocating a 'Node' when we are already at the minimum
    Node(Leaf, _, _) -> tree
    Node(a, b, c) -> go(a, b, c)
  }
}

fun find-min(t) {
  val t' = access-min(t)
  match(t') {
    Leaf -> raise("Empty")
    Node(Leaf, x, b) -> (x, t')
    _ -> raise("Can't happen")
  }
}

fun delete-min(t) {
  val t' = access-min(t)
  match(t') {
    Leaf -> raise("Empty")
    Node(Leaf, x, b) -> b
    _ -> raise("Can't happen")
  }
}

// Heapsort

fun from-list(xs, h) {
  match(xs) {
    Nil -> h
    Cons(x, xs) -> from-list(xs, insert(x, h))
  }
}

fun to-list(h) {
  match(h) {
    Leaf -> Nil
    _ {
      val (m, h') = find-min(h)
      Cons(m, to-list(delete-min(h')))
    }
  }
}

public fun heapsort-splay-heaps-fbip(xs : list<int>) : <exn, div> list<int> {
  to-list(from-list(xs, Leaf))
}