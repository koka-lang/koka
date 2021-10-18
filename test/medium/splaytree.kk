module splaytree

public type splaytree<a> {
  Node { left : splaytree<a>; here : a; right : splaytree<a> }
  Leaf
}

public val empty = Leaf

public fun singleton(a) {
  Node(Leaf, a, Leaf)
}

// Insert the elements in the order of the list.
// If the elements are ascending or descending this is linear,
// but in the worst-case this can be quadratic.
public fun from-list(list : list<int>) : splaytree<int> {
  fun go(xs, tree) {
    match(xs) {
      Nil { tree }
      Cons(x, xx) { go(xx, insert(x, tree)) }
    }
  }
  go(list, empty)
}

public fun insert(v : int, tree : splaytree<int>) : splaytree<int> {
  match(insert_(v, tree)) {
    (t, zipper) {
      unsafe-noexn(fn() {
        splay(t, zipper)
      })
    }
  }
}

public fun member(v : int, tree : splaytree<int>) : maybe<splaytree<int>> {
  match(member_(v, tree)) {
    Nothing { Nothing }
    Just((t, zipper)) {
      unsafe-noexn(fn() {
        Just(splay(t, zipper))
      })
    }
  }
}

// Internals.
// We use the following pattern: Every function traverses the tree and creates a 'zipper'
// while going down the splay tree. If the tree is owned this won't allocate, since the
// zipper constructors have the same number of arguments as 'Node'.
// Then we use the 'splay' function to get a splaytree back where the chosen node is at the top.

type splay-zipper<a> {
  Root
  LeftChild  { parent : a; up : splay-zipper<a>; right : splaytree<a> }
  RightChild { parent : a; up : splay-zipper<a>; left  : splaytree<a> }
}

// Find 'v' in a splay tree and return the position as a zipper
fun member_(v : int, t : splaytree<int>) {
  fun go(tree, zipper) {
    match(tree) {
      Leaf { Nothing }
      Node(a, b, c) {
        if(v == b) { Just((tree, zipper)) }
        else if(v <  b) { go(a,  LeftChild(b, zipper, c)) }
        else { go(c, RightChild(b, zipper, a)) }
      }
    }
  }
  go(t, Root)
}

// Insert 'v' into a splay tree and return the position as a zipper
// If 'v' already exists we return its position.
fun insert_(v : int, t : splaytree<int>) {
  fun go(tree, zipper) {
    match(tree) {
      Leaf { (Node(Leaf, v, Leaf), zipper) }
      Node(a, b, c) {
        if(v == b) { (tree, zipper) }
        else if(v < b) { go(a,  LeftChild(b, zipper, c)) }
        else { go(c, RightChild(b, zipper, a)) }
      }
    }
  }
  go(t, Root)
}

// Splay the subtree to the root.
fun splay(tree : splaytree<a>, zpr : splay-zipper<a>) : exn splaytree<a> {
  fun go(e, f, g, zipper) {
    match(zipper) {
      Root { tree }
      LeftChild(h, Root, i)               { Node(e, f, Node(g, h, i)) }
      LeftChild(h, RightChild(d,z,c), i)  { go(Node(c,d,e),f,Node(g,h,i),z) }
      LeftChild(h, LeftChild(j,z,k), i)   { go(e,f,Node(g,h,Node(i,j,k)),z) }
      RightChild(d, Root, c)              { Node(Node(c, d, e), f, g) }
      RightChild(d, RightChild(b,z,a), c) { go(Node(Node(a,b,c),d,e),f,g,z) }
      RightChild(d, LeftChild(h,z,i), c)  { go(Node(c,d,e),f,Node(g,h,i),z) }
    }
  }
  match(tree) {
    Leaf { throw-exn(Exception("Leaf in splay!", ExnAssert)) }
    Node(e, f, g) { go(e, f, g, zpr) }
  }
}