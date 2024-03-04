module rb-bu

import std/num/random
import std/num/int32
import rbtree

type zipper
  NodeR(color : color, lchild : rbtree, key : key, value : tval, zip : zipper)
  NodeL(color : color, zip : zipper, key : key, value : tval, rchild : rbtree)
  Done

fip fun is-red(^t : rbtree) : bool
  match t
    Node(Red) -> True
    _         -> False


fip fun set-black( t : rbtree ) : rbtree
  match t
    Node(_,l,x,v,r) -> Node(Black,l,x,v,r)
    Leaf            -> Leaf


pub fun main()
  benchmain(access)

pub fun access(t : rbtree, k : key) : rbtree
  find(t, k, from-key(k), Done) // .set-black


// // (improved) icfp'22 frame limited reuse paper
// fip fun rebuild(z : zipper, t : rbtree) : rbtree
  // match z
    // NodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    // NodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    // Done -> t

// fip fun balance( z : zipper, root : root ) : rbtree
  // match root
    // Root(_,l,k,v,r) -> match z      // root is always red
      // NodeR(Black, l1, k1, v1, z1) -> rebuild( z1, Node( Black, l1, k1, v1, Node(Red,l,k,v,r) ) )
      // NodeL(Black, z1, k1, v1, r1) -> rebuild( z1, Node( Black, Node(Red,l,k,v,r), k1, v1, r1 ) )
      // // fix red-red violations
      // NodeR(Red, l1, k1, v1, z1) -> match z1 // z1 is always black
        // NodeR(_,l2,k2,v2,z2) -> balance( z2, Root(Red, Node(Black,l2,k2,v2,l1), k1, v1, Node(Black,l,k,v,r)) )
        // NodeL(_,z2,k2,v2,r2) -> balance( z2, Root(Red, Node(Black,l1,k1,v1,l), k, v, Node(Black,r,k2,v2,r2)) )
        // Done -> Node(Black, l1, k1, v1, Node(Red,l,k,v,r))
      // NodeL(Red, z1, k1, v1, r1) -> match z1 // z1 is always black
        // NodeR(_,l2,k2,v2,z2) -> balance( z2, Root(Red, Node(Black,l2,k2,v2,l), k, v, Node(Black,r,k1,v1,r1)) )
        // NodeL(_,z2,k2,v2,r2) -> balance( z2, Root(Red, Node(Black,l,k,v,r), k1, v1, Node(Black,r1,k2,v2,r2)) )
        // Done -> Node(Black, Node(Red,l,k,v,r), k1, v1, r1)
      // Done -> Node(Black,l,k,v,r)

// fip(1) fun find(t : rbtree, k : key, v : tval, z : zipper) : rbtree
  // match t
    // Node(c, l, kx, vx, r)
      // -> if   kx < k then find(r, k, v, NodeR(c,l,kx,vx,z))
         // elif kx > k then find(l, k, v, NodeL(c,z,kx,vx,r))
         // else rebuild(z, Node(c,l,k,v,r))
    // Leaf -> balance(z, Root(Red,Leaf,k,v,Leaf))


// Cormen et al. textbook version
fip fun rebuild(z : zipper, t : rbtree)
  match z
    NodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    NodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    Done -> t

fip fun to-node( root : root ) : rbtree
  match root
    Root(c,l,k,v,r) -> Node(c,l,k,v,r)

fip fun balance( z : zipper, t : root ) : rbtree
  match z
    NodeR(Red, l1, k1, v1, z1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> // black
        if is-red(l2) then balance(z2, Root(Red, l2.set-black, k2, v2, Node(Black, l1, k1, v1, t.to-node) ))
        else rebuild(z2, Node(Black, Node(Red,l2,k2,v2,l1), k1, v1, t.to-node))
      NodeL(_,z2,k2,v2,r2) -> // black
        if is-red(r2) then balance(z2, Root(Red, Node(Black,l1,k1,v1,t.to-node), k2, v2, r2.set-black))
        else match t
          Root(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red,l1,k1,v1,l), k, v, Node(Red,r,k2,v2,r2)))
      Done -> Node(Black, l1, k1, v1, t.to-node)
    NodeL(Red, z1, k1, v1, r1) -> match z1
      NodeL(_,z2,k2,v2,r2) -> // black
        if is-red(r2) then balance(z2, Root(Red, Node(Black, t.to-node, k1, v1, r1), k2, v2, r2.set-black ))
        else rebuild(z2, Node(Black, t.to-node, k1, v1, Node(Red,r1,k2,v2,r2)))
      NodeR(_,l2,k2,v2,z2) -> // black
        if is-red(l2) then balance(z2, Root(Red, l2.set-black, k2, v2, Node(Black,t.to-node,k1,v1,r1) ))
        else match t
          Root(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red,l2,k2,v2,l), k, v, Node(Red,r,k1,v1,r1)))
      Done -> Node(Black, t.to-node, k1, v1, r1)
    z -> rebuild(z, t.to-node)

fip(1) fun find(t : rbtree, key : key, v : tval, z : zipper) : rbtree
  match t
    Node(c, l, kx, vx, r)
      -> if   key < kx then find(l, key, v, NodeL(c, z, kx, vx, r))
         elif key > kx then find(r, key, v, NodeR(c, l, kx, vx, z))
         else rebuild(z, Node(c, l, key, v, r))
    Leaf -> balance(z, Root(Red, Leaf, key, v, Leaf))


/*
// original icfp'22 frame-limited reuse paper
type balance-node
  Balance(color : color, lchild : rbtree, key : key, value : tval, rchild : rbtree)


fip fun rebuild(z : zipper, t : rbtree) : rbtree
  match z
    NodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    NodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    Done -> t

fip fun balance( z : zipper, l : rbtree, k : key, v : tval, r : rbtree, u : balance-node ) : rbtree
  match z
    NodeR(Red, l1, k1, v1, z1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> balance( z2, Node(Black,l2,k2,v2,l1), k1, v1, Node(Black,l,k,v,r), u )
      NodeL(_,z2,k2,v2,r2) -> balance( z2, Node(Black,l1,k1,v1,l), k, v, Node(Black,r,k2,v2,r2), u )
      Done -> Node(Black, l1, k1, v1, Node(Red,l,k,v,r))
    NodeL(Red, z1, k1, v1, r1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> balance( z2, Node(Black,l2,k2,v2,l), k, v, Node(Black,r,k1,v1,r1), u )
      NodeL(_,z2,k2,v2,r2) -> balance( z2, Node(Black,l,k,v,r), k1, v1, Node(Black,r1,k2,v2,r2), u )
      Done -> Node(Black, Node(Red,l,k,v,r), k1, v1, r1)
    NodeR(Black, l1, k1, v1, z1) -> rebuild( z1, Node( Black, l1, k1, v1, Node(Red,l,k,v,r) ) )
    NodeL(Black, z1, k1, v1, r1) -> rebuild( z1, Node( Black, Node(Red,l,k,v,r), k1, v1, r1 ) )
    Done -> Node(Black,l,k,v,r)

fip(1) fun find(t : rbtree, k : key, v : tval, z : zipper) : rbtree
  match t
    Node(c, l, kx, vx, r)
      -> if k < kx   then find(l, k, v, NodeL(c, z, kx, vx, r))
         elif k > kx then find(r, k, v, NodeR(c, l, kx, vx, z))
         else rebuild(z, Node(c, l, kx, vx, r))
    Leaf -> balance(z, Leaf, k, v, Leaf, Balance(Red,Leaf,0,0,Leaf))
*/
