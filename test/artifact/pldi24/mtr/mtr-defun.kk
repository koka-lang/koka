module mtr-defun

import std/num/int32
import tree

type zipper
  Done              
  NodeL(up : zipper, key : key, right : tree )
  NodeR(left : tree, key : key, up : zipper )
  
fip(1) fun access( t : tree, k : key ) : tree
  mtr(t,k,Done)
  
fip(1) fun mtr( t : tree, k : key, z : zipper ) : tree
  match t
    Node(l,x,r) -> if   x < k then mtr( r, k, NodeR(l,x,z) )
                   elif x > k then mtr( l, k, NodeL(z,x,r) )
                   else moveup( z, Node(l,x,r) )
    Leaf        -> moveup( z, Node(Leaf,k,Leaf) )

fip fun moveup( z : zipper, t : tree ) : tree
  match z
    Done -> t
    NodeL(up,x,r) -> moveup( up, rotate-left(Node(t,x,r)) )
    NodeR(l,x,up) -> moveup( up, rotate-right(Node(l,x,t)) )

fip fun rotate-right( t : tree ) : tree
  match t
    Node(l,x,Node(rl,rx,rr)) -> Node(Node(l,x,rl),rx,rr)
    Node(l,x,r) -> Node(l,x,r)
    Leaf        -> Leaf

fip fun rotate-left( t : tree ) : tree
  match t
    Node(Node(ll,lx,lr),x,r) -> Node(ll,lx,Node(lr,x,r))
    _ -> t  // note: FIP warning but should be ok?

pub fun main()
  test5(access)   
  //test6(access)

