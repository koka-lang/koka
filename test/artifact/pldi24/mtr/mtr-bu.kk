module mtr-bu

import std/num/int32
import tree

pub fip(1) fun access( t : tree, k : key ) : tree
  insert(t,k)

pub fip(1) fun insert( t : tree, k : key ) : tree
  insertz(t,k,Done)

type zipper
  NodeR( left : tree, key : key, up : zipper )
  NodeL( up : zipper, key : key, right : tree )
  Done

fip(1) fun insertz( t : tree, k : key, z : zipper ) : tree
  match t
    Node(l,x,r) -> if   x < k then insertz( r, k, NodeR(l,x,z) )
                   elif x > k then insertz( l, k, NodeL(z,x,r) )
                   else mtr( Root(l,x,r), z )
    Leaf        -> mtr( Root(Leaf,k,Leaf), z )  // insert if not found


fip fun mtr( root : root, z : zipper ) : tree
  match root
    Root(l,x,r) -> match z
      NodeR(zl,zx,up) -> mtr( Root(Node(zl,zx,l),x,r), up )
      NodeL(up,zx,zr) -> mtr( Root(l,x,Node(r,zx,zr)), up )
      Done            -> Node(l,x,r)

pub fun test()
  test5(access)   // does not follow Fig 6
  //test6(access)

pub fun main()
  benchmain(access)


