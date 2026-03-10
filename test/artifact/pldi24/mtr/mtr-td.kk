module mtr-td

import std/num/int32
import tree

pub fip(1) fun access( t : tree, k : key ) : tree
  insert(t,k)

pub fip(1) fun insert( t : tree, k : key ) : tree
  mtr(t,k,ctx _, ctx _)

// with index substitution:
// fip(1) fun mtr( t : tree, k : key, accl : ctx<tree>, accr : ctx<tree> ) : tree
//   match t
//     Node(l,x,r) -> if   x < k then mtr( r, k, accl[ctx Node(l,x,_)], accr )
//                    elif x > k then mtr( l, k, accl, accr[ctx Node(_,x,r)] )
//                    else Node( accl[l], x, accr[r] )
//     Leaf        -> Node( accl[Leaf], k, accr[Leaf] )

// with operators:
fip(1) fun mtr( t : tree, k : key, accl : ctx<tree>, accr : ctx<tree> ) : tree
  match t
    Node(l,x,r) -> if   x < k then mtr( r, k, accl ++ ctx Node(l,x,_), accr )
                   elif x > k then mtr( l, k, accl, accr ++ ctx Node(_,x,r) )
                   else Node( accl ++. l, x, accr ++. r )
    Leaf        -> Node( accl ++. Leaf, k, accr ++. Leaf )



pub fun test()
  test5(access)
  //test6(access)

pub fun main()
  benchmain(access)



