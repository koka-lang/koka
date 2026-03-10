module mtr-cps

import tree

pub fun access( t : tree, k : key ) : exn tree
  mtr(t,k,id)

fun mtr( t : tree, k : key, cont : tree -> exn tree ) : exn tree
  match t
    Node(l,x,r) -> if   x < k then mtr( r, k, fn(Node(s,f,b)) cont(Node(Node(l,x,s),f,b)) )
                   elif x > k then mtr( l, k, fn(Node(s,f,b)) cont(Node(s,f,Node(b,x,r))) )
                   else cont(Node(l,x,r))
    Leaf        -> cont(Node(Leaf,k,Leaf))


pub fun main()
  test5(access)   
  //test6(access)

