module mtr-rec

import std/num/int32
import tree

pub fbip(1) fun access( t : tree, k : key ) : tree  
  insert(t,k)
  
pub fbip(1) fun insert( t : tree, k : key ) : tree  
  val Root(l,x,r) = mtr(t,k)
  Node(l,x,r)

fbip(1) fun mtr( t : tree, k : key ) : root
  match t
    Node(l,x,r) -> if   x < k then match mtr(r,k)
                                     Root(s,f,b) -> Root( Node(l,x,s), f, b)
                   elif x > k then match mtr(l,k)
                                     Root(s,f,b) -> Root( s, f, Node(b,x,r))
                   else Root(l,x,r)
    Leaf        -> Root(Leaf,k,Leaf)

// pub fbip(1) fun insert( t : tree, k : key ) : tree  
  // val (l,x,r) = mtr(t,k)
  // Node(l,x,r)

// fbip fun mtr( t : tree, k : key ) : (tree, key, tree)
  // match t
    // Node(l,x,r) -> if   x < k then match mtr(r,k)
                                     // (s,f,b) -> (Node(l,x,s), f, b)
                   // elif x > k then match mtr(l,k)
                                     // (s,f,b) -> (s, f, Node(b,x,r))
                   // else (l,x,r)
    // Leaf        -> (Leaf,k,Leaf)

pub fun test()  
  test5(access)
  //test6(access)

pub fun main()
  benchmain(access)
