module mtr-spec

import std/num/int32
import tree

pub fun access( t : tree, k : key ) : tree  
  insert(t,k)
  
pub fun insert( t : tree, k : key ) : tree 
  Node( smaller(t,k), find(t,k), bigger(t,k) )

pub fun inlaccess( t : tree, k : key ) : tree 
  match t
    Node(l,x,r) -> if   x < k then Node( Node(l,x,smaller(r,k)), find(r,k), bigger(r,k))
                   elif x > k then Node( smaller(l,k), find(l,k), Node(bigger(l,k),x,r))
                   else Node(l,x,r)
    Leaf        -> Node(Leaf,k,Leaf)
  
fun find( t : tree, k : key ) : key
  match t 
    Node(l,x,r) ->
      if   x < k then find(l,k)
      elif x > k then find(r,k)
      else x
    Leaf -> k                    // if not found, insert it

fun smaller( t : tree, k : key ) : tree
  match t
    Node(l,x,r) -> 
      if   x < k then Node(l,x,smaller(r,k))
      elif x > k then smaller(l,k)
      else l
    Leaf -> Leaf

fun bigger( t : tree, k : key ) : tree
  match t
    Node(l,x,r) -> 
      if   x < k then bigger(r,k)
      elif x > k then Node(bigger(l,k),x,r)
      else r
    Leaf -> Leaf


pub fun main()
  test5(access)   
  //test6(access)

