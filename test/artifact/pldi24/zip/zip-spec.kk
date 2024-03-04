// Tarjan, Levy, and Timmel's zip trees.
// a zip tree is an ordered binary search tree where ever node also has a _rank_
// the rank of a node is greater than the rank of the left child,
// and greater or equal to the right child. 
// It is max heap-ordered with respect to ranks with ties broken in favor of smaller keys.
// 
// We derive the rank "pseudo randomly" from the key so we can combine search 
// and insert. This determines the shape of the tree completely.
//
// When we "unzip" along a key k, we split the path in a tree with smaller
// element than k, and another tree with larger elements than k. (and then
// put node k on top)
module zip-spec

import std/num/int32
import std/num/random
import ziptree

pub fun access( t : ztree, k : key ) : ztree 
  find( t, rank-of(k), k ) 

fun find( t : ztree, rank : rank, k : key ) : ztree 
  match t
    Node(rnk,l,x,r) | is-higher-rank( (rnk,x), (rank,k) )
      -> if (x < k) then Node(rnk, l, x, find(r,rank,k) )
                    else Node(rnk, find(l,rank,k ), x, r)
    _ -> match unzip(t,k)
           (s,b) -> Node(rank,s,k,b)

fun unzip( t : ztree, k : key ) : (ztree,ztree)         
  (smaller(t,k), bigger(t,k))

fun smaller( t : ztree, k : key ) : ztree
  match t
    Node(rnk,l,x,r) -> 
      if (x < k) then Node(rnk,l,x,smaller(r,k))
      elif (x > k) then smaller(l,k)
      else l
    Leaf -> Leaf 

fun bigger( t : ztree, k : key ) : ztree
  match t
    Node(rnk,l,x,r) -> 
      if (x < k) then bigger(r,k)
      elif (x > k) then Node(rnk,bigger(l,k),x,r)
      else r
    Leaf -> Leaf 

pub fun main() : io ()
  benchmain(access)
