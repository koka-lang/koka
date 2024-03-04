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
module zip-bu

import std/num/int32
import std/num/random
import ziptree

type zipper
  NodeR( rank : rank, left : ztree, key : key, up : zipper )
  NodeL( rank : rank, up : zipper, key : key, right : ztree )
  Done

/*
pub fun access( t : ztree, k : key ) : ztree 
  find( t, rank-of(k), k, Done ) 

fun find( t : ztree, rank : rank, k : key, z : zipper ) : ztree 
  match t
    Node(rnk,l,x,r) | is-higher-rank( (rnk,x), (rank,k) )
      -> if (x < k) then find(r, rank, k, NodeR(rnk, l, x, z))
                    else find(l, rank, k, NodeL(rnk, z, x, r))
    Node(_,_,x,_) | x == k
      -> rebuild( z, t )
    _ -> match unzip(t,k,Done,Done)
           (s,b) -> rebuild( z, Node(rank,s,k,b) )
*/

pub fip(1) fun access( t : ztree, k : key ) : ztree 
  find( t, rank-of(k), k, ctx _) 

fip fun has-key( ^t : ztree, k : key ) : bool
  match t 
    Node(_,_,x,_) -> x == k
    _             -> False  

fip(1) fun find( t : ztree, rank : rank, k : key, acc : ctx<ztree> ) : ztree 
  match t
    Node(rnk,l,x,r) | is-higher-rank( (rnk,x), (rank,k) )
      -> if x < k then find(r, rank, k, acc ++ ctx Node(rnk, l, x, _))
                  else find(l, rank, k, acc ++ ctx Node(rnk, _, x, r))
    t -> if t.has-key(k) then acc ++. t
                         else match unzip(t,k,Done,Done)
                                (s,b) -> acc ++. Node(rank,s,k,b) 


fip fun unzip( t : ztree, k : key, zs : zipper, zb : zipper ) : (ztree,ztree)         
  match t
    Node(rnk,l,x,r) -> if x < k then unzip( r, k, NodeR(rnk,l,x,zs), zb)
                                else unzip( l, k, zs, NodeL(rnk,zb,x,r))
    Leaf -> (rebuild(zs,Leaf), rebuild(zb,Leaf))

fip fun rebuild( z : zipper, t : ztree ) : ztree
  match z 
    NodeR(rnk,l,x,up) -> rebuild(up, Node(rnk,l,x,t))
    NodeL(rnk,up,x,r) -> rebuild(up, Node(rnk,t,x,r))
    Done              -> t

pub fun main() : io ()
  benchmain(access)
