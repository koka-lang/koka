module zip-rec

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
  match t
    Node(rnk,l,x,r) -> 
      if (x < k) then match unzip(r,k)
                        (s,b) -> (Node(rnk,l,x,s),b)
      elif (x > k) then match unzip(l,k)
                          (s,b) -> (s,Node(rnk,b,x,r))
      else (l,r)
    Leaf -> (Leaf,Leaf)

pub fun main() : io ()
  benchmain(access)
