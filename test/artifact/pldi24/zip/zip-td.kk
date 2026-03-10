// Tarjan, Levy, and Timmel's zip trees.
// a zip tree is an ordered binary search tree where ever node also has a _rank_
// the rank of a node is greater than the rank of the left child,
// and greater or equal to the right child.
//
// We derive the rank "pseudo randomly" from the key so we can combine search
// and insert. This determines the shape of the tree completely.
//
// When we "unzip" along a key k, we split the path in a tree with smaller
// element than k, and another tree with larger elements than k. (and then
// put node k on top)
module zip-td

import std/num/int32
import std/num/random
import ziptree



pub fip(1) fun access( t : ztree, k : key ) : ztree
  find( t, rank-of(k), k, ctx _ )

pub fun accessx( t : ztree, r : rank, k : key ) : ztree
  find( t, r, k, ctx _)

fip fun has-key( ^t : ztree, k : key ) : bool
  match t
    Node(_,_,x,_) -> x == k
    _             -> False

fip(1) fun find( t : ztree, rank : rank, k : key, acc : ctx<ztree> ) : ztree
  match t
    Node(rnk,l,x,r) | is-higher-rank( (rnk,x), (rank,k) )
      -> if x < k then find( r, rank, k, acc ++ ctx Node(rnk,l,x,_) )
                  else find( l, rank, k, acc ++ ctx Node(rnk,_,x,r) )
    t -> if t.has-key(k) then acc ++. t
                         else match unzip( t, k, ctx _, ctx _)
                                (s,b) -> acc ++. Node(rank,s,k,b)

fip fun unzip( t : ztree, k : key, accl : ctx<ztree>, accr : ctx<ztree> ) : (ztree,ztree)
  match t
    Node(rnk,l,x,r) -> if x < k then unzip( r, k, accl ++ ctx Node(rnk,l,x,_), accr )
                                else unzip( l, k, accl, accr ++ ctx Node(rnk,_,x,r) )
    Leaf -> (accl ++. Leaf, accr ++. Leaf)

// fip fun unzipx( t : ztree, k : key, accl : ctx<ztree>, accr : ctx<ztree> ) : (ztree,ztree)
//   match t
//     Node(rnk,l,x,r) -> if x < k then val (top,r') = collectr( r, k, accl ++ ctx Node(rnk,l,x,_) ) in unzipx( unsafe-decreasing(r'), k, top, accr )
//                                 else val (top,l') = collectl( l, k, accr ++ ctx Node(rnk,_,x,r) ) in unzipx( unsafe-decreasing(l'), k, accl, top )
//     Leaf -> (accl ++. Leaf, accr ++. Leaf)

fun collectr( t : ztree, k : key, top : ctx<ztree> ) : (ctx<ztree>,ztree)
  match t
    Node(rnk,l,x,r) | x < k -> collectr( r, k, top ++ ctx Node(rnk,l,x,_) )
    _ -> (top,t)

fun collectl( t : ztree, k : key, top : ctx<ztree> ) : (ctx<ztree>,ztree)
  match t
    Node(rnk,l,x,r) | x < k -> collectl( l, k, top ++ ctx Node(rnk,_,x,r) )
    _ -> (top,t)



pub fbip fun union( t1 : ztree, t2 : ztree ) : ztree
  match t1
    Leaf -> t2
    Node(rnk,l1,x,r1) ->
      val (top,l2,r2) = split( t2, rnk, x, ctx _ )
      top ++. Node(rnk, union(l1,l2), x, union(r1,r2))

fbip fun split( t : ztree, rank : rank, k : key, acc : ctx<ztree> ) : (ctx<ztree>,ztree,ztree)
  match t
    Node(rnk,l,x,r) | is-higher-rank( (rnk,x), (rank,k) )
          -> if x < k  then split(r, rank, k, acc ++ ctx Node(rnk,l,x,_))
                       else split(l, rank, k, acc ++ ctx Node(rnk,_,x,r))
    Node(_,l,x,r) | x == k -> (acc,l,r)
    t -> val (s,b) = unzip(t, k, ctx _, ctx _) in (acc,s,b)




pub fun main() : io ()
  benchmain(access)


// sequence of accesses
pub fun seqx( t : ztree, access : (ztree,rank,key) -> e ztree, accesses : list<(rank,key)> ) : e list<ztree>
  match accesses
    Nil -> []
    Cons((r,k),is) -> val t1 = t.access(r,k) in Cons(t1,seqx(t1,access,is))


// Show sequence of accesses
pub fun seqprintx( t0 : ztree, access : (ztree,rank,key) -> <io|e> ztree, accesses : list<(rank,key)> ) : <io|e> ()
  println("original:\n")
  t0.show.println
  zip(accesses,seqx(t0,access,accesses)).foreach fn(((r,k),t))
    //val (r,k) = (rk : (rank,key))
    println("\naccess " ++ k.show ++ "@" ++ show(r) ++ ":\n")
    t.print-tex //show.println
