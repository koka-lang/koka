/*
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*/
module binarytrees

import std/os/env
import std/os/task
import std/num/int32

type tree
  Tip
  Node( left : tree, right : tree )


// make a perfectly balanced binary tree of `depth`
fun make( depth : int32 ) : div tree
  if depth > 0.int32
    then Node( make(depth.dec), make(depth.dec) ) 
    else Node( Tip, Tip )


// FBIP in action: use a visitor to run the checksum tail-recursively
type visit
  Done
  NodeR( right : tree, v : visit )


// tail-recursive checksum
fun checkv( t : tree, v : visit, acc : int32 ) : div int32
  match t
    Node(l,r) -> checkv( l, NodeR(r,v), acc.inc)
    Tip       -> match v
      NodeR(r,v') -> checkv( r, v', acc)
      Done        -> acc


// normal checksum  
fun checkr( t : tree ) : div int32 
  match t
    Node(l,r) -> l.checkr + r.checkr.inc
    Tip       -> zero
  

fun check( t : tree ) : div int32
  checkv(t, Done, zero)
  //t.checkr
  


// generate `count` trees of `depth` and return the total checksum
fun sum-count( count : int32, depth : int32 ) : div int32
  fold-int32(0.int32, count, 0.int32) fn(i,csum)
    csum + make(depth).check
  

// parallel sum count: spawn up to `n` sub-tasks to count checksums
fun psum-count( count : int32, depth : int32 ) : pure int
  val n = 2
  val partc = count / n.int32
  val rest  = count % n.int32
  val parts = list(1,n) fn(i){ task{ sum-count( partc, depth ) }
  (if rest > 0.int32 then sum-count(rest, depth).int else 0) + parts.await.map(int).sum


// for depth to max-depth with stride 2, process 
// many trees of size depth in parallel and compute the total checksum
fun gen-depth( min-depth : int, max-depth : int ) : pure list<(int,int,promise<int>)>
  list(min-depth, max-depth, 2) fn(d)
    val count = shl( 1.int32, (max-depth + min-depth - d).int32)
    (count.int, d, task{ psum-count(count, d.int32) })
    //(count.int, d, task{ sum-count(count, d.int32).int })   // one task per depth    


// show results
fun show( msg : string, depth : int, check : int ) : console ()
  println(msg ++ " of depth " ++ depth.show ++ "\tcheck: " ++ check.show)


// main
pub fun main()
  val n = get-args().head.default("").parse-int.default(21)
  val min-depth = 4
  val max-depth = max(min-depth + 2, n)

  // allocate and free the stretch tree
  val stretch-depth = max-depth.inc
  show( "stretch tree", stretch-depth, make(stretch-depth.int32).check.int )
  // allocate long lived tree
  val long = make(max-depth.int32)

  // allocate and free many trees in parallel
  val trees = gen-depth( min-depth, max-depth )
  trees.foreach fn((count,depth,csum))
    show( count.show ++ "\t trees", depth, csum.await )

  // and check if the long lived tree is still good
  show( "long lived tree", max-depth, long.check.int )

