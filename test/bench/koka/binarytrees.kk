/*
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*/
public module binarytrees

import std/os/env
import std/os/task
import std/num/int32

type tree
  Tip
  Node( left : tree, right : tree )


// make a perfectly balanced binary tree of `depth`
fun make( depth : int ) : div tree
  if depth > 0
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




fun check( t : tree ) : div int
  checkv(t, Done, zero).int
  /*
  // normal checksum  
  match t
    Node(l,r) -> l.check + r.check.inc
    Tip       -> 0
  */


// generate `count` trees of `depth` and return the total checksum
fun sum-count( count : int, depth : int ) : div int
  var csum := 0
  repeat(count)
    csum := csum + make(depth).check
  csum


// parallel sum count: spawn up to `n` sub-tasks to count checksums
fun psum-count( count : int, depth : int ) : pure int
  val n = 8
  val partc = count / n
  val rest  = count % n
  val parts = list(1,n) fn(i){ task{ sum-count( partc, depth ) }
  sum-count(rest, depth) + parts.await.sum


// for depth to max-depth with stride 2, process 
// many trees of size depth in parallel and compute the total checksum
fun gen-depth( min-depth : int, max-depth : int ) : pure list<(int,int,promise<int>)>
  list(min-depth, max-depth, 2) fn(d)
    val count = 2^(max-depth + min-depth - d)    // todo: ensure fast 2^n operation
    (count, d, task{ psum-count(count, d) })
    // (count, d, task{ sum-count(count, d) })   // one task per depth    


// show results
fun show( msg : string, depth : int, check : int ) : console ()
  println(msg ++ " of depth " ++ depth.show ++ "\tcheck: " ++ check.show)


// main
public fun main()
  val n = get-args().head.default("").parse-int.default(21)
  val min-depth = 4
  val max-depth = max(min-depth + 2, n)

  // allocate and free the stretch tree
  val stretch-depth = max-depth.inc
  show( "stretch tree", stretch-depth, make(stretch-depth).check )
  // allocate long lived tree
  val long = make(max-depth)

  // allocate and free many trees in parallel
  val trees = gen-depth( min-depth, max-depth )
  trees.foreach fn((count,depth,csum))
    show( count.show ++ "\t trees", depth, csum.await )

  // and check if the long lived tree is still good
  show( "long lived tree", max-depth, long.check )

