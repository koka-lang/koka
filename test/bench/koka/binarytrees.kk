/*
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*/
public module binarytrees

import std/os/env
import std/os/task

type tree {
  Tip
  Node( left : tree, right : tree )
}

// make a perfectly balanced binary tree of `depth`
fun make( depth : int ) : div tree {
  if (depth > 0) 
    then Node( make(depth.dec), make(depth.dec) ) 
    else Node( Tip, Tip )
}

// FBIP in action: use a visitor to run the checksum tail-recursively
type visit {
  Done
  NodeR( right : tree, v : visit )
}

// tail-recursive checksum
fun checkv( t : tree, v : visit, acc : int ) : div int {  
  match(t) {
    Node(l,r) -> checkv( l, NodeR(r,v), acc.inc)
    Tip       -> match(v) {
      NodeR(r,v') -> checkv( r, v', acc)
      Done        -> acc
    }
  }
}

fun check( t : tree ) : div int {
  checkv(t, Done, 0)
  /*
  // normal checksum  
  match(t) {
    Node(l,r) -> l.check + r.check.inc
    Tip       -> 0
  }
  */
}


// generate `count` trees of `depth` and return the total checksum
fun sum-countx( count : int, depth : int ) : div int {
  var csum := 0
  repeat(count) {
    csum := csum + make(depth).check
  }
  csum
}

fun sum-count( count : int, depth : int ) : pure int {
  //sum-countx(count,depth,0)   
  val n = 4;
  if (count < n) return sum-countx(count,depth) 
  val partc = count / n
  val part_rest = count - (partc * n)
  val parts = list(1,n).map fn(i){ 
    task{ sum-countx( partc + (if (i==1) then part_rest else 0), depth ) }
  }
  val csum  = parts.map( await ).foldl1( fn(i,j){ i+j })
  csum  
  
}

// for depth to max-depth with stride 2, process 
// many trees of size depth in parallel and compute the total checksum
fun gen-depth( depth : int, max-depth : int, min-depth : int = depth ) : pure list<(int,int,promise<int>)> {
  list(min-depth, max-depth, 2) fn(d) {    
    val count = 2^(max-depth + min-depth - d)
    (count, d, task{ sum-count(count, d) })
  }
}

// show results
fun show( msg : string, depth : int, check : int ) : console () {
  println(msg ++ " of depth " ++ depth.show ++ "\tcheck: " ++ check.show)
}

// main
public fun main() {
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
  trees.foreach fn((count,depth,csum)) {
    show( count.show ++ "\t trees", depth, csum.await )
  }
  // and check if the long lived tree is still good
  show( "long lived tree", max-depth, long.check )
}
