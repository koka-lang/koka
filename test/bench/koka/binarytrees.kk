/*
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*/
public module binarytrees

import std/num/int32
import std/os/env
import std/os/task

type tree {
  Tip
  Node( left : tree, right : tree )
}

// make a perfectly balanced binary tree of `depth`
fun make( depth : int32 ) : div tree {
  if (depth > zero) 
    then Node( make(depth.dec), make(depth.dec) ) 
    else Node( Tip, Tip )
}

// FBIP in action: use a visitor to run the checksum tail-recursively
type visit {
  Done
  NodeR( right : tree, v : visit )
}

// tail-recursive checksum
fun checkv( t : tree, v : visit, acc : int32 ) : div int32 {  
  match(t) {
    Node(l,r) -> checkv( l, NodeR(r,v), acc.inc)
    Tip       -> match(v) {
      NodeR(r,v') -> checkv( r, v', acc)
      Done        -> acc
    }
  }
}

fun check( t : tree ) : div int32 {
  checkv(t, Done, zero)
  /*  
  // normal checksum  
  match(t) {
    Node(l,r) -> l.check + r.check.inc
    Tip       -> zero
  }
  */
}


// generate `count` trees of `depth` and return the total checksum
fun sum-countx( count : int32, depth : int32, total : int32 ) : div int32 {
  if (count <= zero) return total
  val csum = make(depth).check 
  sum-countx( count.dec, depth, total+csum )
}

fun sum-count( count : int32, depth : int32 ) : pure int32 {
  //sum-countx(count,depth,zero) 
  
  val n = 4.int32;
  if (count < n) return sum-countx(count,depth,zero) 
  val partc = count / n
  val part_rest = count - (partc * n)
  val parts = list(1,n.int).map fn(i){ 
    task{ sum-countx( partc + (if (i==1) then part_rest else zero), depth, zero ) }
  }
  val csum  = parts.map( await ).foldl1( fn(i,j){ i+j })
  csum  
}

// for depth to max-depth with stride 2, process 
// many trees of size depth in parallel and compute the total checksum
fun gen-depth( depth : int32, max-depth : int32, min-depth : int32 = depth ) : pure list<(int32,int32,promise<int32>)> {
  if (depth > max-depth) return [] 
  val count = shl( one, max-depth - depth + min-depth)
  val res   = (count, depth, task{ sum-count(count, depth) } )
  Cons( res, gen-depth( depth + 2.int32, max-depth, min-depth ) )
}

// show results
fun show( msg : string, depth : int32, check : int32 ) : console () {
  println(msg ++ " of depth " ++ depth.show ++ "\tcheck: " ++ check.show)
}

// main
public fun main() {
  val n = get-args().head.default("").parse-int.default(21)
  val min-depth = 4.int32
  val max-depth = max(min-depth + 2.int32, n.int32)
  // allocate and free the stretch tree
  val stretch-depth = max-depth.inc
  show( "stretch tree", stretch-depth, make(stretch-depth).check )
  // allocate long lived tree
  val long = make(max-depth)
  // allocate and free many trees in parallel
  val trees = gen-depth( min-depth, max-depth )
  trees.last.thd.await
  trees.foreach fn((count,depth,csum)) {
    show( count.show ++ "\t trees", depth, csum.await )
  }
  // and check if the long lived tree is still good
  show( "long lived tree", max-depth, long.check )
}
