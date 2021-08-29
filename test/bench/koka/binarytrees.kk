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

fun make( depth : int32 ) : div tree {
  if (depth > zero) 
    then Node( make(depth.dec), make(depth.dec) ) 
    else Node( Tip, Tip )
}

type visit {
  Done
  NodeR( right : tree, v : visit )
}

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
  match(t) {
    Node(l,r) -> l.check + r.check.inc
    Tip       -> zero
  }
  */
}

fun show( msg : string, depth : int32, check : int32 ) : console () {
  println(msg ++ " of depth " ++ depth.show ++ "\tcheck: " ++ check.show)
}

fun sum-count( count : int32, depth : int32, total : int32 ) : div int32 {
  if (count <= zero) return total
  val csum = make(depth).check 
  sum-count( count.dec, depth, total+csum )
}

val min-depth = 4.int32

fun gen-depth( depth : int32, depth-max : int32 ) : pure list<(int32,int32,promise<int32>)> {
  if (depth > depth-max) return [] 
  val count = shl( one, depth-max - depth + min-depth)
  val res   = (count, depth, task { sum-count(count, depth, zero) } )
  Cons( res, gen-depth( depth + 2.int32, depth-max ) )
}

public fun main() {
  val n = get-args().head.default("").parse-int.default(21)
  val max-depth     = max(min-depth + 2.int32, n.int32)
  val stretch-depth = max-depth.inc
  show( "stretch tree", stretch-depth, make(stretch-depth).check )
  val trees = gen-depth( min-depth, max-depth )
  trees.foreach fn((count,depth,csum)) {
    show( count.show ++ "\t trees", depth, csum.await )
  }
  val long = make(max-depth)
  show( "long lived tree", max-depth, long.check )
}
