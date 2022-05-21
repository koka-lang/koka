/*
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*/
module binarytrees

import std/os/env
import std/os/task
import std/num/int32

type tree  
  Node( left : tree, right : tree )
  Tip


// make a perfectly balanced binary tree of `depth` using FBIP
// to use no extra stack space.
type builder  
  Top
  BuildRight( depth : int, up : builder )
  BuildNode( left : tree, up : builder )

// using mutual recursion
fun make-down( depth : int, builder : builder ) : div tree
  if depth > 0 
    then make-down( depth - 1, BuildRight(depth - 1, builder))
    else make-up( Node(Tip,Tip), builder)

fun make-up( t : tree, builder : builder ) : div tree
  match builder
    BuildRight(depth, up) -> make-down( depth, BuildNode(t, up))
    BuildNode(l, up)      -> make-up( Node(l, t), up)
    Top -> t

// using a single tail recursive definition
type direction  
  Down( depth : int )
  Up( t : tree )

fun make-fbip( dir : direction, builder : builder) : div tree
  match dir
    Down(depth) -> if depth > 0 
      then make-fbip(Down(depth - 1), BuildRight(depth - 1, builder))
      else make-fbip(Up(Node(Tip,Tip)), builder)
    Up(t) -> match builder
      BuildRight(depth, up) -> make-fbip(Down(depth), BuildNode(t, up))
      BuildNode(l, up)      -> make-fbip(Up(Node(l, t)), up)
      Top -> t

// make a perfectly balanced binary tree of `depth`
fun make-rec( depth : int ) : div tree
  if depth > 0
    then Node( make-rec(depth - 1), make-rec(depth - 1) ) 
    else Node( Tip, Tip )

type build  
  Root
  GoRight( depth : int, up : build )

fun make-trmc-up( t : tree, b : build ) : div tree
  match b
    GoRight(depth, Root) ->
      Node(t, make-trmc( depth, Root ))
    GoRight(depth, up) ->
      make-trmc-up( Node(t, make-trmc( depth, Root )), up )
    Root -> t

fun make-trmc( depth : int, b : build ) : div tree
  if depth > 0
    then make-trmc( depth - 1, GoRight(depth - 1, b))
    else make-trmc-up( Node(Tip,Tip), b)

fun make-trmc-fbip( dir : direction, b : build ) : div tree
  match dir
    Down(depth) ->
      if depth > 0
        then make-trmc-fbip( Down(depth - 1), GoRight(depth - 1, b))
        else make-trmc-fbip( Up(Node(Tip,Tip)), b)
    Up(t) -> 
      match b
        GoRight(depth, Root) ->
          Node(t, make-trmc-fbip( Down(depth), Root ))
        GoRight(depth, up) ->
          make-trmc-fbip(Up(Node(t, make-trmc-fbip( Down(depth), Root ))), up )
        Root -> t

fun make( depth : int ) : div tree
  make-rec(depth)
  // make-trmc( depth, Root )
  // make-trmc-fbip( Down( depth), Root )
  // make-fbip(Down(depth), Top)
  // make-down( depth, Top )



// FBIP in action: use a visitor to run the checksum tail-recursively
type visit  
  Done
  NodeR( right : tree, v : visit )

// tail-recursive checksum
fun checkv( t : tree, v : visit, acc : int ) : div int
  match t
    Node(l,r) -> checkv( l, NodeR(r,v), acc.inc)
    Tip       -> match v
      NodeR(r,v') -> checkv( r, v', acc)
      Done        -> acc

// normal checksum  
fun checkr( t : tree ) : div int
  match t
    Node(l,r) -> l.checkr + r.checkr + 1
    Tip       -> 0


fun check( t : tree ) : div int
  checkv(t, Done, 0)
  //t.checkr
  


// generate `count` trees of `depth` and return the total checksum
fun sum-count( count : int, depth : int ) : div int
  fold-int(count+1,0) fn(i,csum)
    // csum + make(depth).check
    csum + make(depth).check
  

// parallel sum count: spawn up to `n` sub-tasks to count checksums
fun psum-count( count : int, depth : int ) : pure int
  val n = 8
  val partc = count / n
  val rest  = count % n
  val parts = list(1,n, fn(i) task{ sum-count( partc, depth ) })
  sum-count(rest, depth) + parts.await.sum


// for depth to max-depth with stride 2, process 
// many trees of size depth in parallel and compute the total checksum
fun gen-depth( min-depth : int, max-depth : int ) : pure list<(int,int,promise<int>)>
  list(min-depth, max-depth, 2) fn(d)
    val count = 2^(max-depth + min-depth - d)    // todo: ensure fast 2^n operation
    (count, d, task{ psum-count(count, d) })
    //(count, d, task{ sum-count(count, d) } )   // one task per depth    
   

// show results
fun show( msg : string, depth : int, check : int ) : console ()
  println(msg ++ " of depth " ++ depth.show ++ "\t check: " ++ check.show)


// main
pub fun main()
  // task-set-default-concurrency(8);
  val n = get-args().head.default("").parse-int.default(21)
  val min-depth = 4
  val max-depth = max(min-depth + 2, n)

  // allocate and free the stretch tree
  val stretch-depth = max-depth.inc
  show( "stretch tree", stretch-depth, make(stretch-depth).check )
  
  // allocate long lived tree
  // val long = make(max-depth)
  val long = make(max-depth)

  // allocate and free many trees in parallel
  val trees = gen-depth( min-depth, max-depth )
  trees.foreach fn((count,depth,csum))
    show( count.show ++ "\t trees", depth, csum.await )

  // and check if the long lived tree is still good
  show( "long lived tree", max-depth, long.check )

