import std/time/timer

type tree  
  Node( left : tree, right : tree )
  Tip

// make a perfectly balanced binary tree of `depth`
fun make-rec( depth : int ) : div tree
  if depth > 0
    then Node( make(depth - 1), make(depth - 1) ) 
    else Node( Tip, Tip )

fun make( depth : int ) : div tree
  make-rec(depth)

noinline fun free( x : a ) : ()
  ()

fun test-tree( depth : int = 25 ) : <ndet,div,console> () 
  println("build tree of depth: " ++ depth.show)
  val t = make( depth )
  println("and freeing it...")
  print-elapsed({ free(t) }, "free tree")

fun test-list( len : int = 100000000 ) : <ndet,div,console> () 
  println("build list of length: " ++ len.show)
  val xs = list(1,len)
  println("and freeing it...")
  print-elapsed({ free(xs) }, "free list")


fun main()
  test-tree()
  test-list()
