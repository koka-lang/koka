// Splay tree bottom-up as by Sleator and Tarjan.
module splay-bu

import std/num/int32
import tree

/*
pub type tree 
  Leaf
  Node(left : tree, key : key, right : tree)

pub type root
  Root(left : tree, key : key, right : tree)  
*/

type zipper
  NodeR( left : tree, key : key, up : zipper )
  NodeL( up : zipper, key : key, right : tree )
  Done

pub fun access( t : tree, k : key ) : tree  
  insertx(t,k)


pub fun insertx( t : tree, k : key ) : tree  
  insertzx(t,k,Done)

fbip(1) fun insertzx( t : tree, k : key, z : zipper ) : tree
  match t 
    Leaf -> splayx(z,Leaf,k,Leaf)
    Node(l,x,r) -> 
      if   x < k then insertzx( r, k, NodeR(l,x,z) )
      elif x > k then insertzx( l, k, NodeL(z,x,r) )
      else splayx(z,l,x,r)

fip(1) fun splayx( z : zipper, tl : tree, tx : key, tr : tree ) : tree
  match z 
    Done -> Node(tl,tx,tr)
    NodeR(rl,rx,zz) -> match zz
      Done -> Node(Node(rl,rx,tl),tx,tr)
      NodeR(l,x,up) -> splayx( up, Node(Node(l,x,rl),rx,tl), tx, tr ) // RR
      NodeL(up,x,r) -> splayx( up, Node(rl,rx,tl), tx, Node(tr,x,r) ) // LR
    NodeL(zz,lx,lr) -> match zz
      Done -> Node(tl,tx,Node(tr,lx,lr))
      NodeR(l,x,up) -> splayx( up, Node(l,x,tl), tx, Node(tr,lx,lr) ) // RL
      NodeL(up,x,r) -> splayx( up, tl, tx, Node(tr,lx,Node(lr,x,r)) ) // LL


pub fip(1) fun insert( t : tree, k : key ) : tree  
  insertz(t,k,Done)

fip(1) fun insertz( t : tree, k : key, z : zipper ) : tree
  match t 
    Leaf -> splay(z, Root(Leaf,k,Leaf))
    Node(l,x,r) -> 
      if   x < k then insertz( r, k, NodeR(l,x,z) )
      elif x > k then insertz( l, k, NodeL(z,x,r) )
      else splay(z, Root(l,x,r))

fip fun splay( z : zipper, root : root ) : tree
  match root 
    Root(tl,tx,tr) -> match z 
      Done -> Node(tl,tx,tr)
      NodeR(rl,rx,zz) -> match zz
        Done -> Node(Node(rl,rx,tl),tx,tr)
        NodeR(l,x,up) -> splay( up, Root(Node(Node(l,x,rl),rx,tl),tx,tr) ) // RR
        NodeL(up,x,r) -> splay( up, Root(Node(rl,rx,tl),tx,Node(tr,x,r)) ) // LR
      NodeL(zz,lx,lr) -> match zz
        Done -> Node(tl,tx,Node(tr,lx,lr))
        NodeR(l,x,up) -> splay( up, Root(Node(l,x,tl),tx,Node(tr,lx,lr)) ) // RL
        NodeL(up,x,r) -> splay( up, Root(tl,tx,Node(tr,lx,Node(lr,x,r))) ) // LL

/*
fun splay2( z : zipper, root : root ) : tree
  match root 
    Root(tl,tx,tr) -> match z 
      NodeR(rl,rx,NodeR(l,x,up)) -> splay2( up, Root(Node(Node(l,x,rl),rx,tl),tx,tr) ) // RR
      NodeL(NodeR(l,x,up),lx,lr) -> splay2( up, Root(Node(l,x,tl),tx,Node(tr,lx,lr)) ) // RL
      NodeR(rl,rx,NodeL(up,x,r)) -> splay2( up, Root(Node(rl,rx,tl),tx,Node(tr,x,r)) ) // LR
      NodeL(NodeL(up,x,r),lx,lr) -> splay2( up, Root(tl,tx,Node(tr,lx,Node(lr,x,r))) ) // LL
      NodeR(rl,rx,Done) -> Node(Node(rl,rx,tl),tx,tr)
      NodeL(Done,lx,lr) -> Node(tl,tx,Node(tr,lx,lr))
      Done -> Node(tl,tx,tr)
*/    

/*
fun splayx( z : zipper, root : root ) : tree
  match root 
    Root(l,x,r) -> match z 
      Done -> Node(l,x,r)
      NodeR(zl,zx,zz) -> match zz
        Done -> Node(Node(zl,zx,l),x,r)
        NodeR(zzl,zzx,zzz) -> splayx( zzz, Root(Node(Node(zzl,zzx,zl),zx,l),x,r) )
        NodeL(zzz,zzx,zzr) -> splayx( zzz, Root(Node(zl,zx,l),x,Node(r,zzx,zzr)) )
      NodeL(zz,zx,zr) -> match zz
        Done -> Node(l,x,Node(r,zx,zr))
        NodeR(zzl,zzx,zzz) -> splayx( zzz, Root(Node(zzl,zzx,l),x,Node(r,zx,zr)) )
        NodeL(zzz,zzx,zzr) -> splayx( zzz, Root(l,x,Node(r,zx,Node(zr,zzx,zzr))) )      
*/
/*
// Equational reason from bottom-up to top-down,
// but this recursive top-down has the transformational property!

type root2
  RootL( left : tree, key : key, rright : root )
  RootR( rleft : root, key : key, right : tree )
  RootI( left : tree, key : key, right : tree )

pub fun access-td( t : tree, k : key ) : tree  
  insert-td(t,k)

fun insert-td( t : tree, k : key ) : tree
  match insertk(t,k)
    RootI(l,x,r)              -> Node(l,x,r)
    RootL(l,x,Root(zl,zx,zr)) -> Node(l,x,Node(zl,zx,zr))
    RootR(Root(zl,zx,zr),x,r) -> Node(Node(zl,zx,zr),x,r)

fun insertk( t : tree, k : key ) : root2
  match t 
    Leaf -> RootI(Leaf,k,Leaf)
    Node(tl,tx,tr) -> 
      if   tx < k then match insertk(tr,k)
                        // if we see just one, we remember we went right
                        RootI(l,x,r)              -> RootR(Root(tl,tx,l),x,r)  
                        // otherwise rotate 2 nodes at a time
                        RootL(l,x,Root(zl,zx,zr)) -> RootI(Node(tl,tx,l),x,Node(zl,zx,zr))
                        RootR(Root(zl,zx,zr),x,r) -> RootI(Node(Node(tl,tx,zl),zx,zr),x,r)
      elif tx > k then match insertk(tl,k) 
                        RootI(l,x,r)              -> RootL(l,x,Root(r,tx,tr))
                        RootL(l,x,Root(zl,zx,zr)) -> RootI(l,x,Node(zl,zx,Node(zr,tx,tr)))
                        RootR(Root(zl,zx,zr),x,r) -> RootI(Node(zl,zx,zr),x,Node(r,tx,tr))
      else RootI(tl,tx,tr)
  */   

/*
pub fun test()
  // half cycles for Fig 6
  test5(access)
  // test6(access) 
  println("\n\n and now with derived topdown:\n")
  test5(access-td)
*/

pub fun main()
  benchmain(access)
