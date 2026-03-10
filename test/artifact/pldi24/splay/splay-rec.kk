module splay-rec

import std/num/int32
import tree

pub fun access( t : tree, k : key ) : tree  
  insert(t,k)

pub fun insert( t : tree, k : key ) : tree  
  val Root(l,x,r) = splay(t,k)
  Node(l,x,r)

pub fun splay(t : tree, k : key ) : root
  match t
    Node(l,x,r) ->
      if x < k then match r
        Node(rl,rx,rr) ->
          if   rx < k then match splay(rr,k) 
                             Root(rrl,rrx,rrr) -> Root(Node(Node(l,x,rl),rx,rrl),rrx,rrr)
          elif rx > k then match splay(rl,k)
                             Root(rll,rlx,rlr) -> Root(Node(l,x,rll),rlx,Node(rlr,rx,rr))
          else Root(Node(l,x,rl),rx,rr)
        Leaf -> Root(Node(l,x,Leaf),k,Leaf)
      elif x > k then match l
        Node(ll,lx,lr) ->
          if   lx < k then match splay(lr,k)
                             Root(lrl,lrx,lrr) -> Root(Node(ll,lx,lrl),lrx,Node(lrr,x,r))
          elif lx > k then match splay(ll,k)
                             Root(lll,llx,llr) -> Root(lll,llx,Node(llr,lx,Node(lr,x,r)))      
          else Root(ll,lx,Node(lr,x,r))          
        Leaf -> Root(Leaf,k,Node(Leaf,x,r))
      else Root(l,x,r)
    Leaf -> Root(Leaf,k,Leaf)

// pub fun access(ts : list<tree>, i : int) : list<tree>
  // ts.map(fn(t) t.access(i))


pub fun main()
  //test5(access)   // does not follow Fig 6
  //test6(access)
  benchmain(access)
