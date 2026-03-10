module splay-td

import std/num/int32
import tree


pub fun access( t : tree, k : key ) : tree  
  insert(t,k)

pub fun insert( t : tree, k : key ) : tree  
  splay(t,k,ctx _,ctx _)
  

pub fip(1) fun splay(t : tree, k : key, accl : ctx<tree>, accr : ctx<tree> ) : tree
  match t
    Node(l,x,r) ->
      if x < k then match r
        Node(rl,rx,rr) ->
          if   rx < k then splay(rr, k, accl ++ ctx Node(Node(l,x,rl),rx,_), accr)            // Root(rrl,rrx,rrr) -> Root(Node(Node(l,x,rl),rx,rrl),rrx,rrr)
          elif rx > k then splay(rl, k, accl ++ ctx Node(l,x,_), accr ++ ctx Node(_,rx,rr))   // Root(rll,rlx,rlr) -> Root(Node(l,x,rll),rlx,Node(rlr,rx,rr))
          else  Node( accl ++. Node(l,x,rl),  rx, accr ++. rr )
        Leaf -> Node( accl ++. Node(l,x,Leaf), k, accr ++. Leaf )
      elif x > k then match l
        Node(ll,lx,lr) ->
          if   lx < k then splay(lr, k, accl ++ ctx Node(ll,lx,_), accr ++ ctx Node(_,x,r)) //  Root(lrl,lrx,lrr) -> Root( Node(ll,lx,lrl),lrx,Node(lrr,x,r))
          elif lx > k then splay(ll, k, accl, accr ++ ctx Node(_,lx,Node(lr,x,r)))           //  Root(lll,llx,llr) -> Root(lll,llx,Node(llr,lx,Node(lr,x,r)))      
          else  Node( accl ++. ll, lx, accr ++. Node(lr,x,r) )          
        Leaf -> Node( accl ++. Leaf, k, accr ++. Node(Leaf,x,r) )
      else  Node( accl ++. l, x, accr ++. r )
    Leaf -> Node( accl ++. Leaf, k, accr ++. Leaf )
    


pub fun test()
  test5(access)   // does not follow Fig 6
  //test6(access)


pub fun main()
  benchmain(access)
