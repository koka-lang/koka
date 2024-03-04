module rb-rec

import std/num/random
import std/num/int32
import rbtree

fip fun is-red(^t : rbtree) : bool
  match t
    Node(Red) -> True
    _         -> False


fip fun set-black( t : rbtree ) : rbtree
  match t
    Node(_,l,x,v,r) -> Node(Black,l,x,v,r)
    Leaf            -> Leaf

pub fun main()
  benchmain(access)

pub fun access(t : rbtree, k : key) : rbtree
  find(t, k, from-key(k)).set-black


// Recursive Okasaki style
fip fun balance-left( root : root ) : rbtree
  match root
    Root(_,l,k,v,r) -> match l    // root is always black, l is always red, now fix possible black-red-red violations:
      Node(_,Node(Red,lx,kx,vx,rx),ky,vy,ry)
        -> Node(Red,Node(Black,lx,kx,vx,rx),ky,vy,Node(Black,ry,k,v,r))
      Node(_,ly,ky,vy,Node(Red,lx,kx,vx,rx))
        -> Node(Red,Node(Black,ly,ky,vy,lx),kx,vx,Node(Black,rx,k,v,r))
      Node(_,lx,kx,vx,rx)
       -> Node(Black,Node(Red,lx,kx,vx,rx),k,v,r)
      Leaf -> Node(Black,Leaf,k,v,r)


fip fun balance-right( root : root) : rbtree
  match root
    Root(_,l,k,v,r) -> match r    // root is always black, r is always red, now fix possible black-red-red violations:
      Node(_,Node(Red,lx,kx,vx,rx),ky,vy,ry)
        -> Node(Red,Node(Black,l,k,v,lx),kx,vx,Node(Black,rx,ky,vy,ry))
      Node(_,lx,kx,vx,Node(Red,ly,ky,vy,ry))
        -> Node(Red,Node(Black,l,k,v,lx),kx,vx,Node(Black,ly,ky,vy,ry))
      Node(_,lx,kx,vx,rx)
        -> Node(Black,l,k,v,Node(Red,lx,kx,vx,rx))
      Leaf -> Node(Black,l,k,v,Leaf)

// Okasaki relying on TRMC
fbip(1) fun find(t : rbtree, k : key, v : tval) : rbtree
  match t
    Node(Red, l, kx, vx, r)
      -> if k < kx then Node(Red, find(l, k, v), kx, vx, r)
         elif k > kx then Node(Red, l, kx, vx, find(r, k, v))
         else Node(Red, l, k, v, r)
    Node(Black, l, kx, vx, r)
      -> if k < kx then (if is-red(l) then balance-left( Root(Black, find(l,k,v), kx, vx, r) )
                                      else Node(Black, find(l, k, v), kx, vx, r))
         elif k > kx then (if is-red(r) then balance-right( Root(Black, l, kx, vx, find(r,k,v)) )
                                        else Node(Black, l, kx, vx, find(r, k, v)))
         else Node(Black, l, k, v, r)
    Leaf -> Node(Red, Leaf, k, v, Leaf)

/*
// Okasaki with explicit context
fbip(1) fun find(t : rbtree,k : key, v : tval, acc : ctx<rbtree> ) : rbtree
  match t
    Node(Red,l,kx,vx,r)
      -> if kx < k then find( r, k, v, acc ++ ctx Node(Red,l,kx,vx,_))
         elif kx > k then find( l, k, v, acc ++ ctx Node(Red,_,kx,vx,r))
         else acc ++. Node(Red,l,k,v,r)
    Node(Black,l,kx,vx,r)
      -> if   kx < k then (if is-red(r) then acc ++. balance-right( Root(Black, l, kx, vx, find(r,k,v,ctx _)))
                                        else find( r, k, v, acc ++ ctx Node(Black,l,kx,vx,_)))
         elif kx > k then (if is-red(l) then acc ++. balance-left( Root(Black, find(l,k,v,ctx _), kx, vx, r))
                                        else find( l, v, v, acc ++ ctx Node(Black,_,kx,vx,r)))
         else acc ++. Node(Black,l,k,v,r)
    Leaf -> acc ++. Node(Red,Leaf,k,v,Leaf)
*/
