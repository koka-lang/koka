// Adapted from https ://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean
import std/os/env

type color
  Red
  Black

type tree
  Node(color : color, left : tree, key : int, value : bool, right : tree)
  Leaf
  

fun is-red(t : tree) : bool
  match t
    Node(Red) -> True
    _         -> False


fun balance-left(l :tree, k : int, v : bool, r : tree) : tree
  match l
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)
    Leaf -> Leaf
    

fun balance-right(l : tree, k : int, v : bool, r : tree) : tree
  match r
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))
    Leaf -> Leaf
    

fun ins(t : tree, k : int, v : bool) : tree
  match t
    Node(Red, l, kx, vx, r)
      -> if k < kx then Node(Red, ins(l, k, v), kx, vx, r)
         elif k > kx then Node(Red, l, kx, vx, ins(r, k, v))
         else Node(Red, l, k, v, r)
    Node(Black, l, kx, vx, r)
      -> if k < kx then (if is-red(l) then balance-left(ins(l,k,v), kx, vx, r)
                                      else Node(Black, ins(l, k, v), kx, vx, r))
         elif k > kx then (if is-red(r) then balance-right(l, kx, vx, ins(r,k,v))
                                        else Node(Black, l, kx, vx, ins(r, k, v)))
         else Node(Black, l, k, v, r)
    Leaf -> Node(Red, Leaf, k, v, Leaf)
    

fun set-black(t : tree) : tree
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t



fun insert(t : tree, k : int, v : bool) : tree
  if is-red(t)
    then ins(t, k, v).set-black
    else ins(t, k, v)


fun fold(t : tree, b : a, f : (int, bool, a) -> a) : a
  match t
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
    Leaf -> b
    

fun make-tree(n : int) : div tree
  fold-int( n, Leaf) fn(i,t)
    t.insert( i, i%10 == 0 )


fun count( t : tree ) : int 
  t.fold(0) fn(k,v,r)
    if (v) then r + 1 else r


pub fun main()
  val n = get-args().head("").parse-int.default(4200000)
  val t = make-tree(n)
  val v = t.count
  v.println
