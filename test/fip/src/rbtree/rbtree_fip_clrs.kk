import std/num/int32
import std/os/env

type color 
  Red
  Black 

type tree
  Node(color : color, lchild : tree, key : int32, value : bool, rchild : tree)
  Leaf
  
fip fun is-red(^t : tree) : bool
  match t
    Node(Red) -> True
    _         -> False

type accum
  Done
  NodeL(color : color, lchild : accum, key : int32, value : bool, rchild : tree)
  NodeR(color : color, lchild : tree, key : int32, value : bool, rchild : accum)

fip(1) fun ins(t : tree, key : int32, v : bool, z : accum) : exn tree
  match t
    Node(c, l, kx, vx, r)
      -> if key < kx then ins(l, key, v, NodeL(c, z, kx, vx, r))
         elif key > kx then ins(r, key, v, NodeR(c, l, kx, vx, z))
         else balance(z, Node(c, l, key, v, r))
    Leaf -> balance(z, Node(Red, Leaf, key, v, Leaf))

fip fun set-black(t : tree) : tree
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    t -> t

fip fun rebuild(z : accum, t : tree) // Turn the zipper into a tree without rotating
  match z
    NodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    NodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    Done -> t

fip fun balance( z : accum, t : tree ) : exn tree
  match z
    NodeR(Red, l1, k1, v1, z1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> // black
        if is-red(l2) then balance(z2, Node(Red, l2.set-black, k2, v2, Node(Black, l1, k1, v1, t) ))
        else rebuild(z2, Node(Black, Node(Red,l2,k2,v2,l1), k1, v1, t))
      NodeL(_,z2,k2,v2,r2) -> // black
        if is-red(r2) then balance(z2, Node(Red, Node(Black,l1,k1,v1,t), k2, v2, r2.set-black))
        else match t
          Node(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red,l1,k1,v1,l), k, v, Node(Red,r,k2,v2,r2)))
      Done -> Node(Black, l1, k1, v1, t)
    NodeL(Red, z1, k1, v1, r1) -> match z1
      NodeL(_,z2,k2,v2,r2) -> // black
        if is-red(r2) then balance(z2, Node(Red, Node(Black, t, k1, v1, r1), k2, v2, r2.set-black ))
        else rebuild(z2, Node(Black, t, k1, v1, Node(Red,r1,k2,v2,r2)))
      NodeR(_,l2,k2,v2,z2) -> // black
        if is-red(l2) then balance(z2, Node(Red, l2.set-black, k2, v2, Node(Black,t,k1,v1,r1) ))
        else match t
          Node(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red,l2,k2,v2,l), k, v, Node(Red,r,k1,v1,r1)))
      Done -> Node(Black, t, k1, v1, r1)
    z -> rebuild(z, t)


fip(1) fun insert(t : tree, k : int32, v : bool) : <exn> tree
  ins(t, k, v, Done)


fun fold(t : tree, b : a, f: (int32, bool, a) -> a) : a
  match t
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
    Leaf                -> b


fun make-tree-aux(n : int32, t : tree) : pure tree
  if n <= zero then t else
    val n1 = n.dec
    make-tree-aux(n1, insert(t, n1, n1 % 10.int32 == zero))

pub fun make-tree(n : int32) : pure tree
  make-tree-aux(n, Leaf)


fun test(n : int32) 
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val t = make-tree(n)
            acc + t.fold(zero) fn(k,v,r:int32){ if v then r.inc else r }
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)