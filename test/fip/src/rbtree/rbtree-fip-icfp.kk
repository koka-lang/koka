import std/num/int32
import std/os/env

ref type pad
  Pad

type color
  Red
  Black

type tree
  Node(color : color, lchild : tree, key : int32, value : bool, rchild : tree)
  Leaf()

type balance-node
  Balance(color : color, lchild : tree, key : int32, value : bool, rchild : tree)

type accum
  Done
  NodeL(color : color, lchild : accum, key : int32, value : bool, rchild : tree)
  NodeR(color : color, lchild : tree, key : int32, value : bool, rchild : accum)

fip fun rebuild(z : accum, t : tree) : tree
  match z
    NodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    NodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    Done -> t

fip fun balance( z : accum, l : tree, k : int32, v : bool, r : tree, u : balance-node ) : tree
  match z
    NodeR(Black, l1, k1, v1, z1) -> rebuild( z1, Node( Black, l1, k1, v1, Node(Red,l,k,v,r) ) )
    NodeL(Black, z1, k1, v1, r1) -> rebuild( z1, Node( Black, Node(Red,l,k,v,r), k1, v1, r1 ) )
    NodeR(Red, l1, k1, v1, z1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> balance( z2, Node(Black,l2,k2,v2,l1), k1, v1, Node(Black,l,k,v,r), u )
      NodeL(_,z2,k2,v2,r2) -> balance( z2, Node(Black,l1,k1,v1,l), k, v, Node(Black,r,k2,v2,r2), u )
      Done -> Node(Black, l1, k1, v1, Node(Red,l,k,v,r))
    NodeL(Red, z1, k1, v1, r1) -> match z1
      NodeR(_,l2,k2,v2,z2) -> balance( z2, Node(Black,l2,k2,v2,l), k, v, Node(Black,r,k1,v1,r1), u )
      NodeL(_,z2,k2,v2,r2) -> balance( z2, Node(Black,l,k,v,r), k1, v1, Node(Black,r1,k2,v2,r2), u )              
      Done -> Node(Black, Node(Red,l,k,v,r), k1, v1, r1)
    Done -> Node(Black,l,k,v,r)

fip(1) fun ins(t : tree, k : int32, v : bool, z : accum) : tree
  match t
    Node(c, l, kx, vx, r)
      -> if k < kx   then ins(l, k, v, NodeL(c, z, kx, vx, r))
         elif k > kx then ins(r, k, v, NodeR(c, l, kx, vx, z))
         else rebuild(z, Node(c, l, kx, vx, r))
    Leaf -> balance(z, Leaf, k, v, Leaf, Balance(Red,Leaf,0.int32,True,Leaf))

fip(1) fun insert(t : tree, k : int32, v : bool) : tree
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
