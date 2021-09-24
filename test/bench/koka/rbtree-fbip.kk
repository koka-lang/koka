// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean
import std/num/int32
import std/os/env

type color
  Red
  Black


type tree
  Node(color : color, lchild : tree, key : int32, value : bool, rchild : tree)
  Leaf
  

fun is-red(t : tree) : bool
  match t
    Node(Red) -> True
    _         -> False


type zipper
  NodeR(color : color, lchild : tree, key : int32, value : bool, zip : zipper)
  NodeL(color : color, zip : zipper, key : int32, value : bool, rchild : tree)
  Done

fun balance( z : zipper, c : color, l : tree, k : int32, v : bool, r : tree ) : div tree
  match (c,z) 
    (Red, NodeR( Red, l1, k1, v1, NodeR(_c2, l2, k2, v2, z2) ))      
      -> z2.balance( Red, Node(Black, l2, k2, v2, l1), k1, v1, Node(Black, l, k, v, r) )
    (Red, NodeR( Red, l1, k1, v1, NodeL(_c2, z2, k2, v2, r2) ))
      -> z2.balance( Red, Node(Black, l1, k1, v1, l), k, v, Node(Black, r, k2, v2, r2) )
    (Red, NodeL( Red, NodeR(_c2, l2, k2, v2, z2), k1, v1, r1 ))
      -> z2.balance( Red, Node(Black, l2, k2, v2, l), k, v, Node(Black, r, k1, v1, r1) )
    (Red, NodeL( Red, NodeL(_c2, z2, k2, v2, r2), k1, v1, r1 ))
      -> z2.balance( Red, Node(Black, l, k, v, r), k1, v1, Node(Black, r1, k2, v2, r2) )              
    (_, NodeR( c1, l1, k1, v1, z1 ))
      -> z1.balance( c1, l1, k1, v1, Node(c,l,k,v,r) )
    (_, NodeL( c1, z1, k1, v1, r1 ))
      -> z1.balance( c1, Node(c,l,k,v,r), k1, v1, r1 )              
    (_, Done) 
      -> Node(c,l,k,v,r)


fun balance-black(t : tree, z : zipper)
  match z
    NodeR(c, l, k, v, z1) -> z1.balance(c, l, k, v, t)
    NodeL(c, z1, k, v, r) -> z1.balance(c, t, k, v, r)
    Done -> t


fun ins(t : tree, k : int32, v : bool, z : zipper) : div tree
  match t
    Node(Red, l, kx, vx, r)
      -> if k < kx   then ins(l, k, v, NodeL(Red, z, kx, vx, r))
         elif k > kx then ins(r, k, v, NodeR(Red, l, kx, vx, z))
         else z.balance(Red, l, k, v, r)
    Node(Black, l, kx, vx, r)
      -> if k < kx   then ins(l, k, v, NodeL(Black, z, kx, vx, r))
         elif k > kx then ins(r, k, v, NodeR(Black, l, kx, vx, z))
         else // balance-black( t, z )
               match z
                NodeR(c1, l1, k1, v1, z1) -> z1.balance(c1, l1, k1, v1, t)
                NodeL(c1, z1, k1, v1, r1) -> z1.balance(c1, t, k1, v1, r1)
                Done -> t
    Leaf -> z.balance(Red, Leaf, k, v, Leaf)


fun set-black(t : tree) : tree
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t


fun insert(t : tree, k : int32, v : bool) : div tree
  if is-red(t)
    then ins(t, k, v, Done).set-black
    else ins(t, k, v, Done)


fun fold(t : tree, b : a, f: (int32, bool, a) -> a) : a
  match t
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
    Leaf                -> b


fun make-tree-aux(n : int32, t : tree) : div tree
  if n <= zero then t else
    val n1 = n.dec
    make-tree-aux(n1, insert(t, n1, n1 % 10.int32 == zero))

fun make-tree(n : int32) : div tree
  make-tree-aux(n, Leaf)
  

fun main()
  val n = get-args().head("").parse-int.default(4200000).int32
  val t = make-tree(n)
  val v = t.fold(zero) fn(k,v,r:int32){ if (v) then r.inc else r }
  v.show.println
