// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.lean
import std/num/int32
import std/os/env

type color
  Red
  Black


type tree
  Node(color : color, lchild : tree, key : int32, rchild : tree)
  Leaf()
  

fun is-red(t : tree) : bool
  match t
    Node(Red) -> True
    _         -> False


fun balance-left(l :tree, k : int32, r : tree) : tree
  match l
    Node(_, Node(Red, lx, kx, rx), ky, ry)
      -> Node(Red, Node(Black, lx, kx, rx), ky, Node(Black, ry, k, r))
    Node(_, ly, ky, Node(Red, lx, kx, rx))
      -> Node(Red, Node(Black, ly, ky, lx), kx, Node(Black, rx, k, r))
    Node(_, lx, kx, rx)
      -> Node(Black, Node(Red, lx, kx, rx), k, r)
    Leaf -> Leaf
    

fun balance-right(l : tree, k : int32, r : tree) : tree
  match r
    Node(_, Node(Red, lx, kx, rx), ky, ry)
      -> Node(Red, Node(Black, l, k, lx), kx, Node(Black, rx, ky, ry))
    Node(_, lx, kx, Node(Red, ly, ky, ry))
      -> Node(Red, Node(Black, l, k, lx), kx, Node(Black, ly, ky, ry))
    Node(_, lx, kx, rx)
      -> Node(Black, l, k, Node(Red, lx, kx, rx))
    Leaf -> Leaf
    

fun ins(t : tree, k : int32) : tree
  match t
    Node(Red, l, kx, r)
      -> if k < kx then Node(Red, ins(l, k), kx, r)
         elif k > kx then Node(Red, l, kx, ins(r, k))
         else Node(Red, l, k, r)
    Node(Black, l, kx, r)
      -> if k < kx then (if is-red(l) then balance-left(ins(l,k), kx, r)
                                      else Node(Black, ins(l, k), kx, r))
         elif k > kx then (if is-red(r) then balance-right(l, kx, ins(r,k))
                                        else Node(Black, l, kx, ins(r, k)))
         else Node(Black, l, k, r)
    Leaf -> Node(Red, Leaf, k, Leaf)


fun set-black(t : tree) : tree
  match t
    Node(_, l, k, r) -> Node(Black, l, k, r)
    _ -> t


inline fun insert(t : tree, k : int32 ) : tree
  if is-red(t)
    then ins(t, k).set-black
    else ins(t, k)


fun fold(t : tree, b : a, f: (int32, a) -> a) : a
  match t
    Node(_, l, k, r) -> r.fold( f(k, l.fold(b, f)), f)
    Leaf             -> b


fun make-tree-aux(n : int32, t : tree) : div tree
  if n <= zero then t else
    val n1 = n.dec
    make-tree-aux(n1, insert(t, n1))

fun make-tree(n : int32) : div tree
  make-tree-aux(n, Leaf)
  

pub fun main()
  val n = get-args().head("").parse-int.default(4200000).int32
  val t = make-tree(n)
  val v = t.fold(zero) fn(k,r:int32){ if (k % 10.int32 == 0.int32) then r.inc else r }
  v.show.println
