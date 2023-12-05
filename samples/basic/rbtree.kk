/* Example of red-black tree<a> balanced insertion.

   This uses Okasaki style functional rebalancing.
   See also the benchmarks in `test/bench/koka` and `rbtree-fbip.kk`
*/
module rbtree

import std/os/env

type color
  Red
  Black


type tree<a>
  Node(color : color, left : tree<a>, key : int, value : a, right : tree<a>)
  Leaf


fun is-red(t : tree<a>) : bool
  match t
    Node(Red) -> True
    _         -> False


fun balance-left(l :tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match l
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)
    Leaf -> Leaf



fun balance-right(l : tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match r
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))
    Leaf -> Leaf


fun ins(t : tree<a>, k : int, v : a) : tree<a>
  match t
    Node(Red, l, kx, vx, r)
      -> if k < kx then Node(Red, ins(l, k, v), kx, vx, r)
         elif k == kx then Node(Red, l, k, v, r)
         else Node(Red, l, kx, vx, ins(r, k, v))
    Node(Black, l, kx, vx, r)
      -> if k < kx then (if is-red(l) then balance-left(ins(l,k,v), kx, vx, r)
                                      else Node(Black, ins(l, k, v), kx, vx, r))
         elif k == kx then Node(Black, l, k, v, r)
         elif is-red(r) then balance-right(l, kx, vx, ins(r,k,v))
         else Node(Black, l, kx, vx, ins(r, k, v))
    Leaf -> Node(Red, Leaf, k, v, Leaf)


fun set-black(t : tree<a>) : tree<a>
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t


// Tree insertion
fun insert(t : tree<a>, k : int, v : a) : tree<a>
  if t.is-red
    then ins(t, k, v).set-black
    else ins(t, k, v)


// Fold the elements of a tree
fun fold(t : tree<a>, acc : b, f : (int, a, b) -> b) : b
  match t
    Node(_, l, k, v, r) -> fold( r, f(k,v,fold(l,acc,f)), f )
    Leaf                -> acc


// Make a boolean tree
fun make-tree(n : int) : tree<bool>
  fold-int( 0, n - 1, Leaf) fn(i,t)
    t.insert( i, i%10 == 0 )


// Count the nodes with `True` values.
fun count( t : tree<bool> ) : int
  t.fold(0) fn(k,v,r)
    if v then r + 1 else r


// Benchmark for `n` insertions
pub fun bench(n : int)
  make-tree(n).count.println


pub fun main()
  val n = get-args().head("").parse-int.default(4200000)
  bench(n)
