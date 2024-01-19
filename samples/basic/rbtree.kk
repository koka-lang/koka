/* Example of red-black tree balanced insertion.

   This is based on the rebalancing algorithm shown in
   Chris Okasaki's beautiful book, "Purely Functional Data Structures",
   (Cambridge University Press, 1998, <https://www.amazon.com/Purely-Functional-Data-Structures-Okasaki/dp/0521663504>).

   See also the benchmarks in `example/bench/koka` and `rbtree-fbip.kk`
*/
module rbtree

import std/os/env

// A tree node color is either `Red` or `Black`
type color
  Red
  Black

// An ordered tree with `:int` keys and values `:a` in each node.
type tree<a>
  Node(color : color, left : tree<a>, key : int, value : a, right : tree<a>)
  Leaf


fun tree/is-red(t : tree<a>) : bool
  match t
    Node(Red) -> True
    _         -> False


// balance with a new the left-side for a node `k`, `v`, `r`
fun balance-left(l :tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match l
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)
    Leaf -> Leaf


// balance with a new right-side for a node `l`, `k`, `v`
fun balance-right(l : tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match r
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))
    Leaf -> Leaf


// insert a new value `v` with key `k` into a tree `t`, rebalancing on the way up
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


// Balanced tree insertion
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
  fold( 0, n - 1, Leaf) fn(i,t)
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
