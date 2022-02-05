/* Example of red-black tree balanced insertion
   using "functional-but-inplace" (FBIP) techniques for improved performance.
   
   The novel application of FBIP to improve on standard 
   Okasaki-style red-black tree rebalancing was initially done by Anton Lorenzen.

   See also `rbtree.kk`, the benchmarks in `test/bench/koka`, and
   <https://koka-lang.github.io/koka/doc/book.html#why-fbip> for more information on FBIP.
*/
module rbtree-fbip

import std/os/env

type color
  Red
  Black

type tree<a>
  Node(color : color, lchild : tree<a>, key : int, value : a, rchild : tree<a>)
  Leaf

// We use a "zipper" to keep track of where we are in the tree  
type zipper<a>
  NodeR(color : color, lchild : tree<a>, key : int, value : a, zip : zipper<a>)   // we came from the right
  NodeL(color : color, zip : zipper<a>, key : int, value : a, rchild : tree<a>)   // we came from the left
  Done

// Reconstruct a tree from a zipper (updates in place)
fun move-up(z : zipper<a>, t : tree<a>) : tree<a>
  match z
    NodeR(c, l, k, v, z1) -> z1.move-up(Node(c, l, k, v, t))
    NodeL(c, z1, k, v, r) -> z1.move-up(Node(c, t, k, v, r))
    Done -> t

// Rebalance upwards from a zipper
fun balance-red( z : zipper<a>, l : tree<a>, k : int, v : a, r : tree<a> ) : tree<a>
  match z
    NodeR(Black, l1, k1, v1, z1) -> z1.move-up( Node( Black, l1, k1, v1, Node(Red,l,k,v,r) ) )
    NodeL(Black, z1, k1, v1, r1) -> z1.move-up( Node( Black, Node(Red,l,k,v,r), k1, v1, r1 ) )
    NodeR(Red, l1, k1, v1, z1) -> match z1
      NodeR(_c2, l2, k2, v2, z2) -> z2.balance-red( Node(Black, l2, k2, v2, l1), k1, v1, Node(Black, l, k, v, r) )
      NodeL(_c2, z2, k2, v2, r2) -> z2.balance-red( Node(Black, l1, k1, v1, l), k, v, Node(Black, r, k2, v2, r2) )
      Done -> Node(Black, l1, k1, v1, Node(Red,l,k,v,r))
    NodeL(Red, z1, k1, v1, r1) -> match z1
      NodeR(_c2, l2, k2, v2, z2) -> z2.balance-red( Node(Black, l2, k2, v2, l), k, v, Node(Black, r, k1, v1, r1) )
      NodeL(_c2, z2, k2, v2, r2) -> z2.balance-red( Node(Black, l, k, v, r), k1, v1, Node(Black, r1, k2, v2, r2) )              
      Done -> Node(Black, Node(Red,l,k,v,r), k1, v1, r1)
    Done -> Node(Black,l,k,v,r)


// Find an insertion point while constructing a zipper (updates in place)
fun ins(t : tree<a>, k : int, v : a, z : zipper<a>) : tree<a>
  match t
    Node(c, l, kx, vx, r)
      -> if k < kx   then ins(l, k, v, NodeL(c, z, kx, vx, r))
         elif k > kx then ins(r, k, v, NodeR(c, l, kx, vx, z))
         else z.move-up(Node(c, l, kx, vx, r))  // z.move-up(t) is slower (but not a reuse bug); to be investigated
    Leaf -> z.balance-red(Leaf, k, v, Leaf)


// Insert an element into the tree
fun insert(t : tree<a>, k : int, v : a) : tree<a>
  ins(t, k, v, Done)


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

