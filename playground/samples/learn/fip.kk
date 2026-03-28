// Examples of "Fully in-place" functional programming
//
// Work in progress.
// For a nice overview, see the introduction of:
//  "FP^2: Fully in-Place Functional Programming",
//  Anton Lorenzen, Daan Leijen, and Wouter Swierstra, ICFP'23.
//  <https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fip-tr-v2.pdf>
//
// And also:
//  "The Functional Essence of Imperative Binary Search Trees",
//  Anton Lorenzen, Daan Leijen, Wouter Swierstra, and Sam Lindley, PLDI'24
//  <https://www.microsoft.com/en-us/research/publication/fiptree-tr/>
//
// Prerequisites:
//   `learn/basic`
//   `learn/implicits`
module learn/fip

import std/core/undiv

// We can use the `fip` keyword to ensure that,
// _if the (non-borrowed) parameters are unique at runtime_,
// the function does not allocate or deallocate memory, and uses constant stack space.
// For example:
fip fun rev-acc( xs : list<a>, acc : list<a> ) : list<a>
  match xs
    Cons(x,xx) -> rev-acc(xx, Cons(x,acc))
    Nil        -> acc

fip fun rev( xs : list<a> ) : list<a>
  rev-acc(xs,[])

pub fun example-rev()
  [1,2,3].rev.println

// The `rev` function is fip: due to Perceus reference counting,
// if the argument list `xs` is unique at runtime, each `Cons` cell
// is reused and updated _in-place_ for the reversed accumulator:
// no memory is (de)allocated and constant stack space is used (as it
// is tail-recursive).
//
// There are severe restrictions on `fip` functions to make this guarantee.
// See the paper for details. In essence, all owned parameters must be
// used linearly, and the function can only call `fip` functions itself.

// We can still use `rev` persistently as well and have a full
// functional semantics where the argument list is copied when needed.
// The best of both worlds: we can write a purely functional version
// but get in-place update when possible without having to write multiple
// versions of the same functionality, e.g. an in-place updating set and a persistent set
pub fun example-persistent()
  val xs = list(1,5)
  xs ++ rev(xs)

// Sometimes, this copying can be unnecessary .. we are working on providing
// better warnings for these situations
pub fun example-bad()
  val xs = list(1,5)
  val ys = rev(xs)        // `xs` gets copied as it used later on for its `length`
  val n  = xs.length      // if we would swap the `ys` and `n` definitions we would avoid a copy of `xs`
  (ys,n).println

// Similarly, we can write the weaker `fbip` keyword for a function that does not allocate any memory,
// but is allowed to deallocate and use arbitrary stack space.
fbip fun filter-odd( xs : list<int> ) : list<int>
  match xs
    Cons(x,xx) -> if is-odd(x) then Cons(x,xx.filter-odd) else xx.filter-odd
    Nil        -> Nil


// We can write `fip(n)` (and `fbip(n)`), where `n` is a constant, to allow the function
// to allocate at most `n` constructors. This is useful for example to write in-place
// tree insertion where we may need to allocate a leaf node for a newly inserted element
type tree<a>
  Bin( left : tree<a>, value : a, right : tree<a> )
  Tip

fip(1) fun insert( t : tree<int>, k : int ) : tree<int>
  match t
    Bin(l,x,r) -> if (x==k) then Bin(l,k,r)
                  elif (x < k) then Bin(l,x,insert(r,k))
                  else Bin(insert(l,k),x,r)
    Tip        -> Bin(Tip,k,Tip)


// Unfortunately, we cannot quite check a recursive polymorphic fip version of `insert` yet
// since we cannot (yet) express second-rank borrow information where the compare
// function does not only need to be borrowed, but borrow its arguments as well
// (e.g. we need `^?cmp : (^a,^a) -> order`).
//
// The `insert-poly` actually _does_ execute in-place at runtime like a `fip(1)`
// function, we just can't check it statically (at this time).
fun insert-poly( t : tree<a>, k : a, ^?cmp : (a,a) -> order ) : tree<a>
  match t
    Bin(l,x,r) -> if (x==k) then Bin(l,k,r)
                  elif (x < k) then Bin(l,x,insert-poly(r,k))
                  else Bin(insert-poly(l,k),x,r)
    Tip        -> Bin(Tip,k,Tip)

// We can get around the previous restriction by using the `:order2<a>`
// (value) type which contains not only the `:order`, but also the elements
// themselves in ascending order.
// Instead of a bare comparison, we use an `order2` function that returns
// such an ordered pair -- this way we can keep using the elements `k` and `x` in
// a linear way while being polymorphic over the elements.
// A subtle point here is that `order2` is passed borrowed and as such can itself
// allocate/deallocate; the `fip` annotation only checks if a function is intrinsically
// fully in-place -- modulo the borrowed function parameters. (Consider the `map` function for example).
fip(1) fun insert-order( t : tree<a>, k : a, ^?order2 : (a,a) -> order2<a> ) : tree<a>
  match t
    Bin(l,x,r) -> match order2(x,k)
                    Eq2(kx)     -> Bin(l,kx,r)
                    Lt2(kx,kk)  -> Bin(l,kx,insert-order(r,kk))
                    Gt2(kk,kx)  -> Bin(insert-order(l,kk),kx,r)
    Tip        -> Bin(Tip,k,Tip)

// Return the height of a tree.
fun height( t : tree<a> ) : int
  match t
    Tip        -> 0
    Bin(l,_,r) -> 1 + max( l.height, r.height )

// Return the sum of a tree
fun sum( t : tree<int> ) : int
  match t
    Tip        -> 0
    Bin(l,x,r) -> x + l.sum + r.sum


// Convert a list to a tree given an ordering on the elements
fun tree( elems : list<a>, ^?order2 : (a,a) -> order2<a> ) : tree<a>
  elems.foldl(Tip) fn(t,i)
    insert-order(t,i)


fun example-order()
  [[2,3,1].tree.height,
   ['a','b','c'].tree.height,
   [2,3,1].tree(?order2=fn(x,y) Eq2(x)).height].println

// To map in-place over a tree in constant stack space we use a zipper.
// In this case a zipper from `:tree<a>` to `:tree<b>`.
// Each `Bin`, `BinR`, and `BinL` match in size so we can reuse
// them in-place when traversing a tree.
type zipper<a,b>
  BinL( up : zipper<a,b>, x : a, right : tree<a> )
  BinR( left : tree<b>, x : b, up : zipper<a,b>  )
  Done

// Map a function `f` over all element of a tree.
fip fun tmap( t : tree<a>, ^f : a -> <div|e> b ) : <div|e> tree<b>
  tmap-down(t, f, Done)

// Unfortunately, due to the mutualy recursive group, the compiler always
// infers divergence. This might be mitigated by defining this in a single
// recursive function with a direction parameter `Down(t : tree<a>)` and
// `Up(t : tree<b>)`.

// traverse down the left spine accumulating a left zipper
fip fun tmap-down( t : tree<a>, ^f : a -> <div|e> b, acc : zipper<a,b> ) : <div|e> tree<b>
  match t
    Bin(l,x,r) -> tmap-down( l, f, BinL( acc, x, r ) )
    Tip        -> tmap-up( Tip, f, acc )

// and traverse back up visiting all the right nodes recursively
fip fun tmap-up( t : tree<b>, ^f : a -> <div|e> b, acc : zipper<a,b> ) : <div|e> tree<b>
  match acc
    BinL( up, x, r )  -> tmap-down( r, f, BinR( t, f(x), up ) )  // we still need to do the right child
    BinR( l, fx, up)  -> tmap-up( Bin(l, fx, t), f, up )         // both children are done
    Done              -> t

fun example-tmap()
  list(1,10).tree.tmap(inc).sum  // = 65


// Todo.. more examples with zippers:
// - Morris traversal
// - Splay tree restructuring


pub fun main()
  example-persistent().println
  example-tmap().println
