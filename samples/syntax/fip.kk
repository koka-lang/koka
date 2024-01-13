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
//  Anton Lorenzen, Daan Leijen, Wouter Swierstra, and Sam Lindley,
//  <https://www.microsoft.com/en-us/research/publication/fiptree-tr/>
//
// Prerequisites:
//   `syntax/basic`
module syntax/fip

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
// The `insert-poly`` actually _does_ execute in-place at runtime like a `fip(1)`
// function, we just can't check it statically (at this time).
fun insert-poly( t : tree<a>, k : a, ^?cmp : (a,a) -> order ) : tree<a>
  match t
    Bin(l,x,r) -> if (x==k) then Bin(l,k,r)
                  elif (x < k) then Bin(l,x,insert-poly(r,k))
                  else Bin(insert-poly(l,k),x,r)
    Tip        -> Bin(Tip,k,Tip)


// Todo.. more examples with zippers:
// - Tree mapping
// - Morris traversal
// - Splay tree restructuring

pub fun main()
  example-persistent().println()