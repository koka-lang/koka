// First-class constructor contexts allow specifying efficient
// top-down algorithms directly in a purely functional style.
// See the paper for a more in-depth discussion:
//
// >  "The Functional Essence of Imperative Binary Search Trees",
// >  Anton Lorenzen, Daan Leijen, Wouter Swierstra, and Sam Lindley,
// >  <https://www.microsoft.com/en-us/research/publication/fiptree-tr/>
//
// Prerequisites:
//   - `syntax/basic`
//   - `syntax/with`
module syntax/contexts

// Consider the well-known `map` function that maps a function over a list
fun map-std( xs : list<a>, ^f : a -> e b ) : e list<b>
  match xs
    Cons(x,xx) -> Cons( f(x), xx.map-std(f) )
    Nil        -> Nil

// note: we use `^` on the `f` parameter to borrow it, which means it is
// considered live for the entire duration of the function and does not need
// to be reference counted or drop'd.

// Naively, the `map-std` function would use stack space linear in the size
// of the first list as it is not directly tail recursive. It turns out
// that Koka can actually optimize tail recursive calls under a series
// of constructors and `map-std` will be compiled in an efficient way.
// See the paper:
//
// >  "Tail Recursion Modulo Context: An Equational Approach",
// >  Daan Leijen and Anton Lorenzen, POPL'23,
// >  <https://www.microsoft.com/en-us/research/publication/tail-recursion-modulo-context-an-equational-approach-2/>
//
// To be sure this optimization happens, we can write `tail fun` to get
// a warning if our function is not quite TRMC.
tail fun map-tail( xs : list<a>, ^f : a -> e b ) : e list<b>
  match xs
    Cons(x,xx) -> Cons( f(x), xx.map-tail(f) )
    Nil        -> Nil

pub fun test-tail()
  [1,2,3].map(inc).println

// The `map-tail` function is the recommended way to write functions
// like `map` in Koka -- clean, functional, and efficient!

// However, the TRMC optimization cannot always apply. How would we
// then write an efficient `map` function? Commonly, functional programmers
// use an accumulator to write a tail-recursive version of `map`.
tail fun map-acc( xs : list<a>, f : a -> e b, acc : list<b> ) : e list<b>
  match xs
    Cons(x,xx) -> xx.map-acc(f, Cons(f(x), acc))
    Nil        -> acc.reverse

fun map-acc-entry(xs,f)
  map-acc(xs,f,[])

pub fun test-acc()
  [1,2,3].map-acc-entry(inc).println

// But `map-acc` is still not great since we build the accumulation list
// in reverse, and need to `reverse` it again in the end
// (_bottom-up_, doing two passes over the list).
// A C programmer would instead write a _top-down_ algorithm
// traversing the list once and returning the (remembered) root
// of the list at the end. To do this in a purely functional way
// we need first-class constructor contexts.

// We can create a context using the `ctx` keyword. A context must
// consist of constructors and have a single _hole_ (written as `_`).
// For example, `ctx Cons(1,Cons(2,_))` or `ctx Bin(Bin(Leaf 1, _),Leaf 3)`
// There are two operations on context, append and apply:
//
// `(++)  : (cctx<a,b>, cctx<b,c>) -> cctx<a,c>`  // append
// `(++.) : (cctx<a,b>, b) -> a`                  // apply
//
// Append takes a context of type `:a` with hole `:b` and writes a
// a context `cctx<b,c>` into the hole to get a new context of type
// `:a` with hole `:c`. Apply `(.++)` plugs the hole `:b` in a
// context `cctx<a,b>` and returns the whole structure `:a`.
// Often the hole and the structure are of the same type, and
// we have the `alias ctx<a> = cctx<a,a>`.
//
// Now we can write a top-down `map` ourselves: (note: we don't need `tail` here as `fip` implies `tail` already)
fip fun map-td( xs : list<a>, ^f : a -> e b, acc : ctx<list<b>> ) : e list<b>
  match xs
    Cons(x,xx) -> xx.map-td( f, acc ++ ctx Cons(f(x), _) )  // plug `Cons(f(x),_)` into the hole in `acc`
    Nil        -> acc ++. Nil                               // plug the hole with `Nil`

fip fun map-td-entry(xs,^f)
  map-td(xs,f,ctx _)     // start with an empty context

pub fun test-td()
  [1,2,3].map-td-entry(inc).println

// The top-down version is efficient and in the fast path (where the
// list is unique at runtime and not used persistently) executes
// much like the loop that the C programmer would write: traversing the
// list once and updating the elements in place (due to Perceus). When the arguments
// are unique, the append and apply of contexts are constant time operations.

// The TRMC version `map-tail` is still preferred as it is more clear and
// internally optimizes to the `map-td` version. However, the optimization
// can not alway be done and in such case we need first-class constructor contexts.
// Consider the `flatten` function that concatenates a list of lists:

fip fun concat(xs : list<a>, ys : list<a>) : list<a>   // also defined as `std/core/list/(++)`
  match xs
    Cons(x,xx) -> Cons(x, concat(xx,ys))
    Nil        -> ys

fbip fun flatten( xss : list<list<a>> ) : list<a>
  match xss
    Cons(xs,xxs) -> concat( xs, flatten(xxs) )
    Nil          -> Nil

// Here, `concat` is TRMC, but `flatten` is not (since the recursive call is inside a function call)
// With constructor contexts though we can represent the flattened list as a context
// with a hole in the tail element:
fip fun concat-td( xs : list<a>, acc : ctx<list<a>> ) : ctx<list<a>>
  match xs
    Cons(x,xx) -> concat-td( xx, acc ++ ctx Cons(x,_) )
    Nil        -> acc

fbip fun flatten-td( xss : list<list<a>>, acc : ctx<list<a>> ) : ctx<list<a>>
  match xss
    Cons(xs,xxs) -> flatten-td( xxs, concat-td( xs, acc ) )
    Nil          -> acc

fbip fun flatten-entry( xss : list<list<a>> ) : list<a>
  flatten-td( xss, ctx _) ++. Nil

pub fun example-flatten()
  [[1,2],[3,4],[]].flatten-entry.println

// Another example is the `partition` function where we
// need two contexts to build the two result lists in one
// traversal.
tail fun partition-td(xs : list<a>, ^pred : a -> e bool, acc1 : ctx<list<a>>, acc2 : ctx<list<a>>): e (list<a>, list<a>)
  match xs
    Nil -> (acc1 ++. Nil, acc2 ++. Nil)
    Cons(x,xx) -> if pred(x)
      then partition-td(xx, pred, acc1 ++ ctx Cons(x,_), acc2)
      else partition-td(xx, pred, acc1,                  acc2 ++ ctx Cons(x,_))

fun partition-entry( xs : list<a>, ^pred : a -> e bool ) : e (list<a>,list<a>)
  partition-td(xs, pred, ctx _, ctx _)

pub fun example-partition()
  [1,2,3,4,5].partition-entry(is-odd)


// See <https://www.microsoft.com/en-us/research/uploads/prod/2023/07/fiptree-tr-v4.pdf>
// for more complex examples on balanced tree insertion algorithms.

pub fun main()
  example-partition().println()

