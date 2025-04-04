// Experimental feature: Lazy constructors
// This is still in design and may change.
//
// Lazy constructors make it possible to defunctionalize lazy computations into
// constructors. This can make debugging easier as well as lead to better reuse
// behaviour and thus better performance.
//
// Prerequisites:
//  - learn/basic
//  - learn/fip
module learn/lazycons

// Lazy constructors are a special kind of constructors that can be used to define
// lazy data structures. Each lazy constructor contains a piece of code that is
// given in its definition. For example, consider the `:stream1<a>` type:

// The `lazy` keyword indicates that our stream type contains lazy constructors.
lazy type stream1<a>
  // `Nil1` and `Cons1` are regular constructors
  // If we only had these two constructors, our stream would be isomorphic to a list.
  Nil1
  Cons1( head : a, tail : stream1<a> )
  // The `lazy` keyword indicates that this is a lazy constructor:
  lazy Append1( xs : stream1<a>, ys : stream1<a> ) -> append1(xs, ys)

// We can create an instance of `:stream1<a>` using any of the `Nil1`, `Cons1`
// or `Append1` constructors using the usual syntax:

val xs = Append1(Cons1(1, Cons1(2, Nil1)), Cons1(3, Nil1))

// However, lazy constructors behave differently when we match on a `stream1<a>`.
fun append1( xs : stream1<a>, ys : stream1<a> ) : div stream1<a>
  match stream1/lazy-force(xs) // This is an exhaustive match
    Nil1        -> ys
    Cons1(x,xx) -> Cons1(x, Append1(xx, ys))
// But what if `xs = Append1(xs1, ys)`? In this case, Koka will evaluate
// and memoize the code given in the `Append1` lazy constructor. In pseudo-code:
//  Append1(xs1, ys) ->
//    val xs' = append1(xs1, ys) // evaluate the code in the constructor
//    xs := Indirection(xs')     // write an indirection node to memoize
//    append1(xs', ys)           // try the match again

// Occasionally, it can be helpful to perform only a single step of evaluation
// on a lazy constructor. Koka provides the special `lazy-step` function for this
// purpose. For example, lazy-step(xs) returns unit, but under the hood, it will
// evaluate `xs` to `Cons1(1, Append1(Cons1(2, Nil1), Cons1(3, Nil1)))`.

// Let us now look at a full example of lazy constructors.
lazy type stream<a>
  SNil
  SCons( head : a, tail : stream<a> )
  // 'SAppend' implements stream concatenation. Unlike in `:stream1<a>`,
  // we inline the `sappend` function here. This helps with reuse analysis.
  lazy SAppend( xs : stream<a>, ys : stream<a> ) ->
    // We perform a single step of evaluation on `ys`;
    // this will be explained later.
    stream/lazy-step(ys)
    match xs
      SNil        -> ys
      // In the next line, Koka can reuse the `SAppend` constructor for
      // the `SCons` and the `SCons` constructor for the `SAppend`.
      // Reuse for lazy constructors is special, since it also implements
      // memoization. This means that a lazy constructor can only be reused
      // for a constructor in head-position (like `SCons` here):
      SCons(x,xx) -> SCons(x,SAppend(xx,ys)) 
  // 'SReverse(xs, acc)' implements 'reverse xs ++ acc'.
  // Again, we can apply reuse analysis here.
  lazy SReverse( xs : stream<a>, acc : stream<a> ) ->
    match xs
      SNil         -> acc
      SCons(x, xx) ->
        // This may be surprising: we are allowed to return a new lazy constructor here.
        // If we match on a stream that is an 'SReverse', Koka will recursively evaluate
        // this code until we reach the base case 'SNil' and return 'acc'.
        SReverse( xx, SCons(x, acc) )

// Koka provides another special built-in language feature to make it
// possible to match on lazy constructors. In a `lazy match`, Koka will
// neither evaluate the lazy constructors nor memoize them.
// We can print our stream type using the lazy-match statement:
fun debug-show( xs : stream<a>, ?show : a -> string ) : exn string
  lazy match xs
    SNil        -> "SNil"
    SCons(x,xx) -> "SCons(" ++ show(x) ++ ", " ++ debug-show(xx) ++ ")"
    SAppend(xs,ys) -> "SAppend(" ++ debug-show(xs) ++ ", " ++ debug-show(ys) ++ ")"
    SReverse(xs,acc) -> "SReverse(" ++ debug-show(xs) ++ ", " ++ debug-show(acc) ++ ")"

// As an example of our lazy streams, let us implement the Bankers Queue
// (see "Purely Functional Data Structures" by Chris Okasaki, Sect. 6.3.2).
//
// A queue is a data structure where elements can be added to the end ('snoc')
// and removed from the front ('uncons'). For example:
//
//  [1, 2, 3].snoc(4) == [1, 2, 3, 4]
//  [1, 2, 3].uncons() == Just (1, [2, 3])
//
// The Bankers Queue allows us to implement 'snoc' and 'uncons' in O(1) amortized time.
// The idea is to use to streams, where one stream ('front') is used for 'uncons' and
// the other stream ('rear') is used for 'snoc'. Whenever the 'rear' stream becomes
// longer than the 'front' stream, we reverse the 'rear' stream and append it to the
// end of the 'front' stream. By using lazy constructors, our amortized time bound
// also holds in a persistent setting (as outlined in Okasaki's book).

struct bqueue<a>
  front : stream<a>
  front-len : int
  rear : stream<a>
  rear-len : int

val bempty = Bqueue( SNil, 0, SNil, 0 )

fun check( Bqueue(front, front-len, rear, rear-len) : bqueue<a> ) : bqueue<a>
  if (front-len >= rear-len)
    then Bqueue(front, front-len, rear, rear-len)
    else Bqueue( SAppend(front, SReverse(rear, SNil)), // append the reversed rear to the front
                 front-len + rear-len,                 // the front thus contains all elements
                 SNil, 0)                              // meanwhile, the rear is empty

// Note that in `SAppend(front, SReverse(rear, SNil))`, the second argument of
// `SAppend` is always a `SReverse` constructor. This motivates our use of
// `lazy-step` in the definition of `SAppend`. Each time we perform one step of
// the stream concatenation, we also perform one step of the reverse operation.
// This means that once we have fully appended the two streams, we are also
// (almost) done with the reverse operation. This gives our implementation
// a worst-case time complexity of O(log(n)), compared with Okasaki's O(n).

// We snoc an element to the end of the queue by adding it to the rear stream
// and check that our invariant is maintained.
fun snoc( Bqueue(front,front-len,rear,rear-len) : bqueue<a>, x : a ) : bqueue<a>
  Bqueue(front, front-len, SCons(x,rear), rear-len+1).check

// We can uncons an element from the front of the queue. By our invariant, if the front
// stream is empty, so is the rear stream. Otherwise, we return the head of the front stream
// and check that our invariant is maintained on the remaining elements.
fun uncons( Bqueue(front,front-len,rear,rear-len) : bqueue<a> ) : div maybe<(a,bqueue<a>)>
  match front
    SCons(x,xx) -> Just((x, Bqueue(xx,front-len - 1,rear,rear-len).check))
    SNil        -> Nothing

fun show( Bqueue(front,front-len,rear,rear-len) : bqueue<a>, ?show : a -> string ) : exn string
  "Bqueue(" ++ debug-show(front) ++ ", " ++ show(front-len) ++ ", " ++ debug-show(rear) ++ ", " ++ show(rear-len) ++ ")"

pub fun main()
  val q = bempty.snoc(1).snoc(2).snoc(3).snoc(4).snoc(5)
  println(show(q))
  fold(0, 4, q) fn(_, q)
    val Just((_, q)) = q.uncons()
    println(show(q))
    q
  ()