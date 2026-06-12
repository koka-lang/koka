// Experimental feature: Lazy constructors
// This is still in design and may change.
//
// Lazy constructors make it possible to defunctionalize lazy computations into
// constructors. This can make debugging easier as well as lead to better reuse
// behaviour and thus better performance.
//
// For a nice overview, see:
//  "First-order Laziness",
//  Anton Lorenzen, Daan Leijen, Wouter Swierstra, and Sam Lindley, ICFP'25.
//  <https://antonlorenzen.de/papers/lazycons.pdf>
//
// Prerequisites:
//  - learn/basic
module learn/lazycons

// Lazy constructors are a special kind of constructors that can be used to define
// lazy data structures. Each lazy constructor contains a piece of code that is
// given in its definition. For example, consider the `:stream1<a>` type:

// The `lazy` keyword indicates that our stream type contains lazy constructors.
lazy type stream1<a>
  // `Nil1` and `Cons1` are regular constructors -- if we only had these two constructors, our stream would be isomorphic to a list.
  Nil1
  Cons1( head : a, tail : stream1<a> )
  // but we also add a lazy constructor for appending streams:
  lazy Append1( xs : stream1<a>, ys : stream1<a> ) ->
    match xs
      Nil1 -> ys                                  // ignore this warning for now
      Cons1(x,xx) -> Cons1(x, Append1(xx,ys))

// We can create an instance of `:stream1<a>` using any of the `Nil1`, `Cons1`
// or `Append1` constructors using the usual syntax:
val xs = Append1(Cons1(1, Cons1(2, Nil1)), Cons1(3, Nil1))

// However, lazy constructors behave differently when we match on a `stream1<a>`.
fun head1( xs : stream1<a> ) : div maybe<a>
  match xs           // This is an exhaustive match which evaluates (and memoizes) `Append1` on-demand
    Nil1       -> Nothing
    Cons1(x,_) -> Just(x)


// Let us now look at a full example of lazy constructors.
lazy type stream<a>
  SNil
  SCons( head : a, tail : stream<a> )

  // 'SAppend' implements stream concatenation.
  lazy SAppend( xs : stream<a>, ys : stream<a> ) ->
    // We perform a single step of evaluation on `ys`; this will be explained later.
    stream/lazy-step(ys)
    match xs
      SNil        -> ys
      SCons(x,xx) -> SCons(x,SAppend(xx,ys))
                  // In the above case, Koka can reuse the `SAppend` constructor for
                  // the `SCons` and the `SCons` constructor for the `SAppend`.
                  // Reuse for lazy constructors is special, since it also implements
                  // memoization. This means that a lazy constructor can only be reused
                  // for a constructor in head-position (like `SCons` here):

  // 'SReverse(xs, acc)' implements 'reverse xs ++ acc'. Again, we can apply reuse analysis here.
  lazy SReverse( xs : stream<a>, acc : stream<a> ) ->
    match xs
      SNil        -> acc
      SCons(x,xx) -> SReverse(xx,SCons(x,acc))
                  // The previous case may be surprising: we are allowed to return a new lazy constructor here.
                  // If we match on a stream that is an 'SReverse', Koka will (tail) recursively evaluate
                  // this code until we reach the base case 'SNil' and return 'acc'.


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


fun example1()
  val s = SAppend(SCons(1,SNil), SCons(2,SNil))
  s.debug-show.println
  match s
    SCons(_,tl1) -> 
      s.debug-show.println
      match tl1
        SCons(_,_) -> s.debug-show.println
        _ -> s.debug-show.println            // note how s changes in-place!
    _ -> s.debug-show.println
  

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
  flen : int
  rear : stream<a>
  rlen : int

val bempty = Bqueue( SNil, 0, SNil, 0 )

fun check( Bqueue(front, flen, rear, rlen) : bqueue<a> ) : bqueue<a>
  if (flen >= rlen)
    then Bqueue(front, flen, rear, rlen)
    else Bqueue( SAppend(front, SReverse(rear, SNil)), // append the reversed rear to the front
                 flen + rlen,                          // the front thus contains all elements
                 SNil, 0)                              // meanwhile, the rear is empty


// Occasionally, it can be helpful to perform only a single step of evaluation
// on a lazy constructor. Koka provides the special `lazy-step` function for this
// purpose. For example, `lazy-step(xs)` returns unit, but under the hood, it will
// evaluate `xs` to `Cons1(1, Append1(Cons1(2, Nil1), Cons1(3, Nil1)))`.

// Note that in `SAppend(front, SReverse(rear, SNil))`, the second argument of
// `SAppend` is always a `SReverse` constructor. This motivates our use of
// `lazy-step` in the definition of `SAppend`. Each time we perform one step of
// the stream concatenation, we also perform one step of the reverse operation.
// This means that once we have fully appended the two streams, we are also
// (almost) done with the reverse operation. This gives our implementation
// a worst-case time complexity of O(log(n)), compared with Okasaki's O(n).

// We snoc an element to the end of the queue by adding it to the rear stream
// and check that our invariant is maintained.
fun snoc( Bqueue(front,flen,rear,rlen) : bqueue<a>, x : a ) : bqueue<a>
  Bqueue(front, flen, SCons(x,rear), rlen+1).check

// We can uncons an element from the front of the queue. By our invariant, if the front
// stream is empty, so is the rear stream. Otherwise, we return the head of the front stream
// and check that our invariant is maintained on the remaining elements.
fun uncons( Bqueue(front,flen,rear,rlen) : bqueue<a> ) : div maybe<(a,bqueue<a>)>
  match front
    SCons(x,xx) -> Just((x, Bqueue(xx,flen - 1,rear,rlen).check))
    SNil        -> Nothing

fun show( Bqueue(front,flen,rear,rlen) : bqueue<a>, ?show : a -> string ) : exn string
  "Bqueue(" ++ debug-show(front) ++ ", flen=" ++ show(flen) ++ ", " ++ debug-show(rear) ++ ", rlen=" ++ show(rlen) ++ ")"

// Show a queue during unconsing
pub fun main()
  val queue = bempty.snoc(1).snoc(2).snoc(3).snoc(4).snoc(5)
  queue.show.println
  fold(0, 4, queue) fn(_, q)
    val Just((_,qtail)) = q.uncons()
    qtail.show.println
    qtail
  ()

