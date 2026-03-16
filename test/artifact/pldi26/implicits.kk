// All code examples from the paper
// "Syntactic Implicit Parameters with Static Overloading"
//
// Run as:
// ```
// stack exec koka -- -e test/artifact/pldi26/implicits.kk
// ```
//
// In VS Code, press Ctrl+Alt (Ctrl+Option on macOS) to see inferred
// implicits and qualified names displayed as inlay hints.
module implicits

// --------------------------------------------------------
// Section 1, Introduction
// --------------------------------------------------------

// Write a function that needs a _base_ parameter.
fun show-int( x : int, ?base : int ) : div string
  if x > base
    then show-int( x / base ) ++ show-int( x % base )
    else show-digit(x)

fun test1a()
  val base = 10
  show-int(42)    // is elaborated to `show-int(42,base)` where `base` becomes the last parameter

fun test1b()
  val base = 16
  show-int(42)    // also elaborated to `show-int(42,base)`

// Illustrate locally qualified names
// (note: we use `myshow` instead of `show` to avoid nameclashes with the std/core library)
fun int/myshow( x : int ) : string
  show(x)

fun character/myshow( x : char ) : string
  show(x)

fun bool/myshow( x : bool ) : string
  show(x)

// we can always use explicit names:
fun test1c()
  character/myshow('a')

// or let static overloading resolve it based on the type context
fun test1d()
  myshow('a')            // elaborates to `character/myshow('a')`

fun test1e()
  myshow(1)              // elaborates to `int/myshow(1)`

// but we have no abstraction!
fun wrong/list-myshow(xs : list<a> ) : string
   match xs
    Cons(x,xx) -> myshow(x) ++ "::" ++ list-myshow(xx) // rejected
    Nil        -> "[]"


// we can use an implicit parameter though
fun list/myshow( xs : list<a>, ?myshow : a -> string ) : string
  match xs
    Cons(x,xx) -> myshow(x) ++ "::" ++ myshow(xx)
    Nil        -> "[]"

fun test1f()
  list/myshow( [1,2], int/myshow)   // explicit version

fun test1g()
  myshow( [1,2] )       // elaborated to `list/myshow( [1,2], int/myshow)`

fun test1h()
  myshow( [[1],[2]] )   // elaborated to `list/myshow( [[1],[2]], list/myshow(_,int/myshow))`

fun wrong/unresolved()
  myshow([])            // element type is not known



// --------------------------------------------------------
// Section 2, Formalization
// --------------------------------------------------------

fun test2a()
  fun mod/plus( x, y, ?mod )
    (x+y) % mod
  val mod = 8
  mod/plus(5,6)         // elaborates to `mod/plus(5,6,mod)`


// showing tuples with multiple implicit parameters (Section 2.7)
fun tuple/myshow( (x,y) : (a,b), ?fst/myshow : a -> string, ?snd/myshow : b -> string ) : string
  "(" ++ fst/myshow(x) ++ "," ++ snd/myshow(y) ++ ")"  // we can actually write `myshow(x)`,`myshow(y)` as well

fun test2b()
  myshow((1,'a'))     // elaborates to `tuple/myshow((1,'a'),int/myshow,character/myshow)`

fun test2c()
  myshow([(1,['a']),(2,['b'])])
  // this elaborates to:
  // `list/myshow([(1,['a']),(2,['b'])], ?myshow=tuple/myshow(_,?fst/myshow=int/myshow,?snd/myshow=list/myshow(_,character/myshow)))`



// --------------------------------------------------------
// Section 3, Implementation
// See <busy-beaver.kk> for the Busy Beaver example
// --------------------------------------------------------

// We need to limit the search or otherwise we can have infinite resolves. (Section 3.1)
fun foo( x : int, ?foo : int -> int )  : int
  ?foo(x)

fun wrong/recurse()
  foo(1)



// --------------------------------------------------------
// Section 4, Syntactic Implicits and Static Overloading in Practice
// --------------------------------------------------------


fun test4a()
  myshow( (1,True), ?snd/myshow=fn(x) "*")   // pass an implicit parameter explicitly

// Unlike Haskell Type classes, we don't declare types upfront
// and there is no restriction on the type signature
fun tuple/first( (x,_) : (a,b) ) : a
  x

fun triple/first( (x,_,_) : (a,b,c) ) : a
  x

fun test4b()
  (1,True).first + (2,'a',3.0).first

// comparison functions (Section 4.1)
fun bool/cmpx( x : bool, y : bool ) : order
  cmp(x,y)

fun int/cmpx( x : int, y : int ) : order
  cmp(x,y)

fun character/cmpx( x : char, y : char ) : order
  cmp(x,y)

// define a generic equality operation in the default namespace
fun default/is-equal( x : a, y : a, ?cmpx : (a,a) -> order ) : bool
  match cmpx(x,y)
    Eq -> True
    _  -> False

// and a specialized one for int's
fun int/is-equal( x : int, y : int ) : bool
  x==y

// resolved without ambiguity as `default/` is in an outer namespace
fun test4c()
  (is-equal(1,2) && is-equal('a','b'))  // elaborates to `int/is-equal(1,2) && default/is-equal('a','b',character/cmpx)`



// We can group operations (Section 4.2)
struct num<a>
  (+) : (a,a) -> a
  (*) : (a,a) -> a

// and provide some "instances"
val int/num : num<int> = Num((+),(*))

// and use "dot" notation to unpack the members concisely
fun fma( x : a, y : a, z : a, .?num : num<a> ) : a
  (x*y) + z

// the above is just syntactic sugar for:
fun fma-explicit( x : a, y : a, z : a, ?num : num<a> ) : a
  match num
    Num((+),(*)) -> (x*y) + z


// Phantom implicits (Section 4.3)
// For some builtins the compiler can supply a special value (namely, kk-line and kk-file)
fun assert-line( condition : bool, msg : string, ?kk-line : int ) : console ()
  if condition then () else println("custom assertion failed at " ++ kk-line.myshow ++ ": " ++ msg)

fun test4d()
  assert-line(False,"test1")   // kk-line is supplied as the current line

// and we can _abstract_ over these builtin values
fun assert-fline(condition : bool, msg : string, ?kk-line : int, ?kk-file : string ) : console ()
  assert-line(condition, kk-file ++ ": " ++ msg )    // passes kk-line as an implicit parameter

fun test4e()
  assert-fline(False,"test2")



// (Section 4.3.1)
// A fancy phantom implicit is `hdiv` of type `hdiv<h,a,e>`
// It captures the constraint that `e` must contain the `div` effect
// if the type `a` possibly contains the heap type `h`
//
// The following function is not recursive ... but Koka still
// infers the `div` effect! How? Because the dereferencing requires
// such implicit `hdiv` "evidence": `fun ref/(!)( r : ref<h,a>, ?hdiv : hdiv<h,a,e>) : <read<h>|e> a`
//
fun landin()                // `div`` effect is inferred
  val r = ref( fn() () )    // initialize with a parameter-less function that returns unit
  fun self()
    (!r)()                  // self calls the function in the reference
  r := self                 // store self in r now
  self()                    // and call it (recursively even though that is not apparent in the syntax)


fun read2( r : ref<h,a>, ?hdiv : hdiv<h,a,e> ) : <read<h>|e> (a,a)
  val x = !r
  (x,x)

fun test4f()
  val r = ref(1)
  read2(r).myshow



// helpers
fun show-digit(x : int) : string
  string(if x < 10 then '0' + x.char else 'A' + (x - 10).char)

fun main()
  println("\nsection 1:")
  test1a().println
  test1b().println
  test1c().println
  test1d().println
  test1e().println
  test1f().println
  test1g().println
  test1h().println
  println("\nsection 2:")
  test2a().println
  test2b().println
  test2c().println
  println("\nsection 4:")
  test4a().println
  test4b().println
  test4c().println
  test4d().println
  test4e().println
  test4f().println
  println("\ndone.")
