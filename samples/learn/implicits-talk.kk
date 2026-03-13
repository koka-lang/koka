// "Syntactic Implicit Parameters with Static Overloading", Daan Leijen and Tim Whiting
//
// See: https://www.microsoft.com/en-us/research/publication/syntactic-implicit-parameters-with-static-overloading-extended-version/
//
module learn/implicits-talk

// Idea 1: allow qualified names everywhere
//
// Suppose we have some top-level show definitions
// where we use _local qualification_ to disambiguate them
// 
// Note: this already occurs naturally in many languages if you 
// import `myshow` definitions from different modules.

fun int/myshow( x : int ) : string
  show(x)

fun character/myshow( x : char ) : string
  show(x)

fun bool/myshow( x : bool ) : string
  show(x)

// we can now write explicitly:
fun test1()
  character/myshow('n')







// Idea 2:
//
// _static overloading_ is strictly an elaboration from plain names 
// to qualified names based on the type context.

fun test2a()
  myshow(1)            // elaborates to int/myshow(1)

fun test2b()
  myshow('a')

// but no abstraction!
fun wrong/poly(x)
  myshow(x)






// Idea 3:
//
// _Syntactic_ implicit parameters are parameter names that are  
// supplied literally as arguments _at the call site_,
//
// (i.e. they are resolved by name at the call site, essentially a form of dynamic binding)

// Write a function that needs a _base_ parameter:
fun natshow( x : int, ?base : int ) : div string
  if x > base 
    then natshow( x / base ) ++ natshow( x % base )
    else show-digit(x)

fun test3()     
  val base = 16
  natshow(42)    // is elaborated to `natshow(42,base)` where `base` becomes the last parameter








// Each of these three ideas is simple and straightforward to implement, 
// but their combination is surprisingly expressive!
// (as they invoke each other recursively)

fun list/myshow( xs : list<a>, ?myshow : a -> string ) : string
  match xs
    Cons(x,xx) -> myshow(x) ++ "::" ++ myshow(xx) 
    Nil        -> "[]"

fun test4()
  list/myshow( [1,2], int/myshow) 












fun test4a()
  myshow( [1,2] )
  // 1. Qualify based on the type context `list<int> -> _a`
  //    elaborates to: `list/myshow([1,2])`
  //
  // 2. Add the implicit parameter for `list/myshow : (list<a>, ?show:a->string) -> string`
  //    elaborates to: `list/myshow([1,2], myshow)`      
  //
  // 3. Qualify the (now explicit) argument `myshow` with type context `int`
  //    elaborates to: `list/myshow([1,2],int/myshow)`
  // done.



fun test4b() 
  myshow(['a','b']) 

fun test4c() 
  list/myshow([[1],[2,3]])



// or with tuples
fun tuple/myshow( (x,y) : (a,b), ?fst/myshow : a -> string, ?snd/myshow : b -> string ) : string
  "(" ++ myshow(x) ++ "," ++ myshow(y) ++ ")"

// or explicitly without types as:
fun tuple/myshowx( (x,y), ?fst/myshow, ?snd/myshow )
  "(" ++ fst/myshow(x) ++ "," ++ snd/myshow(y) ++ ")"

fun test5a()
  tuple/myshow( (1,'a'), myshow, myshow )


fun test5() 
  myshow([(1,['a']),(2,['b'])])    
  // this elaborates to:
  // `list/myshow([(1,['a']),(2,['b'])], ?myshow=tuple/myshow(_,?fst/myshow=int/myshow,?snd/myshow=list/myshow(_,char/myshow)))`





// We need to limit the search or otherwise we can have infinite resolves.
// Consider:
fun foo( x : int, ?foo : int -> int )  : int
  ?foo(x)

fun wrong/recurse()
  foo(1)




// Type Classes always automatically abstract where
// `myshow x = show x` will have type `Show a => a -> String`.
// But with static overloading we need to be explicit about this.
fun wrong/myshow( x ) : string
  show(x)

fun good-myshow( x, ?show ) : string
  show(x)


// Unlike Haskell Type classes, we don't declare upfront
// and therefore, there is no restriction on the type signature
fun tuple/first( (x,_) : (a,b) ) : a
  x

fun triple/first( (x,_,_) : (a,b,c) ) : a
  x

fun test-first()
  (1,True).first + (2,'a',3.0).first


// Similarly, functional dependencies and associated types
// arise naturally through regular type signatures
// For example the types of `a` and `b` below 
// determine `c` based on the instantiated types and available definitions for `(+)`.
fun zipsum(xs: list<a>, ys: list<b>, ?(+): (a, b) -> c) : list<c>
  zipwith(xs,ys,(+))

// The following definition is how we may use parameterized maps with keys and values
fun naive-union-parameterized(map1: m<k,v>, map2: m<k,v>, ?insert: (m<k,v>, k, v) -> m<k,v>) : m<k,v>
  map1

// However, we can just as well make specific ones using, say, bitmaps for
// sets of booleans using specific types (without needing the concept of associated types)
fun naive-union-associated(map1: m, map2: m, ?insert: (m, k, v) -> m) : m
  map1

// We can group operations
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




// Phantom implicits
// For some builtins the compiler can supply a special value (namely, kk-line and kk-file)
fun assert-line( condition : bool, msg : string, ?kk-line : string ) : exn ()
  if condition then () else throw("at " ++ kk-line ++ ": " ++ msg)

fun test-assert1()
  assert-line(False,"test1")   // kk-line is supplied as the current line 



// A fancy phantom implicit is `hdiv` of type `hdiv<h,a,e>`
// It captures the constraint that `e` must contain the `div` effect
// if the type `a` possibly contains the heap type `h`
//
// The following function is not recursive ... but Koka still 
// infers the `div` effect! How? Because the dereferencing requires 
// such implicit `hdiv` "evidence": `fun ref/(!)( r : ref<h,a>, ?hdiv : hdiv<h,a,e>) : <read<h>|e> a`
//
fun landin()                // div is inferred 
  val r = ref( fn() () )    // initialize with a parameter-less function that returns unit
  fun self()
    (!r)()                  // self calls the function in the reference
  r := self                 // store self in r now
  self()                    // and call it (recursively even though that is not apparent in the syntax)


// and we can _abstract_ over these builtin values
fun assert-fline(condition : bool, msg : string, ?kk-line : string, ?kk-file : string ) : exn ()
  assert-line(condition, kk-file ++ ": " ++ msg )

fun test-assert2()
  assert-fline(False,"test2")




fun show-digit(x : int) : string
  string(if x < 10 then '0' + x.char else 'A' + (x - 10).char)



