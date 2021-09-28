/* Basic effect handler examples
   Prerequisites:
   - syntax/basic
   - syntax/var
   - syntax/with
*/

module basic

import std/num/random

// --------------------------------------------------------
//  Exceptions
// --------------------------------------------------------

// An effect `:raise` with a single control operation `raise`.
// This is equivalent to writing:
// ```
// effect raise {
//   ctl raise(s:string) : a
// }
// ```

effect ctl raise(s : string ) : a

fun safe-div(x,y) 
  if (y==0) then raise("division by zero") else x / y


fun rcatch(action,h) 
  with ctl raise(s){ h(s) }     // handle `raise` with function `h`
  action()                      // in `action()`


fun zerodiv(x,y) 
  rcatch( { safe-div(x,y) }, fn(s){ 0 } )


// reify the ctlion effect back to a `:maybe` value
fun to-maybe(action) 
  with ctl raise(_s){ Nothing }
  Just(action())


public fun test1() 
  zerodiv(10,0).println



// --------------------------------------------------------
//  Input
//  Essentially models dynamically bound state
// --------------------------------------------------------

// Declare an effect `:input` with a single operation `input`.
// We use `fun` instead of general `control` to ensure that all handlers
// will always resume (at most once) with the result.
effect fun input() : string

fun hello()
  val name = input()
  println("Hello " ++ name ++ ", " ++ input())  // "Hello <input>, <input>"


public fun test2() 
  with fun input(){ "there" }   // handle input; a `fun` operation resumes implicitly
  hello()



// --------------------------------------------------------
//  State
// --------------------------------------------------------

effect state<s> 
  fun get() : s
  fun put( x : s ) : ()


fun counter() 
  val i = get()
  if (i <= 0) then () else
    println("hi")
    put(i - 1);
    counter()


// State handler.
fun state(init, action)
  var s := init              // use a local mutable variable
  with handler
    return(x) { (x,s) }
    fun get() { s }
    fun put(x){ s := x }
  action()


public fun test3() 
  state(1,counter).snd



// --------------------------------------------------------
//  Iterators
// --------------------------------------------------------

// Now we use general control `ctl` as we want to enable breaking
// the interation (and thus not resume). Equivalent to:
// ````
// effect yield<a> {
//   ctl yield( item : a ) : ()
// }
// ````
effect<a> ctl yield( item : a ) : ()

fun iterate(xs : list<a> ) : yield<a> () 
  match xs
    Cons(x,xx) -> { yield(x); iterate(xx) }
    Nil -> ()
    

fun foreach( f : a -> e bool, action : () -> <yield<a>|e> () ) : e () 
  with ctl yield(x){ if f(x) then resume(()) else () }  // resume is explicit in a `ctl` operation
  action()


public fun test4() 
  foreach( fn(x:int){ println(x); x <= 1 } )
    [1,2,3].iterate

fun test4b() 
  with return(x){ () }               // just force final result to ()
  with amb
  with foreach fn(x:int)
    if flip() then println(x) else println("flip false " ++ x.show)
    x <= 1  
  
  [1,2,3].iterate


// --------------------------------------------------------
//  Resume more than once
// --------------------------------------------------------

effect ctl flip() : bool

fun xor() : flip bool 
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)


// handler that randomly flips
val coinflip = handler 
  fun flip() srandom-bool()      // we can still define an operation as `fun` even if declared as `ctl`


// handler that returns all possible outcomes
val amb = handler 
  return(x)  [x] 
  ctl flip() resume(False) ++ resume(True)


public fun test5() 
  amb(xor).show.println



// --------------------------------------------------------
//  Combine effects
// --------------------------------------------------------

fun surprising() : <flip,state<int>> bool 
  val p = flip()
  val i = get()
  put(i+1)
  if (i>0 && p) then xor() else False


public fun test6() 
  state(0){ amb(surprising) }.show.println

public fun test7()
  amb{ state(0,surprising) }.show.println


// --------------------------------------------------------
//  Testing
// --------------------------------------------------------
public fun main() 
  test1()
  test2()
  test3()
  test4()
  test5()
  test6()
  42


fun show( xs : list<(bool,int)> ) : string  
  xs.show-list fn(x) 
    x.show-tuple(show,show)


fun show( x : (list<bool>,int) ) : string  
  x.show-tuple(show,show)

