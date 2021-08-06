// private fun test(x) {
//   if (not(x)) then x else False
//   /*
//   r = if (x) then False else True
//   if r then x else False
//   ~> { case-of-case }
//   if (x) then (if False then x else False) else (if (x) then x else False)
//   ~>
//   if (x) then False else x
//   */
// }

private fun someBoolFunc(x, y) {
  (x && y) || (x && !y)
}

// fun justTrue() {
//   True
// }

// fun andTrue(){
//   val x = True
//   val y = True
//   print("blah")
//   x && y
// }

fun boolMatching() {
  // test(True).println

  // match(True) {
  //   True -> "hi".println
  //   False -> "no".println
  // }

  // if(True) then "T" else "F"
  // if(True && justTrue()) then "T" else "F"
  // if(justTrue()) then "JT" else "JF"
  // if(justTrue() || justTrue()) then "JT" else "JF"
  // if(andTrue()) then "AT" else "AF"// Works when there is no effect in the function. Edit: works now with effect too
  
  // if(printTrue(True, True)) then "PT" else "PF"
  // if(printTrue2(True, True)) then "PT2" else "PF2"
  // val x = True
  // var x := True
  // Works when x is a val, but does not when x is a var
  // if(x) then "JT" else "JF"

  someBoolFunc(True, False).println
}

// fun printTrue(x: bool, y: bool) {
//   print("printTrue")
//   x || y
// }

// fun printTrue2(x: bool, y: bool) {
//   print("printTrue2")
//   printTrue(x, y)
// }

// fun divSomething(x: int, y: int) {
//   var z := x / y
//   True
// }

// effect raise {
//   control raise( msg : string ) : a
// }

// fun safe-divide( x : int, y : int ) : raise int {
//   if (y==0) then raise("div-by-zero") else x / y
// }

// fun raise-const() : int {
//   with handler {
//     control raise(msg){ 42 }
//   } 
//   8 + safe-divide(1,0)
// }


// True path not simplified - Haskell Program Transformation paper example - join points
/*Rather than somehow
mark el as special, the code generator does a simple syntactic escape analysis
to identify variables whose evaluation is certain to take place before the stack
retreats, and implements their evaluation as a simple adjust-stack-and-jump. As
a result we get essentially the same code as a C compiler for our conditional.*/

private fun or(x:bool, y:bool) {
  if (x || y) then "OR_True" else "OR_False"
  match(x) {
    True -> "True"
    _    -> y
  }
}

// fun maybeMatching() {
//   match(Just(1)) { // Maybe - case of known function not called in case of Just(x), works in case of Nothing
//     Just(value) -> "1"
//     Nothing -> "0"
//   }
// }

// fun listMatching() {
//   match(Cons(1, Cons(2, []))) { // Not working as well, case of known function called. Works now
//     Cons(x, xs) -> "Non empty"
//     Nil -> "Empty"
//   }
// }


// fun patConMatching() {
//     match(printTrue(True, True) || True, printTrue(False, False)) {
//       (True, True) -> "First"
//       (False, True) -> "Second"
//       (True, False) -> "Third"
//       (False, False) -> "Fourth"
//   }
// }

// fun printAndReturnTen() {
//   println("Printing 10")
//   10
// }

// fun patLitMatching() { // Works now
//   match(printAndReturnTen()) {
//     1 -> "One"
//     5 -> "Five"
//     10 -> "Ten"
//     _ -> "Def"
//   }
// }

// fun patLitMatchingString() { // Works now
//     match("A") { 
//     "a" -> "lower"
//     "b" -> "no match"
//     "A" -> "found"
//   }
// }

// fun patLitMatchingNum() { // Works now
//     match(1) {
//     1 -> "1"
//     2 -> "2"
//     3 -> "3"
//   }
// }

// fun patWildMatching() { // Works
//     match(True) {
//     _ -> "WildTrue"
//   }
// }

fun foo() { // Works, returns 3 + 4
  val x = match(True, False) {
     (True, y as _) -> return 1
     (False, True) | (True == False) || (False == True) -> 2
     _ -> 3
  }
  x + 4
}

fun sideeffecting_nil() {
  print("Side effecting nil")
  Nil
}

fun main() {
  // if(divSomething(4, 2)) then "DT" else "DF" // Doesn't work

  match (Cons(Cons(foo(),Nil),sideeffecting_nil())) {
    Cons(x as _, _) -> x ++ x
    _ -> []
  }

  // match(Just(10)) { // Maybe - works in case of Nothing, Just involved App
  //   Just(value) -> "1"
  //   Nothing -> "0"
  // }
  
  // match(Left(1)) { // Not working
  //   Left(x) -> "left"
  //   Right(x) -> "Right"
  // }

  // match((1, 1)) { // Not working
  //   (2, 2) -> "both 2"
  //   (1, 2) -> "second 2"
  //   (2, 1) -> "first 2"
  //   (1, 1) -> "found"
  // }
}


/*

scrutinee ~ pattern   ~> E | newscrutexpr

Our E is a list of bindings: val x1 = e1; ...; val x_n = e_n


---------------
// side on operational semantics
where E is a context (of type: expr -> expr): an expression with a hole []  (identity)
For example (let .. in [])

E ::= [] | E expr | val E | let x = E in expr 


(let) let x = val in expr  --->  expr[x := val]
(app) (\x -> e) val        --->  e[x := val]

                e ---> e'
          --------------------
(bigstep)  E[e]  |--->  E[e'] 

---------------------

e ::= @c e1 .. en
   |  x
   |  \x -> e
   |  e1 .. en        application
   |  lit
   |  /\a1..an. e 
   |  e t1 .. tn

p ::= _
   |  x @as p
   |  @c p1 ... pn
   |  lit
   
Single pattern:
   
-------------------------------------    PatWild
e  ~  \_   ~> @match [] | e


e  ~  p  ~> @match E | e'  
-------------------------------------------    PatVar
e  ~  x @as p   ~> @match E;@val x = e' | x  


e_i  ~  p_i  ~> @match E_i | e'_i   
------------------------------------------------------------------------  PatCon
@c e1 ... en  ~  @c p1 ... pn   ~> @match E_1;...;E_n | @c e'_1 ... e'_n

(note: @c e1 ... en  looks like App (Con ..) [args]  _or_ App (TypeApp Con [typeargs) [args])


e_i  ~  p_i  ~> x_i   x = if any x_i @nomatch then @nomatch else @unknown
------------------------------------------------------------------------  PatCon-Fail
@c e1 ... en  ~  @c p1 ... pn   ~> x


@c1 ...  ~  @c2 ...   ~>  @nomatch
x        ~  any       ~>  @unknown
\x -> e  ~  any       ~>  @nomatch
e1 .. en ~  any       ~>  @nomatch


Multi-pattern rules:

e_i ~ p_i  ~>  @match E_i | e'_i
-----------------------------------------------------------
e1 ... en  ~~  p_1 ... p_n   ~>  @match E_1;e'_1;...;E_n;e'_n


(implied: if any e_i is @nomatch or @unknown; the whole thing is @nomatch/@unknown)
(also if any is @nomatch, then @nomatch)


Match rule:

 e_1 ... e_n ~~ pj_1 ... pj_n  ~> @nomatch   (forall j. j < i)
 e_1 ... e_n ~~ pi_1 ... pi_n  ~> @match E
---------------------------------------------------------------
@match e_1 ... e_n \{ 
  p1_1 ... p1_n  -> b1
  ...
  pm_1 ... pm_n  -> bm
\}  ~> E;bi


+ fail


For example:

match (Cons (Cons(foo(),Nil),sideeffecting_nil()) {
  Cons( x as \_, \_) -> x && x
}
~>
val x = Cons(foo(),Nil); Cons(x,sideeffecting_nil()); x && x

*/