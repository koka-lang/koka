/*
  r = if (x) then False else True
  if r then x else False
  ~> { case-of-case }
  if (x) then (if False then x else False) else (if (x) then x else False)
  ~>
  if (x) then False else x
*/
private fun test(x) {
  if (not(x)) then x else False
}

private fun bool1() {
    test(True).println
}

private fun someBoolFunc(x, y) {
  (x && y) || (x && !y)
}

private fun bool2() {
  someBoolFunc(True, False).println
}

private fun bool3() {
  match(True) {
    True -> "hi".println
    False -> "no".println
  }
}

private fun bool4() {   
  if(True) then "T" else "F"
}
private fun bool5() { 
  if(True && justTrue()) then "T" else "F"
}
private fun bool6() { 
  if(justTrue()) then "JT" else "JF"
}
private fun bool7() { 
  if(justTrue() || justTrue()) then "JT" else "JF"
}
private fun bool8() { 
  if(andTrue()) then "AT" else "AF" // Works when there is no effect in the function. Edit: works now with effect too
}
private fun bool9() { 
  if(printTrue(True, True)) then "PT" else "PF"
}
private fun bool10() { 
  if(printTrue2(True, True)) then "PT2" else "PF2"
}
private fun bool11() { 
  val x = True
  // var x := True
  if(x) then "XT" else "XF" // Works when x is a val, but does not when x is a var
}


fun justTrue() {
  True
}

fun andTrue(){
  val x = True
  val y = True
  print("blah")
  x && y
}

fun printTrue(x: bool, y: bool) {
  print("printTrue")
  x || y
}

fun printTrue2(x: bool, y: bool) {
  print("printTrue2")
  printTrue(x, y)
}


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
}

fun maybeMatching() { // Works after the subpatter matching changes
  match(Just(10)) { // Maybe - case of known function not called in case of Just(x), works in case of Nothing
    Just(value) -> value
    Nothing -> 0
  }
}

fun listMatching() { // All work after the subpattern matching changes
  match(Cons(1, Cons(2, []))) { // Not working as well, case of known function called. Works now
    Cons(x, xs) -> "Non empty: " ++ show(x)
    Nil -> "Empty"
  }
}

fun patConMatching() {
    match(printTrue(True, True) || True, printTrue(False, False)) {
      (True, True) -> "First"
      (False, True) -> "Second"
      (True, False) -> "Third"
      (False, False) -> "Fourth"
  }
}

fun printAndReturnTen() {
  println("Printing 10")
  10
}

fun patLitMatching() { // error: '_null_' undeclared (first use in this function)
  match(printAndReturnTen()) {
    1 -> "One"
    5 -> "Five"
    10 -> "Ten"
    _ -> "Def"
  }
}

fun patLitMatchingString() { // Works now
  match("A") { 
    "a" -> "lower"
    "b" -> "no match"
    "A" -> "found"
  }
}

fun patLitMatchingNum() { // Works now
  match(1) {
    1 -> "1"
    2 -> "2"
    3 -> "3"
  }
}

fun patWildMatching() { // Works
  match(True) {
    _ -> "WildTrue"
  }
}

fun foo() { // Works
  val x = match(False, False) {
     (False, True) | (True == False) || (False == True) -> 2
     (True, y as _) -> return 1
     (_, _) -> 3
  }
  x + 4
}

// 1.   y as Con  -> substitute y = Con
// 2.   y as Con(pat1,...,pat_n)  -> ??  we need shape environment: y |-> Con(pat1,...,patN)
// and then use that in the kmatch functions  y ~ Con(...)
// 3.   use this as well for "match(x}{ Con(..) -> e1; ... }"  with: x |-> Con in e1

fun foo2() { // Works
  val x = match(False, False) {
     (False, y as True) | (True == False) || (False == True) -> 2                        
     (True, y as _) -> return 1
     (False, y as False) | not(y) -> return 1
                         | not(not(y)) -> 4
     (_, _) -> 3
  }
  x + 4
}

fun sideeffecting_nil() {
  print("Side effecting nil")
  Nil
}

fun multiConsWithEffect() {
  match (Cons(Cons(foo(),Nil),sideeffecting_nil())) { // Works after the subpattern changes
    Cons(x as _, _) -> x ++ x
    _ -> []
  }
}

fun eitherMatching() { // Works after subpattern changes
  match(Left(1)) {
    Left(x) -> "left"
    Right(x) -> "Right"
  }
}

fun tupleMatching() { // Works after subpattern changes
  match((1, 1)) {
    (2, 2) -> "both 2"
    (1, 2) -> "second 2"
    (2, 1) -> "first 2"
    (1, 1) -> "found"
  }
}

fun main() {
  // if(divSomething(4, 2)) then "DT" else "DF" // Doesn't work
  
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

e_i ~ p_i  ~>  @match E_i | e'_i  (forall i)
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