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
