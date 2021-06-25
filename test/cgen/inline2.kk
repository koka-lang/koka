
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

private fun andd(x, y) {
  (x && y) || (x && !y)
}

fun main() {
  
  // match(True) {
  //   True -> "hi".println
  //   False -> "no".println
  // }
  
  // Works now
  // test(True).println

  // Works now
  // if(True) then "T" else "F"

  // Works
  andd(True, False).println

  // Case of known function itself not called
  // match(1) {
  //   1 -> "1"
  //   2 -> "2"
  //   3 -> "3"
  // }

// Does not work -> case of known function not called
  // match("A") {
  //   "a" -> "lower"
  //   "b" -> "no match"
  //   "A" -> "found"
  // }

// Maybe - case of known function not called in case of Just(x), works in case of Nothing
  // match(Nothing) {
  //   Just(value) -> "1"
  //   Nothing -> "0"
  // }

  // Below is not working as well, case of known function called. Works now
  // match([]) {
  //   Cons(x, xs) -> "Non empty"
  //   Nil -> "Empty"
  // }
  
  // Either - not working
  // match(Left(1)) {
  //   Left(x) -> "left"
  //   Right(x) -> "Right"
  // }

  // Not working
  // match((1, 1)) {
  //   (2, 2) -> "both 2"
  //   (1, 2) -> "second 2"
  //   (2, 1) -> "first 2"
  //   (1, 1) -> "found"
  // }
}
