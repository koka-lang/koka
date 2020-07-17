// Test value boxing
fun foo(x,y) {
  x
}

fun match1(xs:list<maybe<int>>, y:int) {    
  match(xs) { 
    Cons(Just(x),xx) | x == y -> x
    Cons(Nothing) -> 2
    _ -> y
  } 
}
