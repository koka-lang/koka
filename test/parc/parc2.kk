fun match1(xs:list<int>, y:int, z :int) {    
  match(xs) { 
    Cons(x,xx) | x == z -> x
    Cons(x,xx) | x == y -> y
    Cons | y == z -> 2
    _ -> y
  } 
}
