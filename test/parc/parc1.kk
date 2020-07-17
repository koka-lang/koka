fun foo1(x,y) { x }
fun foo2(x:int,y:int) { x + x }

fun match1(xs:list<int>, y:int) {    // drop y once
  match(xs) { 
    Cons(x,xx) -> match(xs){ 
                    Cons -> x
                    Nil -> 2 
                  } 
    Cons -> 2
    _ -> y
  } 
}

fun match2(xs:list<int>, y:int, z :int) {    // drop y once, drop z early
  match(xs) { 
    Cons(x,xx) -> match(xs){ 
                    Cons -> x
                    Nil -> 2
                  } 
    Cons -> 2
    _ -> y
  } 
}
