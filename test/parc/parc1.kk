fun foo1(x,y) { x }
fun foo2(x:int,y:int) { x + x }

fun match1(xs:list<int>, y:int) {    // drop y once
  match(xs) {
    Cons(x,_) -> match(xs){
                    Cons -> x
                    Nil -> 2
                  }
    Cons -> 2
    _ -> y
  }
}

fun match2(xs:list<int>, y:int, z :int) {    // drop y once, drop z early
  match(xs) {
    Cons(x,_) -> match(xs){
                    Cons -> x
                    Nil -> 2
                  }
    Cons -> 2
    _ -> y
  }
}


fun match3(xs:list<int>) {
  match(xs) {
    Cons as ys -> ys  // don't dup(ys); drop(xs)
    Nil -> []
  }
}

fun match4(xs:list<a>, ys:list<a>): list<a> {
  match(xs) {
    Cons(z) -> match(ys) {
                 Cons(z) -> [z]
                 Nil -> xs
               }
    Nil -> ys
  }
}

fun match5(xs:list<int>) {
  match(xs) {
    // this 'ys' cannot be changed to 'xs' to be even more ambiguous
    Cons(xs) as ys -> xs
    Nil -> 1
  }
}
