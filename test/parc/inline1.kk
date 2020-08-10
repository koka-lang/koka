fun test( xs : list<int> ) : int {
  match(xs) {
    Cons(x,Cons(y,_)) -> x + y
    _ -> 42
  }
}
