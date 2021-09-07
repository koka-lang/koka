fun foo() { 
  val x = match(False, False) {
     (False, True) | (True == False) || (False == True) -> 2
     (True, y as _) -> return 1
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
