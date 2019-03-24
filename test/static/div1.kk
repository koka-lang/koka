type nat {
  Zero
  Succ( :nat)
}


fun h(x,y) {
  match(x) {
    Zero -> match(y) {
             Zero -> Zero
             Succ(s) -> h(x,s)
            }
    Succ(s) -> h(s,y)
  }
}

