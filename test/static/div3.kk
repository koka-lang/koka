type ord {
  Zero
  Succ(pred:ord)
  Lim(lim:int -> ord)
}


fun addord(x,y) {
  match(x) {
    Zero    -> y
    Succ(s) -> Succ(addord(s,y))
    Lim(f)  -> Lim( fn(i:int) { addord( f(i), y ) } )
  }
}

