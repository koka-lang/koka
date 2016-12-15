type ord {
  Zero
  Succ(pred:ord)
  Lim(lim:int -> ord)
}


function addord(x,y) {
  match(x) {
    Zero    -> y
    Succ(s) -> Succ(addord(s,y))
    Lim(f)  -> Lim( fun(i:int) { addord( f(i), y ) } )
  }
}

