type nat {
  Zero
  Succ(succ:nat)
}


function ack(x,y) {
  match(x) {
    Zero -> Succ(y)
    Succ(s) -> ack(s,match(y) {
                       Zero -> Succ(Zero)
                       Succ(t) -> ack(x,t) })
  }
}

