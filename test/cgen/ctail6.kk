type tree {
  Bin(l:tree, r:tree)
  Tip(value:int)
}

fun rmap( t : tree, f : int -> int ) : tree {
  match(t) {
    Bin(l,r) -> Bin(l,rmap(r,f))
    Tip(i)   -> Tip(f(i))
  }  
}


fun rmapacc( t : tree, f : int -> int, acc : tree -> tree ) : tree {
  match(t) {
    Bin(l,r) -> {
      rmapacc(r,f,fn(res){ acc(Bin(l,res)) })
    }
    Tip(i) -> {
      acc(Tip(f(i)))
    }
  }  
}
