val n  = 1000000
val xs = list(1,n)
  
fun lookup( xs : list<a>, pred : a -> bool) : maybe<a>
  match xs
    Cons(x,xx) -> if pred(x) then Just(x) else lookup(xx,pred)
    Nil -> Nothing

pub fun main()
  repeat(1000) 
    xs.lookup( fn(x) x == 0 )  
    ()

