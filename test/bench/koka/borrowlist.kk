val n  = 1000000
val xs = list(1,n)

fun lookup2( xs : list<a>, pred : a -> bool) : maybe<a>
  match xs
    Cons(x,xx) -> if pred(x) then Just(x) else lookup2(xx,pred)
    Nil -> Nothing

fun lookup1( ^xs : list<a>, pred : a -> bool) : maybe<a>
  match xs
    Cons(x,xx) -> if pred(x) then Just(x) else lookup1(xx,pred)
    Nil -> Nothing

pub fun test1()
  repeat(100) 
    xs.lookup1( fn(x) x == n )
    ()

pub fun test2()
  repeat(100) 
    xs.lookup2( fn(x) x == n )
    ()    
