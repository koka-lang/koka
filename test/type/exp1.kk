pub type xlist<a>
  Cons( head : a, tail : xlist<a> )
  XNil

/*
pub type ylist<a>
  ZCons( hd : a, tl : ylist<a> )
  YNil
*/

// checks potential exponential type check time
pub fun test1( x,y )
  Cons(x,Cons(y,Cons(x, Cons(y,(Cons(x,Cons(y,Cons(x, Cons(y, XNil )))))))))
