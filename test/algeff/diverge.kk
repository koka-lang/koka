// --------------------------------------------------------
// 
// --------------------------------------------------------

effect diverge {
  fix( f : (a -> diverge b, a) -> diverge b, x : a ) : b
}

val diverge = handler {
  return x  -> x
  fix(f,x)    -> resume( f(fun(y){ fix(f,y) }, x) ) 
}

effect mut {
  newref(x : a) : mref<a>
  getref(r : mref<a> ) : a
  setref(r : mref<a>, x : a) : ()
}

