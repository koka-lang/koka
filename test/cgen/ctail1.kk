type value slot<a>

extern resolve-hole() : a {
  c inline "kk_box_any"
  js inline "undefined"
}


extern resolve-slot(obj : a, field : string) : slot<a> { }  
extern resolve( acc : slot<a>, x : a ) : () { }
extern resolve-next( acc : slot<a>, obj : a, field : string ) : slot<a> { }


fun mapx( xs : list<a>, f : a -> b ) : list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
    Nil        -> Nil
  }
}

fun ileave( xs : list<a>, ys : list<a> ) : div list<a> {
  match((xs,ys)) {
    (Cons(x,xx), Cons(y,yy)) -> Cons(x, Cons(y, ileave(xx,yy)))
    _ -> Nil
  }
}
