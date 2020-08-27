type value slot<a>

extern inline ".resolve-hole"() : a { }
extern inline ".resolve-slot"(x : a, s : string) : slot<a> { }
extern inline ".resolve-next"(acc : slot<a>, x : a, y : a, s : string ) : slot<a> { }
extern inline ".resolve"( acc : slot<a>, x : a ) : () { }

fun mapx( xs : list<a>, f : a -> b ) : list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
    Nil        -> Nil
  }
}

fun append(xs : list<a>, ys :list<a> ) : list<a> {
  match(xs) {
    Cons(x,xx) -> Cons(x,append(xx,ys))
    Nil        -> ys
  }
}

fun ileave( xs : list<a>, ys : list<a> ) : div list<a> {
  match((xs,ys)) {
    (Cons(x,xx), Cons(y,yy)) -> Cons(x, Cons(y, ileave(xx,yy)))
    _ -> Nil
  }
}

type tree {
  Bin( left: tree, right: tree )
  Tip( value : int )
}

type spine {
  Done
  TLeft( left:tree, spine: spine )
  TRight( right:tree, spine: spine )
}

fun spinemap(f : int -> int, t : tree, spine : spine) : div tree {
  match(spine) {
    Done -> t
    TRight(r,s) -> binmap(f,r,TLeft(t,s))
    TLeft(l,s)  -> spinemap(f, Bin(l,t), s)
  }
}

fun binmap( f : int -> int, t : tree, spine : spine) : div tree 
{
  match(t) {
    Bin(l,r) {
      binmap(f,l,TRight(r,spine))  // reuse bin      
    }
    Tip(x) {
      spinemap(f,Tip(f(x)),spine)      
    }
  }
}

fun bmap( f : int -> int, t : tree) : div tree 
{
  match(t) {
    Bin(l,r) -> Bin(bmap(f,l),bmap(f,r))
    Tip(x)   -> Tip(f(x))
  }
}


fun show(t : tree) : string {
  match(t) {
    Bin(l,r) -> "(" + l.show + " * " + r.show + ")"
    Tip(i)   -> i.show
  }
}
