
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


/*
get{ 
  x <- ref->value  
  dup(x)
}

set(y){
  x <- ref->value
  ref->value=y
  drop(x)  
}
*/
/*
fun rev(xs : list<a>, acc : list<a> ) : list<a> {
  match(xs) {
    Cons(x,xx) -> rev(xx,Cons(x,acc))
    Nil        -> acc
  }
}
*/
/*

type tree {
  Bin( left: tree, right: tree )
  Tip( value : int )
}

type spine {
  Done
  BinL( left:tree, spine: spine )
  BinR( right:tree, spine: spine )
}

fun spinemap(f : int -> int, t : tree, spine : spine) : div tree {
  match(spine) {
    Done -> t
    BinR(r,s) -> binmap(f,r, BinL(t,s))
    BinL(l,s) -> spinemap(f, Bin(l,t), s)
  }
}

fun binmap( f : int -> int, t : tree, spine : spine) : div tree 
{
  match(t) {
    Bin(l,r) {
      binmap(f,l,BinR(r,spine))  // reuse bin      
    }
    Tip(x) {
      spinemap(f,Tip(f(x)),spine)      
    }
  }
}

type direction {
  Up
  Down
}

fun sbmap(f : int -> int, t : tree, spine : spine, d : direction) : div tree {
  if (d.down?) {      
    // do down the left spine
    match(t) {
      Bin(l,r) {
        sbmap(f,l,BinR(r,spine),Down)  // go down left, and remember the right subtrees 
      }
      Tip(x) {
        sbmap(f,Tip(f(x)),spine,Up)    // go up through the saved spine
      }
    }
  }
  else {
    // go up through the saved spine
    match(spine) {
      Done      -> t                  
      BinR(r,s) -> sbmap(f,r,BinL(t,s),Down)   // down into a right subtree, and remember to Bin later
      BinL(l,s) -> sbmap(f, Bin(l,t), s, Up)   // restore a Bin node and keep going up
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
*/
