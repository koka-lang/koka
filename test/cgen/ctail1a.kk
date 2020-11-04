
value type cfield<a>

value type ctail<a> {
  con CTail( :a, :cfield<a> )
  con CTailNil
}

inline extern cfield-hole() : a { }
inline extern cfield-set( field : cfield<a>, res : a ) : () { }
inline extern cfield-of( obj : a, conname : string, fieldname : string ) : cfield<a> { }

inline fun ctail-nil() : ctail<a> {
  CTailNil
}

inline fun ctail-flink( acc : ctail<a>, res : a, field : cfield<a> ) : ctail<a> {
  match(acc) {
    CTail(head,flink) {
      cfield-set(flink,res)
      CTail(head,field)
    }
    CTailNil -> CTail(res,field)
  }
}

inline fun ctail-fresolve( acc : ctail<a>, rest : a ) : a {
  match(acc) {
    CTail(head,flink) {
      cfield-set(flink,rest)
      head
    }
    CTailNil -> rest
  }
}

inline extern cfield-set-blink( field : cfield<a>, blink : blink<a> ) : () { }
inline extern cfield-get-blink( field : cfield<a> ) : blink<a> { }

reference type blink<a> {
  BLink( _node : a, _blink : cfield<a> )
  BNil
}

inline fun ctail-blink( acc : ctail<a>, res : a, field : cfield<a> ) : ctail<a> {
  match(acc) {
    CTail(last,blink) {
      cfield-set-blink(field, BLink(last,blink))
      CTail(res,field)
    }
    CTailNil -> {
      // ctail-set(field, ctail-hole())  // already done
      CTail(res,field)
    }
  }
}

private fun ctail-blink-resolve( blink : blink<a>, rest : a ) : a {
  match(blink) {
    BLink(node,field) {
      val next = cfield-get-blink(field)
      cfield-set(field, rest)
      ctail-blink-resolve( unsafe-decreasing(next), node )
    }
    BNil -> rest
  }
}

inline fun ctail-bresolve( acc : ctail<a>, rest : a ) : a {
  match(acc) {
    CTail(node,field) {
      val next = cfield-get-blink(field)
      cfield-set(field, rest)
      ctail-blink-resolve( next, node )
    }
    CTailNil -> rest
  }
}

inline fun ctail-link( forward : bool, acc : ctail<a>, res : a, field : cfield<a> ) : ctail<a> {
  if (forward) then ctail-flink(acc,res,field) else ctail-blink(acc,res,field)
}
inline fun ctail-resolve( forward : bool, acc : ctail<a>, rest : a ) : a {
  if (forward) then ctail-fresolve(acc,rest) else ctail-bresolve(acc,rest)
}


fun ctail-mapf( xs : list<a>, f : a -> b, acc : ctail<list<b>> ) : list<b> {
  match(xs) {
    Cons(x,xx) {
      val hd    = Cons(f(x),cfield-hole())
      val field = cfield-of(hd,"std/core/Cons","tail")
      ctail-mapf( xx, f, ctail-flink(acc,hd,field) )
    }
    Nil -> ctail-fresolve(acc,Nil)
  }
}

fun mapf( xs : list<a>, f : a -> b ) : list<b> {
  ctail-mapf(xs,f,CTailNil)
}

fun ctail-mapb( xs : list<a>, f : a -> b, acc : ctail<list<b>> ) : list<b> {
  match(xs) {
    Cons(x,xx) {
      val hd    = Cons(f(x),cfield-hole())
      val field = cfield-of(hd,"std/core/Cons","tail")
      ctail-mapb( xx, f, ctail-blink(acc,hd,field) )
    }
    Nil -> ctail-bresolve(acc,Nil)
  }
}

fun mapb( xs : list<a>, f : a -> b ) : list<b> {
  ctail-mapb(xs,f,CTailNil)
}

fun ctail-mapx( xs : list<a>, f : a -> b, forward : bool, acc : ctail<list<b>> ) : list<b> {
  match(xs) {
    Cons(x,xx) {
      val hd    = Cons(f(x),cfield-hole())
      val field = cfield-of(hd,"std/core/Cons","tail")
      ctail-mapx( xx, f, forward, ctail-link(forward,acc,hd,field) )
    }
    Nil -> ctail-resolve(forward,acc,Nil)
  }
}

fun mapx( xs : list<a>, f : a -> b ) : list<b> {
  ctail-mapx(xs,f,True,CTailNil)
}

fun main() {
  list(1,10000000).mapf(inc).sum.println
  //list(1,10000000).mapb(inc).sum.println
  //list(1,10000000).mapx(inc).sum.println
  //list(1,10000000).map(inc).sum.println
  /*
  val xs = list(1,5)
  var i := 0
  for(1,10000000) fn(j){
    i := i + xs.mapf(inc).length
  }
  println(i)
  */
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
