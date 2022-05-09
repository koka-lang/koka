

fun ctail-mapf( xs : list<a>, f : a -> e b, acc : ctail<list<b>> ) : e list<b> {
  match(xs) {
    Cons(x,xx) {
      val hd    = Cons(f(x),cfield-hole())
      val field = cfield-of(hd,"std/core/Cons","tail")
      ctail-mapf( xx, f, ctail-flink(acc,hd,field) )
    }
    Nil -> ctail-fresolve(acc,Nil)
  }
}

fun mapf( xs : list<a>, f : a -> e b ) : e list<b> {
  ctail-mapf(xs,f,ctail-nil())
}


fun tmap( xs : list<a>, f : a -> b ) : list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),tmap(xx,f))
    Nil        -> Nil
  }
}

effect out {
  ctl out(s : string) : ()
}


fun main() {
  list(1,10000000).tmap(inc).sum.println
  //list(1,10000000).mapb(inc).sum.println
  //list(1,10000000).mapx(inc).sum.println
  //list(1,10000000).map(inc).sum.println
  //test-many()
  //test-ctl()
  //test-multi()
}

/*
fun test-many() {
  val xs = list(1,5)
  var i := 0
  for(1,10000000) fn(j){
    i := i + xs.tmap(inc).length
  }
  println(i)
}

fun test-ctl() {
  with ctl out(s:string) {
    println(s); resume(())
  }
  list(1,10).tmap(fn(i:int){ if (i.is-odd) i.show.out; i+1 }).sum.println
}
*/
effect nondet {
  ctl flip() : bool
  ctl fail() : a
}

fun ctail-knapsack(w : int, vs : list<int>, acc : list<int> -> list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then acc([])
  else {
    val v = select(vs)
    ctail-knapsack(w - v, vs, fn(res){ Cons(v,res) })
    // Cons(v,knapsack(w - v, vs))
  }
}
fun tknapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  ctail-knapsack(w,vs,id)
}

fun knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = select(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

fun select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (flip()) then x else select(xx)
  }
}

val solutions = handler {
  return  x      -> [x]
  ctl fail() -> []
  ctl flip() -> resume(True) + resume(False)
}

fun show( xss : list<list<int>> ) : string {
  xss.show-list(fn(xs) { xs.show-list(core/show) } )
}

fun test-multi() {
  solutions{ knapsack(3,[3,2,1]) }
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
    Bin(l,r) -> "(" ++ l.show ++ " * " ++ r.show ++ ")"
    Tip(i)   -> i.show
  }
}
*/
