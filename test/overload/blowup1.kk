
fun int/eq( x : int, y : int ) : bool
  (x == y)

fun char/eq( x : char, y : char ) : bool
  (x == y)

fun list/eq<a>( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> list/eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil


// Element-wise tuple equality
fun tuple2/eq( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/eq : (a,a) -> bool, ?snd/eq : (b,b) -> bool ) : bool
  eq(x1,x2) && eq(y1,y2)

// // Element-wise triple equality
// fun tuple3/eq( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ?fst/eq : (a,a) -> bool, ?snd/eq : (b,b) -> bool, ?thd/eq : (c,c) -> bool ) : bool
//   eq(x1,x2) && eq(y1,y2) && eq(z1,z2)

fun test0(xs:list<list<list<int>>>)
  eq([],xs)

fun test1( x : _a, ?eq : (_a,_a) -> bool )
  eq([],[x])

fun test2( xs, ?eq )
  eq([],[])

