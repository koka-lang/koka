
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

fun maybe/eq( mb1 : maybe<a>, mb2 : maybe<a>, ?eq : (a,a) -> bool ) : bool
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> eq(x,y)
                  Nothing -> False
    Nothing  -> match mb2
                  Nothing -> True
                  _       -> False



fun int/cmpx(x : int, y : int ) : order
  int/cmp(x,y)

fun char/cmpx(x : char, y : char ) : order
  char/cmp(x,y)

fun list/cmpx<a>( xs : list<a>, ys : list<a>, ?cmpx : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> match cmpx(x,y)
                      Eq  -> cmpx(xx,yy)
                      ord -> ord
      Nil -> Gt
    Nil -> match ys
      Nil -> Eq
      _   -> Lt

fun maybe/cmpx( mb1 : maybe<a>, mb2 : maybe<a>, ?cmpx : (a,a) -> order ) : order
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> cmpx(x,y)
                  Nothing -> Gt
    Nothing  -> match mb2
                  Nothing -> Eq
                  _       -> Lt


// Generic equality if `cmp` exists
fun cmpx/eq(x : a, y : a, ?cmpx : (a,a) -> order ) : bool
  match cmpx(x,y)
    Eq -> True
    _  -> False


// Element-wise tuple equality
fun tuple2/eq( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/eq : (a,a) -> bool, ?snd/eq : (b,b) -> bool ) : bool
  eq(x1,x2) && eq(y1,y2)

// Element-wise triple equality
fun tuple3/eq( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ?fst/eq : (a,a) -> bool, ?snd/eq : (b,b) -> bool, ?thd/eq : (c,c) -> bool ) : bool
  eq(x1,x2) && eq(y1,y2) && eq(z1,z2)

fun test1( x : _a, ?eq : (_a,_a) -> bool )
  eq([],[x])

fun test2( xs, ?eq )
  eq([],[])
