
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

fun contains( xs : list<a>, y : a, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> if eq(x,y) then True else contains(xx,y)
    Nil        -> False

fun equal( x : a, y : a, ?eq : (a,a) -> bool ) : bool
  eq(x,y)

fun test1()
  equal([1,2],[1,3]).println

fun test2()
  contains(['a','b'],'c').println


fun int/cmpx(x : int, y : int ) : order
  int/cmp(x,y)

fun char/cmpx(x : char, y : char ) : order
  Eq

fun list/cmpx<a>( xs : list<a>, ys : list<a>, ?cmpx : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> match cmpx(x,y)
        Eq  -> list/cmpx(xx,yy)
        ord -> ord
      Nil -> Gt
    Nil -> match ys
      Nil -> Eq
      _   -> Lt

// If we add `cmpx/eq` we get ambiguities. e.g. `eq(1,2)`
// can be resolved as `eq = int/eq` but also with `eq = cmpx/eq(int/cmpx)`
// We cannot just prefer values, since `eq([1],[2])`
// can be resolved as `eq = list/eq(int/eq)` but also as `eq = cmpx/eq(list/cmpx(int/cmpx))`.
// For implicits we instead prefer shortest chains originating from the most locals.
fun cmpx/eq( x : a, y : a, ?cmpx : (a,a) -> order ) : bool
  cmpx(x,y) == Eq


fun test3a( ?eq : (int,int) -> bool )
  equal(1,2)  // use `?eq` instead of `int/eq`  to prefer locals

fun test3b()
  equal(1,2)  // use `int/eq` instead of `cmpx/eq(int/cmpx)` as it is shorter

fun test4a(x : int, y :int, ?cmpx : (int,int) -> order)
  (equal(x,y)  // use `cmpx/eq(cmpx)` (instead of `int/eq`) to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmpx(x,y))

fun test4b(x : int, y :int)
  val cmpx = int/cmpx
  (equal(x,y)  // use `cmpx/eq(cmpx)` to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmpx(x,y))

fun show( ord : order ) : string
  match ord
    Lt -> "lt"
    Eq -> "eq"
    Gt -> "gt"


fun test5()
  cmpx([1,2],[1,3]).show.println

fun xcompare( x : a, y : a, ?cmpx : (a,a) -> order ) : order
  cmpx(x,y)

fun cmpx-and-eq(xs : list<a>, y : a, ?cmpx : (a,a) -> order) : bool
  //fun cmpl(a,b) cmp_list(a,b) // haskell could infer this from `instance Ord a => Ord [a]`)
  // fun eq(a,b)   eq_cmp(a,b)  // haskell could infer this from `class Eq a => Ord a`
  xcompare(xs,xs) == Eq &&      // we can resolve this automatically to `list/cmpx(cmpx)` since `cmpx` directly does not type check
  list/cmpx(xs,xs) == Eq &&      // or be explicit

  cmpx(xs,xs) == Eq &&
  eq(y,y) &&                    // this can be resolved automatically without the local `eq` since we have `cmpx/eq(cmpx)`
  contains(xs,y) &&
  int/cmpx(1,2) == Eq  &&
  cmpx(1,2) == Eq  &&         // we cannot resolve this to `int/cmpx` unless we allow a bypass
  xcompare(1,2) == Eq  &&        // we can resolve ?cmpx=int/cmpx as we allow bypass for implicits.. (as long as a local cmpx does not type check)
  ?cmpx(y,y) == Eq &&
  cmpx(y,y) == Eq

// foo :: Ord a => [a] -> a -> Bool
// foo xs y   = (1 == 2) &&    // use `instance Eq Int`
//              xs == xs       // use `class Eq a => Ord a`, and `instance Ord a => Ord [a]`

fun test6()
  // ['a','b'].cmpx-and-eq('a')
  [1,2].map(show).join.show