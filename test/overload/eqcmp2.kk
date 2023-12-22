// This works with extended resolving for
// variable function applications f(x,..) where `f` can be
// 1. disambguated (using shortest chains)
// 2. use a global instead of a local as long as the local is the implicit parameter

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
  eq([1,2],[1,3]).println

fun test2()
  contains(['a','b'],'c').println


fun int/cmp(x : int, y : int ) : order
  int/compare(x,y)

fun char/cmp(x : char, y : char ) : order
  Eq

fun list/cmp<a>( xs : list<a>, ys : list<a>, ?cmp : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> match cmp(x,y)
        Eq  -> list/cmp(xx,yy)
        ord -> ord
      Nil -> Gt
    Nil -> match ys
      Nil -> Eq
      _   -> Lt

// If we add `cmp/eq` we get ambiguities. e.g. `eq(1,2)`
// can be resolved as `eq = int/eq` but also with `eq = cmp/eq(int/cmp)`
// We cannot just prefer values, since `eq([1],[2])`
// can be resolved as `eq = list/eq(int/eq)` but also as `eq = cmp/eq(list/cmp(int/cmp))`.
// For implicits we instead prefer shortest chains originating from the most locals.
fun cmp/eq( x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Eq

fun test3()
  // equal(1,2)  // use `int/eq` instead of `cmp/eq(int/cmp)` as it is shorter
  eq(1,2)        // is ambiguous as regular identifier resolution doesn't do this (and we have `int/eq` and `cmp/eq(int/cmp)`)

fun test4a(x : int, y :int, ?cmp : (int,int) -> order)
  (eq(x,y)  // use `cmp/eq(cmp)` to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmp(x,y))

fun test4b(x : int, y :int)
  val cmp = int/cmp
  (eq(x,y)  // use `cmp/eq(cmp)` to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmp(x,y))

fun show( ord : order ) : string
  match ord
    Lt -> "lt"
    Eq -> "eq"
    Gt -> "gt"


fun test5()
  cmp([1,2],[1,3]).show.println

fun compare( x : a, y : a, ?cmp : (a,a) -> order ) : order
  cmp(x,y)

fun cmp-and-eq(xs : list<a>, y : a, ?cmp : (a,a) -> order) : bool
  //fun cmpl(a,b) cmp_list(a,b) // haskell could infer this from `instance Ord a => Ord [a]`)
  // fun eq(a,b)   eq_cmp(a,b)  // haskell could infer this from `class Eq a => Ord a`
  compare(xs,xs) == Eq &&       // we can resolve this automatically to `list/cmp(cmp)` since `cmp` directly does not type check
  list/cmp(xs,xs) == Eq &&      // or be explicit
  cmp(xs,xs) == Eq &&           // we can resolve this automatically to `list/cmp(cmp)` since `cmp` directly does not type check
                                // we allow to resolve the local bypass `list/cmp` since it eventually uses the local `cmp` directly,
                                // i.e. it is a type conversion on the local.
  eq(y,y) &&                    // this can be resolved automatically without the local `eq` since we have `cmp/eq(cmp)`
  contains(xs,y)


fun test6()
  ['a','b'].cmp-and-eq('a')
