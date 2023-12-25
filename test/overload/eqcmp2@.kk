// another design:
// - we can define functions with an @ in it
// - if we call `f@` any function of name `f@xxx` matches and we resolve by type information
//   (and disambiguate by locals first, and then shortest chains with most locals)
// - this makes it explicit when overloading is allowed (only on @ names)


fun eq( x : a, y : a, ?eq@ : (a,a) -> bool ) : bool
  eq@(x,y)


fun eq@int( x : int, y : int ) : bool
  (x == y)

fun eq@char( x : char, y : char ) : bool
  (x == y)

fun eq@list<a>( xs : list<a>, ys : list<a>, ?eq@ : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq(xx,yy)            // ?eq@ = eq@, and ?eq@ = eq@list(eq@)
                 // | eq@(x,y) -> eq@list(xx,yy)   // or be explicit
      _ -> False
    Nil -> ys.is-nil

fun contains( xs : list<a>, y : a, ?eq@ : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> if eq(x,y) then True else contains(xx,y)
    Nil        -> False


fun test1()
  eq([1,2],[1,3]).println  // ?eq@ resolves to eq@list(eq@int)

fun test2()
  contains(['a','b'],'c').println


fun cmp( x : a, y : a, ?cmp@ : (a,a) -> order ) : order
  cmp@(x,y)

fun cmp@int(x : int, y : int ) : order
  int/compare(x,y)

fun cmp@char(x : char, y : char ) : order
  Eq

fun cmp@list<a>( xs : list<a>, ys : list<a>, ?cmp@ : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> match cmp(x,y)   // or explicit as `cmp@(x,y)`
        Eq  -> cmp(xx,yy)            // or explicit as `cmp@list` (with `?cmp@=cmp@`)
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
fun eq@cmp( x : a, y : a, ?cmp@ : (a,a) -> order ) : bool
  cmp(x,y) == Eq


fun test3()
  eq(1,2)  // ?eq@ is resolved as eq@cmp(eq@int)

fun test4a(x : int, y :int, ?cmp@ : (int,int) -> order)
  (eq(x,y)    // use `eq@cmp(cmp@)` to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmp(x,y))  // use `cmp@`

fun test4b(x : int, y :int)
  val cmp@ = cmp@int
  (eq(x,y)    // use `eq@cmp(cmp@)` to prefer implicits originating from the local declarations (instead of the global `int/eq`)
  ,cmp(x,y))  // use `cmp@`

fun show( ord : order ) : string
  match ord
    Lt -> "lt"
    Eq -> "eq"
    Gt -> "gt"


fun test5()
  cmp([1,2],[1,3]).show.println



fun cmp-and-eq(xs : list<a>, y : a, ?cmp@local : (a,a) -> order) : bool
  //fun cmpl(a,b) cmp_list(a,b) // haskell could infer this from `instance Ord a => Ord [a]`)
  // fun eq(a,b)   eq_cmp(a,b)  // haskell could infer this from `class Eq a => Ord a`
  cmp@list(xs,xs) == Eq &&      // be explicit
  cmp(xs,xs) == Eq &&           // we can resolve this `?cmp@ = cmp@list(cmp@)` (allow bypass of cmp@ to match the type)
  eq(y,y)    &&                 // this can be resolved `?eq@ = eq@cmp(cmp@)`
  cmp(1,2) == Eq


fun test6()
  ['a','b'].cmp-and-eq('a')
