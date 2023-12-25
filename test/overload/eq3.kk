
fun eq_int( x : int, y : int ) : bool
  (x == y)

fun eq_char( x : char, y : char ) : bool
  (x == y)

fun eq_list<a>( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq_list(xx,yy)
      _ -> False
    Nil -> ys.is-nil

fun contains( xs : list<a>, y : a, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> if eq(x,y) then True else contains(xx,y)
    Nil        -> False

fun equal<a>( x : a, y : a, ?eq : (a,a) -> bool ) : bool
  eq(x,y)

fun test1()
  equal([1,2],[1,3]).println

fun test2()
  contains(['a','b'],'c').println


fun cmp_int(x : int, y : int ) : order
  compare(x,y)

fun cmp_char(x : char, y : char ) : order
  compare(x,y)

fun cmp_list<a>( xs : list<a>, ys : list<a>, ?cmp : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> match cmp(x,y)
        Eq  -> cmp_list(xx,yy)
        ord -> ord
      Nil -> Gt
    Nil -> match ys
      Nil -> Eq
      _   -> Lt

// If we add `eq_cmp` we get ambiguities. e.g. `(1,2).equal`
// can be resolved as `?eq = eq_int` but also with `?eq = eq_cmp(cmp_int)`
// We cannot just prefer values, since `([1],[2]).equal`
// can be resolved as `?eq = eq_list(eq_int)` but also as `?eq = eq_cmp(cmp_list(cmp_int))`.
// We instead prefer shortest chains (using the most locals)
fun eq_cmp( x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Eq

fun test3()
  equal(1,2)  // use `eq_int` instead of `eq_cmp(cmp_int)` as it is shorter

fun test4a(x : int, y :int, ?cmp : (int,int) -> order)
  (equal(x,y)  // use `eq_cmp(cmp)` to prefer implicits from the local declarations (instead of the global `eq_int`)
  ,comp(x,y))

fun test4b(x : int, y :int)
  val cmp = cmp_int
  (equal(x,y)  // use `eq_cmp(cmp)` to prefer implicits from the local declarations (instead of the global `eq_int`)
  ,comp(x,y))

fun show( ord : order ) : string
  match ord
    Lt -> "lt"
    Eq -> "eq"
    Gt -> "gt"

fun comp<a>( x : a, y : a, ?cmp : (a,a) -> order ) : order
  cmp(x,y)


fun test5()
  comp([1,2],[1,3]).show.println


fun cmp-and-eq(xs : list<a>, y : a, ?cmp : (a,a) -> order) : bool
  //fun cmpl(a,b) cmp_list(a,b)  // haskell could infer this from `instance Ord a => Ord [a]`)
  // fun eq(a,b)   eq_cmp(a,b)    // haskell could infer this from `class Eq a => Ord a`
  (comp(xs,xs) == Eq) && // we can resolve this automatically to `cmp_list(cmp)` since `cmp` directly does not type check
   equal(y,y) &&  // this can be resolved automatically without the local `eq` since we have `eq_cmp`
   contains(xs,y)

fun test6()
  ['a','b'].cmp-and-eq('a')