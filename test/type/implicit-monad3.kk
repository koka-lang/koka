// monads as implicit parameters
// use ?? to unpack pure and bind

ref struct monad<m>
  pure : forall<a> a -> m<a>
  bind : forall<a,b> (m<a>, a -> m<b>) -> m<b>

ref struct monadplus<m>
  monad : monad<m>
  plus  : forall<a> (m<a>,m<a>) -> m<a>

//fun sequence( ms : list<m<a>>, ?monad : monad<m> ) : m<list<a>>
fun sequence( ms, ??monad )
  match ms
    Nil -> pure([])
    Cons(m,mm) ->
      with x  <- bind(m)
      with xx <- bind(sequence(mm))
      pure(Cons(x,xx))

val monad_list = Monad( fn(x) [x], fn(m,f) m.flatmap(f) )
val monad_maybe = Monad( fn(x) Just(x), fn(m,f) match m { Just(x) -> f(x); _ -> Nothing } )

val monadplus_list = Monadplus( monad_list, (++) )
val monadplus_maybe = Monadplus( monad_maybe, fn(m1,m2) match m1
                                                          Nothing -> m2
                                                          Just    -> m1
                               )

fun monad_monadplus( ??monadplus )
  monad


fun mbind( m, f, ??monad )
  bind(m,f)

fun mpure( x, ??monad )
  pure(x)

fun testtwice( m, ??monadplus )
  // this works since we named the field `monad` which gets now introduced locally
  with x <- mbind(plus(m,m))
  mpure([x])

// exceptions
fun test1()
  match [Just(1), Just(2)].sequence(?monad=monad_maybe)  // ambiguous: `monad_maybe` or `monad_monadplus(monadplus_maybe)`
    Just(xs) -> xs.show
    Nothing  -> "Nothing"

// list of successes
fun test2()
  [[1],[2,3],[4,5,6]].sequence(?monad=monad_list).show-list(show)

