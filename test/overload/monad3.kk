// monads as implicit parameters
// use ?? to unpack pure and bind

ref struct monad<m>
  pure : forall<a> a -> m<a>
  bind : forall<a,b> (m<a>, a -> m<b>) -> m<b>

ref struct monadplus<m>
  base  : monad<m>                          // field names starting with "base" get unpacked recursively in ??
  plus  : forall<a> (m<a>,m<a>) -> m<a>

//fun sequence( ms : list<m<a>>, ??monad : monad<m> ) : m<list<a>>
fun sequence( ms, ??monad )
  match ms
    Nil -> pure([])
    Cons(m,mm) ->
      with x  <- bind(m)
      with xx <- bind(sequence(mm))
      pure(Cons(x,xx))

val list/monad  = Monad( fn(x) [x], fn(m,f) m.flatmap(f) )
val maybe/monad = Monad( fn(x) Just(x), fn(m,f) match m { Just(x) -> f(x); _ -> Nothing } )

val list/monadplus  = Monadplus( list/monad, (++) )
val maybe/monadplus = Monadplus( maybe/monad, fn(m1,m2) match m1
                                                          Nothing -> m2
                                                          Just    -> m1
                               )

fun monadplus/monad( ??monadplus )
  base

fun testtwice( m, ??monadplus )
  // this works since the base member is unpacked as well, as:
  // ??monadplus ~> match monadplus
  //                  Monadplus base plus -> match base
  //                                           Monad pure bind -> ...
  with x <- bind(plus(m,m))
  pure([x])

// exceptions
fun test1()
  match [Just(1), Just(2)].sequence  // ambiguous: `maybe/monad` or `monadplus/monad(maybe/monadplus)`
    Just(xs) -> xs.show
    Nothing  -> "Nothing"

// list of successes
fun test2()
  [[1],[2,3],[4,5,6]].sequence.show-list(show)

fun test3()
  [[1],[2,3]].testtwice.concat.sequence.show-list(show)