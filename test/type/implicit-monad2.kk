// monads as implicit parameters
// use ?? to unpack pure and bind

value struct monad<m>
  pure : forall<a> a -> m<a>
  bind : forall<a,b> (m<a>, a -> m<b>) -> m<b>


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

// exceptions
fun test1()
  match [Just(1), Just(2)].sequence
    Just(xs) -> xs.show
    Nothing  -> "Nothing"

// list of successes
fun test2()
  [[1],[2,3],[4,5,6]].sequence.show-list(show)
