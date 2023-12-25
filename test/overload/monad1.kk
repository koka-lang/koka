// monads as implicit parameters

value struct monad<m>
  mpure : forall<a> a -> m<a>
  mbind : forall<a,b> (m<a>, a -> m<b>) -> m<b>

fun pure( x : a, ?monad : monad<m> ) : m<a>
  (monad.mpure)(x)

fun bind( x : m<a>, f : a -> m<b>, ?monad : monad<m> ) : m<b>
  (monad.mbind)(x,f)

fun sequence( ms : list<m<a>>, ?monad : monad<m> ) : m<list<a>>
//fun sequence( ms, ?monad )
  match ms
    Nil -> pure([])
    Cons(m,mm) ->
      with x  <- bind(m)
      with xx <- bind(sequence(mm))
      pure(Cons(x,xx))

val list/monad = Monad( fn(x) [x], fn(m,f) m.flatmap(f) )
val maybe/monad = Monad( fn(x) Just(x), fn(m,f) match m { Just(x) -> f(x); _ -> Nothing } )

// exceptions
fun test1()
  match [Just(1), Just(2)].sequence
    Just(xs) -> xs.show
    Nothing  -> "Nothing"

// list of successes
fun test2()
  [[1],[2,3],[4,5,6]].sequence.show-list(show)
