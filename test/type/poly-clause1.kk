// Clauses of operations with a polymorphic result are generalized with their inner instantiations

effect raise
  ctl raise( msg : string ) : a

effect choice
  fun choose( x : a, y : a ) : a

fun handle-raise() : int
  with ctl raise(msg) 42
  raise("x")

fun handle-choice() : int
  with fun choose(x,y) x
  choose(1,2)

val h = handler
  ctl raise(msg) 0

fun auto( f : forall<a> a -> e a ) : e (forall<a> a -> e a)
  f(f)
