module syn-nested1b
import syn-nested1a

pub noinline fun apply-cb( f : (x : my-alias) -> int ) : int
  f(Cons(1,Cons(2,Nil)))
