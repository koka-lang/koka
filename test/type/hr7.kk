// ,("(undefined :: some a. [a -> a] -> Int) (undefined :: some c. [(forall d. d -> c) -> c])", Wrong)

fun ok() {
  (throw("hi") : some<a> list<a -> (forall<b> b -> b)> -> exn int)(throw("there") : some<c> list<(forall<d> d -> d) -> c>)
}