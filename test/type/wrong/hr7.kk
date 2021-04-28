// ,("(undefined :: some a. [a -> a] -> Int) (undefined :: some c. [(forall d. d -> c) -> c])", Wrong)

fun wrong() {
  (throw("hi") : some<a> list<a -> a> -> int)(throw("there") : some<c> list<(forall<d> d -> c) -> c>)
}