// ,("(undefined :: some a. [a -> a] -> Int) (undefined :: some c. [(forall d. d -> c) -> c])", Wrong)

fun wrong() {
  (throw("hi") : some<a> list<a -> a> -> int)(throw("there") : list<(forall<d> d -> d) -> (int -> int)>)
}