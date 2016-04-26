// ,("(undefined :: some a. [a -> a] -> Int) (undefined :: some c. [(forall d. d -> c) -> c])", Wrong)

function wrong() {
  (error("hi") : some<a> list<a -> a> -> int)(error("there") : list<(forall<d> d -> d) -> (int -> int)>)
}