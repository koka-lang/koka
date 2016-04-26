// ,("(undefined :: some a. [a -> a] -> Int) (undefined :: some c. [(forall d. d -> c) -> c])", Wrong)

function ok() {
  (error("hi") : some<a> list<a -> (forall<b> b -> b)> -> exn int)(error("there") : some<c> list<(forall<d> d -> d) -> c>)
}