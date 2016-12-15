function auto(f : forall<a> a -> a)  -> f(f)

function wrong(f) {
  (auto(f))
}