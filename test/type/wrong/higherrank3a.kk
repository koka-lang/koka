function auto(f : forall<a> a -> a)  -> f(f)

function wrong(f) {
  (f(1), f(True), auto(f))
}
