function auto(f : forall<a> a -> a)  -> f(f)

function ok(f : forall<a> a -> a ) {
  (auto(f))
}