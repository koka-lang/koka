fun auto(f : forall<a> a -> a) { f(f) }

fun ok(f : forall<a> a -> a ) {
  (auto(f))
}
