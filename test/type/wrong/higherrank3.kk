fun auto(f : forall<a> a -> a) { f(f) }

fun wrong(f) {
  (auto(f), f(1), f(true))
}
