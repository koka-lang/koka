fun auto(f : forall<a> a -> a) { f(f) }

fun wrong(f) {
  (f(1), f(True), auto(f))
}
