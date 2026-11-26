// An inferred recursive effect keeps the heap free during isolation.
// Its hdiv evidence must still be resolved in the function body.
fun recur()
  val r = ref(0)
  val x = !r
  recur()

// Exercise the non-isolatable concrete-heap branch as well.
fun read-global(r : ref<global,int>) : _ int
  val x = !r
  read-global(r)
