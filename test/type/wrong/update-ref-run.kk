// Inside `run` the heap of a local reference is unified with `global` by the update, and
// `run` then rejects it, as it requires a heap of its own.
fun local-ref() : div int
  run fn()
    val r = ref(1)
    var x := 1
    x := x + !r
    x
