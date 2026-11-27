// How the heap of a reference read in the right-hand side of `x := e` is matched against the
// bound `ioc`, where the only reference heap is `global` (`st<global>` in `ioc`).

// A reference in the global heap is allowed as is.
fun global-ref( r : ref<global,int> ) : <st<global>,div> int
  var x := 1
  x := x + !r
  x

// An unannotated parameter: its heap is unified with `global`.
fun unannotated( r )
  var x := 1
  x := x + !r
  x

// A reference allocated in the function: its heap is unified with `global` as well, so the
// function has `alloc<global>` instead of a heap that `run` could discharge.
fun allocated()
  val r = ref(1)
  var x := 1
  x := x + !r
  x

// A reference whose value type contains its heap: reading it adds `div`, which must be in the
// effect before the update closes it.
fun heap-in-value( r : ref<global,() -> <read<global>> int> )
  var x := 1
  x := x + (!r)()
  x

// A reference read before the update needs no matching: the update only reads `x`.
fun read-before( r : ref<h,int> ) : <read<h>,div> int
  var x := 1
  val y = !r
  x := x + y
  x

// `::=` is a plain assignment and accepts any heap.
fun assign( r : ref<h,int> ) : <read<h>,div> int
  var x := 1
  x ::= x + !r
  x
