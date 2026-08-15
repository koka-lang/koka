import specbox-lib

// See specbox-lib.kk: `set` is specialized from the .kki inline section. If the
// interface reader hands out overlapping type-variable ids, the specialized
// definition keeps a raw id in its result type and the generated C fails to
// compile with `kk_box_t` errors.
fun main()
  val m1 = Vmap([]).set(1, 10)
  println( m1.elems.length )
  val m2 = m1.set(2, 20).set(1, 99)
  println( m2.elems.length )
  println( m2.elems.lookup(fn(k) k == 1).map(fn(v) v).default(0) )
