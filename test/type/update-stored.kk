// a value holding functions that use the local variables can be updated, and such functions can be
// stored, as long as the update does not call them
fun f() : div list<int>
  var y := 10
  val gy = fn() y
  var fs := [fn() 0]
  fs := Cons(gy, fs)
  fs := fs.drop(0)
  fs.map(fn(g) g())
