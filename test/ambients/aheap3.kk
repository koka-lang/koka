ambient heap {
  control new-ref(init : a) : ref<a>
}

ambient instance ref<a> in heap {
  control get() : a
  control set( value : a ) : ()
  control free() : ()
}

// bug: we need to add `:inst` (or any other handled effect)
// or the optimizer gets too aggressive :-(
fun with-ref(init : a, action : (ref<a>) -> <inst|e> b ) : <inst|e> b {
  fun f() {
    with r = instance {
      control get()  { (fun(s) { resume(s)(s) }) }
      control set(x) { (fun(s) { resume(())(x)}) }
      control free() { (fun(s) { resume-shallow(())(s) }) }
    }
    val x = action(r)
    (fun(s){ x })
  }
  f()(init)
}

fun heap( action ) {
  with control new-ref(init) { with-ref(init,resume) }
  action()
}

fun escape() : heap ref<string> {
  new-ref("hi")
}

fun main() {
  with heap
  val r1 = new-ref(40)
  val r2 = escape()
  println( r1.get + r2.get.count)
}

fun example1() {
  with heap
  with r1 : ref<int> = with-ref(40)
  val r2 = escape()
  val x = r1.get
  r1.free
  println( x + r2.get.count)
}

fun wrong1() {
  with heap
  val r1 = new-ref(42)
  r1.free
  println(r1.get)  // use after free
}
