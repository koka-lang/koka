ambient heap {
  control new-ref(init : a) : ref<a>
}

ambient instance ref<a> in heap {
	fun get() : a
	fun set( value : a ) : ()
  control free() : ()
}

type result<e,a> {
  Done( result: a)
  Free( continue: () -> e result<e,a> )
}

// bug: we need to add `:inst` or we get a wrong cps translation
fun with-ref(init : a, action : (ref<a>) -> <exn,inst|e> b ) : <exn,inst|e> b {
  val res = {
    var s := init
    with r = instance {
      fun get()      { s }
      fun set(x)     { s := x }
      control free() { Free( { resume-shallow(()) } ) }
    }
    Done(action(r))
  }
  match(res) {
    Done(x) -> x
    Free(f) -> match( f() ) {
      Done(x) -> x
      _       -> error("impossible!")
    }
  }
}

fun heap( action ) {
  with control new-ref(init) {
    with-ref(init,resume)
  }
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

fun wrong1()  {
  with heap
  val r1 = new-ref(42)
  r1.free
  println("hi")
  println(r1.get)  // use after free
}
