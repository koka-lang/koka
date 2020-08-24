ambient heap {
  control new-ref(init : a) : ref<a>
}

ambient instance ref<a> in heap {
	control get() : a
	control set( value : a ) : ()
  control free() : ()
}

fun with-ref(init,action) {
  with r = instance (s=init) {
    control get()  { resume(s,s)  }
    control set(x) { resume((),x) }
    control free() { resume-shallow(()) }
  }
  action(r)
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

fun wrong1() {
  with heap
  val r1 = new-ref(42)
  r1.free
  println(r1.get)
}
