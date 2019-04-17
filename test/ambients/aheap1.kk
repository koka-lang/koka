ambient heap {
  control new-ref(init : a) : ref<a>
}

ambient instance ref<a> in heap {
	fun get() : a
	fun set( value : a ) : ()
}


fun with-ref(init,action) {
  var s := init
  with r = instance {
    fun get()      { s }
    fun set(x)     { s := x }
  }
  action(r)
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
