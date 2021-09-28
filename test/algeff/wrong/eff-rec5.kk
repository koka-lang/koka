
effect state {
  fun get() : hide
  fun set(x : hide) : ()
}
type hide {
  Hide(unhide : int -> state int)
}

fun new-ref(init,action) {
  var s := init
  handle(action)
    fun get()  { s }
    fun set(x) { s := x }
}

fun recurse() {
  with new-ref(Hide(id))
  fun foo(i : int) {
    (get().unhide)(i)
  }
  set(Hide(foo))
  foo(42)
}

type xstate :: HX {
  con OpsSet( xset : xset )
}

type xset {
  con OpSet( x : xhide )
}

type xhide {
  con XHide( unhide : int -> xstate int )
}
