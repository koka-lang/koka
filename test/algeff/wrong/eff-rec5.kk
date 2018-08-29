
effect state {
  fun get() : hide
  fun set(x : hide) : ()
}
type hide {
  Hide(unhide : int -> state int)
}

val new-ref = handler(s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun recurse() {
  using new-ref(Hide(id))
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
