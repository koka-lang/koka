
effect state {
  fun get() : hide
  fun set(x : hide ) : ()
}
type hide {
  Hide(unhide : int -> state int)
}

fun new-ref( init : hide, action : () -> state a) : a {
  val st = handle(action) {
    return x -> fun(s){ x }
    get()    -> fun(s){ resume(s)(s) }
    set(x)   -> fun(s){ resume(())(x) }
  }
  st(init)
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
