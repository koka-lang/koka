
effect state {
  ctl get() : hide
  ctl set(x : hide ) : ()
}
type hide {
  Hide(unhide : int -> state int)
}

fun new-ref( init : hide, action : () -> state a) : a {
  val st = handle(action) {
    return(x) -> fn(s){ x }
    ctl get()     -> fn(s){ resume(s)(s) }
    ctl set(x)    -> fn(s){ resume(())(x) }
  }
  st(init)
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
