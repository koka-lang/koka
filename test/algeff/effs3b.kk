import effs3

function mmap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mmap(f,xx))
  }
}

function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

function xorss() {
  val xs = mmap( fun(i) { xors() }, [1,2] )
  xs.join(",")
}

function upcast1() {
  val xs = mmap( id, ["x","y"]) // id is getting cps transformed
  if (flip()) then xs.join(",") else "darn"
}

function upcast2() {
  mmap(id,["x","y"]).join(",")  // mmap should use a default k, and id should be transformed...
}

function xors() : amb string {
  show(xor())
}
 
function amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

