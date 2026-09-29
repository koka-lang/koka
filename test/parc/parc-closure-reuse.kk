// A closure parameter is not a reuse donor: its size depends on what it
// captures, not on its result type, so `f` is dropped rather than reused for `Pt`.
type shape
  Pt(x : int, y : int)
  Sz(w : int, h : int)

fun pick(f : () -> shape, b : bool) : shape
  if b then Pt(1, 2) else f()
