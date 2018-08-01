// with expression syntax
implicit val width: int
implicit val height: int

val res1 = with { val height = 5 } in { height + height }

val res2 = with val height = 5 in height + height

val res3 = with height = 5 in height + height

val res4 = with height = 5 in
           with width = 7 in height + width

// the optimizer inlines `1 + 3` into `height() { resume(1 + 3) }` which
// is problematic when accessing the state.
val res5 = with (s = 7) { val height = 1 + 3 } in height * height

fun res6() {
  with (s = 7) { val height = { println("foo"); 1 + 3 } } in height * height
}

effect counter {
  fun tick(): ()
  fun get(): int
}

val res7 = with (s = 0) {
  fun tick() { resume((), s + 1) }
  fun get() { resume(s, s) }
} in {
  tick()
  tick()
  tick()
  get()
}
