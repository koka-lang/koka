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
  (with (s = 7) { val height = { println("foo"); 1 + 3 } } in height * height)
}

effect counter {
  fun tick(): ()
  fun get(): int
}

val res7 = with (s = 0) {
  effect fun tick() { resume((), s + 1) }
  effect fun get() { resume(s, s) }
} in {
  tick()
  tick()
  tick()
  get()
}

// with statement syntax
val res8 = {
  with { val width = 5 }
  width + width
}

val res9 = {
  with { val width = 5 }
  with { val height = 9 }
  width * height
}

val res10 = {
  with width = 5
  with height = 9
  width * height
}

effect amb { fun flip(): bool }

val returnClause = {
  with height = 4
  with flip() -> (resume(True) : list<_>) + resume(False)
  with return a -> [a]

  if (flip()) 3 else height
}

fun evaluationOrderStmt() {
  with height = { println("height"); 4 }
  with width  = { println("width"); 7 }
  height + width
}

fun evaluationOrderExpr() {
  (with height = { println("height"); 4 } in
   with width  = { println("width"); 7 } in
   height + width)
}

fun main() {
  res6()
  evaluationOrderStmt().println
  evaluationOrderExpr().println
}
