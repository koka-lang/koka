// with expression syntax
ambient val width: int
ambient val height: int

val res1 = with { val height = 5 } in { height + height }

val res2 = with val height = 5 in height + height

val res3 = with val height = 5 in height + height

val res4 = with val height = 5 in
           with val width = 7 in height + width

// the optimizer inlines `1 + 3` into `height() { resume(1 + 3) }` which
// is problematic when accessing the state.
// val res5 = with (s = 7) { val height = 1 + 3 } in height * height

fun res6() {
  (with { val height = { println("foo"); 1 + 3 } } in height * height)
}

ambient counter {
  fun tick(): ()
  fun get(): int
}

fun res7() {
  var s := 0
  with {
    fun tick() { s := s + 1 }
    fun get()  { s }
  }
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
  with val width = 5
  with val height = 9
  width * height
}

ambient amb { control flip(): bool }

val returnClause = {
  with val height = 4
  with control flip() { (resume(True) : list<_>) + resume(False) }
  with return a -> [a]

  if (flip()) 3 else height
}

fun evaluationOrderStmt() {
  with val height = { println("height"); 4 }
  with val width  = { println("width"); 7 }
  height + width
}

fun evaluationOrderExpr() {
  (with val height = { println("height"); 4 } in
   with val width  = { println("width"); 7 } in
   height + width)
}

fun main() {
  res6().println
  res7().println
  evaluationOrderStmt().println
  evaluationOrderExpr().println
}
