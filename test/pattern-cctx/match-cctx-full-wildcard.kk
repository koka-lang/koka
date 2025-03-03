fun main(): int {
  val a = ctx Cons(10, _)

  matchctx a
    _ -> 0
}
