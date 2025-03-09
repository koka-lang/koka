fun main(): int {
  val a = ctx Cons(0, _)

  matchctx a
    Cons(x, ctx xs) -> xs
}
