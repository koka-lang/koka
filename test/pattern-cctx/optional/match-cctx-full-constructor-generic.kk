fun main(): int {
  val a = ctx Cons(0, _)

  matchctx a
    // it already infers that the result type is list<int>,
    // but the hole type is still generic, it should also be list<int>
    // it is an optional feature for the cctx-matching to support generics
    Cons(x, ctx xs) -> x
}
