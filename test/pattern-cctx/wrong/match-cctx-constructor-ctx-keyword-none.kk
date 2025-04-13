type listInt
  NilInt
  ConsInt(head : int, tail : listInt)

fun main(): int {
  val a = ctx ConsInt(0, _)

  matchctx a
    ConsInt(head, tail) -> 0
}
