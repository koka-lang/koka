type listInt
  NilInt
  ConsInt(head : int, tail : listInt)

fun main(): listInt {
  val a = ctx ConsInt(0, _)

  matchctx a
    ConsInt(_, ctx xs) -> xs
}
