type listInt
  NilInt
  ConsInt(head : int, tail : listInt)

fun main(): listInt {
  val a = ctx ConsInt(0, _)

  matchctx a
    // matching on a zero-arg constructor should not be possible with a cctx
    NilInt -> 0
}
