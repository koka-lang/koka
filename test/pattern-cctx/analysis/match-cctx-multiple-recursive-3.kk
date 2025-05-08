type treeInt
  LeafInt
  NodeInt(left : treeInt, element : int, right : treeInt)

fun main(): int {
  val a = ctx NodeInt(LeafInt, 0, _)

  matchctx a
    NodeInt(ctx _left, _e, _r) -> 1
    // this should not be a pattern by type
    // NodeInt(_, ctx _element, _) -> 2
    NodeInt(_l, _e, ctx _right) -> 3
    ctx hole -> 0
}
