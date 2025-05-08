type treeInt
  LeafInt
  NodeInt(left : treeInt, element : int, right : treeInt)

fun main(): int {
  val a = ctx NodeInt(_, 0, LeafInt)

  matchctx a
    NodeInt(ctx _left, _, _) -> 1
    // this should not be a pattern by type
    //NodeInt(_, ctx _element, _) -> 2
    NodeInt(_, _, ctx _right) -> 3
    ctx hole -> 0
}
