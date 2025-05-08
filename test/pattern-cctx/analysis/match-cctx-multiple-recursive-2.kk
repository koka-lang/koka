type treeInt
  LeafInt
  NodeInt(left : treeInt, element : int, right : treeInt)

fun main(): int {
  val a = ctx NodeInt(LeafInt, _, LeafInt)

  matchctx a
    // this should not be a pattern by type
    //NodeInt(ctx _left, _, _) -> 1
    NodeInt(_, ctx _element, _) -> 2
    // this should not be a pattern by type
    //NodeInt(_, _, ctx _right) -> 3
    ctx hole -> 0
}
