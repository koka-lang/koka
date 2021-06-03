type tree<a> { Node(data : a, children : list<tree<a>>) }

fun mapTree(Node(a, children) : tree<a>, f : a -> b) : _ tree<b> {
    Node(f(a), map(children, fn (tree) { tree.mapTree(f) }))
}

val f = {
//   mapTree(Node(1, []), fn(x) { x + 1 })  
  val y = 2
  y + y
}

/*
mapTree([1,2,3], (+1))
mapTree_plusone([1,2,3])

// step one
fun mapTree_plusone(Node(a, children) : tree<int>) {
    Node(a + 1, map(children, mapTree_plusone))
}

// step two
fun mapTree_plusone_specialize_map(Node(a, children) : tree<int>) {
    Node(a + 1, map_mapTree_plusone(children))
}
*/

fun main() {
    val tree = Node(1, [Node(2, []), Node(3, [])])
    tree.mapTree(fn(x) { x + 1 })
}