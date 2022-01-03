import std/core

type tree<a> { Node(data : a, children : list<tree<a>>) }

// why does koka require a div effect here?
fun show(Node(a : int, children) : tree<int>) : div string {
    // why do I need std/core/show to be fully qualified here?
    "Tree(" ++ std/core/show(a) ++ ", [" ++ children.map(show).join(", ") ++ "])"
}

fun mapT(Node(a, children) : tree<a>, f : a -> e b) : e tree<b> {
    Node(f(a), map(children, fn(tree) { tree.mapT(f) }))
}

fun main() {
    val tree = Node(1, [Node(2, []), Node(3, [])])
    tree.mapT(inc).show.println
}