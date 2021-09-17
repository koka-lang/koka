type tree<a> {  Bin( left: tree<a>, right : tree<a> ); Leaf( elems : list<a> ) }

fun mapTree(tree : tree<a>, f : a -> b)  : _ tree<b> {
    match (tree) {
        Leaf(xs) -> Leaf(xs.map(f))
        Bin(left, right) -> Bin(mapTree(left, f), mapTree(right, f))
    }
}

fun main() {
    val t = Bin(Leaf([1,2,3]), Leaf([4,5,6]))
    val _ = t.mapTree(fn(x) { x + 1 })
    ()
}