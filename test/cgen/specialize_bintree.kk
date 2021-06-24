type tree<a> {  Bin( left: tree<a>, right : tree<a> ); Leaf( elems : list<a> ) }

fun mapTree(tree : tree<a>, f : a -> b)  : _ tree<b> {
    match (tree) {
        Leaf(xs) -> Leaf(xs.map(f))
        Bin(left, right) -> Bin(mapTree(left, f), mapTree(right, f))
    }
}

/*
mapTee(tree, (+1))
mapTree_plusone(tree)

// step one
// we can do this if we remove the condition that the specialized function needs to be called in the body
mapTree_plusone(tree : tree<int>) {
    match (tree) {
        Leaf(xs) -> Leaf(xs.map(+1))
        Bin(left, right) -> Bin(mapTree_plusone(left), mapTree_plusone(right))
    }
}

// step two
mapTree_plusone(tree : tree<int>) {
    match (tree) {
        Leaf(xs) -> Leaf(xs.map_plusone)
        Bin(left, right) -> Bin(mapTree_plusone(left), mapTree_plusone(right))
    }
}
*/

fun main() {
    val t = Bin(Leaf([1,2,3]), Leaf([4,5,6]))
    val _ = t.mapTree(fn(x) { x + 1 })
    ()
}