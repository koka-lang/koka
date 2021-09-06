fun map2(xs : list<_>, f) {
    map(xs, f)
}

fun map3(xs : list<_>, f) {
    map2(xs, f)
}

fun main() {
    [1,2,3].map3(fn(x) { x + 1 }).show.println
}