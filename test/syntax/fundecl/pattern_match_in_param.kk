fun f(a: int, Cons(x, _xs), Just(Cons(_, Cons(y, _)) as v) : maybe<list<int>>) {
    val _ = v
    a + x + y
}

fun g(_, _ as a: int) {
    val _ = a
    1
}

fun h(Cons(x, _xs) : list<int> = [1, 2]) {
    x
}

fun main() {
    f(2, [1], Just([2, 3])).println
    g(1, 2).println
    h([2,3]).println
}
