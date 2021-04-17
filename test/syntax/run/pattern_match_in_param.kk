fun f(a: int, Cons(x, _xs), Just(Cons(_, Cons(y, _)) as v) : maybe<list<int>>) : exn int {
    val _ = v
    a + x + y
}

fun g(_, _ as a: int) {
    val _ = a
    1
}

fun h(Cons(x, _xs) : list<int> = [1, 2]) : exn int{
    x
}

fun i(0) {
    1
}

fun j(((0: int) : int) as a) : exn int {
    a + 1
}

fun k((Cons(_x, _) : list<int>) as y) : exn list<int> {
   y
}

fun l((x : (_, _, _)) : (_, _, _)) : total _ {
    x.fst
}

public fun main() {
    f(2, [1], Just([2, 3])).println
    (fn() { f(1, [1], Nothing).show }).try(show).println
    g(1, 2).println
    h([2,3]).println
    i(0).println
    j(0).println
    (fn() { k([]).show }).try(show).println
    l((1, 2, 3)).println
}
