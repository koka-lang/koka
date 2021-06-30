
fun f(a: int, Cons(x, _xs), v as Just(Cons(_, Cons(y, _))) : maybe<list<int>>) : exn int {
    val _ = v
    a + x + y
}

fun g(_, a as _ : int) {
    val _ = a
    1
}

fun h(Cons(x, _xs) : list<int> = [1, 2]) : exn int{
    x
}

fun i(0) {
    1
}

fun j(a as ((0: int) : int)) : exn int {
    a + 1
}

fun k(y as (Cons(_x, _) : list<int>)) : exn list<int> {
   y
}

fun l((x : (_, _, _)) : (_, _, _)) : total _ {
    x.fst
}

public fun main() {
    "f(2, [1], Just([2, 3]))\t-> ".print
    f(2, [1], Just([2, 3])).println

    "f(1, [1], Just([1]))\t-> ".print
    (fn() { f(1, [1], Just([1])).show }).try(show).println

    "g(1, 2)\t\t\t-> ".print
    g(1, 2).println

    "h([2,3])\t\t-> ".print
    h([2,3]).println

    "h()\t\t\t-> ".print
    h().println

    "i(0)\t\t\t-> ".print
    i(0).println

    "j(0)\t\t\t-> ".print
    j(0).println

    "k([])\t\t\t-> ".print
    (fn() { k([]).show }).try(show).println

    "l((1, 2, 3))\t\t-> ".print
    l((1, 2, 3)).println
}
