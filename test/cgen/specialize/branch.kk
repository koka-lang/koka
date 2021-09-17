fun map_other(xs : list<a>, f : a -> b, g : a -> b) : list<b> {
    val isEven : bool = xs.length % 2 == 0
    match (xs) {
        Nil -> Nil
        Cons(x, xx) | isEven    -> Cons(f(x), xx.map_other(f, g))
                    // this branch seems to get ignored if this function is total
                    | _         -> Cons(g(x), xx.map_other(g, f))
    }
}

fun main() {
    val plusone = fn(x) { x + 1 }
    val minusone = fn(x) { x - 1 }
    [1,2,3,4].map_other(plusone, minusone).foreach(println)
}