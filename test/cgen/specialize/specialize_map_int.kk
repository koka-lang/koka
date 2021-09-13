fun map_int(xs : list<int>, f : int -> int) : list<int> {
    match (xs) {
        Nil -> Nil
        Cons(x, xx) -> Cons(x.f, xx.map_int(f))
    }
}

public fun a() {
    map_int([1,2,3], fn(x) { x + 1 })
}

fun main() {
    a().foreach(println)
}