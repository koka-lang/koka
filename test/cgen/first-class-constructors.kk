// Tests C code generation of constructors treated as first-class values.

fun cons-alias() : ((a, list<a>) -> list<a>) {
    return Cons
}

fun main() {
    val list_of_maybes = map(cons-alias()(1, [2, 3]), Just)
    val list_of_ints = map(
        list_of_maybes,
        fn(maybe_el) { match(maybe_el) { Just(el) -> el } }
    )
    println(show(list_of_ints))
}
