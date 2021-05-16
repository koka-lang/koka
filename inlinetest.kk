// sr -- --core --fno-optctail inlinetest.kk

fun noeffectmap(f : int -> int, xs : list <int>) : list<int> {
    match (xs) {
        Nil -> Nil
        Cons(x, xs) -> Cons(f(x), noeffectmap(f, xs))
    }
}

// fun mapplusone(xs : list<int>) : list<int> {
//     // this gets inlined during the simplification pass
//     // val plusone = fn (x) { x + 1 }
//     match (xs) {
//         Nil -> Nil
//         Cons(x, xs) -> Cons(plusone(x), mapplusone(xs))
//     }
// }

// fun mapEither(xs : list<either<a, b>>, f : a -> x, g : b -> y) : list<either<x, y>> {
//     match (xs) {
//         Nil -> Nil
//         Cons(Left(a), xs) -> Cons(Left(f(a)), mapEither(xs, f, g))
//         Cons(Right(b), xs) -> Cons(Right(g(b)), mapEither(xs, f, g))
//     }
// }

// fun mapEven(xs : list<a>, even : a -> b, odd : a -> b) : _ list<b> {
//     match (xs) {
//         Nil -> Nil
//         Cons(x, xs) -> Cons(even(x), mapOdd(xs, even, odd))
//     }
// }

// fun mapOdd(xs : list<a>, even : a -> b, odd : a -> b) : _ list<b> {
//     match(xs) {
//         Nil -> Nil
//         Cons(x, xs) -> Cons(odd(x), mapEven(xs, even, odd))
//     }
// }

// first step
// fun mapEvenOne(xs : list<a>, odd : a -> b) : _ list<b> {
//     match (xs) {
//         Nil -> Nil
//         Cons(x, xs) -> Cons(x+1, mapOdd(xs, even, odd))
//     }
// }

// fun mapOddOne(xs : list<a>, even : a -> b) : _ list<b> {
//     match(xs) {
//         Nil -> Nil
// !!! we previously changed mapEven to mapEvenOne . We need to keep track of this here
// at this point even is being passed in but not used at all
//         Cons(x, xs) -> Cons(x - 1, mapEvenOne(xs, odd))
//     }
// }


fun main() {
    // f(2).println
    // val f = fn(x) { x + 3 }
    // (fn(x) { x + 3 })(2).println
    // mapEven([1,2,3,4,5], fn (x) { x+1 }, fn(x) { x - 1 } ).foreach(println)
    noeffectmap(fn (x) { x + 1 }, [1,2,3]).foreach(println)
}