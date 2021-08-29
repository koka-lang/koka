// Haskell's Data.List.sort function ported to Koka

module haskell-sort

alias elem = int

fun sequences(xs : list<elem>) {
    match(xs) {
        Cons(a, Cons(b, xs)) {
            if(a > b) then descending(b, [a], xs)
            else ascending(b, (fn(x) { Cons(a, x) }), xs)
        }
        _ -> [xs]
    }
}

fun descending(a : elem, as_, bs) {
    match(bs) {
        Cons(b, bs) | a > b -> descending(b, Cons(a, as_), bs)
        _ -> Cons(Cons(a, as_), sequences(bs))
    }
}

fun ascending(a : elem, as_, bs) {
    match(bs) {
        Cons(b, bs) | !(a > b) -> ascending(b, fn(ys) { as_(Cons(a, ys)) }, bs)
        _ -> Cons(as_([a]), sequences(bs))
    }
}

fun merge-all(xs) {
    match(xs) {
        Cons(x, Nil) -> x
        _ -> merge-all(merge-pairs(xs))
    }
}

fun merge-pairs(xs) {
    match(xs) {
        Cons(a, Cons(b, xs)) -> Cons(merge(a, b), merge-pairs(xs))
        _ -> xs
    }
}

fun merge(as_ : list<elem>, bs_ : list<elem>) : <exn,div> list<elem> {
    match(as_, bs_) {
        (Cons(a, as'), Cons(b, bs')) {
            if(a > b) then Cons(b, merge(as_, bs'))
            else Cons(a, merge(as', bs_))
        }
        (Nil, _) -> bs_
        (_, Nil) -> as_
    }
}

fun sort(xs : list<elem>) {
    merge-all(sequences(xs))
}