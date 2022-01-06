// Haskell's Data.List.sort function ported to Koka

module haskell-sort

alias elem = int

fun sequences(xs : list<elem>) : <div> list<list<elem>> {
    match(xs) {
        Cons(a, Cons(b, xs1)) {
            if(a > b) then descending(b, [a], xs1)
            else ascending(b, (fn(x) { Cons(a, x) }), xs1)
        }
        _ -> [xs]
    }
}

fun descending(a : elem, as_ : list<elem>, bs : list<elem>) : <div> list<list<elem>> {
    match(bs) {
        Cons(b, bs1) | a > b -> descending(b, Cons(a, as_), bs1)
        _ -> Cons(Cons(a, as_), sequences(bs))
    }
}

fun ascending(a : elem, f : list<elem> -> list<elem>, bs : list<elem>) : <div> list<list<elem>> {
    match(bs) {
        Cons(b, bs1) | (a <= b) ->
          ascending(b, fn(ys) { f(Cons(a, ys)) }, bs1)
        _ -> Cons(f([a]), sequences(bs))
    }
}

fun merge-all(xs : list<list<elem>>) : <div> list<elem> {
    match(xs) {
        Cons(x, Nil) -> x
        _ -> merge-all(merge-pairs(xs))
    }
}

fun merge-pairs(xs : list<list<elem>>) : <div> list<list<elem>> {
    match(xs) {
        Cons(a, Cons(b, xs1)) -> Cons(merge(a, b), merge-pairs(xs1))
        _ -> xs
    }
}

fun merge(as' : list<elem>, bs : list<elem>) : <div> list<elem> {
    match(as') {
        Cons(a, as1) -> match(bs) {
            Cons(b, bs1) ->
              if a > b then Cons(b, merge(as', bs1))
              else Cons(a, merge(as1, bs))
            Nil -> as'
        }
        Nil -> bs
    }
}

pub fun merge-sort(xs : list<int>) : <div> list<int> {
    merge-all(sequences(xs))
}