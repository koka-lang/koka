// Avoid the non-tail recursion of 'merge-pairs'
// For some reason this is actually slower than Okasaki's version
// even though it should show good reusing behaviour..

module pairing-heaps-tailcall

import base

alias elem = int

type heap {
    E
    T(a : elem, hs : list<heap>)
}

fun singleton(a) {
    T(a, [])
}

fun merge(h1, h2) {
    match(h1, h2) {
        (h, E) -> h
        (E, h) -> h
        (T(x, hs1), T(y, hs2)) {
            if(x <= y) then T(x, Cons(h2, hs1))
            else T(y, Cons(h1, hs2))
        }
    }
}

fun insert(x, h) {
    merge(singleton(x), h)
}

fun merge-pairs(hs) {
    match(hs) {
        Nil -> E
        Cons(h, Nil) -> h
        Cons(h1, Cons(h2, hs)) -> merge(merge(h1, h2), merge-pairs(hs))
    }
}

fun merge-later(h, ms) {
    match(ms) {
        Nil -> h
        Cons(h2, ms) -> merge-later(merge(h, h2), ms)
    }
}

fun merge-pairs-tailcall(hs, ms) {
    match(hs) {
        Nil -> merge-later(E, ms)
        Cons(h, Nil) -> merge-later(h, ms)
        Cons(h1, Cons(h2, hs)) -> merge-pairs-tailcall(hs, Cons(merge(h1, h2), ms)) // merge(merge(h1, h2), merge-pairs(hs))
    }
}

fun find-min(h) {
    match(h) {
        E -> raise("Empty")
        T(x, hs) -> x
    }
}

fun delete-min(h) {
    match(h) {
        E -> raise("Empty")
        T(x, hs) -> merge-pairs-tailcall(hs, [])
    }
}

fun from-list2(xs) {
    xs.map(fn(x) { singleton(x) }).merge-pairs-tailcall([])
}

// Heapsort

fun from-list(xs, h) {
    match(xs) {
        Nil -> h
        Cons(x, xs) -> from-list(xs, insert(x, h))
    }
}

fun to-list(h) {
    match(h) {
        E -> []
        T(x, hs) -> Cons(x, to-list(merge-pairs-tailcall(hs, [])))
    }
}

pub fun heapsort-pairing-heaps-tailcall(xs : list<int>) : <exn, div> list<int> {
    to-list(from-list(xs, E))
}