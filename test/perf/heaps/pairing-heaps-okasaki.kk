// Section 5.5

module pairing-heaps-okasaki

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

fun find-min(h) {
    match(h) {
        E -> raise("Empty")
        T(x, hs) -> x
    }
}

fun delete-min(h) {
    match(h) {
        E -> raise("Empty")
        T(x, hs) -> merge-pairs(hs)
    }
}

fun from-list2(xs) {
    xs.map(fn(x) { singleton(x) }).merge-pairs
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
        T(x, hs) -> Cons(x, to-list(merge-pairs(hs)))
    }
}

public fun heapsort-pairing-heaps-okasaki(xs : list<int>) : <exn, div> list<int> {
    to-list(from-list(xs, E))
}