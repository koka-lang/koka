// Section 3.1

module weight-biased-leftist-heaps-okasaki

import std/num/int32
import base

alias elem = int

type heap {
    E
    T(i : int32, e : elem, l : heap, r : heap)
}

fun rank(h) {
    match(h) {
        E -> 0.int32
        T(r, _, _, _) -> r
    }
}

fun singleton(x) {
    T(1.int32, x, E, E)
}

fun merge(h1, h2) {
    match(h1, h2) {
        (h, E) -> h
        (E, h) -> h
        (T(r1, x, a1, b1), T(r2, y, a2, b2)) {
            if(x < y) {
                if(rank(a1) < rank(b1) + r2)
                then T(r1 + r2, x, merge(b1, T(r2, y, a2, b2)), a1)
                else T(r1 + r2, x, a1, merge(b1, T(r2, y, a2, b2)))
            } else {
                if(rank(a2) < rank(b2) + r1)
                then T(r1 + r2, y, merge(b2, T(r1, x, a1, b1)), a2)
                else T(r1 + r2, y, a2, merge(b2, T(r1, x, a1, b1)))
            }
        }
    }
}

fun insert(x, h) { merge(singleton(x), h) }

fun find-min(h) {
    match(h) {
        E -> raise("Empty")
        T(_, x, _, _) -> x
    }
}

fun delete-min(h) {
    match(h) {
        E -> raise("Empty")
        T(_, _, a, b) -> merge(a, b)
    }
}

fun merge-pairs(xs) {
    match(xs) {
        Cons(h1, Cons(h2, xs')) -> Cons(merge(h1, h2), merge-pairs(xs'))
        _ -> xs
    }
}

fun from-heap-list(xs) {
    match(xs) {
        Nil -> E
        Cons(h, Nil) -> h
        _ -> from-heap-list(merge-pairs(xs))
    }
}

fun from-list2(xs) {
    xs.map(fn(x) { singleton(x) }).from-heap-list
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
        E -> Nil
        T(_, m, a, b) -> Cons(m, to-list(merge(a, b)))
    }
}

public fun heapsort-weight-biased-leftist-heaps-okasaki(xs : list<int>) : <div> list<int> {
    to-list(from-list(xs, E))
}