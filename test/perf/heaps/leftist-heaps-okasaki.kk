// Section 3.1

module leftist-heaps-okasaki

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

fun makeT(x, a, b) {
    val ra = rank(a); val rb = rank(b)
    if(ra >= rb) { T(rb + 1.int32, x, a, b) }
    else { T(ra + 1.int32, x, b, a) }
}

fun singleton(x) {
    T(1.int32, x, E, E)
}

fun merge(h1, h2) {
    match(h1, h2) {
        (h, E) -> h
        (E, h) -> h
        (T(_, x, a1, b1), T(_, y, a2, b2)) {
            if(x <= y) { makeT(x, a1, merge(b1, h2)) }
            else { makeT(y, a2, merge(h1, b2)) }
        }
    }
}

// fun insert(x, h) { merge(T(1, x, E, E), h) }
fun insert(x, h) {
    match(h) {
        E -> T(1.int32, x, E, E)
        T(_, y, a, b) {
            if(x <= y) { T(1.int32, x, h, E) }
            else { makeT(y, a, insert(x, b)) }
        }
    }
}

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
        E -> []
        _ -> Cons(find-min(h), to-list(delete-min(h)))
    }
}

pub fun heapsort-leftist-heaps-okasaki(xs : list<int>) : <exn, div> list<int> {
    to-list(from-list2(xs))
}