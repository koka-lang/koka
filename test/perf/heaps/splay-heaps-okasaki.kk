// Section 5.4

module splay-heaps-okasaki

import std/num/int32
import base

alias elem = int

type tree {
    E
    T(l : tree, a : elem, r : tree)
}

fun partition(pivot : elem, t : tree) {
    match(t) {
        E -> (E, E)
        T(a, x, b) {
            if(x <= pivot) {
                match(b) {
                    E -> (t, E)
                    T(b1, y, b2) {
                        if(y <= pivot) {
                            val (small, big) = partition(pivot, b2)
                            (T(T(a, x, b1), y, small), big)
                        } else {
                            val (small, big) = partition(pivot, b1)
                            (T(a, x, small), T(big, y, b2))
                        }
                    }
                }
            } else {
                match(a) {
                    E -> (E, t)
                    T(a1, y, a2) {
                        if(y <= pivot) {
                            val (small, big) = partition(pivot, a2)
                            (T(a1, y, small), T(big, x, b))
                        } else {
                            val (small, big) = partition(pivot, a1)
                            (small, T(big, y, T(a2, x, b)))
                        }
                    }
                }
            }
        }
    }
}

fun insert(x : elem, h : tree) {
    val (a, b) = partition(x, h)
    T(a, x, b)
}

fun merge(t2, t) {
    match(t2) {
        E -> t
        T(a, x, b) {
            val (ta, tb) = partition(x, t)
            T(merge(ta, a), x, merge(tb, b))
        }
    }
}

fun find-min(t) {
    match(t) {
        E -> raise("Empty")
        T(E, x, b) -> x
        T(a, x, b) -> find-min(a)
    }
}

fun delete-min(t) {
    match(t) {
        E -> raise("Empty")
        T(E, x, b) -> b
        T(T(E, x, b), y, c) -> T(b, y, c)
        T(T(a, x, b), y, c) -> T(delete-min(a), x, T(b, y, c))
    }
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

pub fun heapsort-splay-heaps-okasaki(xs : list<int>) : <exn, div> list<int> {
    to-list(from-list(xs, E))
}