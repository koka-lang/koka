// Section 3.2

// TODO: This segfaults randomly on an M1
// Maybe this is a memory barrier bug with the new arch?

module binomial-heaps-okasaki

import std/num/int32
import base

alias elem = int

pub type tree {
    Node(root : elem, c : list<tree>)
}

pub type heap {
    Empty
    Heap(minimum : elem, t : list<(int32, tree)>)
}

pub fun tree(h) {
    match(h) {
        Empty -> []
        Heap(_, t) -> t
    }
}

fun cmp-min(h, m) {
    match(h) {
        Empty -> m
        Heap(m2, _) -> min(m, m2)
    }
}

fun link(t1 : (int32, tree), t2 : (int32, tree)) {
    val l = match(t1.snd, t2.snd) {
        (Node(x1, c1), Node(x2, c2)) {
            if(x1 <= x2) { Node(x1, Cons(t2.snd,c1)) }
            else { Node(x2, Cons(t1.snd, c2)) }
        }
    }
    (t1.fst + 1.int32, l)
}

fun insert-tree(t : (int32, tree), ts : list<(int32, tree)>) {
    match(ts) {
        Nil -> [t]
        Cons(t', ts') -> if(t.fst < t'.fst) { Cons(t, ts) }
          else { insert-tree(link(t, t'), ts') }
    }
}

fun insert(x : elem, h : heap) {
    Heap(h.cmp-min(x), insert-tree((0.int32, Node(x, [])), h.tree))
}

fun merge-lists(ts1 : list<(int32, tree)>, ts2 : list<(int32, tree)>) : <div> list<(int32, tree)> {
    match(ts1, ts2) {
        (ts, Nil) -> ts
        (Nil, ts) -> ts
        (Cons(t1, ts1'), Cons(t2, ts2')) {
            if(t1.fst < t2.fst) { Cons(t1, merge-lists(ts1', ts2)) }
            elif(t2.fst < t1.fst) { Cons(t2, merge-lists(ts1, ts2')) }
            else { insert-tree(link(t1, t2), merge-lists(ts1', ts2')) }
        }
    }
}

fun merge(h1 : heap, h2 : heap) {
    match(h1, h2) {
        (Empty, h) -> h
        (h, Empty) -> h
        (Heap(m1, t1), Heap(m2, t2)) -> Heap(min(m1, m2), merge-lists(t1, t2))
    }
}

fun find-min(h : heap) {
    match(h) {
        Empty -> raise("Empty")
        Heap(m, _) -> m
    }
}

pub fun remove-min-tree(ts : list<(int32, tree)>) {
    match(ts) {
        Nil -> raise("Empty")
        Cons(t, Nil) -> (t.snd, Nil)
        Cons(t, ts) {
            val (t', ts') = remove-min-tree(ts)
            if(t.snd.root <= t'.root) { (t.snd, ts) }
            else { (t', Cons(t, ts')) }
        }
    }
}

pub fun show-tree(t) {
    match(t) {
        Node(r, ts) {
            "Node(" ++ show(r) ++ ", [" ++ ts.map(fn(x) { show-tree(x) ++ ", " }).join ++ "])"
        }
    }
}

pub fun show-trees(ts : list<(int32, tree)>) {
  "[" ++ ts.map(fn((i:int32, t)) { "(" ++ show(i.int) ++ "," ++ show-tree(t) ++ "), " }).join ++ "]"
}

pub fun show-pair(p : (tree, list<(int32, tree)>)) {
    match(p) {
        (t, ts) -> "(" ++ show-tree(t) ++ ", [" ++ ts.map(fn((i:int32, t)) { "(" ++ show(i.int) ++ "," ++ show-tree(t) ++ "), " }).join ++ "])"
    }
}

fun find-min-list(ts : list<(int32, tree)>, m : maybe<elem>) : maybe<elem> {
    match(ts) {
        Nil -> m
        Cons(t, ts) -> find-min-list(ts, match(m) {
            Nothing -> Just(t.snd.root)
            Just(m) -> Just(min(m, t.snd.root))
        })
    }
}

fun add-ranks(ts : list<tree>, r = 0.int32) : list<(int32, tree)> {
    match(ts) {
        Nil -> Nil
        Cons(t, ts') -> Cons((r, t), add-ranks(ts', r + 1.int32))
    }
}

fun delete-min(ts : heap) {
    val (Node(_, ts1), ts2) = remove-min-tree(ts.tree)
    val new-tree = merge-lists(ts1.reverse.add-ranks, ts2)
    match(find-min-list(new-tree, Nothing)) {
        Nothing -> Empty
        Just(m) -> Heap(m, new-tree)
    }
}

// Heapsort

pub fun from-list(xs, h) {
    match(xs) {
        Nil -> h
        Cons(x, xs) -> from-list(xs, insert(x, h))
    }
}

pub fun to-list(h) {
    match(h) {
        Empty -> []
        Heap(m, _) -> Cons(m, to-list(delete-min(h)))
    }
}

pub fun heapsort-binomial-heaps-okasaki(xs : list<int>) : <exn, div, console> list<int> {
    to-list(from-list(xs, Empty))
}

fun main() {
    println(show(heapsort-binomial-heaps-okasaki([1,2,3])))
}