/*---------------------------------------------------------------------------
  Copyright 2024-2026, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Ordered maps, as red-black trees (`std/data/rbtree`).

Keys are ordered by an implicit `?cmp`, and are borrowed by every comparison.
Iteration (`list`, `keys`, `foldl`, `foreach`) is in key order.
*/
module std/data/map

import std/data/rbtree

pub value struct map<k,v>
  tree: rbtree<k,v>

pub fun set(s: map<k,v>, k: k, v: v, ^?cmp: (k,k) -> e order): e map<k,v>
  val Map(tree) = s
  Map(rbtree/set(tree, k, v))

// the existing value wins
pub fun add(s: map<k,v>, k: k, v: v, ^?cmp: (k,k) -> e order): e map<k,v>
  val Map(tree) = s
  Map(rbtree/add(tree, k, v))

pub fun insert(s: map<k,v>, k: k, f: (maybe<v>) -> e v, ^?cmp: (k,k) -> e order): e map<k,v>
  val Map(tree) = s
  Map(rbtree/insert(tree, k, f))

pub fun remove(s: map<k,v>, k: k, ^?cmp: (k,k) -> e order): e map<k,v>
  val Map(tree) = s
  Map(rbtree/remove(tree, k))

pub fun contains(s: map<k,v>, ^k: k, ^?cmp: (k,k) -> e order): e bool
  val Map(tree) = s
  rbtree/contains(tree, k)

pub fun empty(): map<k,v>
  Map(empty())

pub fun list(s: map<k,v>) : list<(k,v)>
  val Map(tree) = s
  tree.list

pub fun keys(s: map<k,v>) : list<k>
  val Map(tree) = s
  tree.keys

pub fun values(s: map<k,v>) : list<v>
  val Map(tree) = s
  tree.values

pub fun lookup(s: map<k,v>, ^k: k, ^?cmp: (k,k) -> e order): e maybe<v>
  val Map(tree) = s
  rbtree/lookup(tree, k)

pub fun show(s: map<k,v>, ?k/show: k -> e string, ?v/show: v -> e string): e string
  val Map(tree) = s
  tree.list.show

pub fun kvalue/map(s: map<k,v>, f: (k, v) -> e x): e map<k,x>
  val Map(tree) = s
  Map(tree.map(f))

pub fun value/map(s: map<k,v>, f: (v) -> e x): e map<k,x>
  val Map(tree) = s
  Map(tree.map(f(_)))

// lexicographic over the entries in key order
pub fun cmp(s1: map<k,v>, s2: map<k,v>, ^?k/cmp: (k,k) -> e order, ^?v/cmp: (v,v) -> e order): e order
  fun go(xs, ys)
    match xs
      Nil -> if ys.is-nil then Eq else Lt
      std/core/types/Cons((k1, v1), xx) -> match ys
       Nil -> Gt
       std/core/types/Cons((k2, v2), yy) -> match k/cmp(k1, k2)
         Eq -> match v/cmp(v1, v2)
           Eq -> go(xx, yy)
           o -> o
         o -> o
  go(s1.list, s2.list)

pub fun (==)(s1: map<k,v>, s2: map<k,v>, ^?k/cmp: (k,k) -> e order, ^?v/cmp: (v,v) -> e order): e bool
  cmp(s1, s2) == Eq

//----------------------------------------------------------------------------
// Map operations
//----------------------------------------------------------------------------

pub fun is-empty(s: map<k,v>): bool
  match s
    Map(Leaf) -> True
    _ -> False

pub fun size(s: map<k,v>): int
  s.list.length

pub fun contains-key(s: map<k,v>, ^k: k, ^?cmp: (k,k) -> e order): e bool
  s.contains(k)

// later entries win, as with repeated `set`
pub fun from-list(l: list<(k,v)>, ^?cmp: (k,k) -> e order): e map<k,v>
  l.foldl(empty(), fn(acc, kv) { val (k, v) = kv; acc.set(k, v) })

pub fun entries(s: map<k,v>): list<(k,v)>
  s.list

// lookup-or-throw
pub fun get(s: map<k,v>, ^k: k, ^?cmp: (k,k) -> <exn|e> order): <exn|e> v
  match s.lookup(k)
    Just(x) -> x
    Nothing -> throw("map/get: key not found")

pub fun get-default(s: map<k,v>, ^k: k, default: v, ^?cmp: (k,k) -> e order): e v
  match s.lookup(k)
    Just(x) -> x
    Nothing -> default

// If `k` is present apply `f(old, b)`, otherwise insert `b`.
pub fun update(s: map<k,v>, k: k, b: v, f: (v, v) -> e v, ^?cmp: (k,k) -> e order): e map<k,v>
  fun combine(mb)
    match mb
      Just(old) -> f(old, b)
      Nothing   -> b
  s.insert(k, combine)

// ADD-IF-ABSENT for every pair (the existing value wins), as `add`
pub fun add-all(s: map<k,v>, l: list<(k,v)>, ^?cmp: (k,k) -> e order): e map<k,v>
  l.foldl(s, fn(acc, kv) { val (k, v) = kv; acc.add(k, v) })

pub fun listadd/(+)(s: map<k,v>, l: list<(k,v)>, ^?cmp: (k,k) -> e order): e map<k,v>
  s.add-all(l)

pub fun foldl(s: map<k,v>, acc: a, f: (a, k, v) -> e a): e a
  s.list.foldl(acc, fn(x, kv) { val (k, v) = kv; f(x, k, v) })

pub fun foreach(s: map<k,v>, f: (k, v) -> e ()): e ()
  s.list.foreach(fn(kv) { val (k, v) = kv; f(k, v) })

pub fun filter(s: map<k,v>, pred: (k, v) -> e bool, ^?cmp: (k,k) -> e order): e map<k,v>
  s.list.foldl(empty()) fn(acc, kv)
    val (k, v) = kv
    if pred(k, v) then acc.set(k, v) else acc

// Folds `s2` INTO `s1`, so `f` sees (value-from-s1, value-from-s2).
pub fun union-with(s1: map<k,v>, s2: map<k,v>, f: (v, v) -> e v, ^?cmp: (k,k) -> e order): e map<k,v>
  s2.list.foldl(s1, fn(acc, kv) { val (k, v) = kv; acc.update(k, v, f) })

// left-biased: the value from `s1` wins
pub fun union(s1: map<k,v>, s2: map<k,v>, ^?cmp: (k,k) -> e order): e map<k,v>
  union-with(s1, s2, fn(x, _) x)

// Later maps' values come first in `combine`: each map is folded into the accumulator with the arguments flipped.
pub fun unions-with(l: list<map<k,v>>, combine: (v, v) -> e v, ^?cmp: (k,k) -> e order): e map<k,v>
  l.foldl(empty(), fn(acc, m) union-with(acc, m, fn(x, y) combine(y, x)))

// earlier maps win
pub fun unions(l: list<map<k,v>>, ^?cmp: (k,k) -> e order): e map<k,v>
  l.foldl(empty(), fn(acc, m) union(acc, m))

// Removes every key in `ks`; absent keys are ignored.
pub fun remove-all(s: map<k,v>, ks: list<k>, ^?cmp: (k,k) -> e order): e map<k,v>
  ks.foldr(s, fn(k, acc) acc.remove(k))
