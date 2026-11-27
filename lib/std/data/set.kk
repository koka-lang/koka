/*---------------------------------------------------------------------------
  Copyright 2024-2026, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Ordered sets, as red-black trees (`std/data/rbtree`).

Elements are ordered by an implicit `?cmp`, and are borrowed by every comparison.
Iteration (`list`) is in element order.
*/
module std/data/set

import std/data/rbtree

pub value struct set<k>
  tree: rbtree<k,()>

pub fun set/add(s: set<k>, k: k, ^?cmp: (k,k) -> e order): e set<k>
  val Set(tree) = s
  Set(rbtree/set(tree, k, ()))

// Adds `k`, also reporting whether it was new.
// Not spelled `check/add`: a second `add` with the same parameter shape makes every unannotated `s.add(x)` ambiguous.
pub fun check-add(s: set<k>, k: k, ^?cmp: (k,k) -> e order): e (bool, set<k>)
  val Set(tree) = s
  if rbtree/contains(tree, k) then (False, Set(tree))
  else (True, Set(rbtree/set(tree, k, ())))

pub fun contains(s: set<k>, ^k: k, ^?cmp: (k,k) -> e order): e bool
  val Set(tree) = s
  rbtree/contains(tree, k)

pub fun empty(): set<k>
  Set(empty())

pub fun list(s: set<k>) : list<k>
  val Set(tree) = s
  tree.keys

pub fun show(s: set<k>, ?k/show: k -> e string): e string
  val Set(tree) = s
  tree.keys.show

// lexicographic over the elements in key order
pub fun cmp(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e order
  fun go(xs, ys)
    match xs
      Nil -> if ys.is-nil then Eq else Lt
      std/core/types/Cons(x, xx) -> match ys
        Nil -> Gt
        std/core/types/Cons(y, yy) -> match cmp(x, y)
          Eq -> go(xx, yy)
          o -> o
  go(s1.list, s2.list)

//----------------------------------------------------------------------------
// Set operations
//
// There is deliberately no `any`/`all`/`foldl`/`foreach`: those names collide with `list`'s wherever a receiver's type is not yet known, so go through `.list`.
//----------------------------------------------------------------------------

pub fun member(s: set<k>, ^k: k, ^?cmp: (k,k) -> e order): e bool
  s.contains(k)

pub fun is-empty(s: set<k>): bool
  match s
    Set(Leaf) -> True
    _ -> False

pub fun size(s: set<k>): int
  s.list.length

pub fun length(s: set<k>): int
  s.size

pub fun from-list(l: list<k>, ^?cmp: (k,k) -> e order): e set<k>
  l.foldl(empty(), fn(acc, x) acc.set/add(x))

pub fun remove(s: set<k>, k: k, ^?cmp: (k,k) -> e order): e set<k>
  val Set(tree) = s
  Set(rbtree/remove(tree, k))

pub fun filter(s: set<k>, pred: (k) -> e bool, ^?cmp: (k,k) -> e order): e set<k>
  s.list.foldl(empty()) fn(acc, x)
    if pred(x) then acc.set/add(x) else acc

// Inserts `s2`'s elements into `s1`: O(|s2| log |s1|).
// There is no "insert the smaller into the larger" check: the tree stores no count, so `size` is a full walk, and paying it per step makes `unions` over k sets quadratic in the accumulated size.
pub fun union(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e set<k>
  s2.list.foldl(s1, fn(acc, x) acc.set/add(x))

pub fun unions(ss: list<set<k>>, ^?cmp: (k,k) -> e order): e set<k>
  ss.foldl(empty(), fn(acc, s) union(acc, s))

pub fun difference(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e set<k>
  s1.list.foldl(empty()) fn(acc, x)
    if s2.member(x) then acc else acc.set/add(x)

pub fun intersection(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e set<k>
  s1.list.foldl(empty()) fn(acc, x)
    if s2.member(x) then acc.set/add(x) else acc

pub fun disjoint(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e bool
  !s1.list.any(fn(x) s2.member(x))

// the sets share at least one element
pub fun common(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e bool
  s1.list.any(fn(x) s2.member(x))

pub fun is-subset-of(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e bool
  s1.list.all(fn(x) s2.member(x))

pub fun (==)(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e bool
  s1.size == s2.size && s1.is-subset-of(s2)

// --- operator spellings -----------------------------------------------------

pub fun member/(+)(s: set<k>, k: k, ^?cmp: (k,k) -> e order): e set<k>
  s.set/add(k)

pub fun set/(+)(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e set<k>
  union(s1, s2)

pub fun list/(+)(s: set<k>, l: list<k>, ^?cmp: (k,k) -> e order): e set<k>
  l.foldl(s, fn(acc, x) acc.set/add(x))

pub fun member/(-)(s: set<k>, k: k, ^?cmp: (k,k) -> e order): e set<k>
  s.remove(k)

pub fun member-maybe/(-)(s: set<k>, mk: maybe<k>, ^?cmp: (k,k) -> e order): e set<k>
  match mk
    Just(k) -> s.remove(k)
    Nothing -> s

pub fun set/(-)(s1: set<k>, s2: set<k>, ^?cmp: (k,k) -> e order): e set<k>
  difference(s1, s2)

pub fun list/(-)(s: set<k>, l: list<k>, ^?cmp: (k,k) -> e order): e set<k>
  l.foldl(s, fn(acc, x) acc.remove(x))
