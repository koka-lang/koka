/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// TODO unit tests
// TODO maybe allow for compare functions with effects
// TODO maybe add in the other functions from sorted-set
module std/data/sorted-dict

import red-black-tree


abstract struct sorted-dict<k, a>(
  compare: (k, (k, a)) -> order,
  tree: tree<(k, a)>
)

fun from-compare(compare: (k, k) -> order): ((k, (k, a)) -> order) {
  (fun(k1, v2) {
    match (v2) {
      (k2, _) -> compare(k1, k2)
    }
  })
}

public fun from-list(list: list<(k, a)>, compare: (k, k) -> order): sorted-dict<k, a> {
  val compare2 = from-compare(compare)
  Sorted-dict(compare2, from-list(list, fst, compare2))
}

public fun to-list(dict: sorted-dict<k, a>): list<(k, a)> {
  to-list(dict.tree)
}

public fun empty(compare: (k, k) -> order): sorted-dict<k, a> {
  Sorted-dict(from-compare(compare), empty)
}

public fun [](dict: sorted-dict<k, a>, key: k): maybe<a> {
  unsafe-lookup(dict.tree, key, dict.compare).map(snd)
}

// TODO inline this
// TODO maybe it shouldn't include this ?
public fun [](dict: sorted-dict<k, a>, key: k, value: a): sorted-dict<k, a> {
  set(dict, key, value)
}

public fun has?(dict: sorted-dict<k, a>, key: k): bool {
  // TODO this can be implemented faster
  bool(dict[key])
}

public fun set(dict: sorted-dict<k, a>, key: k, value: a): sorted-dict<k, a> {
  dict(tree = unsafe-insert(dict.tree, key, (key, value), dict.compare, True))
}

public fun remove(dict: sorted-dict<k, a>, key: k): sorted-dict<k, a> {
  dict(tree = unsafe-remove(dict.tree, key, dict.compare))
}

public fun merge-left(left: sorted-dict<k, a>, right: sorted-dict<k, a>): sorted-dict<k, a> {
  val compare = left.compare
  left(
    tree = right.tree.foldl(left.tree) fun(old, a) {
      match (a) {
        // If a value exists in both `left` and `right` it will prefer the value from `left`
        (key, _) -> unsafe-insert(old, key, a, compare, False)
      }
    }
  )
}

public fun merge-right(left: sorted-dict<k, a>, right: sorted-dict<k, a>): sorted-dict<k, a> {
  val compare = right.compare
  right(
    tree = left.tree.foldl(right.tree) fun(old, a) {
      match (a) {
        // If a value exists in both `left` and `right` it will prefer the value from `right`
        (key, _) -> unsafe-insert(old, key, a, compare, False)
      }
    }
  )
}

public fun subset-of?(smaller: sorted-dict<k, a>, bigger: sorted-dict<k, a>): bool {
  smaller.tree.find-first fun(a) {
    match (a) {
      (key, _) -> if (bigger.has?(key)) {
        Nothing
      } else {
        Just(False)
      }
    }
  }.default(True)
}
