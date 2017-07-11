/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// TODO unit tests
// TODO disjoint unions
// TODO power set
// TODO cartesian product
// TODO maybe allow for compare functions with effects
module std/data/sorted-set

import red-black-tree


abstract struct sorted-set<a>(
  compare: (a, a) -> order,
  tree: tree<a>
)

public fun from-list(list: list<a>, compare: (a, a) -> order): sorted-set<a> {
  Set(compare, from-list(list, compare))
}

public fun to-list(set: sorted-set<a>): list<a> {
  to-list(set.tree)
}

public fun empty(compare: (a, a) -> order): sorted-set<a> {
  Set(compare, empty)
}

public fun has?(set: sorted-set<a>, value: a): bool {
  // TODO this can be implemented faster
  bool(unsafe-lookup(set.tree, value, set.compare))
}

public fun insert(set: sorted-set<a>, value: a): sorted-set<a> {
  set(tree = unsafe-insert(set.tree, value, set.compare, True))
}

public fun remove(set: sorted-set<a>, value: a): sorted-set<a> {
  set(tree = unsafe-remove(set.tree, value, set.compare))
}

public fun exclude(set: sorted-set<a>, exclude: sorted-set<a>): sorted-set<a> {
  val compare = set.compare
  set(
    tree = exclude.tree.foldl(set.tree) fun(old, value) {
      unsafe-remove(old, value, compare)
    }
  )
}

public fun intersection-right(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  val compare = right.compare
  right(
    tree = right.tree.foldl(empty) fun(old, value) {
      if (left.has?(value)) {
        unsafe-insert(old, value, compare, True)
      } else {
        old
      }
    }
  )
}

public fun intersection-left(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  intersection-right(right, left)
}

public fun difference-left(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  // TODO implement this more efficiently
  union-left(exclude(left, right), exclude(right, left))
}

public fun difference-right(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  // TODO implement this more efficiently
  union-right(exclude(left, right), exclude(right, left))
}

public fun union-left(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  val compare = left.compare
  left(
    tree = right.tree.foldl(left.tree) fun(old, value) {
      // If a value exists in both `left` and `right` it will prefer the value from `left`
      unsafe-insert(old, value, compare, False)
    }
  )
}

public fun union-right(left: sorted-set<a>, right: sorted-set<a>): sorted-set<a> {
  val compare = right.compare
  right(
    tree = left.tree.foldl(right.tree) fun(old, value) {
      // If a value exists in both `left` and `right` it will prefer the value from `right`
      unsafe-insert(old, value, compare, False)
    }
  )
}

public fun subset-of?(smaller: sorted-set<a>, bigger: sorted-set<a>): bool {
  smaller.tree.find-first fun(value) {
    if (bigger.has?(value)) {
      Nothing
    } else {
      Just(False)
    }
  }.default(True)
}
