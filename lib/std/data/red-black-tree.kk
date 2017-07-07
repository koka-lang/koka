/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Adapted from https://github.com/sweirich/dth/blob/b888942f33425a871d46105862683e8950d15b23/examples/red-black/RedBlack.lhs
// TODO unit tests
module std/data/red-black-tree

type color {
  Red
  Black
}

abstract type tree<a> {
  Empty
  Node(left: tree<a>, value: a, right: tree<a>, color: color)
}


fun color(tree: tree<a>): color {
  match (tree) {
    Empty -> Black
    Node(_, _, _, color) -> color
  }
}


public val empty: forall<a> tree<a> = Empty


public fun single(value: a): tree<a> {
  Node(empty, value, empty, Black)
}


// This is unsafe because it's possible to use different compare functions on the same tree
public fun unsafe-lookup(tree: tree<a>, key: b, compare: (b, a) -> order): maybe<a> {
  match (tree) {
    Node(left, value, right, _) ->
      match (compare(key, value)) {
        Lt -> unsafe-lookup(left, key, compare)
        Eq -> Just(value)
        Gt -> unsafe-lookup(right, key, compare)
      }

    Empty ->
      Nothing
  }
}


public fun foldl(tree: tree<a>, initial: b, fn: (b, a) -> e b): e b {
  match (tree) {
    Node(left, value, right, _) -> foldl(right, fn(foldl(left, initial, fn), value), fn)
    Empty -> initial
  }
}


public fun foldr(tree: tree<a>, initial: b, fn: (a, b) -> e b): e b {
  match (tree) {
    Node(left, value, right, _) -> foldr(left, fn(value, foldr(right, initial, fn)), fn)
    Empty -> initial
  }
}


public fun to-list(tree: tree<a>): list<a> {
  tree.foldr([], Cons)
}


// TODO should this be called unsafe ?
public fun from-list(list: list<a>, compare: (a, a) -> order): tree<a> {
  list.foldl(empty) fun(l, r) {
    unsafe-insert(l, r, compare, True)
  }
}


fun balance(tree: tree<a>): tree<a> {
  match (tree) {
    Node(Node(Node(a, x, b, Red), y, c, Red), z, d, Black) ->
    Node(Node(a, x, b, Black), y, Node(c, z, d, Black), Red)

    Node(Node(a, x, Node(b, y, c, Red), Red), z, d, Black) ->
    Node(Node(a, x, b, Black), y, Node(c, z, d, Black), Red)

    Node(a, x, Node(Node(b, y, c, Red), z, d, Red), Black) ->
    Node(Node(a, x, b, Black), y, Node(c, z, d, Black), Red)

    Node(a, x, Node(b, y, Node(c, z, d, Red), Red), Black) ->
    Node(Node(a, x, b, Black), y, Node(c, z, d, Black), Red)

    a -> a
  }
}

fun balance-left(left: tree<a>, value: a, right: tree<a>): tree<a> {
  match (left) {
    Node(a, x, b, Red) ->
      Node(Node(a, x, b, Black), value, right, Red)

    _ -> match (right) {
      Node(a, y, b, Black) ->
        balance(Node(left, value, Node(a, y, b, Red), Black))

      Node(Node(a, y, b, Black), z, c, Red) ->
        Node(Node(left, value, a, Black), y, balance(Node(b, z, redden(c), Black)), Red)

      _ -> unreachable()
    }
  }
}

fun balance-right(left: tree<a>, value: a, right: tree<a>): tree<a> {
  match (right) {
    Node(b, y, c, Red) ->
      Node(left, value, Node(b, y, c, Black), Red)

    _ -> match (left) {
      Node(a, x, b, Black) ->
        balance(Node(Node(a, x, b, Red), value, right, Black))

      Node(a, x, Node(b, y, c, Black), Red) ->
        Node(balance(Node(redden(a), x, b, Black)), y, Node(c, value, right, Black), Red)

      _ -> unreachable()
    }
  }
}

fun unsafe-merge(left: tree<a>, right: tree<a>): tree<a> {
  // TODO replace this with multi-pattern matching ?
  match (left) {
    Empty -> right

    Node(a, x, b, Red) -> match (right) {
      Empty -> left

      Node(c, y, d, Red) ->
        match (unsafe-merge(b, c)) {
          Node(b1, z, c1, Red) -> Node(Node(a, x, b1, Red), z, Node(c1, y, d, Red), Red)
          bc -> Node(a, x, Node(bc, y, d, Red), Red)
        }

      _ -> Node(a, x, unsafe-merge(b, right), Red)
    }

    Node(a, x, b, Black) -> match (right) {
      Empty -> left

      Node(c, y, d, Black) ->
        match (unsafe-merge(b, c)) {
          Node(b1, z, c1, Red) -> Node(Node(a, x, b1, Black), z, Node(c1, y, d, Black), Red)
          bc -> balance-left(a, x, Node(bc, y, d, Black))
        }

      Node(b1, x1, c, Red) ->
        Node(unsafe-merge(left, b1), x1, c, Red)

      _ -> unreachable() // TODO remove this after Koka's exhaustiveness checker is fixed
    }

    _ -> unreachable() // TODO remove this after Koka's exhaustiveness checker is fixed
  }
}


fun blacken(tree: tree<a>): tree<a> {
  match (tree) {
    Node(left, value, right, Red) -> Node(left, value, right, Black)
    a -> a
  }
}

fun redden(tree: tree<a>): tree<a> {
  match (tree) {
    Node(left, value, right, Black) -> Node(left, value, right, Red)
    _ -> unreachable()
  }
}


fun unsafe-insert1(tree: tree<a>, value: a, compare: (a, a) -> order, replace: bool): tree<a> {
  match (tree) {
    Node(left, middle, right, color) ->
      match (compare(value, middle)) {
        Lt -> balance(Node(unsafe-insert1(left, value, compare, replace), middle, right, color))

        Gt -> balance(Node(left, middle, unsafe-insert1(right, value, compare, replace), color))

        Eq -> if (replace) {
          // TODO in this situation we should avoid balancing, because the structure hasn't changed
          Node(left, value, right, color) // TODO return the Node unchanged if value is equal to middle
        } else {
          // TODO in this situation we should immediately return all the way to the root, because nothing has changed
          tree
        }
      }

    Empty -> Node(empty, value, empty, Red)
  }
}

// This is unsafe because it's possible to use different compare functions on the same tree
public fun unsafe-insert(tree: tree<a>, value: a, compare: (a, a) -> order, replace: bool): tree<a> {
  blacken(unsafe-insert1(tree, value, compare, replace))
}


fun unsafe-remove1(tree: tree<a>, value: a, compare: (a, a) -> order): tree<a> {
  match (tree) {
    Node(left, middle, right, _) ->
      match (compare(value, middle)) {
        Lt -> match (left) {
          Node(_, _, _, Black) -> balance-left(unsafe-remove1(left, value, compare), middle, right)
          _ -> Node(unsafe-remove1(left, value, compare), middle, right, Red)
        }

        Eq -> unsafe-merge(left, right)

        Gt -> match (right) {
          Node(_, _, _, Black) -> balance-right(left, middle, unsafe-remove1(right, value, compare))
          _ -> Node(left, middle, unsafe-remove1(right, value, compare), Red)
        }
      }

    Empty -> tree
  }
}

// This is unsafe because it's possible to use different compare functions on the same tree
public fun unsafe-remove(tree: tree<a>, value: a, compare: (a, a) -> order): tree<a> {
  blacken(unsafe-remove1(tree, value, compare))
}
