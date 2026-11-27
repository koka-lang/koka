/*---------------------------------------------------------------------------
  Copyright 2024-2026, Anton Lorenzen, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Red-black trees, updated bottom-up through a zipper.

A red-black tree is a binary search tree whose nodes are colored red or black, such that no red node has a red child and every path from the root to a leaf passes the same number of black nodes.
Together these keep the tree balanced: its depth is at most twice the logarithm of its size.

Every write walks down to its key while recording the path in a `:zipper`, then rebuilds the tree from the bottom while restoring the invariants.
The zipper constructors have the same shape as the tree nodes, so a uniquely owned tree is updated in place: each step reuses the node it came from.
The algorithms follow "The Functional Essence of Imperative Binary Search Trees" (Lorenzen, Leijen, Swierstra, and Lindley, PLDI 2024).

Keys are ordered by an implicit `?cmp`, and are borrowed by every comparison.
This module is the representation underneath `std/data/map` and `std/data/set`, which most programs should use instead.
*/
module std/data/rbtree

import std/core/undiv

pub type color
  Red
  Black

pub type rbtree<k,v>
  Leaf
  Node(color : color, left : rbtree<k,v>, key : k, value : v, right : rbtree<k,v>)

// A red node whose subtrees may still violate the invariants, while it is rebalanced.
pub type root<k,v>
  Root(color : color, left : rbtree<k,v>, key : k, value : v, right : rbtree<k,v>)

// The path from the root down to a subtree: each step keeps the node it came from, without the child that was taken.
pub type zipper<k,v>
  ZNodeR(color : color, lchild : rbtree<k,v>, key : k, value : v, zip : zipper<k,v>)
  ZNodeL(color : color, zip : zipper<k,v>, key : k, value : v, rchild : rbtree<k,v>)
  Done

pub inline fun empty() : rbtree<k,v>
  Leaf

// The entries in key order.
pub fun list(t : rbtree<k,v>) : list<(k,v)>
  fun go(u : rbtree<k,v>, acc : list<(k,v)>) : list<(k,v)>
    match u
      Leaf -> acc
      Node(_, l, k, v, r) -> go(l, Cons((k, v), go(r, acc)))
  go(t, [])

pub fun keys(t : rbtree<k,v>) : list<k>
  fun go(u : rbtree<k,v>, acc : list<k>) : list<k>
    match u
      Leaf -> acc
      Node(_, l, k, _, r) -> go(l, Cons(k, go(r, acc)))
  go(t, [])

pub fun values(t : rbtree<k,v>) : list<v>
  fun go(u : rbtree<k,v>, acc : list<v>) : list<v>
    match u
      Leaf -> acc
      Node(_, l, _, v, r) -> go(l, Cons(v, go(r, acc)))
  go(t, [])

pub fun contains(t : rbtree<k,v>, ^key : k, ^?cmp : (k,k) -> e order) : e bool
  match t
    Leaf -> False
    Node(_, l, kx, _, r) ->
      match cmp(key, kx)
        Lt -> contains(l, key)
        Gt -> contains(r, key)
        Eq -> True

pub fun lookup(t : rbtree<k,v>, ^key : k, ^?cmp : (k,k) -> e order) : e maybe<v>
  match t
    Leaf -> Nothing
    Node(_, l, kx, v, r) ->
      match cmp(key, kx)
        Lt -> lookup(l, key)
        Gt -> lookup(r, key)
        Eq -> Just(v)

pub fun value/map(t : rbtree<k,v>, f : (v) -> e x) : e rbtree<k,x>
  match t
    Leaf -> Leaf
    Node(c, l, k, v, r) -> Node(c, l.value/map(f), k, f(v), r.value/map(f))

pub fun kvalue/map(t : rbtree<k,v>, f : (k, v) -> e x) : e rbtree<k,x>
  match t
    Leaf -> Leaf
    Node(c, l, k, v, r) -> Node(c, l.kvalue/map(f), k, f(k, v), r.kvalue/map(f))

// ----------------------------------------------------------------------------
// Insertion
// ----------------------------------------------------------------------------

pub inline fip fun set-black(t : rbtree<k,v>) : rbtree<k,v>
  match t
    Node(_, l, x, v, r) -> Node(Black, l, x, v, r)
    Leaf -> Leaf

pub inline fip fun tree/is-red(^t : rbtree<k,v>) : bool
  match t
    Node(Red) -> True
    _ -> False

pub inline fip fun to-node(root : root<k,v>) : rbtree<k,v>
  match root
    Root(c, l, k, v, r) -> Node(c, l, k, v, r)

// Plugs `t` back into the path `z`; the root is made black.
pub fip fun rebuild(z : zipper<k,v>, t : rbtree<k,v>) : rbtree<k,v>
  match z
    ZNodeR(c, l, k, v, z1) -> rebuild(z1, Node(c, l, k, v, t))
    ZNodeL(c, z1, k, v, r) -> rebuild(z1, Node(c, t, k, v, r))
    Done -> t.set-black

// As `rebuild`, but leaves the color of the root alone.
pub fip fun rebuild-no-set(z : zipper<k,v>, t : rbtree<k,v>) : rbtree<k,v>
  match z
    ZNodeR(c, l, k, v, z1) -> rebuild-no-set(z1, Node(c, l, k, v, t))
    ZNodeL(c, z1, k, v, r) -> rebuild-no-set(z1, Node(c, t, k, v, r))
    Done -> t

// Restores the invariants above a red `t` whose parent may also be red, then plugs it into `z`.
pub fip fun balance(z : zipper<k,v>, t : root<k,v>) : rbtree<k,v>
  match z
    ZNodeR(Red, l1, k1, v1, z1) -> match z1
      ZNodeR(_, l2, k2, v2, z2) -> // black
        if is-red(l2) then balance(z2, Root(Red, l2.set-black, k2, v2, Node(Black, l1, k1, v1, t.to-node)))
        else rebuild(z2, Node(Black, Node(Red, l2, k2, v2, l1), k1, v1, t.to-node))
      ZNodeL(_, z2, k2, v2, r2) -> // black
        if is-red(r2) then balance(z2, Root(Red, Node(Black, l1, k1, v1, t.to-node), k2, v2, r2.set-black))
        else match t
          Root(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red, l1, k1, v1, l), k, v, Node(Red, r, k2, v2, r2)))
      Done -> Node(Black, l1, k1, v1, t.to-node)
    ZNodeL(Red, z1, k1, v1, r1) -> match z1
      ZNodeL(_, z2, k2, v2, r2) -> // black
        if is-red(r2) then balance(z2, Root(Red, Node(Black, t.to-node, k1, v1, r1), k2, v2, r2.set-black))
        else rebuild(z2, Node(Black, t.to-node, k1, v1, Node(Red, r1, k2, v2, r2)))
      ZNodeR(_, l2, k2, v2, z2) -> // black
        if is-red(l2) then balance(z2, Root(Red, l2.set-black, k2, v2, Node(Black, t.to-node, k1, v1, r1)))
        else match t
          Root(_, l, k, v, r) -> rebuild(z2, Node(Black, Node(Red, l2, k2, v2, l), k, v, Node(Red, r, k1, v1, r1)))
      Done -> Node(Black, t.to-node, k1, v1, r1)
    z -> rebuild(z, t.to-node)

pub fun zip/set(t : rbtree<k,v>, key : k, value : v, z : zipper<k,v>, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  match t
    Node(c, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> zip/set(l, key, value, ZNodeL(c, z, kx, vx, r))
        Gt -> zip/set(r, key, value, ZNodeR(c, l, kx, vx, z))
        Eq -> rebuild(z, Node(c, l, key, value, r))
    Leaf -> balance(z, Root(Red, Leaf, key, value, Leaf))

pub fun zip/add(t : rbtree<k,v>, key : k, value : v, z : zipper<k,v>, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  match t
    Node(c, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> zip/add(l, key, value, ZNodeL(c, z, kx, vx, r))
        Gt -> zip/add(r, key, value, ZNodeR(c, l, kx, vx, z))
        Eq -> rebuild(z, Node(c, l, key, vx, r))
    Leaf -> balance(z, Root(Red, Leaf, key, value, Leaf))

pub fun zip/insert(t : rbtree<k,v>, key : k, z : zipper<k,v>, f : (maybe<v>) -> e v, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  match t
    Node(c, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> zip/insert(l, key, ZNodeL(c, z, kx, vx, r), f)
        Gt -> zip/insert(r, key, ZNodeR(c, l, kx, vx, z), f)
        Eq -> rebuild(z, Node(c, l, key, f(Just(vx)), r))
    Leaf -> balance(z, Root(Red, Leaf, key, f(Nothing), Leaf))

// Maps `key` to `value`, replacing a present value.
pub fun set(t : rbtree<k,v>, key : k, value : v, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  zip/set(t, key, value, Done)

// Maps `key` to `value` only if `key` is absent: a present key keeps its value.
pub fun add(t : rbtree<k,v>, key : k, value : v, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  zip/add(t, key, value, Done)

// Maps `key` to `f(old)`, where `old` is the present value, if any.
pub fun insert(t : rbtree<k,v>, key : k, f : (maybe<v>) -> e v, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  zip/insert(t, key, Done, f)

// ----------------------------------------------------------------------------
// Deletion
//
// Removing a black node leaves its path one black node short, a "double black" that is moved up the zipper until a rotation or a red node absorbs it.
// The figure numbers refer to the PLDI 2024 paper.
// ----------------------------------------------------------------------------

// Removes `key`; an absent key rebuilds the tree unchanged.
pub fun remove(t : rbtree<k,v>, key : k, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  zip/remove(t, key, Done)

pub fun zip/remove(t : rbtree<k,v>, key : k, z : zipper<k,v>, ^?cmp : (k,k) -> e order) : e rbtree<k,v>
  match t
    Node(c, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> zip/remove(l, key, ZNodeL(c, z, kx, vx, r))
        Gt -> zip/remove(r, key, ZNodeR(c, l, kx, vx, z))
        Eq -> do-remove(c, z, l, r)
    Leaf -> rebuild(z, Leaf) // not found (Figure 3)

// Balance final zipper
pub inline fip fun db-balance-rebuild(zip : zipper<k,v>, t : rbtree<k,v>) : rbtree<k,v>
  match t
    Node(Black, Node(Red, Node(Red, a, x, vx, b), y, vy, c), z, vz, d)
      -> zip.rebuild(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)))
    Node(Black, Node(Red, a, x, vx, Node(Red, b, y, vy, c)), z, vz, d)
      -> zip.rebuild(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)))
    Node(Black, a, x, vx, Node(Red, Node(Red, b, y, vy, c), z, vz, d))
      -> zip.rebuild(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)))
    Node(Black, a, x, vx, Node(Red, b, y, vy, Node(Red, c, z, vz, d)))
      -> zip.rebuild(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)))
    t' -> zip.rebuild(t')

// Double black rotation (final zipper)
pub fun db-rotate(z : zipper<k,v>, t : rbtree<k,v>) : rbtree<k,v>
  tail fun db-balance-rotate-build(zip, t')
    match t' // BB cases
      Node(_, Node(Red, a, x, vx, Node(Red, b, y, vy, c)), z', vz, d) ->
        zip.rebuild(Node(Black, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z', vz, d)))
      Node(_, a, x, vx, Node(Red, Node(Red, b, y, vy, c), z', vz, d)) ->
        zip.rebuild(Node(Black, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z', vz, d)))
      t'' -> db-rotate(zip.pretend-decreasing, t'')
  match z
    // First case Figure 6, t is x, z is y, double black is discharged and we can just rebuild normally
    ZNodeL(Red, z', y, vy, Node(Black, c, kz, vz, d)) ->
      db-balance-rebuild(z', Node(Black, Node(Red, t, y, vy, c), kz, vz, d))
    ZNodeR(Red, Node(Black, a, x, vx, b), y, vy, z') ->
      db-balance-rebuild(z', Node(Black, a, x, vx, Node(Red, b, y, vy, t)))
    // Second case Figure 7, t is x, z is y
    ZNodeL(Black, z', y, vy, Node(Black, c, kz, vz, d)) ->
      z'.db-balance-rotate-build(Node(Black, Node(Red, t, y, vy, c), kz, vz, d))
    ZNodeR(Black, Node(Black, a, x, vx, b), y, vy, z') ->
      z'.db-balance-rotate-build(Node(Black, a, x, vx, Node(Red, b, y, vy, t)))
    // Third case Figure 9, t is w, z is x
    ZNodeL(Black, z', x, vx, Node(Red, Node(Black, c, y, vy, d), kz, vz, e)) ->
      ZNodeL(Black, z', kz, vz, e).db-balance-rebuild(Node(Black, Node(Red, t, x, vx, c), y, vy, d))
    ZNodeR(Black, Node(Red, a, w, vw, Node(Black, b, x, vx, c)), y, vy, z') ->
      ZNodeR(Black, a, w, vw, z').db-balance-rebuild(Node(Black, b, x, vx, Node(Red, c, y, vy, t)))
    // Rebuild one step then rebalance
    ZNodeR(c, l, k, v, z1) -> db-rotate(z1, Node(c, l, k, v, t))
    ZNodeL(c, z1, k, v, r) -> db-rotate(z1, Node(c, t, k, v, r))
    Done -> t.set-black // unreachable while the tree is still double black

pub fip fun rebuild-no-set2(z : zipper<k,v>, t : rbtree<k,v>, ^f : (rbtree<k,v>, bool) -> rbtree<k,v>) : rbtree<k,v>
  match z
    ZNodeR(c, l, k, v, z1) -> rebuild-no-set2(z1, Node(c, l, k, v, t), f)
    ZNodeL(c, z1, k, v, r) -> rebuild-no-set2(z1, Node(c, t, k, v, r), f)
    Done -> f(t, False)

// Balance inner zipper
pub inline fip fun db-balance-rebuild-no-set(zip : zipper<k,v>, t : rbtree<k,v>, ^f : (rbtree<k,v>, bool) -> rbtree<k,v>) : rbtree<k,v>
  match t
    Node(Black, Node(Red, Node(Red, a, x, vx, b), y, vy, c), z, vz, d)
      -> zip.rebuild-no-set2(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)), f)
    Node(Black, Node(Red, a, x, vx, Node(Red, b, y, vy, c)), z, vz, d)
      -> zip.rebuild-no-set2(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)), f)
    Node(Black, a, x, vx, Node(Red, Node(Red, b, y, vy, c), z, vz, d))
      -> zip.rebuild-no-set2(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)), f)
    Node(Black, a, x, vx, Node(Red, b, y, vy, Node(Red, c, z, vz, d)))
      -> zip.rebuild-no-set2(Node(Red, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z, vz, d)), f)
    t' -> zip.rebuild-no-set2(t', f)

pub fun db-rotate-done(z : zipper<k,v>, t : rbtree<k,v>, ^f : (rbtree<k,v>, bool) -> rbtree<k,v>) : rbtree<k,v>
  fbip fun db-balance-rotate-done(zip, t', ^f1)
    match t' // BB cases
      Node(_, Node(Red, a, x, vx, Node(Red, b, y, vy, c)), z', vz, d) ->
        zip.rebuild-no-set2(Node(Black, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z', vz, d)), f1)
      Node(_, a, x, vx, Node(Red, Node(Red, b, y, vy, c), z', vz, d)) ->
        zip.rebuild-no-set2(Node(Black, Node(Black, a, x, vx, b), y, vy, Node(Black, c, z', vz, d)), f1)
      t'' -> db-rotate-done(zip.pretend-decreasing, t'', f1)
  match z
    // First case Figure 6, t is x, z is y, double black is discharged and we can just rebuild normally
    ZNodeL(Red, z', y, vy, Node(Black, c, kz, vz, d)) ->
      z'.db-balance-rebuild-no-set(Node(Black, Node(Red, t, y, vy, c), kz, vz, d), f)
    ZNodeR(Red, Node(Black, a, x, vx, b), y, vy, z') ->
      z'.db-balance-rebuild-no-set(Node(Black, a, x, vx, Node(Red, b, y, vy, t)), f)
    // Second case Figure 7, t is x, z is y
    ZNodeL(Black, z', y, vy, Node(Black, c, kz, vz, d)) ->
      z'.db-balance-rotate-done(Node(Black, Node(Red, t, y, vy, c), kz, vz, d), f)
    ZNodeR(Black, Node(Black, a, x, vx, b), y, vy, z') ->
      z'.db-balance-rotate-done(Node(Black, a, x, vx, Node(Red, b, y, vy, t)), f)
    // Third case Figure 9, t is w, z is x
    ZNodeL(Black, z', x, vx, Node(Red, Node(Black, c, y, vy, d), kz, vz, e)) ->
      ZNodeL(Black, z', kz, vz, e).db-balance-rebuild-no-set(Node(Black, Node(Red, t, x, vx, c), y, vy, d), f)
    ZNodeR(Black, Node(Red, a, w, vw, Node(Black, b, x, vx, c)), y, vy, z') ->
      ZNodeR(Black, a, w, vw, z').db-balance-rebuild-no-set(Node(Black, b, x, vx, Node(Red, c, y, vy, t)), f)
    // Rebuild one step then rebalance
    ZNodeR(c, l, k, v, z1) -> db-rotate-done(z1, Node(c, l, k, v, t), f)
    ZNodeL(c, z1, k, v, r) -> db-rotate-done(z1, Node(c, t, k, v, r), f)
    Done -> f(t, True)

pub tail fun do-remove(c : color, z : zipper<k,v>, l : rbtree<k,v>, r : rbtree<k,v>) : rbtree<k,v>
  if l.is-leaf then
    if r.is-leaf then // no children: replace with a leaf
      if c.is-red then rebuild(z, Leaf) // a red node does not change the black height (Figure 4)
      else db-rotate(z, Leaf) // a black one leaves the path double black
    else rebuild(z, r.set-black) // one child: it takes the node's place, black (Figure 4)
  elif r.is-leaf then
    rebuild(z, l.set-black) // one child (Figure 4)
  else
    // two children: replace the node with its in-order successor, removing the successor on the way
    do-remove-inorder(c, z, l, r, Done)

pub tail fun do-remove-inorder(c : color, z : zipper<k,v>, l : rbtree<k,v>, r : rbtree<k,v>, rz : zipper<k,v>) : rbtree<k,v>
  match r
    // a red successor is replaced by a leaf, and its entry moves to the removed node
    Node(Red, Leaf, k, v, Leaf) ->
      rebuild(z, Node(c, l, k, v, rz.rebuild-no-set(Leaf)))
    // a black successor with a red right child is replaced by that child, made black
    Node(Black, Leaf, k, v, Node(Red, rl, kr, vr, rr)) ->
      rebuild(z, Node(c, l, k, v, rz.rebuild-no-set(Node(Black, rl, kr, vr, rr))))
    // any other black successor leaves its path double black
    Node(Black, Leaf, k, v, rr) ->
      db-rotate-done(rz, rr) fn(r', is-double-black)
        if is-double-black then db-rotate(ZNodeR(c, l, k, v, z), r')
        else ZNodeR(c, l, k, v, z).rebuild(r')
    // keep searching for the successor
    Node(rc, rl, k, v, rr) ->
      do-remove-inorder(c, z, l, rl, ZNodeL(rc, rz, k, v, rr))
    Leaf -> Leaf // unreachable: `r` is not a leaf
