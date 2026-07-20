/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Finite sets of `:int` values, as little-endian Patricia trees.

A Patricia tree is a binary trie over the bits of the elements in which no node has a single child.
Every internal node records the _branching bit_, the first bit at which its elements differ, and the _prefix_, the bits that all of its elements share before that point.
Elements with the branching bit clear are in the left subtree and elements with it set are in the right subtree.
A set with `n` elements therefore has exactly `n` leaves and `n - 1` internal nodes, and its shape depends only on its elements, never on the order in which they were inserted.
In particular two sets are equal exactly when they have the same shape, which is how `==` compares them.

This variant is _little-endian_: bits are examined from the least significant upwards, so the root branches on the lowest bit at which any two elements differ.
A big integer has no highest bit to start from, but any two distinct integers differ at some lowest bit: the lowest set bit of their difference.
Elements are treated as two's complement bit strings, with infinitely many leading ones for a negative number.
The low bits of `x` below a bit `m` are then `x % m`, and `x / m` shifts them out, since `%` and `/` are Euclidean.
A path from the root visits each bit position at most once, so `member`, `insert` and `remove` take `O(min(n, b))` time, where `b` is the number of bits in the largest element.
The bit operations are divisions by powers of two rather than masks; when the elements fit in 64 bits, `std/num/i64set` avoids that cost.
The order in which `fold` and `list` visit the elements is not specified.

There are three variants with the same interface:

- `std/num/i32set`: `:int32` elements. Cheapest on the JavaScript backend, where an `:int64` is a `BigInt`.
- `std/num/i64set`: `:int64` elements.
- `std/data/iset`: unbounded `:int` elements. It is little-endian (it branches on the lowest differing bit) because a big integer has no highest bit to start from.

## Invariants

For every `Bin(p, m, l, r)`:

1. `m` has exactly one bit set: the branching bit.
2. `0 <= p < m`: `p` has the branching bit and every bit above it clear.
3. Every element of `l` and `r` agrees with `p` on every bit below the branching bit.
4. Every element of `l` has the branching bit clear; every element of `r` has it set.
5. Neither `l` nor `r` is `Nil`.

So `Nil` only ever appears as the whole set, and branching bits strictly increase along every path from the root.
Each subtree has a _range_: the values that agree with every enclosing node's prefix and lie on the subtree's side of every enclosing branching bit.

## Implementation: zippers and in-place update

The operations that rebuild a set are loops in which every recursive call is a tail call.
The path from the root to the current subtree is held in a _zipper_: a set with one hole, stored inside out so the innermost node is outermost.
`BinL(p, m, up, r)` stands for `Bin(p, m, _hole_, r)` placed in the hole of `up`, `BinR(p, m, l, up)` for `Bin(p, m, l, _hole_)`, and `zip-up(z, t)` fills the hole with `t`.

Filling the hole yields a valid set exactly when `t` is a valid non-empty set whose elements all lie in the range of the hole.
The branching bit of `t` is then automatically examined after that of the innermost frame, since all values in the range agree on every bit examined up to and including that one.
The update loops keep this property:

- A loop enters `l` or `r` only after checking that the value matches `p` below `m` (`nomatch`) and is on that side of `m` (`zero-bit`). The value being inserted or removed is therefore always in the range of the current hole.
- The subtree put back in the hole holds elements taken from the subtree that was there, plus possibly that value. All of them are in range.
- When `join` places a new `Bin` above the subtree, its branching bit is the lowest bit at which the new value and the subtree's prefix differ. Both are in the range of the hole, so they agree on every bit examined up to and including the branching bit of the enclosing frame, and the new bit is examined strictly later.
- When a removal empties a subtree, the hole cannot hold `Nil` (invariant 5). The innermost frame is dropped instead, and its other child takes the parent's place in the hole of `up`. Those elements were in the parent's range, which is contained in the range of `up`.

`Bin` and the zipper frames have the same size: a prefix, a mask and two pointers.
When the set is uniquely owned, each `Bin` cell is therefore reused for the frame built from it on the way down, and each frame cell for the `Bin` rebuilt from it on the way up.
An insertion allocates only the cells it adds: a new `Tip`, and the `Bin` built by `join`.
That is the bound in `fip(2)` on `insert`.
`remove` is `fbip` rather than `fip` because it frees the removed `Tip` and frame.
When the set is shared, the same code copies just the path from the root to the value, as any persistent update must.

The binary operations use a zipper with four kinds of frame:

- `BinOpPreserveR(p, m, r, up)`: the hole is the left child of `Bin(p, m, _hole_, r)`. The other set has no elements under `r`, so `r` is kept as is.
- `BinOpPreserveL(p, m, l, up)`: the mirror image.
- `BinOpSplit(p, m, r1, r2, up)`: both sets have a node with prefix `p` and branching bit `m`. The hole receives the result for the left children, and `r1` and `r2` are still to be combined.
- `BinOpSplitRight(p, m, l, up)`: the hole receives the result for the right children, and `l` is the finished result for the left ones.

Each frame makes the same promise as in the update loops: whatever fills its hole lies in its range.
A binary loop on `t1` and `t2` keeps the precondition that both lie in the range of the current hole.
Its result then does too, since every element of a union comes from `t1` or `t2`, and every element of an intersection or a difference comes from `t1`.
The loop keeps the precondition as follows:

- All elements of a subtree agree with its prefix on every bit examined before its branching bit. So when the branching bit `m1` of the first set is examined before the branching bit `m2` of the second, the second prefix `p2` decides for all of the second set's elements whether they agree with `p1` before `m1` (`nomatch`) and which side of `m1` they are on (`zero-bit`). The case where `m2` comes first is symmetric.
- If the second set lies on one side of `m1`, the loop pairs that child of the first set with the whole second set. Both lie in the range of that child, so the precondition holds again. Union and difference push a preserve frame for the other child, which keeps its range.
- If both sets have the same branching bit and prefix, `l1` and `l2` lie in the range of the left child and `r1` and `r2` in that of the right. The split frames therefore keep the precondition for both halves.
- Otherwise the ranges are disjoint: the prefixes differ at a bit examined before both branching bits. A union puts the two sets under a new `Bin` built by `join`, which is valid for the same reason as in the update loops. An intersection of disjoint sets is empty, and a difference leaves the first one unchanged.
- When one set branches first, an intersection needs only the child whose range contains the other set, and so does a difference whose second set branches first. The other child cannot meet the other set, so the loop continues with that child alone and pushes no frame.
- A `Tip` on either side is handled by `insert`, `member` or `remove`, which keep the set valid by the argument for the update loops. The result is the other set with one element added or removed, the `Tip` itself, or empty, so all of its elements lie in range.

A union of non-empty sets is never empty, so its frames rebuild a plain `Bin`.
An intersection or a difference may be empty, so its frames rebuild through `bin`, which replaces a node that has an empty child by its other child, by the same argument as for removal.

The predicates `==`, `is-subset-of` and `disjoint` walk both sets together with a zipper of pending checks.
`CheckRight(r1, r2, up)` means that the right children `r1` and `r2` are still to be compared once the left children have been.
A predicate returns `False` at the first failed check and `True` when no checks remain.
*/
module std/data/iset

import std/core/undiv
import std/num/int64

// A finite set of `:int` values.
abstract type iset
  Nil
  Tip(key : int)
  Bin(pre : int, msk : int, left : iset, right : iset)

// The empty set.
pub val empty-iset = iset/Nil

// The empty set.
pub fun empty() : iset
  Nil

// Is the set empty?
pub fun is-empty(t : iset) : bool
  match t
    Nil -> True
    _   -> False

// ----------------------------------------------------------------------------
// Bits
// ----------------------------------------------------------------------------

// `x` with the branching bit `m` and every bit above it cleared: the bits of `x` below `m`.
fip fun mask-prefix(x : int, m : int) : int
  x % m

// Does `x` fall outside the subtree with prefix `p` and branching bit `m`?
fip fun nomatch(x : int, p : int, m : int) : bool
  x % m != p

// Does `x` belong in the left subtree of a node with branching bit `m`?
fip fun zero-bit(x : int, m : int) : bool
  (x / m).is-even

// The branching bit for two distinct prefixes: the lowest bit at which they differ.
fip fun branch-bit(p1 : int, p2 : int) : int
  lowest-bit(p1 - p2, 1)

// `scale` times the lowest set bit of `d != 0`, examining 32 bits per step so that `lo` fits an `:int64` for `ctz`.
fip fun lowest-bit(d : int, scale : int) : int
  val lo = d % 0x1_0000_0000
  if lo == 0 then lowest-bit((d / 0x1_0000_0000).pretend-decreasing, scale * 0x1_0000_0000)
  else scale * exp2(ctz(lo.int64))

// Does a node with branching bit `m1` have a shorter prefix (sit nearer the root) than one with `m2`?
fip fun shorter(m1 : int, m2 : int) : bool
  m1 < m2

// Join two non-empty sets whose ranges are disjoint: `p1` and `p2` are elements or prefixes
// from `t1` and `t2` that differ on a bit below both trees' branching bits.
fip(1) fun join(p1 : int, t1 : iset, p2 : int, t2 : iset) : iset
  val m = branch-bit(p1, p2)
  val p = mask-prefix(p1, m)
  if zero-bit(p1, m) then Bin(p, m, t1, t2) else Bin(p, m, t2, t1)

// Build a node, replacing it by its other child when one child is empty.
inline fun bin(p : int, m : int, l : iset, r : iset) : iset
  match l
    Nil -> r
    _ -> match r
      Nil -> l
      _   -> Bin(p, m, l, r)

// ----------------------------------------------------------------------------
// Zippers (see the module documentation)
// ----------------------------------------------------------------------------

// A set with one hole: the path from the root to the subtree being worked on.
// `BinL` is a node whose left child is the hole, `BinR` one whose right child is.
type zipper
  Done
  BinL(pre : int, msk : int, up : zipper, right : iset)
  BinR(pre : int, msk : int, left : iset, up : zipper)

// The zipper of the binary operations; see the module documentation for the frames.
type bin-op-zipper
  BinOpDone
  BinOpPreserveR(pre : int, msk : int, right : iset, up : bin-op-zipper)
  BinOpPreserveL(pre : int, msk : int, left : iset, up : bin-op-zipper)
  BinOpSplit(pre : int, msk : int, r1 : iset, r2 : iset, up : bin-op-zipper)
  BinOpSplitRight(pre : int, msk : int, left : iset, up : bin-op-zipper)

// The checks still pending in a predicate over two sets.
type check-zipper
  CheckDone
  CheckRight(r1 : iset, r2 : iset, up : check-zipper)

// Fill the hole of `z` with the non-empty set `t`.
fip fun zip-up(z : zipper, t : iset) : iset
  match z
    Done              -> t
    BinL(p, m, up, r) -> zip-up(up, Bin(p, m, t, r))
    BinR(p, m, l, up) -> zip-up(up, Bin(p, m, l, t))

// Fill the hole of `z` by removing it: its parent is replaced by the hole's sibling.
fbip fun zip-up-remove(z : zipper) : iset
  match z
    Done              -> Nil
    BinL(_, _, up, r) -> zip-up(up, r)
    BinR(_, _, l, up) -> zip-up(up, l)

// ----------------------------------------------------------------------------
// Queries
// ----------------------------------------------------------------------------

// Is `x` in the set?
pub fip fun member(^t : iset, x : int) : bool
  match t
    Nil    -> False
    Tip(y) -> x == y
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then False
      elif zero-bit(x, m) then member(l, x)
      else member(r, x)

// The number of elements in the set, in `O(n)` time.
// The recursion is as deep as the tree, which is bounded by the number of bits in a key.
pub fbip fun size(^t : iset) : int
  match t
    Nil             -> 0
    Tip(_)          -> 1
    Bin(_, _, l, r) -> size(l) + size(r)

// ----------------------------------------------------------------------------
// Updates
// ----------------------------------------------------------------------------

// Add `x` to the set.
pub fip(2) fun insert(t : iset, x : int) : iset
  insert-loop(t, x, Done)

fip(2) fun insert-loop(t : iset, x : int, z : zipper) : iset
  match t
    Nil -> zip-up(z, Tip(x))
    Tip(y) ->
      if x == y then zip-up(z, Tip(y))
      else zip-up(z, join(x, Tip(x), y, Tip(y)))
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then zip-up(z, join(x, Tip(x), p, Bin(p, m, l, r)))
      elif zero-bit(x, m) then insert-loop(l, x, BinL(p, m, z, r))
      else insert-loop(r, x, BinR(p, m, l, z))

// Remove `x` from the set; the set is unchanged if `x` is absent.
pub fbip fun remove(t : iset, x : int) : iset
  remove-loop(t, x, Done)

fbip fun remove-loop(t : iset, x : int, z : zipper) : iset
  match t
    Nil -> zip-up(z, Nil)
    Tip(y) ->
      if x == y then zip-up-remove(z)
      else zip-up(z, Tip(y))
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then zip-up(z, Bin(p, m, l, r))
      elif zero-bit(x, m) then remove-loop(l, x, BinL(p, m, z, r))
      else remove-loop(r, x, BinR(p, m, l, z))

// ----------------------------------------------------------------------------
// Traversals
// ----------------------------------------------------------------------------

// Combine the elements with `f`, starting from `init`, in an unspecified order.
pub fbip fun fold(t : iset, init : a, ^f : (a, int) -> e a) : e a
  fold-loop(t, init, f, Done)

// `Nil` is only ever the whole set, so at a `Nil` the zipper is `Done`.
fbip fun fold-loop(t : iset, acc : a, ^f : (a, int) -> e a, z : zipper) : e a
  match t
    Nil -> acc
    Tip(x) ->
      val acc1 = f(acc, x)
      match z
        Done              -> acc1
        BinL(_, _, up, r) -> fold-loop(r.pretend-decreasing, acc1, f, up)
        BinR(_, _, l, up) -> fold-loop(l.pretend-decreasing, acc1, f, up)
    Bin(p, m, l, r) -> fold-loop(l.pretend-decreasing, acc, f, BinL(p, m, z, r))

// The elements of the set, in an unspecified order.
pub fun list(t : iset) : list<int>
  t.fold([], fn(acc, x) Cons(x, acc))

// The set of the given elements.
pub fun from-list(xs : list<int>) : iset
  xs.foldl(Nil, insert)

// Show the elements, in an unspecified order.
pub fun show(t : iset) : string
  "{" ++ t.list.map(show).join(", ") ++ "}"

// ----------------------------------------------------------------------------
// Binary operations
// ----------------------------------------------------------------------------

// The elements in either set.
pub fun union(t1 : iset, t2 : iset) : iset
  union-loop(t1, t2, BinOpDone)

fun union-loop(t1 : iset, t2 : iset, z : bin-op-zipper) : iset
  inline tail fun union-up(z1 : bin-op-zipper, res : iset) : iset
    match z1
      BinOpDone                    -> res
      BinOpPreserveR(p, m, r, up)  -> union-up(up, Bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up)  -> union-up(up, Bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> union-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l, up) -> union-up(up, Bin(p, m, l, res))
  match t1
    Nil    -> union-up(z, t2)
    Tip(x) -> union-up(z, t2.insert(x))
    Bin(p1, m1, l1, r1) -> match t2
      Nil    -> union-up(z, t1)
      Tip(x) -> union-up(z, t1.insert(x))
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then        // `t2` lies under one child of `t1`, or outside it
          if nomatch(p2, p1, m1) then union-up(z, join(p1, t1, p2, t2))
          elif zero-bit(p2, m1) then union-loop(l1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveR(p1, m1, r1, z))
          else union-loop(r1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveL(p1, m1, l1, z))
        elif shorter(m2, m1) then      // `t1` lies under one child of `t2`, or outside it
          if nomatch(p1, p2, m2) then union-up(z, join(p1, t1, p2, t2))
          elif zero-bit(p1, m2) then union-loop(t1.pretend-decreasing, l2.pretend-decreasing, BinOpPreserveR(p2, m2, r2, z))
          else union-loop(t1.pretend-decreasing, r2.pretend-decreasing, BinOpPreserveL(p2, m2, l2, z))
        elif p1 == p2 then
          union-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, z))
        else
          union-up(z, join(p1, t1, p2, t2))

// The elements in both sets.
pub fun intersection(t1 : iset, t2 : iset) : iset
  intersection-loop(t1, t2, BinOpDone)

fun intersection-loop(t1 : iset, t2 : iset, z : bin-op-zipper) : iset
  inline tail fun inter-up(z1 : bin-op-zipper, res : iset) : iset
    match z1
      BinOpDone                    -> res
      BinOpPreserveR(p, m, r, up)  -> inter-up(up, bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up)  -> inter-up(up, bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> intersection-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l, up) -> inter-up(up, bin(p, m, l, res))
  match t1
    Nil    -> inter-up(z, Nil)
    Tip(x) -> if t2.member(x) then inter-up(z, t1) else inter-up(z, Nil)
    Bin(p1, m1, l1, r1) -> match t2
      Nil    -> inter-up(z, Nil)
      Tip(x) -> if t1.member(x) then inter-up(z, t2) else inter-up(z, Nil)
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then        // only the child of `t1` that `t2` lies under can meet it
          if nomatch(p2, p1, m1) then inter-up(z, Nil)
          elif zero-bit(p2, m1) then intersection-loop(l1.pretend-decreasing, t2.pretend-decreasing, z)
          else intersection-loop(r1.pretend-decreasing, t2.pretend-decreasing, z)
        elif shorter(m2, m1) then      // only the child of `t2` that `t1` lies under can meet it
          if nomatch(p1, p2, m2) then inter-up(z, Nil)
          elif zero-bit(p1, m2) then intersection-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
          else intersection-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
        elif p1 == p2 then
          intersection-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, z))
        else
          inter-up(z, Nil)

// The elements of `t1` that are not in `t2`.
pub fun difference(t1 : iset, t2 : iset) : iset
  difference-loop(t1, t2, BinOpDone)

fun difference-loop(t1 : iset, t2 : iset, z : bin-op-zipper) : iset
  inline tail fun diff-up(z1 : bin-op-zipper, res : iset) : iset
    match z1
      BinOpDone                    -> res
      BinOpPreserveR(p, m, r, up)  -> diff-up(up, bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up)  -> diff-up(up, bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> difference-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l, up) -> diff-up(up, bin(p, m, l, res))
  match t1
    Nil    -> diff-up(z, Nil)
    Tip(x) -> if t2.member(x) then diff-up(z, Nil) else diff-up(z, t1)
    Bin(p1, m1, l1, r1) -> match t2
      Nil    -> diff-up(z, t1)
      Tip(x) -> diff-up(z, t1.remove(x))
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then        // `t2` lies under one child of `t1`, or outside it
          if nomatch(p2, p1, m1) then diff-up(z, t1)
          elif zero-bit(p2, m1) then difference-loop(l1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveR(p1, m1, r1, z))
          else difference-loop(r1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveL(p1, m1, l1, z))
        elif shorter(m2, m1) then      // `t1` lies under one child of `t2`, or outside it
          if nomatch(p1, p2, m2) then diff-up(z, t1)
          elif zero-bit(p1, m2) then difference-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
          else difference-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
        elif p1 == p2 then
          difference-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, z))
        else
          diff-up(z, t1)

// ----------------------------------------------------------------------------
// Predicates
// ----------------------------------------------------------------------------

// Do the two sets have the same elements? Sets are canonical, so this compares shapes.
pub fun (==)(t1 : iset, t2 : iset) : bool
  eq-loop(t1, t2, CheckDone)

fun eq-loop(t1 : iset, t2 : iset, z : check-zipper) : bool
  tail fun eq-up(z1 : check-zipper) : bool
    match z1
      CheckDone              -> True
      CheckRight(r1, r2, up) -> eq-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    Nil -> match t2
      Nil -> eq-up(z)
      _   -> False
    Tip(x) -> match t2
      Tip(y) -> if x == y then eq-up(z) else False
      _      -> False
    Bin(p1, m1, l1, r1) -> match t2
      Bin(p2, m2, l2, r2) ->
        if p1 == p2 && m1 == m2 then eq-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
        else False
      _ -> False

// Do the two sets differ?
pub fun (!=)(t1 : iset, t2 : iset) : bool
  !(t1 == t2)

// Is every element of `t1` also in `t2`?
pub fun is-subset-of(t1 : iset, t2 : iset) : bool
  subset-loop(t1, t2, CheckDone)

fun subset-loop(t1 : iset, t2 : iset, z : check-zipper) : bool
  tail fun subset-up(z1 : check-zipper) : bool
    match z1
      CheckDone              -> True
      CheckRight(r1, r2, up) -> subset-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    Nil    -> subset-up(z)
    Tip(x) -> if t2.member(x) then subset-up(z) else False
    Bin(p1, m1, l1, r1) -> match t2
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then False  // `t1` has elements on both sides of `m1`, while all of `t2` agree on it
        elif shorter(m2, m1) then      // `t1` must lie under one child of `t2`
          if nomatch(p1, p2, m2) then False
          elif zero-bit(p1, m2) then subset-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
          else subset-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
        elif p1 == p2 then
          subset-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
        else False
      _ -> False                       // a `Bin` has at least two elements

// Do the two sets have no element in common?
pub fun disjoint(t1 : iset, t2 : iset) : bool
  disjoint-loop(t1, t2, CheckDone)

fun disjoint-loop(t1 : iset, t2 : iset, z : check-zipper) : bool
  tail fun disjoint-up(z1 : check-zipper) : bool
    match z1
      CheckDone              -> True
      CheckRight(r1, r2, up) -> disjoint-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    Nil    -> disjoint-up(z)
    Tip(x) -> if t2.member(x) then False else disjoint-up(z)
    Bin(p1, m1, l1, r1) -> match t2
      Nil    -> disjoint-up(z)
      Tip(x) -> if t1.member(x) then False else disjoint-up(z)
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then        // only the child of `t1` that `t2` lies under can meet it
          if nomatch(p2, p1, m1) then disjoint-up(z)
          elif zero-bit(p2, m1) then disjoint-loop(l1.pretend-decreasing, t2.pretend-decreasing, z)
          else disjoint-loop(r1.pretend-decreasing, t2.pretend-decreasing, z)
        elif shorter(m2, m1) then      // only the child of `t2` that `t1` lies under can meet it
          if nomatch(p1, p2, m2) then disjoint-up(z)
          elif zero-bit(p1, m2) then disjoint-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
          else disjoint-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
        elif p1 == p2 then
          disjoint-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
        else
          disjoint-up(z)
