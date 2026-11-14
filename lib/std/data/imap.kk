/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Finite maps from `:int` keys to values, as little-endian Patricia trees.

A Patricia tree is a binary trie over the bits of the keys in which no node has a single child.
Every internal node records the _branching bit_, the first bit at which its keys differ, and the _prefix_, the bits that all of its keys share before that point.
Keys with the branching bit clear are in the left subtree and keys with it set are in the right subtree.
A map with `n` keys therefore has exactly `n` leaves and `n - 1` internal nodes, and its shape depends only on its set of keys, never on the order in which they were inserted.

This variant is _little-endian_: bits are examined from the least significant upwards, so the root branches on the lowest bit at which any two keys differ.
A big integer has no highest bit to start from, but any two distinct integers differ at some lowest bit: the lowest set bit of their difference.
Keys are treated as two's complement bit strings, with infinitely many leading ones for a negative number.
The low bits of `x` below a bit `m` are then `x % m`, and `x / m` shifts them out, since `%` and `/` are Euclidean.
A path from the root visits each bit position at most once, so `lookup`, `insert` and `remove` take `O(min(n, b))` time, where `b` is the number of bits in the largest key.
The bit operations are divisions by powers of two rather than masks; when the keys fit in 64 bits, `std/num/i64map` avoids that cost.
The order in which `fold` and `list` visit the entries is not specified.

There are three variants with the same interface:

- `std/num/i32map`: `:int32` keys. Cheapest on the JavaScript backend, where an `:int64` is a `BigInt`.
- `std/num/i64map`: `:int64` keys.
- `std/data/imap`: unbounded `:int` keys. It is little-endian (it branches on the lowest differing bit) because a big integer has no highest bit to start from.

## Invariants

For every `Bin(p, m, l, r)`:

1. `m` has exactly one bit set: the branching bit.
2. `0 <= p < m`: `p` has the branching bit and every bit above it clear.
3. Every key in `l` and `r` agrees with `p` on every bit below the branching bit.
4. Every key in `l` has the branching bit clear; every key in `r` has it set.
5. Neither `l` nor `r` is `Nil`.

So `Nil` only ever appears as the whole map, and branching bits strictly increase along every path from the root.
Each subtree has a _range_: the keys that agree with every enclosing node's prefix and lie on the subtree's side of every enclosing branching bit.

## Bias

When two values meet under one key, the later one wins: `insert` overwrites, and `union(t1, t2)` keeps the value from `t2`.
A combining function always receives the values in the order in which their sources appear in the call: `union-with(t1, t2, f)` calls `f(v1, v2)`, and `insert-with(t, k, v, f)` calls `f(old, v)`.
`insert-new` is the one operation that keeps the existing value, as its name says.

## Implementation: zippers and in-place update

The operations that rebuild a map are loops in which every recursive call is a tail call.
The path from the root to the current subtree is held in a _zipper_: a map with one hole, stored inside out so the innermost node is outermost.
`BinL(p, m, up, r)` stands for `Bin(p, m, _hole_, r)` placed in the hole of `up`, `BinR(p, m, l, up)` for `Bin(p, m, l, _hole_)`, and `zip-up(z, t)` fills the hole with `t`.

Filling the hole yields a valid map exactly when `t` is a valid non-empty map whose keys all lie in the range of the hole.
The branching bit of `t` is then automatically examined after that of the innermost frame, since all keys in the range agree on every bit examined up to and including that one.
The update loops keep this property:

- A loop enters `l` or `r` only after checking that the key matches `p` below `m` (`nomatch`) and is on that side of `m` (`zero-bit`). The key being inserted, updated or removed is therefore always in the range of the current hole.
- The subtree put back in the hole holds keys taken from the subtree that was there, plus possibly that key. All of them are in range.
- When `join` places a new `Bin` above the subtree, its branching bit is the lowest bit at which the new key and the subtree's prefix differ. Both are in the range of the hole, so they agree on every bit examined up to and including the branching bit of the enclosing frame, and the new bit is examined strictly later.
- When a removal empties a subtree, the hole cannot hold `Nil` (invariant 5). The innermost frame is dropped instead, and its other child takes the parent's place in the hole of `up`. Those keys were in the parent's range, which is contained in the range of `up`.

`Bin` and the zipper frames have the same size: a prefix, a mask and two pointers.
When the map is uniquely owned, each `Bin` cell is therefore reused for the frame built from it on the way down, and each frame cell for the `Bin` rebuilt from it on the way up.
An update allocates only the cells it adds: a new `Tip`, and the `Bin` built by `join`.
That is the bound in `fbip(2)` on `insert`, `insert-new`, `insert-with` and `update`.
They are `fbip` rather than `fip` because they may free cells: the value an `insert` replaces, the unused new value in `insert-new`, or the `Tip` and frame of a key that `update` removes.
When the map is shared, the same code copies just the path from the root to the key, as any persistent update must.

The binary operations use a zipper with four kinds of frame:

- `BinOpPreserveR(p, m, r, up)`: the hole is the left child of `Bin(p, m, _hole_, r)`. The other map has no keys under `r`, so `r` is kept as is.
- `BinOpPreserveL(p, m, l, up)`: the mirror image.
- `BinOpSplit(p, m, r1, r2, up)`: both maps have a node with prefix `p` and branching bit `m`. The hole receives the result for the left children, and `r1` and `r2` are still to be combined.
- `BinOpSplitRight(p, m, l, up)`: the hole receives the result for the right children, and `l` is the finished result for the left ones.

Each frame makes the same promise as in the update loops: whatever fills its hole lies in its range.
A binary loop on `t1` and `t2` keeps the precondition that both lie in the range of the current hole.
Its result then does too, since every key of a union comes from `t1` or `t2`, and every key of a difference comes from `t1`.
The loop keeps the precondition as follows:

- All keys of a subtree agree with its prefix on every bit examined before its branching bit. So when the branching bit `m1` of the first map is examined before the branching bit `m2` of the second, the second prefix `p2` decides for all of the second map's keys whether they agree with `p1` before `m1` (`nomatch`) and which side of `m1` they are on (`zero-bit`). The case where `m2` comes first is symmetric.
- If the second map lies on one side of `m1`, the loop pairs that child of the first map with the whole second map. Both lie in the range of that child, so the precondition holds again. Union and difference push a preserve frame for the other child, which keeps its range.
- If both maps have the same branching bit and prefix, `l1` and `l2` lie in the range of the left child and `r1` and `r2` in that of the right. The split frames therefore keep the precondition for both halves.
- Otherwise the ranges are disjoint: the prefixes differ at a bit examined before both branching bits. A union puts the two maps under a new `Bin` built by `join`, which is valid for the same reason as in the update loops. A difference leaves the first map unchanged.
- In a difference, when the second map branches first, only its child whose range contains the first map can share keys with it. The loop continues with that child alone and pushes no frame.
- A `Tip` on either side is handled by `insert-with`, `member` or `remove`, which keep the map valid by the argument for the update loops. The result is the other map with one key added or removed, the `Tip` itself, or empty, so all of its keys lie in range.

A union of non-empty maps is never empty, so its frames rebuild a plain `Bin`.
A difference may be empty, so its frames rebuild through `bin`, which replaces a node that has an empty child by its other child, by the same argument as for removal.
*/
module std/data/imap

import std/core/undiv
import std/num/int64

// A finite map from `:int` keys to values of type `:v`.
abstract type imap<v>
  Nil
  Tip(key : int, value : v)
  Bin(pre : int, msk : int, left : imap<v>, right : imap<v>)

// The empty map.
pub val empty-imap = imap/Nil

// The empty map.
pub fun empty() : imap<v>
  Nil

// Is the map empty?
pub fun is-empty(t : imap<v>) : bool
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

// Join two non-empty maps whose ranges are disjoint: `p1` and `p2` are keys or prefixes
// from `t1` and `t2` that differ on a bit below both trees' branching bits.
fip(1) fun join(p1 : int, t1 : imap<v>, p2 : int, t2 : imap<v>) : imap<v>
  val m = branch-bit(p1, p2)
  val p = mask-prefix(p1, m)
  if zero-bit(p1, m) then Bin(p, m, t1, t2) else Bin(p, m, t2, t1)

// Build a node, replacing it by its other child when one child is empty.
inline fun bin(p : int, m : int, l : imap<v>, r : imap<v>) : imap<v>
  match l
    Nil -> r
    _ -> match r
      Nil -> l
      _   -> Bin(p, m, l, r)

// ----------------------------------------------------------------------------
// Zippers (see the module documentation)
// ----------------------------------------------------------------------------

// A map with one hole: the path from the root to the subtree being worked on.
// `BinL` is a node whose left child is the hole, `BinR` one whose right child is.
type zipper<v>
  Done
  BinL(pre : int, msk : int, up : zipper<v>, right : imap<v>)
  BinR(pre : int, msk : int, left : imap<v>, up : zipper<v>)

// The zipper of `map`, which turns an `imap<v>` into an `imap<w>` in place:
// the subtree to the right of the hole is not yet mapped, the one to the left already is.
type map-zipper<v, w>
  MapDone
  MapBinL(pre : int, msk : int, up : map-zipper<v, w>, right : imap<v>)
  MapBinR(pre : int, msk : int, left : imap<w>, up : map-zipper<v, w>)

// The zipper of the binary operations; see the module documentation for the frames.
type bin-op-zipper<v>
  BinOpDone
  BinOpPreserveR(pre : int, msk : int, right : imap<v>, up : bin-op-zipper<v>)
  BinOpPreserveL(pre : int, msk : int, left : imap<v>, up : bin-op-zipper<v>)
  BinOpSplit(pre : int, msk : int, r1 : imap<v>, r2 : imap<v>, up : bin-op-zipper<v>)
  BinOpSplitRight(pre : int, msk : int, left : imap<v>, up : bin-op-zipper<v>)

// Fill the hole of `z` with the non-empty map `t`.
fip fun zip-up(z : zipper<v>, t : imap<v>) : imap<v>
  match z
    Done              -> t
    BinL(p, m, up, r) -> zip-up(up, Bin(p, m, t, r))
    BinR(p, m, l, up) -> zip-up(up, Bin(p, m, l, t))

// Fill the hole of `z` by removing it: its parent is replaced by the hole's sibling.
fbip fun zip-up-remove(z : zipper<v>) : imap<v>
  match z
    Done              -> Nil
    BinL(_, _, up, r) -> zip-up(up, r)
    BinR(_, _, l, up) -> zip-up(up, l)

// ----------------------------------------------------------------------------
// Queries
// ----------------------------------------------------------------------------

// The value stored under `key`, if any.
pub tail fun lookup(t : imap<v>, key : int) : maybe<v>
  match t
    Nil       -> Nothing
    Tip(k, v) -> if key == k then Just(v) else Nothing
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then Nothing
      elif zero-bit(key, m) then lookup(l, key)
      else lookup(r, key)

// Is `key` in the map?
pub fip fun member(^t : imap<v>, key : int) : bool
  match t
    Nil       -> False
    Tip(k, _) -> key == k
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then False
      elif zero-bit(key, m) then member(l, key)
      else member(r, key)

// The number of entries in the map, in `O(n)` time.
// The recursion is as deep as the tree, which is bounded by the number of bits in a key.
pub fbip fun size(^t : imap<v>) : int
  match t
    Nil             -> 0
    Tip(_, _)       -> 1
    Bin(_, _, l, r) -> size(l) + size(r)

// ----------------------------------------------------------------------------
// Updates
// ----------------------------------------------------------------------------

// Store `value` under `key`, replacing any existing value.
pub fbip(2) fun insert(t : imap<v>, key : int, value : v) : imap<v>
  insert-loop(t, key, value, Done)

fbip(2) fun insert-loop(t : imap<v>, key : int, value : v, z : zipper<v>) : imap<v>
  match t
    Nil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(key, value))
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      elif zero-bit(key, m) then insert-loop(l, key, value, BinL(p, m, z, r))
      else insert-loop(r, key, value, BinR(p, m, l, z))

// Store `value` under `key` unless `key` is already present, in which case the map is unchanged.
pub fbip(2) fun insert-new(t : imap<v>, key : int, value : v) : imap<v>
  insert-new-loop(t, key, value, Done)

fbip(2) fun insert-new-loop(t : imap<v>, key : int, value : v, z : zipper<v>) : imap<v>
  match t
    Nil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(k, v))
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      elif zero-bit(key, m) then insert-new-loop(l, key, value, BinL(p, m, z, r))
      else insert-new-loop(r, key, value, BinR(p, m, l, z))

// Store `value` under `key`; if `key` is already present with value `old`, store `combine(old, value)` instead.
pub fbip(2) fun insert-with(t : imap<v>, key : int, value : v, ^combine : (v, v) -> e v) : e imap<v>
  insert-with-loop(t, key, value, combine, Done)

fbip(2) fun insert-with-loop(t : imap<v>, key : int, value : v, ^combine : (v, v) -> e v, z : zipper<v>) : e imap<v>
  match t
    Nil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(k, combine(v, value)))
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      elif zero-bit(key, m) then insert-with-loop(l, key, value, combine, BinL(p, m, z, r))
      else insert-with-loop(r, key, value, combine, BinR(p, m, l, z))

// Apply `f` to the value under `key` (`Nothing` if absent).
// The key is then set to the result if it is `Just`, and removed if it is `Nothing`.
pub fbip(2) fun update(t : imap<v>, key : int, ^f : maybe<v> -> e maybe<v>) : e imap<v>
  update-loop(t, key, f, Done)

fbip(2) fun update-loop(t : imap<v>, key : int, ^f : maybe<v> -> e maybe<v>, z : zipper<v>) : e imap<v>
  match t
    Nil ->
      match f(Nothing)
        Nothing -> zip-up(z, Nil)
        Just(v) -> zip-up(z, Tip(key, v))
    Tip(k, v) ->
      if key == k then
        match f(Just(v))
          Nothing   -> zip-up-remove(z)
          Just(new) -> zip-up(z, Tip(key, new))
      else
        match f(Nothing)
          Nothing   -> zip-up(z, Tip(k, v))
          Just(new) -> zip-up(z, join(key, Tip(key, new), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then
        match f(Nothing)
          Nothing   -> zip-up(z, Bin(p, m, l, r))
          Just(new) -> zip-up(z, join(key, Tip(key, new), p, Bin(p, m, l, r)))
      elif zero-bit(key, m) then update-loop(l, key, f, BinL(p, m, z, r))
      else update-loop(r, key, f, BinR(p, m, l, z))

// Remove `key` from the map; the map is unchanged if `key` is absent.
pub fbip fun remove(t : imap<v>, key : int) : imap<v>
  remove-loop(t, key, Done)

fbip fun remove-loop(t : imap<v>, key : int, z : zipper<v>) : imap<v>
  match t
    Nil -> zip-up(z, Nil)
    Tip(k, v) ->
      if key == k then zip-up-remove(z)
      else zip-up(z, Tip(k, v))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, Bin(p, m, l, r))
      elif zero-bit(key, m) then remove-loop(l, key, BinL(p, m, z, r))
      else remove-loop(r, key, BinR(p, m, l, z))

// ----------------------------------------------------------------------------
// Traversals
// ----------------------------------------------------------------------------

// Apply `f` to every value. The shape is unchanged, so a uniquely owned map is mapped in place.
pub fun map(t : imap<v>, ^f : v -> e w) : e imap<w>
  map-loop(t, f, MapDone)

fun map-loop(t : imap<v>, ^f : v -> e w, z : map-zipper<v, w>) : e imap<w>
  tail fun map-up(z1 : map-zipper<v, w>, ^f1 : v -> e w, res : imap<w>) : e imap<w>
    match z1
      MapDone              -> res
      MapBinL(p, m, up, r) -> map-loop(r.pretend-decreasing, f1, MapBinR(p, m, res, up))
      MapBinR(p, m, l, up) -> map-up(up, f1, Bin(p, m, l, res))
  match t
    Nil             -> map-up(z, f, Nil)
    Tip(k, v)       -> map-up(z, f, Tip(k, f(v)))
    Bin(p, m, l, r) -> map-loop(l.pretend-decreasing, f, MapBinL(p, m, z, r))

// Combine the entries with `f`, starting from `init`, in an unspecified order.
pub fbip fun fold(t : imap<v>, init : a, ^f : (a, int, v) -> e a) : e a
  fold-loop(t, init, f, Done)

// `Nil` is only ever the whole map, so at a `Nil` the zipper is `Done`.
fbip fun fold-loop(t : imap<v>, acc : a, ^f : (a, int, v) -> e a, z : zipper<v>) : e a
  match t
    Nil -> acc
    Tip(k, v) ->
      val acc1 = f(acc, k, v)
      match z
        Done              -> acc1
        BinL(_, _, up, r) -> fold-loop(r.pretend-decreasing, acc1, f, up)
        BinR(_, _, l, up) -> fold-loop(l.pretend-decreasing, acc1, f, up)
    Bin(p, m, l, r) -> fold-loop(l.pretend-decreasing, acc, f, BinL(p, m, z, r))

// The entries of the map, in an unspecified order.
pub fun list(t : imap<v>) : list<(int, v)>
  t.fold([], fn(acc, k, v) Cons((k, v), acc))

// A map holding the given entries; a later entry for a key replaces an earlier one.
pub fun from-list(xs : list<(int, v)>) : imap<v>
  xs.foldl(Nil, fn(acc, (k, v)) acc.insert(k, v))

// Show the entries, in an unspecified order.
pub fun show(t : imap<v>, ?v/show : v -> e string) : e string
  "{" ++ t.list.map(fn((k, v)) k.show ++ ": " ++ v.show).join(", ") ++ "}"

// ----------------------------------------------------------------------------
// Binary operations
// ----------------------------------------------------------------------------

// The union of two maps; for a key in both, the value from `t2` wins.
pub fun union(t1 : imap<v>, t2 : imap<v>) : imap<v>
  union-with(t1, t2, fn(_, v2) v2)

// The union of two maps; for a key in both, the value is `combine(v1, v2)`.
pub fun union-with(t1 : imap<v>, t2 : imap<v>, ^combine : (v, v) -> e v) : e imap<v>
  union-loop(t1, t2, combine, BinOpDone)

fun union-loop(t1 : imap<v>, t2 : imap<v>, ^combine : (v, v) -> e v, z : bin-op-zipper<v>) : e imap<v>
  inline tail fun union-up(z1 : bin-op-zipper<v>, ^combine1 : (v, v) -> e v, res : imap<v>) : e imap<v>
    match z1
      BinOpDone                    -> res
      BinOpPreserveR(p, m, r, up)  -> union-up(up, combine1, Bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up)  -> union-up(up, combine1, Bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> union-loop(r1.pretend-decreasing, r2.pretend-decreasing, combine1, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l, up) -> union-up(up, combine1, Bin(p, m, l, res))
  match t1
    Nil       -> union-up(z, combine, t2)
    Tip(k, v) -> union-up(z, combine, insert-with(t2, k, v, fn(v2, v1) combine(v1, v2)))
    Bin(p1, m1, l1, r1) -> match t2
      Nil       -> union-up(z, combine, t1)
      Tip(k, v) -> union-up(z, combine, insert-with(t1, k, v, combine))
      Bin(p2, m2, l2, r2) ->
        if shorter(m1, m2) then        // `t2` lies under one child of `t1`, or outside it
          if nomatch(p2, p1, m1) then union-up(z, combine, join(p1, t1, p2, t2))
          elif zero-bit(p2, m1) then union-loop(l1.pretend-decreasing, t2.pretend-decreasing, combine, BinOpPreserveR(p1, m1, r1, z))
          else union-loop(r1.pretend-decreasing, t2.pretend-decreasing, combine, BinOpPreserveL(p1, m1, l1, z))
        elif shorter(m2, m1) then      // `t1` lies under one child of `t2`, or outside it
          if nomatch(p1, p2, m2) then union-up(z, combine, join(p1, t1, p2, t2))
          elif zero-bit(p1, m2) then union-loop(t1.pretend-decreasing, l2.pretend-decreasing, combine, BinOpPreserveR(p2, m2, r2, z))
          else union-loop(t1.pretend-decreasing, r2.pretend-decreasing, combine, BinOpPreserveL(p2, m2, l2, z))
        elif p1 == p2 then
          union-loop(l1.pretend-decreasing, l2.pretend-decreasing, combine, BinOpSplit(p1, m1, r1, r2, z))
        else
          union-up(z, combine, join(p1, t1, p2, t2))

// The entries of `t1` whose key is not in `t2`.
pub fun difference(t1 : imap<v>, t2 : imap<v>) : imap<v>
  difference-loop(t1, t2, BinOpDone)

fun difference-loop(t1 : imap<v>, t2 : imap<v>, z : bin-op-zipper<v>) : imap<v>
  inline tail fun diff-up(z1 : bin-op-zipper<v>, res : imap<v>) : imap<v>
    match z1
      BinOpDone                    -> res
      BinOpPreserveR(p, m, r, up)  -> diff-up(up, bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up)  -> diff-up(up, bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> difference-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l, up) -> diff-up(up, bin(p, m, l, res))
  match t1
    Nil       -> diff-up(z, Nil)
    Tip(k, _) -> if t2.member(k) then diff-up(z, Nil) else diff-up(z, t1)
    Bin(p1, m1, l1, r1) -> match t2
      Nil       -> diff-up(z, t1)
      Tip(k, _) -> diff-up(z, t1.remove(k))
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
