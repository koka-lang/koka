/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module std/data/int-map
import std/core/undiv
import std/num/int64

/// An efficient map from integers to values based on Big-Endian Patricia Trees.
abstract type int-map<v>
  IMNil
  Tip(key: int64, value: v)
  Bin(pre: int64, msk: int64, left: int-map<v>, right: int-map<v>)

/// The empty map.
pub val empty-intmap = IMNil

/// Create an empty map.
pub fun empty() : int-map<v>
  IMNil

/// Check if a map is empty.
pub fun is-empty(m: int-map<v>) : bool
  match m
    IMNil -> True
    _ -> False

// ----------------------------------------------------------------------------
// Bit manipulation helpers
// ----------------------------------------------------------------------------

/// Calculate the prefix of `x` masked by `m`.
/// The mask `m` is expected to have exactly one bit set.
/// The result preserves the bits of `x` above the bit set in `m`, and clears the rest.
fip fun mask-prefix(x: int64, m: int64) : int64
  x.and((m.or(m - one)).not)

/// Check if `x` matches the prefix `p` with mask `m`.
/// Returns `True` if the bits of `x` above `m` do not match `p`.
fip fun nomatch(x: int64, p: int64, m: int64) : bool
  mask-prefix(x, m) != p

/// Check if the bit at mask `m` is zero in `x`.
fip fun zero-bit(x: int64, m: int64) : bool
  x.and(m) == zero

/// Find the highest bit where two bits differ.
/// Returns a mask with exactly that bit set.
fip fun highest-bit-mask(x: int64) : int64
  val z = clz(x)
  one.shl(63 - z)

/// Join two disjoint trees `t1` and `t2` with prefixes `p1` and `p2`.
/// Finds the highest bit where `p1` and `p2` differ and creates a new `Bin` node.
fip(1) fun join(p1: int64, t1: int-map<v>, p2: int64, t2: int-map<v>) : int-map<v>
  val m = highest-bit-mask(p1.xor(p2))
  val p = mask-prefix(p1, m)
  if zero-bit(p1, m) then
    Bin(p, m, t1, t2)
  else
    Bin(p, m, t2, t1)

// ----------------------------------------------------------------------------
// Zipper for tail-recursive traversal
// ----------------------------------------------------------------------------

/// A zipper represents the path from the root to the current node,
/// allowing us to reconstruct the tree (zip up) after modification.
type zipper<v>
  Done
  BinL(pre: int64, msk: int64, up: zipper<v>, right: int-map<v>)
  BinR(pre: int64, msk: int64, left: int-map<v>, up: zipper<v>)

// A zipper that maps from v to w
type map-zipper<v, w>
  MapDone
  MapBinL(pre: int64, msk: int64, up: map-zipper<v, w>, right: int-map<v>)
  MapBinR(pre: int64, msk: int64, left: int-map<w>, up: map-zipper<v, w>)

/// Reconstruct the tree by zipping up from the current position `t` using the zipper `z`.
pub fip fun zip-up(z: zipper<v>, t: int-map<v>) : int-map<v>
  match z
    Done -> t
    BinL(p, m, up, r) -> zip-up(up, Bin(p, m, t, r))
    BinR(p, m, l, up) -> zip-up(up, Bin(p, m, l, t))

/// Update a value in the map.
/// The function `f` is applied to the current value (if any).
/// If `f` returns `Just(v)`, the value is updated/inserted.
/// If `f` returns `Nothing`, the value is removed.
pub fun update(t: int-map<v>, key: int64, f: maybe<v> -> maybe<v>) : int-map<v>
  update-loop(t, key, f, Done)

fun update-loop(t: int-map<v>, key: int64, f: maybe<v> -> maybe<v>, z: zipper<v>) : int-map<v>
  match t
    IMNil -> 
      match f(Nothing)
        Nothing -> zip-up(z, IMNil)
        Just(v) -> zip-up(z, Tip(key, v))
    Tip(k, v) ->
      if key == k then
        match f(Just(v))
          Nothing -> 
             match z
               Done -> IMNil
               BinL(_, _, up, r) -> zip-up(up, r)
               BinR(_, _, l, up) -> zip-up(up, l)
          Just(new-v) -> zip-up(z, Tip(key, new-v))
      else
        match f(Nothing)
          Nothing -> zip-up(z, Tip(k, v))
          Just(new-v) -> zip-up(z, join(key, Tip(key, new-v), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then
        match f(Nothing)
          Nothing -> zip-up(z, Bin(p, m, l, r))
          Just(new-v) -> zip-up(z, join(key, Tip(key, new-v), p, Bin(p, m, l, r)))
      else if zero-bit(key, m) then update-loop(l, key, f, BinL(p, m, z, r))
      else update-loop(r, key, f, BinR(p, m, l, z))

/// Set a value in the map, overwriting if it exists.
pub fbip(2) fun set(t: int-map<v>, key: int64, value: v) : int-map<v>
  set-loop(t, key, value, Done)

fbip(2) fun set-loop(t: int-map<v>, key: int64, value: v, z: zipper<v>) : int-map<v>
  match t
    IMNil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(key, value))
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      else if zero-bit(key, m) then set-loop(l, key, value, BinL(p, m, z, r))
      else set-loop(r, key, value, BinR(p, m, l, z))

/// Add a value to the map, only if the key is not already present.
pub fbip(2) fun add(t: int-map<v>, key: int64, value: v) : int-map<v>
  add-loop(t, key, value, Done)

fbip(2) fun add-loop(t: int-map<v>, key: int64, value: v, z: zipper<v>) : int-map<v>
  match t
    IMNil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(k, v)) // Already exists, do nothing
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      else if zero-bit(key, m) then add-loop(l, key, value, BinL(p, m, z, r))
      else add-loop(r, key, value, BinR(p, m, l, z))

/// Lookup a value in the map.
pub tail fun lookup(t: int-map<v>, key: int64) : maybe<v>
  match t
    IMNil -> Nothing
    Tip(k, v) -> if key == k then Just(v) else Nothing
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then Nothing
      else if zero-bit(key, m) then lookup(l, key)
      else lookup(r, key)

/// Check if a key is in the map.
pub tail fun member(t: int-map<v>, key: int64) : bool
  match t
    IMNil -> False
    Tip(k, _) -> key == k
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then False
      else if zero-bit(key, m) then member(l, key)
      else member(r, key)

/// Remove a key from the map.
pub fun remove(t: int-map<v>, key: int64) : int-map<v>
  remove-loop(t, key, Done)

fbip fun remove-loop(t: int-map<v>, key: int64, z: zipper<v>) : int-map<v>
  match t
    IMNil -> zip-up(z, IMNil)
    Tip(k, v) ->
      if key == k then
        match z
          Done -> IMNil
          BinL(_, _, up, r) -> zip-up(up, r)
          BinR(_, _, l, up) -> zip-up(up, l)
      else zip-up(z, Tip(k, v))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, Bin(p, m, l, r))
      else if zero-bit(key, m) then remove-loop(l, key, BinL(p, m, z, r))
      else remove-loop(r, key, BinR(p, m, l, z))

/// Get the number of elements in the map.
pub fun size(t: int-map<v>) : int
  size-loop(t, 0, Done)

fun size-loop(t: int-map<v>, acc: int, z: zipper<v>) : int
  tail fun size-zip-up(z1: zipper<v>, acc1: int) : int
    match z1
      Done -> acc1
      BinL(_, _, up, r) -> size-loop(r.pretend-decreasing, acc1, up)
      BinR(_, _, l, up) -> size-loop(l.pretend-decreasing, acc1, up)
  match t
    IMNil -> size-zip-up(z, acc)
    Tip(_, _) -> size-zip-up(z, acc + 1)
    Bin(p, m, l, r) -> size-loop(l.pretend-decreasing, acc, BinL(p, m, z, r))

/// Map a function over the values in the map.
pub fun map(t: int-map<v>, ^f: (v) -> e w) : e int-map<w>
  map-loop(t, f, MapDone)

fun map-loop(t: int-map<v>, ^f: (v) -> e w, z: map-zipper<v, w>) : e int-map<w>
  tail fun map-zip-up(z1: map-zipper<v, w>, ^f1: (v) -> e w, res: int-map<w>) : e int-map<w>
    match z1
      MapDone -> res
      MapBinL(p, m, up, r) -> 
         map-loop(r.pretend-decreasing, f1, MapBinR(p, m, res, up))
      MapBinR(p, m, l, up) ->
         map-zip-up(up, f1, Bin(p, m, l, res))
  
  match t
    IMNil -> map-zip-up(z, f, IMNil)
    Tip(k, v) -> map-zip-up(z, f, Tip(k, f(v)))
    Bin(p, m, l, r) -> map-loop(l.pretend-decreasing, f, MapBinL(p, m, z, r))

/// Convert the map to a list of (key, value) pairs.
pub fun list(t: int-map<v>) : list<(int64, v)>
  fold(t, [], fn(acc, k, v) Cons((k, v), acc))

/// Fold over the elements of the map.
pub tail fun fold(t: int-map<v>, init: a, ^f: (a, int64, v) -> a) : a
  fold-loop(t, init, f, Done)

fun fold-loop(t: int-map<v>, acc: a, ^f: (a, int64, v) -> a, z: zipper<v>) : a
  tail fun fold-up(z1: zipper<v>, acc1: a, ^f1: (a, int64, v) -> a) : a
    match z1
      Done -> acc1
      BinL(_, _, up, r) -> fold-loop(r.pretend-decreasing, acc1, f1, up)
      BinR(_, _, l, up) -> fold-loop(l.pretend-decreasing, acc1, f1, up)
  match t
    IMNil -> fold-up(z.pretend-decreasing, acc, f)
    Tip(k, v) -> fold-up(z.pretend-decreasing, f(acc, k, v), f)
    Bin(p, m, l, r) -> fold-loop(l.pretend-decreasing, acc, f, BinL(p, m, z, r))

/// Create a map from a list of (key, value) pairs.
pub fun from-list(xs: list<(int64, v)>) : int-map<v>
  xs.foldl(IMNil, fn(acc, (k, v)) set(acc, k, v))

/// Convert the map to a string representation.
pub fun show(t: int-map<v>, ?v/show: (v) -> e string) : e string
  "{" ++ list(t).map(fn((k, v)) show(k) ++ ": " ++ show(v)).join(", ") ++ "}"

// Check if mask m1 is shorter (more significant) than m2.
fun shorter(m1: int64, m2: int64) : bool
  clz(m1) < clz(m2)

// Zipper for tail-recursive binary operations (union, intersection, difference).
type bin-op-zipper<v>
  BinOpDone
  // Recursing on left child of a node, preserving the right child.
  BinOpPreserveR(pre: int64, msk: int64, right: int-map<v>, up: bin-op-zipper<v>)
  // Recursing on right child of a node, preserving the left child.
  BinOpPreserveL(pre: int64, msk: int64, left: int-map<v>, up: bin-op-zipper<v>)
  // Recursing on left children of both nodes.
  BinOpSplit(pre: int64, msk: int64, r1: int-map<v>, r2: int-map<v>, up: bin-op-zipper<v>)
  // Recursing on right children of both nodes.
  BinOpSplitRight(pre: int64, msk: int64, l_res: int-map<v>, up: bin-op-zipper<v>)

/// Insert with a combining function.
pub fun insert-with(t: int-map<v>, key: int64, value: v, combine: (v, v) -> e v) : e int-map<v>
  insert-with-loop(t, key, value, combine, Done)

fun insert-with-loop(t: int-map<v>, key: int64, value: v, combine: (v, v) -> e v, z: zipper<v>) : e int-map<v>
  match t
    IMNil -> zip-up(z, Tip(key, value))
    Tip(k, v) ->
      if key == k then zip-up(z, Tip(k, combine(value, v)))
      else zip-up(z, join(key, Tip(key, value), k, Tip(k, v)))
    Bin(p, m, l, r) ->
      if nomatch(key, p, m) then zip-up(z, join(key, Tip(key, value), p, Bin(p, m, l, r)))
      else if zero-bit(key, m) then insert-with-loop(l, key, value, combine, BinL(p, m, z, r))
      else insert-with-loop(r, key, value, combine, BinR(p, m, l, z))

// Smart constructor that avoids creating Bins with empty children.
inline fun bin(p: int64, m: int64, l: int-map<v>, r: int-map<v>) : int-map<v>
  match l
    IMNil -> r
    _ -> match r
      IMNil -> l
      _ -> Bin(p, m, l, r)

fun prefix-val(t: int-map<v>) : int64
  match t
    Tip(x, _) -> x
    Bin(p, _, _, _) -> p
    IMNil -> zero

/// Union of two maps. Left-biased.
pub fun union(t1: int-map<v>, t2: int-map<v>) : int-map<v>
  union-with(t1, t2, fn(v1, _) v1)

/// Union with a combining function.
pub fun union-with(t1: int-map<v>, t2: int-map<v>, ^combine: (v, v) -> e v) : e int-map<v>
  union-loop(t1, t2, combine, BinOpDone)

fun union-loop(t1: int-map<v>, t2: int-map<v>, ^combine: (v, v) -> e v, zipper: bin-op-zipper<v>) : e int-map<v>
  inline tail fun union-zip-up(z: bin-op-zipper<v>, ^combine2: (v, v) -> e v, res: int-map<v>) : e int-map<v>
    match z
      BinOpDone -> res
      BinOpPreserveR(p, m, r, up) -> union-zip-up(up, combine2, Bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up) -> union-zip-up(up, combine2, Bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> 
        union-loop(r1.pretend-decreasing, r2.pretend-decreasing, combine2, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l_res, up) ->
        union-zip-up(up, combine2, Bin(p, m, l_res, res))

  inline tail fun union-next(p_high, m_high, l_high, r_high, t_high, p_low, t_low, z_curr, ^combine2)
    if nomatch(p_low, p_high, m_high) then union-zip-up(z_curr, combine2, join(p_high, t_high, p_low, t_low))
    else if zero-bit(p_low, m_high) then union-loop(l_high.pretend-decreasing, t_low.pretend-decreasing, combine2, BinOpPreserveR(p_high, m_high, r_high, z_curr))
    else union-loop(r_high.pretend-decreasing, t_low.pretend-decreasing, combine2, BinOpPreserveL(p_high, m_high, l_high, z_curr))

  match t1
    IMNil -> union-zip-up(zipper, combine, t2)
    Tip(k, v) -> union-zip-up(zipper, combine, insert-with(t2, k, v, combine))
    Bin(p1, m1, l1, r1) ->
      match t2
        IMNil -> union-zip-up(zipper, combine, t1)
        Tip(k, v) -> union-zip-up(zipper, combine, insert-with(t1, k, v, fn(v1, v2) combine(v2, v1)))
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then union-next(p1, m1, l1, r1, t1, p2, t2, zipper, combine)
          else if shorter(m2, m1) then union-next(p2, m2, l2, r2, t2, p1, t1, zipper, combine)
          else if p1 == p2 then
            union-loop(l1.pretend-decreasing, l2.pretend-decreasing, combine, BinOpSplit(p1, m1, r1, r2, zipper))
          else
            union-zip-up(zipper, combine, join(p1, t1, p2, t2))

/// Compute the difference of two maps (keys in t1 but not in t2).
pub fun difference(t1: int-map<v>, t2: int-map<v>) : int-map<v>
  difference-loop(t1, t2, BinOpDone)

// Tail-recursive difference loop.
fun difference-loop(t1: int-map<v>, t2: int-map<v>, z: bin-op-zipper<v>) : int-map<v>
  inline tail fun diff-zip-up(z1: bin-op-zipper<v>, t: int-map<v>) : int-map<v>
    match z1
      BinOpDone -> t
      BinOpPreserveR(p, m, r, up) -> diff-zip-up(up, bin(p, m, t, r))
      BinOpPreserveL(p, m, l, up) -> diff-zip-up(up, bin(p, m, l, t))
      BinOpSplit(p, m, r1, r2, up) ->
        difference-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, t, up))
      BinOpSplitRight(p, m, l_res, up) ->
        diff-zip-up(up, bin(p, m, l_res, t))
  match t1
    IMNil -> diff-zip-up(z, IMNil)
    Tip(x, _) -> 
      if member(t2, x) then diff-zip-up(z, IMNil)
      else diff-zip-up(z, t1)
    Bin(p1, m1, l1, r1) ->
      match t2
        IMNil -> diff-zip-up(z, t1)
        Tip(x, _) -> diff-zip-up(z, remove(t1, x))
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then
            val p2_ = prefix-val(t2)
            if nomatch(p2_, p1, m1) then diff-zip-up(z, t1)
            else if zero-bit(p2_, m1) then difference-loop(l1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveR(p1, m1, r1, z))
            else difference-loop(r1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveL(p1, m1, l1, z))
          else if shorter(m2, m1) then
            val p1_ = prefix-val(t1)
            if nomatch(p1_, p2, m2) then diff-zip-up(z, t1)
            else if zero-bit(p1_, m2) then difference-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
            else difference-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
          else if p1 == p2 then
            difference-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, z))
          else
            diff-zip-up(z, t1)
