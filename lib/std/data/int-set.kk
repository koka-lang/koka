/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module std/data/int-set
import std/core/undiv
import std/num/int64

/// An efficient set of integers based on Big-Endian Patricia Trees.
abstract type int-set
  ISNil
  Tip(key: int64)
  Bin(pre: int64, msk: int64, left: int-set, right: int-set)

/// The empty set.
pub val empty-intset = ISNil

/// Create an empty set.
pub fun empty() : int-set
  ISNil

/// Check if a set is empty.
/// ```
/// empty().is-empty == True
/// insert(empty(), 1.int64).is-empty == False
/// ```
pub fun is-empty(s: int-set) : bool
  match s
    ISNil -> True
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
fip(1) fun join(p1: int64, t1: int-set, p2: int64, t2: int-set) : int-set
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
type zipper
  Done
  BinL(pre: int64, msk: int64, up: zipper, right: int-set)
  BinR(pre: int64, msk: int64, left: int-set, up: zipper)

// Zipper for tail-recursive boolean checks on two trees (e.g. equality, subset, disjoint).
// Stores pending checks for right subtrees.
type check-zipper
  CheckDone
  CheckRight(r1: int-set, r2: int-set, up: check-zipper)

// Zipper for tail-recursive binary operations (union, intersection, difference).
type bin-op-zipper
  BinOpDone
  // Recursing on left child of a node, preserving the right child.
  BinOpPreserveR(pre: int64, msk: int64, right: int-set, up: bin-op-zipper)
  // Recursing on right child of a node, preserving the left child.
  BinOpPreserveL(pre: int64, msk: int64, left: int-set, up: bin-op-zipper)
  // Recursing on left children of both nodes.
  BinOpSplit(pre: int64, msk: int64, r1: int-set, r2: int-set, up: bin-op-zipper)
  // Recursing on right children of both nodes.
  BinOpSplitRight(pre: int64, msk: int64, l_res: int-set, up: bin-op-zipper)

/// Reconstruct the tree by zipping up from the current position `t` using the zipper `z`.
pub fip fun zip-up(z: zipper, t: int-set) : int-set
  match z
    Done -> t
    BinL(p, m, up, r) -> zip-up(up, Bin(p, m, t, r))
    BinR(p, m, l, up) -> zip-up(up, Bin(p, m, l, t))

/// Insert an integer into the set.
/// ```
/// empty().insert(1.int64).list == [1.int64]
/// empty().insert(1.int64).insert(1.int64).list == [1.int64]
/// ```
pub fun insert(t: int-set, x: int64) : int-set
  insert-loop(t, x, Done)

// Tail-recursive insert using a zipper.
fip(2) fun insert-loop(t: int-set, x: int64, z: zipper) : int-set
  match t
    ISNil -> zip-up(z, Tip(x))
    Tip(y) ->
      if x == y then zip-up(z, Tip(y))
      else zip-up(z, join(x, Tip(x), y, Tip(y)))
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then zip-up(z, join(x, Tip(x), p, Bin(p, m, l, r)))
      else if zero-bit(x, m) then insert-loop(l, x, BinL(p, m, z, r))
      else insert-loop(r, x, BinR(p, m, l, z))

/// Check if an integer is in the set.
/// ```
/// empty().insert(1.int64).member(1.int64) == True
/// empty().insert(1.int64).member(2.int64) == False
/// ```
pub tail fun member(t: int-set, x: int64) : bool
  match t
    ISNil -> False
    Tip(y) -> x == y
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then False
      else if zero-bit(x, m) then member(l, x)
      else member(r, x)

// Smart constructor that avoids creating Bins with empty children.
inline fun bin(p: int64, m: int64, l: int-set, r: int-set) : int-set
  match l
    ISNil -> r
    _ -> match r
      ISNil -> l
      _ -> Bin(p, m, l, r)

/// Remove an integer from the set.
/// ```
/// empty().insert(1.int64).remove(1.int64).is-empty == True
/// empty().insert(1.int64).remove(2.int64).list == [1.int64]
/// ```
pub fun remove(t: int-set, x: int64) : int-set
  remove-loop(t, x, Done)

// Tail-recursive remove using a zipper.
fbip fun remove-loop(t: int-set, x: int64, z: zipper) : int-set
  match t
    ISNil -> zip-up(z, ISNil)
    Tip(y) ->
      if x == y then
        match z
          Done -> ISNil
          BinL(_, _, up, r) -> zip-up(up, r)
          BinR(_, _, l, up) -> zip-up(up, l)
      else zip-up(z, Tip(y))
    Bin(p, m, l, r) ->
      if nomatch(x, p, m) then zip-up(z, Bin(p, m, l, r))
      else if zero-bit(x, m) then remove-loop(l, x, BinL(p, m, z, r))
      else remove-loop(r, x, BinR(p, m, l, z))

/// Get the number of elements in the set.
/// ```
/// empty().insert(1.int64).insert(2.int64).size == 2
/// ```
pub fun size(t: int-set) : int
  size-loop(t, 0, Done)

fun size-loop(t: int-set, acc: int, z: zipper) : int
  tail fun size-zip-up(z1: zipper, acc1: int) : int
    match z1
      Done -> acc1
      BinL(_, _, up, r) -> size-loop(r.pretend-decreasing, acc1, up)
      BinR(_, _, l, up) -> size-loop(l.pretend-decreasing, acc1, up)
  match t
    ISNil -> size-zip-up(z, acc)
    Tip(_) -> size-zip-up(z, acc + 1)
    Bin(p, m, l, r) -> size-loop(l.pretend-decreasing, acc, BinL(p, m, z, r))

/// Convert the set to a list of integers.
/// The order of elements is not specified.
pub fun list(t: int-set) : list<int64>
  fold(t, [], fn(acc, x) Cons(x, acc))

/// Fold over the elements of the set.
/// The order of elements is not specified.
pub tail fun fold(t: int-set, init: a, ^f: (a, int64) -> a) : a
  fold-loop(t, init, f, Done)

// Tail-recursive fold using a zipper to simulate the stack.
fun fold-loop(t: int-set, acc: a, ^f: (a, int64) -> a, z: zipper) : a
  // Move up the zipper, processing the other branch if necessary.
  tail fun fold-up(z1: zipper, acc1: a, ^f1: (a, int64) -> a) : a
    match z1
      Done -> acc1
      BinL(_, _, up, r) -> fold-loop(r.pretend-decreasing, acc1, f1, up)
      BinR(_, _, l, up) -> fold-loop(l.pretend-decreasing, acc1, f1, up)
  match t
    ISNil -> fold-up(z.pretend-decreasing, acc, f)
    Tip(x) -> fold-up(z.pretend-decreasing, f(acc, x), f)
    Bin(p, m, l, r) -> fold-loop(l.pretend-decreasing, acc, f, BinL(p, m, z, r))

/// Create a set from a list of integers.
pub fun from-list(xs: list<int64>) : int-set
  xs.foldl(ISNil, insert)

/// Check if two sets are equal.
pub fun (==)(t1: int-set, t2: int-set) : bool
  eq-loop(t1, t2, CheckDone)

fun eq-loop(t1: int-set, t2: int-set, z: check-zipper): bool
  tail fun eq-zip-up(z1: check-zipper) : bool
    match z1
      CheckDone -> True
      CheckRight(r1, r2, up) -> eq-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    ISNil -> match t2
      ISNil -> eq-zip-up(z)
      _ -> False
    Tip(x) -> match t2
      Tip(y) -> if x == y then eq-zip-up(z) else False
      _ -> False
    Bin(p1, m1, l1, r1) -> match t2
      Bin(p2, m2, l2, r2) ->
        if p1 == p2 && m1 == m2 then
          eq-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
        else False
      _ -> False

/// Check if two sets are not equal.
pub fun (!=)(t1: int-set, t2: int-set) : bool
  neq-loop(t1, t2, CheckDone)

fun neq-loop(t1: int-set, t2: int-set, z: check-zipper) : bool
  tail fun neq-zip-up(z1: check-zipper) : bool
    match z1
      CheckDone -> False
      CheckRight(r1, r2, up) -> neq-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    ISNil -> match t2
      ISNil -> neq-zip-up(z)
      _ -> True
    Tip(x) -> match t2
      Tip(y) -> if x == y then neq-zip-up(z) else True
      _ -> True
    Bin(p1, m1, l1, r1) -> match t2
      Bin(p2, m2, l2, r2) ->
        if p1 == p2 && m1 == m2 then
          neq-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
        else True
      _ -> True

/// Convert the set to a string representation.
pub fun show(t: int-set) : string
  "{" ++ list(t).map(show).join(", ") ++ "}"

fun prefix-val(t: int-set) : int64
  match t
    Tip(x) -> x
    Bin(p, _, _, _) -> p
    ISNil -> zero

// Check if mask m1 is shorter (more significant) than m2.
// A shorter mask corresponds to a node higher in the tree.
fun shorter(m1: int64, m2: int64) : bool
  clz(m1) < clz(m2)

/// Compute the union of two sets.
/// ```
/// union(from-list([1.int64, 2.int64]), from-list([2.int64, 3.int64])).list == [1.int64, 2.int64, 3.int64]
/// ```
pub fun union(t1: int-set, t2: int-set) : int-set
  union-loop(t1, t2, BinOpDone)

fun union-loop(t1: int-set, t2: int-set, zipper: bin-op-zipper) : int-set
  inline tail fun union-zip-up(z: bin-op-zipper, res: int-set) : int-set
    match z
      BinOpDone -> res
      BinOpPreserveR(p, m, r, up) -> union-zip-up(up, Bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up) -> union-zip-up(up, Bin(p, m, l, res))
      BinOpSplit(p, m, r1, r2, up) -> 
        union-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l_res, up) ->
        union-zip-up(up, Bin(p, m, l_res, res))
  inline tail fun union-next(p_high, m_high, l_high, r_high, t_high, p_low, t_low, z_curr)
    if nomatch(p_low, p_high, m_high) then union-zip-up(z_curr, join(p_high, t_high, p_low, t_low))
    else if zero-bit(p_low, m_high) then union-loop(l_high.pretend-decreasing, t_low.pretend-decreasing, BinOpPreserveR(p_high, m_high, r_high, z_curr))
    else union-loop(r_high.pretend-decreasing, t_low.pretend-decreasing, BinOpPreserveL(p_high, m_high, l_high, z_curr))
  match t1
    ISNil -> union-zip-up(zipper, t2)
    Tip(x) -> union-zip-up(zipper, insert(t2, x))
    Bin(p1, m1, l1, r1) ->
      match t2
        ISNil -> union-zip-up(zipper, t1)
        Tip(x) -> union-zip-up(zipper, insert(t1, x))
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then union-next(p1, m1, l1, r1, t1, p2, t2, zipper)
          else if shorter(m2, m1) then union-next(p2, m2, l2, r2, t2, p1, t1, zipper)
          else if p1 == p2 then
            union-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, zipper))
          else
            union-zip-up(zipper, join(p1, t1, p2, t2))

/// Compute the intersection of two sets.
/// ```
/// intersection(from-list([1.int64, 2.int64]), from-list([2.int64, 3.int64])).list == [2.int64]
/// ```
pub fun intersection(t1: int-set, t2: int-set) : int-set
  intersection-loop(t1, t2, BinOpDone)

fun intersection-loop(t1: int-set, t2: int-set, zipper: bin-op-zipper) : int-set
  inline tail fun intersect-zip-up(z: bin-op-zipper, res: int-set) : int-set
    match z
      BinOpDone -> res
      BinOpSplit(p, m, r1, r2, up) ->
        intersection-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, res, up))
      BinOpSplitRight(p, m, l_res, up) ->
        intersect-zip-up(up, bin(p, m, l_res, res))
      BinOpPreserveR(p, m, r, up) -> intersect-zip-up(up, bin(p, m, res, r))
      BinOpPreserveL(p, m, l, up) -> intersect-zip-up(up, bin(p, m, l, res))

  inline tail fun intersect-next(p_high, m_high, l_high, r_high, p_low, t_low, z_curr)
    if nomatch(p_low, p_high, m_high) then intersect-zip-up(z_curr, ISNil)
    else if zero-bit(p_low, m_high) then intersection-loop(l_high.pretend-decreasing, t_low.pretend-decreasing, z_curr)
    else intersection-loop(r_high.pretend-decreasing, t_low.pretend-decreasing, z_curr)

  match t1
    ISNil -> intersect-zip-up(zipper, ISNil)
    Tip(x) -> 
      if member(t2, x) then intersect-zip-up(zipper, t1)
      else intersect-zip-up(zipper, ISNil)
    Bin(p1, m1, l1, r1) ->
      match t2
        ISNil -> intersect-zip-up(zipper, ISNil)
        Tip(x) -> 
          if member(t1, x) then intersect-zip-up(zipper, t2)
          else intersect-zip-up(zipper, ISNil)
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then intersect-next(p1, m1, l1, r1, p2, t2, zipper)
          else if shorter(m2, m1) then intersect-next(p2, m2, l2, r2, p1, t1, zipper)
          else if p1 == p2 then
            intersection-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, zipper))
          else
            intersect-zip-up(zipper, ISNil)


/// Compute the difference of two sets (elements in t1 but not in t2).
/// Implemented as a tail-recursive traversal with short-circuiting.
/// ```
/// difference(from-list([1.int64, 2.int64]), from-list([2.int64, 3.int64])).list == [1.int64]
/// ```
pub fun difference(t1: int-set, t2: int-set) : int-set
  difference-loop(t1, t2, BinOpDone)

// Tail-recursive difference loop.
// Maintains the invariant that we are computing `difference(t1, t2)`
// and will plug the result into zipper `z`.
fun difference-loop(t1: int-set, t2: int-set, z: bin-op-zipper) : int-set
  // Reconstruct the tree by zipping up from the current result `t`.
  inline tail fun diff-zip-up(z1: bin-op-zipper, t: int-set) : int-set
    match z1
      BinOpDone -> t
      BinOpPreserveR(p, m, r, up) -> diff-zip-up(up, bin(p, m, t, r))
      BinOpPreserveL(p, m, l, up) -> diff-zip-up(up, bin(p, m, l, t))
      BinOpSplit(p, m, r1, r2, up) ->
        // Left side finished (result t), now process right side (r1, r2)
        difference-loop(r1.pretend-decreasing, r2.pretend-decreasing, BinOpSplitRight(p, m, t, up))
      BinOpSplitRight(p, m, l_res, up) ->
        // Right side finished (result t), combine with left result (l_res)
        diff-zip-up(up, bin(p, m, l_res, t))
  match t1
    ISNil -> diff-zip-up(z, ISNil)
    Tip(x) -> 
      if member(t2, x) then diff-zip-up(z, ISNil)
      else diff-zip-up(z, t1)
    Bin(p1, m1, l1, r1) ->
      match t2
        ISNil -> diff-zip-up(z, t1)
        Tip(x) -> diff-zip-up(z, remove(t1, x))
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then
            // t1 branches before t2; check if t2 is wholly in one subtree of t1
            val p2_ = prefix-val(t2)
            if nomatch(p2_, p1, m1) then diff-zip-up(z, t1)
            else if zero-bit(p2_, m1) then difference-loop(l1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveR(p1, m1, r1, z))
            else difference-loop(r1.pretend-decreasing, t2.pretend-decreasing, BinOpPreserveL(p1, m1, l1, z))
          else if shorter(m2, m1) then
            // t2 branches before t1; descend into the appropriate subtree of t1
            val p1_ = prefix-val(t1)
            if nomatch(p1_, p2, m2) then diff-zip-up(z, t1)
            else if zero-bit(p1_, m2) then difference-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
            else difference-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
          else if p1 == p2 then
            // Same prefix and mask; recursively process both subtrees
            difference-loop(l1.pretend-decreasing, l2.pretend-decreasing, BinOpSplit(p1, m1, r1, r2, z))
          else
            // Different prefixes at same level; trees are disjoint
            diff-zip-up(z, t1)

/// Check if t1 is a subset of t2.
/// ```
/// is-subset-of(from-list([1.int64]), from-list([1.int64, 2.int64])) == True
/// is-subset-of(from-list([1.int64, 2.int64]), from-list([1.int64])) == False
/// ```
pub fun is-subset-of(t1: int-set, t2: int-set) : bool
  subset-loop(t1, t2, CheckDone)

// Tail-recursive subset loop.
tail fun subset-loop(t1: int-set, t2: int-set, z: check-zipper) : bool
  match t1
    ISNil -> match z
      CheckDone -> True
      CheckRight(r1, r2, up) -> subset-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
    Tip(x) -> if member(t2, x) then match z
      CheckDone -> True
      CheckRight(r1, r2, up) -> subset-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
    else False
    Bin(p1, m1, l1, r1) ->
      match t2
        ISNil -> False
        Tip(_) -> False
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then False
          else if shorter(m2, m1) then
            val p1_ = prefix-val(t1)
            if nomatch(p1_, p2, m2) then False
            else if zero-bit(p1_, m2) then subset-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
            else subset-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
          else if p1 == p2 then
            subset-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
          else
            False

/// Check if two sets are disjoint (have no elements in common).
/// ```
/// disjoint(from-list([1.int64]), from-list([2.int64])) == True
/// disjoint(from-list([1.int64]), from-list([1.int64, 2.int64])) == False
/// ```
pub fun disjoint(t1: int-set, t2: int-set) : bool
  disjoint-loop(t1, t2, CheckDone)

// Tail-recursive disjoint loop.
fun disjoint-loop(t1: int-set, t2: int-set, z: check-zipper) : bool
  // Continue with the next check in the zipper for disjoint.
  tail fun disjoint-zip-up(z1: check-zipper) : bool
    match z1
      CheckDone -> True
      CheckRight(r1, r2, up) -> disjoint-loop(r1.pretend-decreasing, r2.pretend-decreasing, up)
  match t1
    ISNil -> disjoint-zip-up(z)
    Tip(x) -> if !member(t2, x) then disjoint-zip-up(z) else False
    Bin(p1, m1, l1, r1) ->
      match t2
        ISNil -> disjoint-zip-up(z)
        Tip(x) -> if !member(t1, x) then disjoint-zip-up(z) else False
        Bin(p2, m2, l2, r2) ->
          if shorter(m1, m2) then
            val p2_ = prefix-val(t2)
            if nomatch(p2_, p1, m1) then disjoint-zip-up(z)
            else if zero-bit(p2_, m1) then disjoint-loop(l1.pretend-decreasing, t2.pretend-decreasing, z)
            else disjoint-loop(r1.pretend-decreasing, t2.pretend-decreasing, z)
          else if shorter(m2, m1) then
            val p1_ = prefix-val(t1)
            if nomatch(p1_, p2, m2) then disjoint-zip-up(z)
            else if zero-bit(p1_, m2) then disjoint-loop(t1.pretend-decreasing, l2.pretend-decreasing, z)
            else disjoint-loop(t1.pretend-decreasing, r2.pretend-decreasing, z)
          else
            if p1 != p2 then disjoint-zip-up(z)
            else disjoint-loop(l1.pretend-decreasing, l2.pretend-decreasing, CheckRight(r1, r2, z))
