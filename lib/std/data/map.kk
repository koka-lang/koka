/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Map

   Todo.
*/
module std/data/map

pub linear effect comparable<k>
  fun compare(k1: k, k2: k): order

pub val int_compare = 
  handler
    fun std/data/map/compare(k1: int, k2: int)
      if k1 < k2 then Lt
      elif k1 > k2 then Gt
      else Eq

pub type map<k,a>
  Tip
  Bin(size: int, k: k, a: a, left: map<k,a>, right: map<k,a>)

pub fun is-empty(m: map<k,a>): bool
  match m
    Tip -> True
    _ -> False

pub fun size(m: map<k,a>): int
  match m
    Tip -> 0
    Bin(size,_,_,_,_) -> size
  
pub fun lookup(m: map<k, a>, k: k): comparable<k> maybe<a>
  match m
    Tip -> Nothing
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> lookup(l, k)
        Gt -> lookup(r, k)
        Eq -> Just(x)

pub fun lookupAssoc(m: map<k, a>, k: k): comparable<k> maybe<(k,a)>
  match m
    Tip -> Nothing
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> lookupAssoc(l, k)
        Gt -> lookupAssoc(r, k)
        Eq -> Just((kx,x))

pub fun member(m: map<k,a>, k: k): comparable<k> bool
  match m.lookup(k)
    Nothing -> False
    Just(_) -> True

pub fun find(m: map<k,a>, k: k): <comparable<k>,exn> a
  match m.lookup(k)
    Nothing -> throw("find: element not in map")
    Just(x) -> x

pub fun findWithDefault(m: map<k,a>, k: k, def: a): comparable<k> a
  match m.lookup(k)
    Nothing -> def
    Just(x) -> x

pub val empty = Tip

pub fun singleton(k: k, a: a): map<k,a>
  Bin(1, k, a, Tip, Tip)

pub fun insert(m: map<k,a>, k: k, a: a): comparable<k> map<k,a>
  try-default(m)
    match m
      Tip -> singleton(k, a)
      Bin(_,kx,x,l,r) ->
        match compare(k, kx)
          Lt -> balance(kx, x, insert(l, k, a), r)
          Gt -> balance(kx, x, l, insert(r, k, a))
          Eq -> Bin(size(m), k, a, l, r)

pub fun insertWith(m: map<k,a>, k: k, a: a, f: (a, a) -> <comparable<k>,exn|e> a): <comparable<k>,exn|e> map<k,a>
  m.insertWithKey(k,a) fn(_, x, y)
    f(x, y)

pub fun insertWithKey(m: map<k,a>, k: k, a: a, f: (k, a, a) -> <comparable<k>,exn|e> a): <comparable<k>,exn|e> map<k,a>
  match m
    Tip -> singleton(k, a)
    Bin(sy, ky, y, l, r) ->
      match compare(k,ky)
        Lt -> balance(ky, y, unsafe-decreasing(l).insertWithKey(k,a,f), r)
        Gt -> balance(ky, y, l, unsafe-decreasing(r).insertWithKey(k,a,f))
        Eq -> Bin(sy, k, f(k, a, y), l, r)

pub fun insertLookupWithKey(m: map<k,a>, k: k, v: a, f: (k, a, a) -> a): <comparable<k>,exn|e> (maybe<a>,map<k,a>)
  match m
    Tip -> (Nothing, singleton(k, v))
    Bin(sy, ky, y, l, r) ->
      match compare(k,ky)
        Lt -> 
          val (found, l') = unsafe-decreasing(l).insertLookupWithKey(k,v,f)
          (found, balance(ky, y, l', r))
        Gt -> 
          val (found, r') = unsafe-decreasing(r).insertLookupWithKey(k,v,f)
          (found, balance(ky, y, l, r'))
        Eq -> (Just(y), Bin(sy, k, f(k, v, y), l, r))

pub fun delete(m: map<k,a>, k: k): <exn,comparable<k>> map<k,a>
  match m
    Tip -> Tip
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> balance(kx, x, delete(l, k), r)
        Gt -> balance(kx, x, l, delete(r, k))
        Eq -> glue(l, r)

pub fun adjust(m: map<k,a>, k: k, f: (a)->a): <exn,comparable<k>> map<k,a>
  m.adjustWithKey(k, fn(_, x) -> f(x))

pub fun adjustWithKey(m: map<k,a>, k: k, f: (k,a) -> a): <exn,comparable<k>> map<k,a>
  m.updateWithKey(k, fn(k', x') -> Just(f(k', x')))

pub fun update(m: map<k,a>, k: k, f: (a) -> maybe<a> ): <exn,comparable<k>> map<k,a>
  m.updateWithKey(k, fn(_, x) -> f(x))

pub fun updateWithKey(m: map<k,a>, k: k, f: (k, a) -> maybe<a>): <exn,comparable<k>> map<k,a>
  match m
    Tip -> Tip
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> balance(kx, x, updateWithKey(l, k, f), r)
        Gt -> balance(kx, x, l, updateWithKey(r, k, f))
        Eq -> match f(k, x)
                Nothing -> glue(l, r)
                Just(x') -> Bin(size(m), k, x', l, r)

pub fun updateLookupWithKey(m: map<k,a>, k: k, f: (k, a) -> maybe<a>): <exn,comparable<k>> (maybe<a>, map<k,a>)
  match m
    Tip -> (Nothing, Tip)
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> 
          val (found, l') = updateLookupWithKey(l, k, f)
          (found, balance(kx, x, l', r))
        Gt -> 
          val (found, r') = updateLookupWithKey(r, k, f)
          (found, balance(kx, x, l, r'))
        Eq -> match f(k, x)
                Nothing -> (Just(x), glue(l, r))
                Just(x') -> (Just(x), Bin(size(m), k, x', l, r))

pub fun alter(m: map<k,a>, k: k, f: (maybe<a>) -> maybe<a>): <exn,comparable<k>> map<k,a>
  match m
    Tip -> match f(Nothing)
             Nothing -> Tip
             Just(x) -> singleton(k, x)
    Bin(_,kx,x,l,r) ->
      match compare(k, kx)
        Lt -> balance(kx, x, alter(l, k, f), r)
        Gt -> balance(kx, x, l, alter(r, k, f))
        Eq -> match f(Just(x))
                Nothing -> glue(l, r)
                Just(x') -> Bin(size(m), k, x', l, r)

pub fun findIndex(m: map<k,a>, k: k): <comparable<k>,exn> int
  match m.lookupIndex(k)
    Nothing -> throw("Map.findIndex: element is not in the map")
    Just(x) -> x

pub fun lookupIndex(m: map<k,a>, k: k): <comparable<k>> maybe<int>
  fun go(m1: map<k,a>, k1: k, i: int): comparable<k> maybe<int>
    match m1
      Tip -> Nothing
      Bin(_, kx, _, l, r) ->
        match compare(k1, kx)
          Lt -> go(l, k1, i)
          Gt -> go(r, k1, i + size(l) + 1)
          Eq -> Just(i + size(l))
  go(m,k,0)

pub fun elemAt(m: map<k,a>, i: int): exn (k,a)
  with int_compare
  m.elemAt_(i)

fun elemAt_(m: map<k,a>, i: int): <comparable<int>,exn> (k,a)
  match m
    Tip -> throw("Map.elemAt: index out of range")
    Bin(_, kx, x, l, r) ->
      match std/data/map/compare(i, l.size)
        Lt -> elemAt_(l, i)
        Gt -> elemAt_(r, i - l.size - 1)
        Eq -> (kx, x)

pub fun updateAt(m: map<k,a>, i: int, f: (k, a) -> maybe<a>): exn map<k,a>
  with int_compare
  m.updateAt_(i, f)

fun updateAt_(m: map<k,a>, i: int, f: (k, a) -> maybe<a>): <comparable<int>,exn> map<k,a>
  match m
    Tip -> throw("Map.updateAt: index out of range")
    Bin(sx, kx, x, l, r) ->
      match std/data/map/compare(i, l.size)
        Lt -> balance(kx, x, updateAt_(l, i, f), r)
        Gt -> balance(kx, x, l, updateAt_(r, i - l.size - 1, f))
        Eq -> match f(kx, x)
                Nothing -> glue(l, r)
                Just(x') -> Bin(sx, kx, x', l, r)

pub fun findMin(m: map<k,a>): exn (k,a)
  match m
    Tip -> throw("Map.findMin: empty map has no minimal element")
    Bin(_, kx, x, Tip, _) -> (kx, x)
    Bin(_, _, _, l, _) -> l.findMin()

pub fun findMax(m: map<k,a>): exn (k,a)
  match m
    Tip -> throw("Map.findMax: empty map has no maximal element")
    Bin(_, kx, x, _, Tip) -> (kx, x)
    Bin(_, _, _, _, r) -> r.findMax()

pub fun deleteMin(m: map<k,a>): exn map<k,a>
  match m
    Tip -> Tip
    Bin(_, _, _, Tip, r) -> r
    Bin(_, kx, x, l, r) -> balance(kx, x, l.deleteMin(), r)

pub fun deleteMax(m: map<k,a>): exn map<k,a>
  match m
    Tip -> Tip
    Bin(_, _, _, l, Tip) -> l
    Bin(_, kx, x, l, r) -> balance(kx, x, l, r.deleteMax())

pub fun updateMin(m: map<k,a>, f: (a) -> maybe<a>): <comparable<k>,exn> map<k,a>
  m.updateMinWithKey(fn(_, x) f(x))

pub fun updateMax(m: map<k,a>, f: (a) -> maybe<a>): <comparable<k>,exn> map<k,a>
  m.updateMaxWithKey(fn(_, x) f(x))

pub fun updateMinWithKey(m: map<k,a>, f: (k, a) -> maybe<a>): <comparable<k>,exn> map<k,a>
  match m
    Tip -> Tip
    Bin(sx, kx, x, Tip, r) -> match f(kx, x)
                               Nothing -> r
                               Just(x') -> Bin(sx, kx, x', Tip, r)
    Bin(_, kx, x, l, r) -> balance(kx, x, l.updateMinWithKey(f), r)

pub fun updateMaxWithKey(m: map<k,a>, f: (k, a) -> maybe<a>): <comparable<k>,exn> map<k,a>
  match m
    Tip -> Tip
    Bin(sx, kx, x, l, Tip) -> match f(kx, x)
                               Nothing -> l
                               Just(x') -> Bin(sx, kx, x', l, Tip)
    Bin(_, kx, x, l, r) -> balance(kx, x, l, r.updateMaxWithKey(f))

pub fun minViewWithKey(m: map<k,a>): exn maybe<((k,a),map<k,a>)>
  match m
    Tip -> Nothing
    _ -> Just(m.deleteFindMin())

pub fun maxViewWithKey(m: map<k,a>): exn maybe<((k,a),map<k,a>)>
  match m
    Tip -> Nothing
    _ -> Just(m.deleteFindMax())

pub fun minView(m: map<k,a>): exn maybe<(a, map<k,a>)>
  match m
    Tip -> Nothing
    _ ->
      val ((_, x), m') = m.deleteFindMin()
      Just((x, m'))

pub fun maxView(m: map<k,a>): exn maybe<(a, map<k,a>)>
  match m
    Tip -> Nothing
    _ ->
      val ((_, x), m') = m.deleteFindMax()
      Just((x, m'))

// pub fun unions(mps: list<map<k,a>>): map<k,a>
//   mps.foldl(empty, union)

// pub fun unionsWith(mps: list<map<k,a>>): map<k,a>
//   mps.foldl(empty, fn(a, b) a.unionWith(b, f))

// pub fun union(m1: map<k,a>, m2: map<k,a>): map<k,a>
//   match (m1, m2)
//     (Tip, _) -> m2
//     (_, Tip) -> m1
//     (_, _) -> hedgeUnionL(m1, m2)

// pub fun hedgeUnionL(m1: map<k,a>, m2: map<k,a>): map<k,a>
//   match (m1, m2)
//     (_, Tip) -> m1
//     (Tip, Bin(_, kx, x, l, r)) -> join(kx, x, filterGt, c)
// TODO: unions / diff / int / etc

// TODO: Eq typeclass methods
pub fun isSubmapOfBy(m1: map<k,a>, m2: map<k,b>, f: (a, b) -> bool): <comparable<k>,exn> bool
  m1.size < m2.size && m1.submap'(m2, f)

fun submap'(m1: map<k,a>, m2: map<k,b>, f: (a, b) -> bool): <comparable<k>,exn> bool
  match (m1, m2)
    (Tip, _) -> True
    (_, Tip) -> False
    (Bin(_, kx, x, l, r), t) ->
      val (lt, found, gt) = t.splitLookup(kx)
      match found
        Nothing -> False
        Just(y) -> f(x, y) && l.unsafe-decreasing().submap'(lt, f) && r.unsafe-decreasing().submap'(gt, f)

pub fun isProperSubmapOfBy(m1: map<k,a>, m2: map<k,b>, f: (a, b) -> bool): <comparable<k>,exn> bool
  m1.size < m2.size && m1.submap'(m2, f)

pub fun filter(m: map<k,a>, f: (a) -> bool): exn map<k,a>
  m.filterWithKey(fn(_, x) f(x))

pub fun filterWithKey(m: map<k,a>, f: (k, a) -> bool): exn map<k,a>
  match m
    Tip -> Tip
    Bin(_, kx, x, l, r) ->
      if f(kx, x) then
        join(kx, x, l.filterWithKey(f), r.filterWithKey(f))
      else
        merge(l.filterWithKey(f), r.filterWithKey(f))

pub fun partition(m: map<k,a>, f: (a) -> bool): exn (map<k,a>, map<k,a>)
  m.partitionWithKey(fn(_, x) f(x))

pub fun partitionWithKey(m: map<k,a>, f: (k, a) -> bool): exn (map<k,a>, map<k,a>)
  match m
    Tip -> (Tip, Tip)
    Bin(_, kx, x, l, r) ->
      val (l1, l2) = l.partitionWithKey(f)
      val (r1, r2) = r.partitionWithKey(f)
      if f(kx, x) then
        (join(kx, x, l1, r1), merge(l2, r2))
      else
        (merge(l1, r1), join(kx, x, l2, r2))

pub fun mapMaybe(m: map<k,a>, f: (a) -> maybe<b>): exn map<k,b>
  m.mapMaybeWithKey(fn(_, x) f(x))

pub fun mapMaybeWithKey(m: map<k,a>, f: (k, a) -> maybe<b>): exn map<k,b>
  match m
    Tip -> Tip
    Bin(_, kx, x, l, r) ->
      match f(kx, x)
        Nothing -> merge(l.mapMaybeWithKey(f), r.mapMaybeWithKey(f))
        Just(y) -> join(kx, y, l.mapMaybeWithKey(f), r.mapMaybeWithKey(f))

pub fun mapEither(m: map<k,a>, f: (a) -> either<b,c>): exn (map<k,b>, map<k,c>)
  m.mapEitherWithKey(fn(_, x) f(x))

pub fun mapEitherWithKey(m: map<k,a>, f: (k, a) -> either<b,c>): exn (map<k,b>, map<k,c>)
  match m
    Tip -> (Tip, Tip)
    Bin(_, kx, x, l, r) ->
      val (l1, l2) = l.mapEitherWithKey(f)
      val (r1, r2) = r.mapEitherWithKey(f)
      match f(kx, x)
        Left(y) ->
          (join(kx, y, l1, r1), merge(l2, r2))
        Right(y) ->
          (merge(l1, r1), join(kx, y, l2, r2))

pub fun map(m: map<k,a>, f: (a) -> b): exn map<k,b>
  m.mapWithKey(fn(_, x) f(x))

pub fun mapWithKey(m: map<k,a>, f: (k, a) -> b): exn map<k,b>
  match m
    Tip -> Tip
    Bin(sz, kx, x, l, r) ->
      Bin(sz, kx, f(kx, x), l.mapWithKey(f), r.mapWithKey(f))

pub fun mapAccum(m: map<k,b>, a: a, f: (a, b) -> (a,c)): (a, map<k,c>)
  m.mapAccumWithKey(a, fn(a', _, x') f(a', x'))

pub fun mapAccumWithKey(m: map<k,b>, a: a, f: (a, k, b) -> (a,c)): (a, map<k,c>)
  match m
    Tip -> (a, Tip)
    Bin(sx, kx, x, l, r) ->
      val (a', r') = r.mapAccumWithKey(a, f)
      val (a'', x') = f(a', kx, x)
      val (a''', l') = l.mapAccumWithKey(a'', f)
      (a''', Bin(sx, kx, x', l', r'))

// TODO: Stuff here

pub fun fold(m: map<k,a>, b: b, f: (b, a) -> b): b
  m.foldrWithKey(b, fn(x, _, y) f(x, y))

pub fun foldrWithKey(m: map<k,a>, b: b, f: (b, k, a) -> b): b
  match m
    Tip -> b
    Bin(_, kx, x, l, r) ->
      l.foldrWithKey(f(r.foldrWithKey(b, f), kx, x), f)

pub fun foldlWithKey(m: map<k,a>, b: b, f: (b, k, a) -> b): b
  match m
    Tip -> b
    Bin(_, kx, x, l, r) ->
      r.foldlWithKey(f(l.foldlWithKey(b, f), kx, x), f)

// TODO: Stuff here
pub fun fromList(xs: list<(k,a)>): <comparable<k>,exn> map<k,a>
  xs.foldl(empty, fn(m, (k, x)) m.insert(k, x))

pub fun toList(m: map<k,a>): list<(k,a)>
  m.toAscList()

pub fun toAscList(m: map<k,a>): list<(k,a)>
  m.foldrWithKey([], fn(xs, k, x) Cons((k,x), xs))

pub fun toDescList(m: map<k,a>): list<(k,a)>
  m.foldlWithKey([], fn(xs, k, x) Cons((k,x), xs))

// TODO: Stuff here


// TODO: Stuff here

pub fun filterGt(m: map<k,a>, cmp: (k) -> order): exn map<k,a>
  match m
    Tip -> Tip
    Bin(_, kx, x, l, r) ->
      match cmp(kx)
        Lt -> join(kx, x, l.filterGt(cmp), r)
        Gt -> r.filterGt(cmp)
        Eq -> r

pub fun filterLt(m: map<k,a>, cmp: (k) -> order): exn map<k,a>
  match m
    Tip -> Tip
    Bin(_, kx, x, l, r) ->
      match cmp(kx)
        Lt -> l.filterLt(cmp)
        Gt -> join(kx, x, l, r.filterLt(cmp))
        Eq -> l

pub fun split(m: map<k,a>, k: k): <comparable<k>,exn> (map<k,a>, map<k,a>)
  match m
    Tip -> (Tip, Tip)
    Bin(_, kx, x, l, r) ->
      match compare(k, kx)
        Lt -> 
          val (lt, gt) = l.split(k)
          (lt, join(kx, x, gt, r))
        Gt -> 
          val (lt, gt) = r.split(k)
          (join(kx, x, l, lt), gt)
        Eq -> (l, r)

fun splitLookup(m: map<k,a>, k: k): <comparable<k>,exn> (map<k,a>, maybe<a>, map<k,a>)
  match m
    Tip -> (Tip, Nothing, Tip)
    Bin(_, kx, x, l, r) ->
      match compare(k, kx)
        Lt -> 
          val (lt, found, gt) = l.splitLookup(k)
          (lt, found, join(kx, x, gt, r))
        Gt -> 
          val (lt, found, gt) = r.splitLookup(k)
          (join(kx, x, l, lt), found, gt)
        Eq -> (l, Just(x), r)

fun splitLookupWithKey(m: map<k,a>, k: k): <comparable<k>,exn> (map<k,a>, maybe<(k,a)>, map<k,a>)
  match m
    Tip -> (Tip, Nothing, Tip)
    Bin(_, kx, x, l, r) ->
      match compare(k, kx)
        Lt -> 
          val (lt, found, gt) = l.splitLookupWithKey(k)
          (lt, found, join(kx, x, gt, r))
        Gt -> 
          val (lt, found, gt) = r.splitLookupWithKey(k)
          (join(kx, x, l, lt), found, gt)
        Eq -> (l, Just((kx, x)), r)

fun join(kx: k, x: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match (l, r)
    (Tip, _) -> insertMin(r, kx, x)
    (_, Tip) -> insertMax(l, kx, x)
    (Bin(sizeL, ky, y, ly, ry), Bin(sizeR, kz, z, lz, rz)) ->
      if delta*sizeL <= sizeR then
        balance(kz, z, join(kx, x, l.unsafe-decreasing(), lz.unsafe-decreasing()), rz)
      elif delta*sizeR <= sizeL then
        balance(ky, y, ly, join(kx, x, ry.unsafe-decreasing(), r.unsafe-decreasing()))
      else
        bin(kx, x, l, r)

fun insertMax(m: map<k,a>, kx: k, x: a): exn map<k,a>
  match m
    Tip -> singleton(kx, x)
    Bin(_, ky, y, l, r) -> balance(ky, y, l, r.insertMax(kx, x))

fun insertMin(m: map<k,a>, kx: k, x: a): exn map<k,a>
  match m
    Tip -> singleton(kx, x)
    Bin(_, ky, y, l, r) -> balance(ky, y, l.insertMin(kx, x), r)

fun merge(l: map<k,a>, r: map<k,a>): exn map<k,a>
  match (l, r)
    (Tip, _) -> r
    (_, Tip) -> l
    (Bin(sizeL, kx, x, lx, rx), Bin(sizeR, ky, y, ly, ry)) -> 
      if sizeL*delta <= sizeR then
        balance(ky, y, merge(l.unsafe-decreasing(),ly.unsafe-decreasing()), ry)
      elif sizeR*delta <= sizeL then
        balance(kx, x, lx, merge(rx.unsafe-decreasing(), r.unsafe-decreasing()))
      else
        glue(l, r)

fun glue(l: map<k,a>, r: map<k,a>): exn map<k,a>
  match (l, r)
    (Tip, _) -> r
    (_, Tip) -> l
    (_, _) -> if l.size > r.size then
        val ((km,m), l') = l.deleteFindMax()
        balance(km, m, l', r)
      else
        val ((km,m), r') = r.deleteFindMin()
        balance(km, m, l, r')

fun deleteFindMin(m: map<k,a>): exn ((k,a), map<k,a>)
  match m
    Tip -> throw("Map.deleteFindMin: empty map has no minimal element")
    Bin(_, kx, x, Tip, r) -> ((kx, x), r)
    Bin(_, kx, x, l, r) ->
      val (km, l') = l.deleteFindMin()
      (km, balance(kx, x, l', r))

fun deleteFindMax(m: map<k,a>): exn ((k,a), map<k,a>)
  match m
    Tip -> throw("Map.deleteFindMax: empty map has no maximal element")
    Bin(_, kx, x, l, Tip) -> ((kx, x), l)
    Bin(_, kx, x, l, r) ->
      val (km, r') = r.deleteFindMax()
      (km, balance(kx, x, l, r'))


val delta = 4;
val ratio = 2;
fun balance(k: k, a: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  val lsize = l.size
  val rsize = r.size
  val sizeX = lsize + rsize + 1
  if lsize + rsize <= 1 then
    Bin(sizeX, k, a, l, r)
  elif rsize >= delta*lsize then
    rotateL(k, a, l, r)
  elif lsize >= delta*rsize then
    rotateR(k, a, l, r)
  else
    Bin(sizeX, k, a, l, r)

fun rotateL(k: k, x: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Tip -> throw("rotateL: Tip")
    Bin(_,_,_,ly,ry) ->
      if ly.size < ratio*ry.size then
        singleL(k, x, l, r)
      else
        doubleL(k, x, l, r)

fun rotateR(k: k, x: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match l
    Tip -> throw("rotateR: Tip")
    Bin(_,_,_,ly,ry) ->
      if ry.size < ratio*ly.size then
        singleR(k, x, l, r)
      else
        doubleR(k, x, l, r)

fun singleL(k1: k, x1: a, t1: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Tip -> throw("singleL: Tip")
    Bin(_,k2,x2,t2,t3) -> bin(k2,x2,bin(k1,x1,t1,t2),t3)

fun singleR(k1: k, x1: a, l: map<k,a>, t3: map<k,a>): exn map<k,a>
  match l
    Tip -> throw("singleR: Tip")
    Bin(_,k2,x2,t1,t2) -> bin(k2,x2,t1,bin(k1,x1,t2,t3))

fun doubleL(k1: k, x1: a, t1: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Bin(_,k2,x2,Bin(_,k3,x3,t2,t3),t4) -> bin(k3,x3,bin(k1,x1,t1,t2),bin(k2,x2,t3,t4))
    _ -> throw("doubleL: Tip")

fun doubleR(k1: k, x1: a, l: map<k,a>, t4: map<k,a>): exn map<k,a>
  match l
    Bin(_,k2,x2,t1,Bin(_,k3,x3,t2,t3)) -> bin(k3,x3,bin(k2,x2,t1,t2),bin(k1,x1,t3,t4))
    _ -> throw("doubleR: Tip")

inline fun bin(k: k, a: a, l: map<k,a>, r: map<k,a>): map<k,a>
  Bin(l.size + r.size + 1, k, a, l, r)


pub alias stringthread = (string) -> string

pub fun thread(f: stringthread, g: stringthread): stringthread
  fn(x) g(f(x))

pub fun thread(f: stringthread, g: string): stringthread
  fn(x) g ++ f(x)

pub fun showString(s: string): stringthread
  fn(x) s ++ x

pub fun showTreeWith(m: map<k,a>, hang: bool, wide: bool, f: (k,a) -> string): string
  if hang then
    m.showsTreeHang(f, wide, [])("")
  else
    m.showsTree(f, wide, [], [])("")

fun showsTree(m: map<k,a>, f: (k,a) -> string, wide: bool, lbars: list<string>, rbars: list<string>): stringthread
  match m
    Tip -> 
      showsBars(lbars).thread("|\n")
    Bin(_, kx, x, Tip, Tip) -> 
      showsBars(lbars).thread(f(kx, x)).thread("\n")
    Bin(_, kx, x, l, r) -> 
      r.showsTree(f, wide, withBar(rbars), withEmpty(rbars))
      .thread(showWide(wide, rbars))
      .thread(showsBars(lbars))
      .thread(f(kx, x)).thread("\n")
      .thread(showWide(wide, lbars))
      .thread(l.showsTree(f, wide, withEmpty(lbars), withBar(lbars)))

fun showsTreeHang(m: map<k,a>, f: (k,a) -> string, wide: bool, bars: list<string>): stringthread
  match m
    Tip -> 
      showsBars(bars).thread("|\n")
    Bin(_, kx, x, Tip, Tip) -> 
      showsBars(bars).thread(f(kx,x)).thread("\n")
    Bin(_, kx, x, l, r) -> 
      showsBars(bars).thread(f(kx,x)).thread("\n")
      .thread(showWide(wide, bars))
      .thread(l.showsTreeHang(f, wide, withBar(bars)))
      .thread(showWide(wide, bars))
      .thread(r.showsTreeHang(f, wide, withEmpty(bars)))

fun showWide(wide: bool, bars: list<string>): stringthread
  if wide then
    showString(bars.foldr("", fn(b, s1) b ++ s1) ++ "|\n")
  else
    id

fun showsBars(bars: list<string>): stringthread
  match bars
    [] -> id
    _ -> showString(bars.tail.foldr("", fn(b, s) b ++ s) ++ "+--")

fun withBar(bars: list<string>): list<string>
  Cons("|  ", bars)

fun withEmpty(bars: list<string>): list<string>
  Cons("   ", bars)