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

// TODO: Update / view methods / unions / diff / int / etc










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

fun rotateL(k: k, a: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Tip -> throw("rotateL: Tip")
    Bin(_,_,_,ly,ry) ->
      if ly.size <= ratio*ry.size then
        singleL(k, a, l, r)
      else
        doubleL(k, a, l, r)

fun rotateR(k: k, a: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match l
    Tip -> throw("rotateR: Tip")
    Bin(_,_,_,ly,ry) ->
      if ry.size < ratio*ly.size then
        singleR(k, a, l, r)
      else
        doubleR(k, a, l, r)

fun singleL(k1: k, a1: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Tip -> throw("singleL: Tip")
    Bin(_,k2,a2,ly,ry) -> bin(k2,a2,bin(k1,a1,l,ly),ry)

fun singleR(k1: k, a1: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match l
    Tip -> throw("singleR: Tip")
    Bin(_,k2,a2,ly,ry) -> bin(k2,a2,ly,bin(k1,a1,ry,r))

fun doubleL(k1: k, a1: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match r
    Bin(_,k2,a2,Bin(_,k3,a3,t2,t3),t4) -> bin(k3,a3,bin(k1,a1,l,t2),bin(k2,a2,t3,t4))
    _ -> throw("doubleL: Tip")

fun doubleR(k1: k, a1: a, l: map<k,a>, r: map<k,a>): exn map<k,a>
  match l
    Bin(_,k2,a2,t1,Bin(_,k3,a3,t2,t3)) -> bin(k3,a3,bin(k2,a2,t1,t2),bin(k1,a1,t3,r))
    _ -> throw("doubleR: Tip")

inline fun bin(k: k, a: a, l: map<k,a>, r: map<k,a>): map<k,a>
  Bin(l.size + r.size + 1, k, a, l, r)