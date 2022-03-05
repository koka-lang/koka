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