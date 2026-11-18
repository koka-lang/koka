// Model-based check of the Patricia tree maps and sets against association lists, for all
// three key types. The key pools cover the sign bit, the extremes of each width, and (for `int`)
// keys beyond 64 bits, so every branching-bit and prefix computation is exercised.
import std/data/imap
import std/data/iset
import std/num/i64map
import std/num/i64set
import std/num/i32map
import std/num/i32set
import std/num/int64
import std/num/int32

// ----------------------------------------------------------------------------
// The model
// ----------------------------------------------------------------------------

type op
  Put(k : int, v : int)
  Del(k : int)
  Upd(k : int)            // update: absent -> 0, even -> remove, odd -> increment

fun insert-sorted(x : (int, int), xs : list<(int, int)>) : list<(int, int)>
  match xs
    Cons(y, ys) | y.fst < x.fst -> Cons(y, insert-sorted(x, ys))
    _ -> Cons(x, xs)

fun sorted(xs : list<(int, int)>) : list<(int, int)>
  xs.foldl([], fn(acc, x) insert-sorted(x, acc))

fun show-entries(xs : list<(int, int)>) : string
  xs.sorted.map(fn((k, v)) k.show ++ ":" ++ v.show).join(",")

fun m-lookup(m : list<(int, int)>, k : int) : maybe<int>
  m.find(fn((k2, _)) k2 == k).map(snd)

fun m-remove(m : list<(int, int)>, k : int) : list<(int, int)>
  m.filter(fn((k2, _)) k2 != k)

fun m-set(m : list<(int, int)>, k : int, v : int) : list<(int, int)>
  Cons((k, v), m.m-remove(k))

fun upd(old : maybe<int>) : maybe<int>
  match old
    Nothing -> Just(0)
    Just(v) -> if v.is-even then Nothing else Just(v + 1)

fun m-apply(m : list<(int, int)>, o : op) : list<(int, int)>
  match o
    Put(k, v) -> m.m-set(k, v)
    Del(k)    -> m.m-remove(k)
    Upd(k)    -> match upd(m.m-lookup(k))
      Nothing -> m.m-remove(k)
      Just(v) -> m.m-set(k, v)

fun m-union-with(m1 : list<(int, int)>, m2 : list<(int, int)>, f : (int, int) -> int) : list<(int, int)>
  m2.foldl(m1) fn(acc, (k, v2))
    match acc.m-lookup(k)
      Nothing  -> acc.m-set(k, v2)
      Just(v1) -> acc.m-set(k, f(v1, v2))

fun m-difference(m1 : list<(int, int)>, m2 : list<(int, int)>) : list<(int, int)>
  m1.filter(fn((k, _)) m2.m-lookup(k).is-nothing)

// Pseudo-random operations over a pool of keys.
fun ops(pool : list<int>, seed : int, n : int) : list<op>
  val len = pool.length
  list(1, n).map fn(i)
    val h = (i * 7919 + seed * 104729) % 1000003
    val k = pool[h % len].default(0)
    match h % 5
      0 -> Del(k)
      1 -> Upd(k)
      _ -> Put(k, h % 100)

// ----------------------------------------------------------------------------
// Maps
// ----------------------------------------------------------------------------

struct map-ops<m>
  empty : m
  insert : (m, int, int) -> m
  insert-new : (m, int, int) -> m
  remove : (m, int) -> m
  update : (m, int) -> m
  lookup : (m, int) -> maybe<int>
  member : (m, int) -> bool
  size : m -> int
  entries : m -> list<(int, int)>
  union : (m, m) -> m
  union-with : (m, m, (int, int) -> int) -> m
  insert-with : (m, int, int, (int, int) -> int) -> m
  difference : (m, m) -> m
  is-empty : m -> bool

fun build(o : map-ops<m>, xs : list<op>) : (m, list<(int, int)>)
  xs.foldl((o.map-ops/empty, [])) fn((t, model), x)
    val t1 = match x
      Put(k, v) -> (o.insert)(t, k, v)
      Del(k)    -> (o.remove)(t, k)
      Upd(k)    -> (o.update)(t, k)
    (t1, model.m-apply(x))

fun check-map(name : string, o : map-ops<m>, pool : list<int>) : console ()
  var failures := 0
  fun expect(what : string, got : string, want : string)
    if got != want then
      failures := failures + 1
      if failures <= 5 then println(name ++ ": " ++ what ++ "\n  got:  " ++ got ++ "\n  want: " ++ want)
  for(1, 40) fn(seed)
    val (t1, m1) = o.build(ops(pool, seed, seed * 3))
    val (t2, m2) = o.build(ops(pool, seed + 1000, seed * 2))
    expect("entries", (o.entries)(t1).show-entries, m1.show-entries)
    expect("size", (o.size)(t1).show, m1.length.show)
    expect("is-empty", (o.is-empty)(t1).show, m1.is-nil.show)
    pool.foreach fn(k)
      expect("lookup " ++ k.show, (o.lookup)(t1, k).show, m1.m-lookup(k).show)
      expect("member " ++ k.show, (o.member)(t1, k).show, m1.m-lookup(k).is-just.show)
      expect("insert-new " ++ k.show, (o.entries)((o.insert-new)(t1, k, 999)).show-entries,
             (if m1.m-lookup(k).is-just then m1 else m1.m-set(k, 999)).show-entries)
      expect("insert-with " ++ k.show, (o.entries)((o.insert-with)(t1, k, 7, fn(a, b) a * 1000 + b)).show-entries,
             m1.m-union-with([(k, 7)], fn(a, b) a * 1000 + b).show-entries)
    expect("union", (o.entries)((o.union)(t1, t2)).show-entries, m1.m-union-with(m2, fn(_, b) b).show-entries)
    expect("union-with", (o.entries)((o.union-with)(t1, t2, fn(a, b) a * 1000 + b)).show-entries,
           m1.m-union-with(m2, fn(a, b) a * 1000 + b).show-entries)
    expect("difference", (o.entries)((o.difference)(t1, t2)).show-entries, m1.m-difference(m2).show-entries)
    expect("remove all", (o.is-empty)(m1.foldl(t1, fn(t, (k, _)) (o.remove)(t, k))).show, "True")
  println(name ++ ": " ++ (if failures == 0 then "ok" else failures.show ++ " failures"))

val imap-ops : map-ops<imap<int>> = Map-ops(
  imap/empty(), fn(t, k, v) t.insert(k, v), fn(t, k, v) t.insert-new(k, v), fn(t, k) t.remove(k),
  fn(t, k) t.update(k, upd), fn(t, k) t.lookup(k), fn(t, k) t.member(k), fn(t) t.size, fn(t) t.list,
  fn(a, b) a.union(b), fn(a, b, f) a.union-with(b, f), fn(t, k, v, f) t.insert-with(k, v, f),
  fn(a, b) a.difference(b), fn(t) t.is-empty)

val i64map-ops : map-ops<i64map<int>> = Map-ops(
  i64map/empty(), fn(t, k, v) t.insert(k.int64, v), fn(t, k, v) t.insert-new(k.int64, v), fn(t, k) t.remove(k.int64),
  fn(t, k) t.update(k.int64, upd), fn(t, k) t.lookup(k.int64), fn(t, k) t.member(k.int64), fn(t) t.size,
  fn(t) t.list.map(fn((k, v)) (k.int, v)),
  fn(a, b) a.union(b), fn(a, b, f) a.union-with(b, f), fn(t, k, v, f) t.insert-with(k.int64, v, f),
  fn(a, b) a.difference(b), fn(t) t.is-empty)

val i32map-ops : map-ops<i32map<int>> = Map-ops(
  i32map/empty(), fn(t, k, v) t.insert(k.int32, v), fn(t, k, v) t.insert-new(k.int32, v), fn(t, k) t.remove(k.int32),
  fn(t, k) t.update(k.int32, upd), fn(t, k) t.lookup(k.int32), fn(t, k) t.member(k.int32), fn(t) t.size,
  fn(t) t.list.map(fn((k, v)) (k.int, v)),
  fn(a, b) a.union(b), fn(a, b, f) a.union-with(b, f), fn(t, k, v, f) t.insert-with(k.int32, v, f),
  fn(a, b) a.difference(b), fn(t) t.is-empty)

// ----------------------------------------------------------------------------
// Sets
// ----------------------------------------------------------------------------

struct set-ops<s>
  empty : s
  insert : (s, int) -> s
  remove : (s, int) -> s
  member : (s, int) -> bool
  size : s -> int
  elems : s -> list<int>
  union : (s, s) -> s
  intersection : (s, s) -> s
  difference : (s, s) -> s
  eq : (s, s) -> bool
  subset : (s, s) -> bool
  disjoint : (s, s) -> bool

fun show-elems(xs : list<int>) : string
  xs.map(fn(x) (x, 0)).show-entries

fun check-set(name : string, o : set-ops<s>, pool : list<int>) : console ()
  var failures := 0
  fun expect(what : string, got : string, want : string)
    if got != want then
      failures := failures + 1
      if failures <= 5 then println(name ++ ": " ++ what ++ "\n  got:  " ++ got ++ "\n  want: " ++ want)
  fun build(xs : list<op>)
    xs.foldl((o.set-ops/empty, [])) fn((t, model), x)
      match x
        Del(k)    -> ((o.remove)(t, k), model.filter(fn(y) y != k))
        Put(k, _) -> ((o.insert)(t, k), Cons(k, model.filter(fn(y) y != k)))
        Upd(k)    -> if model.any(fn(y) y == k) then ((o.remove)(t, k), model.filter(fn(y) y != k))
                     else ((o.insert)(t, k), Cons(k, model))
  fun has(xs : list<int>, x : int) : bool
    xs.any(fn(y) y == x)
  for(1, 40) fn(seed)
    val (t1, m1) = build(ops(pool, seed, seed * 3))
    val (t2, m2) = build(ops(pool, seed + 1000, seed * 2))
    // a set built from the same elements in another order must have the same (canonical) shape
    val t1r = m1.foldl(o.set-ops/empty, fn(t, x) (o.insert)(t, x))
    expect("elems", (o.elems)(t1).show-elems, m1.show-elems)
    expect("size", (o.size)(t1).show, m1.length.show)
    pool.foreach fn(k)
      expect("member " ++ k.show, (o.member)(t1, k).show, m1.has(k).show)
    expect("union", (o.elems)((o.union)(t1, t2)).show-elems, (m1 ++ m2.filter(fn(x) !m1.has(x))).show-elems)
    expect("intersection", (o.elems)((o.intersection)(t1, t2)).show-elems, m1.filter(fn(x) m2.has(x)).show-elems)
    expect("difference", (o.elems)((o.difference)(t1, t2)).show-elems, m1.filter(fn(x) !m2.has(x)).show-elems)
    expect("== reordered", (o.eq)(t1, t1r).show, "True")
    expect("==", (o.eq)(t1, t2).show, (m1.show-elems == m2.show-elems).show)
    expect("subset", (o.subset)(t1, t2).show, m1.all(fn(x) m2.has(x)).show)
    expect("subset of union", (o.subset)(t1, (o.union)(t1, t2)).show, "True")
    expect("intersection subset", (o.subset)((o.intersection)(t1, t2), t2).show, "True")
    expect("disjoint", (o.disjoint)(t1, t2).show, m1.all(fn(x) !m2.has(x)).show)
    expect("disjoint difference", (o.disjoint)((o.difference)(t1, t2), t2).show, "True")
  println(name ++ ": " ++ (if failures == 0 then "ok" else failures.show ++ " failures"))

val iset-ops : set-ops<iset> = Set-ops(
  iset/empty(), fn(t, k) t.insert(k), fn(t, k) t.remove(k), fn(t, k) t.member(k), fn(t) t.size, fn(t) t.list,
  fn(a, b) a.union(b), fn(a, b) a.intersection(b), fn(a, b) a.difference(b),
  fn(a, b) a == b, fn(a, b) a.is-subset-of(b), fn(a, b) a.disjoint(b))

val i64set-ops : set-ops<i64set> = Set-ops(
  i64set/empty(), fn(t, k) t.insert(k.int64), fn(t, k) t.remove(k.int64), fn(t, k) t.member(k.int64), fn(t) t.size,
  fn(t) t.list.map(int),
  fn(a, b) a.union(b), fn(a, b) a.intersection(b), fn(a, b) a.difference(b),
  fn(a, b) a == b, fn(a, b) a.is-subset-of(b), fn(a, b) a.disjoint(b))

val i32set-ops : set-ops<i32set> = Set-ops(
  i32set/empty(), fn(t, k) t.insert(k.int32), fn(t, k) t.remove(k.int32), fn(t, k) t.member(k.int32), fn(t) t.size,
  fn(t) t.list.map(int),
  fn(a, b) a.union(b), fn(a, b) a.intersection(b), fn(a, b) a.difference(b),
  fn(a, b) a == b, fn(a, b) a.is-subset-of(b), fn(a, b) a.disjoint(b))

// ----------------------------------------------------------------------------

val pool32 = list(-12, 12) ++ [0x7FFF_FFFF, negate(0x8000_0000), 0x4000_0000, negate(0x4000_0000), 1000, -1000, 0x10000, 0x10001]
val pool64 = pool32 ++ [0x7FFF_FFFF_FFFF_FFFF, negate(0x8000_0000_0000_0000), 0x1_0000_0000, negate(0x1_0000_0000), 0x4000_0000_0000_0000]
val pool-big = pool64 ++ [0x1_0000_0000_0000_0000, negate(0x1_0000_0000_0000_0000), exp2(100), exp2(100) + 1, negate(exp2(100)), exp2(200) + 12, negate(exp2(64)) - 1]

fun main()
  check-map("i32map", i32map-ops, pool32)
  check-map("i64map", i64map-ops, pool64)
  check-map("imap", imap-ops, pool-big)
  check-set("i32set", i32set-ops, pool32)
  check-set("i64set", i64set-ops, pool64)
  check-set("iset", iset-ops, pool-big)
