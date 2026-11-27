// Model-based check of the red-black tree maps and sets against association lists.
// After every operation the tree must also keep the red-black invariants, since a wrong rotation can leave the entries right and the tree unbalanced.
import std/data/rbtree
import std/data/map
import std/data/set

type op
  Put(k : int, v : int)
  Add(k : int, v : int)
  Del(k : int)
  Ins(k : int)            // insert: absent -> 0, present -> increment

fun insert-sorted(x : (int, int), xs : list<(int, int)>) : list<(int, int)>
  match xs
    Cons(y, ys) | y.fst < x.fst -> Cons(y, insert-sorted(x, ys))
    _ -> Cons(x, xs)

fun sorted(xs : list<(int, int)>) : list<(int, int)>
  xs.foldl([], fn(acc, x) insert-sorted(x, acc))

fun show-entries(xs : list<(int, int)>) : string
  xs.map(fn((k, v)) k.show ++ ":" ++ v.show).join(",")

fun m-lookup(m : list<(int, int)>, k : int) : maybe<int>
  m.find(fn((k2, _)) k2 == k).map(snd)

fun m-remove(m : list<(int, int)>, k : int) : list<(int, int)>
  m.filter(fn((k2, _)) k2 != k)

fun m-set(m : list<(int, int)>, k : int, v : int) : list<(int, int)>
  Cons((k, v), m.m-remove(k))

fun ins(old : maybe<int>) : int
  match old
    Nothing -> 0
    Just(v) -> v + 1

fun m-apply(m : list<(int, int)>, o : op) : list<(int, int)>
  match o
    Put(k, v) -> m.m-set(k, v)
    Add(k, v) -> if m.m-lookup(k).is-just then m else m.m-set(k, v)
    Del(k)    -> m.m-remove(k)
    Ins(k)    -> m.m-set(k, ins(m.m-lookup(k)))

fun apply(t : map<int, int>, o : op) : map<int, int>
  match o
    Put(k, v) -> t.set(k, v)
    Add(k, v) -> t.add(k, v)
    Del(k)    -> t.remove(k)
    Ins(k)    -> t.insert(k, ins)

// Pseudo-random operations over a pool of keys; deletions are frequent so that every rebalancing case of removal is reached.
fun ops(pool : list<int>, seed : int, n : int) : list<op>
  val len = pool.length
  list(1, n).map fn(i)
    val h = (i * 7919 + seed * 104729) % 1000003
    val k = pool[h % len].default(0)
    match h % 6
      0 -> Del(k)
      1 -> Del(k)
      2 -> Ins(k)
      3 -> Add(k, h % 100)
      _ -> Put(k, h % 100)

// The black height of `t`, or `Nothing` if a red node has a red child or two paths differ in black nodes.
fun black-height(t : rbtree<k,v>) : maybe<int>
  match t
    Leaf -> Just(1)
    Node(c, l, _, _, r) ->
      if c.is-red && (l.tree/is-red || r.tree/is-red) then Nothing
      else match (black-height(l), black-height(r))
        (Just(hl), Just(hr)) | hl == hr -> Just(if c.is-red then hl else hl + 1)
        _ -> Nothing

fun valid(t : rbtree<k,v>) : bool
  !t.tree/is-red && black-height(t).is-just

fun check-map(pool : list<int>) : console ()
  var failures := 0
  fun expect(what : string, got : string, want : string)
    if got != want then
      failures := failures + 1
      if failures <= 5 then println("map: " ++ what ++ "\n  got:  " ++ got ++ "\n  want: " ++ want)
  for(1, 60) fn(seed)
    val (t, m, ok) = ops(pool, seed, seed * 4).foldl((map/empty(), [], True)) fn((acc, model, ok), o)
      val t1 = acc.apply(o)
      (t1, model.m-apply(o), ok && t1.tree.valid)
    val (t2, m2) = ops(pool, seed + 1000, seed * 2).foldl((map/empty(), [])) fn((acc, model), o)
      (acc.apply(o), model.m-apply(o))
    expect("invariants " ++ seed.show, ok.show, "True")
    expect("list", t.list.show-entries, m.sorted.show-entries)
    expect("keys", t.keys.show, m.sorted.map(fst).show)
    expect("size", t.size.show, m.length.show)
    expect("is-empty", t.is-empty.show, m.is-nil.show)
    pool.foreach fn(k)
      expect("lookup " ++ k.show, t.lookup(k).show, m.m-lookup(k).show)
      expect("contains " ++ k.show, t.contains(k).show, m.m-lookup(k).is-just.show)
    expect("union-with", t.union-with(t2, fn(a, b) a * 1000 + b).list.show-entries,
           m2.foldl(m, fn(acc, (k, v2)) acc.m-set(k, match acc.m-lookup(k) { Just(v1) -> v1 * 1000 + v2; Nothing -> v2 })).sorted.show-entries)
    expect("union", t.union(t2).list.show-entries, m2.foldl(m, fn(acc, (k, v)) if acc.m-lookup(k).is-just then acc else acc.m-set(k, v)).sorted.show-entries)
    val emptied = m.foldl(t, fn(acc, (k, _)) acc.remove(k))
    expect("remove all", (emptied.is-empty && emptied.tree.valid).show, "True")
  println("map: " ++ (if failures == 0 then "ok" else failures.show ++ " failures"))

fun check-set(pool : list<int>) : console ()
  var failures := 0
  fun expect(what : string, got : string, want : string)
    if got != want then
      failures := failures + 1
      if failures <= 5 then println("set: " ++ what ++ "\n  got:  " ++ got ++ "\n  want: " ++ want)
  fun build(xs : list<op>) : (set<int>, list<int>)
    xs.foldl((set/empty(), [])) fn((s, model), x)
      match x
        Del(k) -> (s.remove(k), model.filter(fn(y) y != k))
        Put(k, _) -> (s.set/add(k), Cons(k, model.filter(fn(y) y != k)))
        Add(k, _) -> (s.set/add(k), Cons(k, model.filter(fn(y) y != k)))
        Ins(k) -> (s.set/add(k), Cons(k, model.filter(fn(y) y != k)))
  fun has(xs : list<int>, x : int) : bool
    xs.any(fn(y) y == x)
  fun elems(xs : list<int>) : list<int>
    xs.map(fn(x) (x, 0)).sorted.map(fst)
  for(1, 40) fn(seed)
    val (s1, m1) = build(ops(pool, seed, seed * 3))
    val (s2, m2) = build(ops(pool, seed + 1000, seed * 2))
    expect("list", s1.list.show, m1.elems.show)
    expect("size", s1.size.show, m1.length.show)
    pool.foreach fn(k)
      expect("member " ++ k.show, s1.member(k).show, m1.has(k).show)
    expect("union", (s1 + s2).list.show, (m1 ++ m2.filter(fn(x) !m1.has(x))).elems.show)
    expect("intersection", s1.intersection(s2).list.show, m1.filter(fn(x) m2.has(x)).elems.show)
    expect("difference", (s1 - s2).list.show, m1.filter(fn(x) !m2.has(x)).elems.show)
    expect("==", (s1 == s2).show, (m1.elems == m2.elems).show)
    expect("is-subset-of", s1.is-subset-of(s2).show, m1.all(fn(x) m2.has(x)).show)
    expect("disjoint", s1.disjoint(s2).show, m1.all(fn(x) !m2.has(x)).show)
  println("set: " ++ (if failures == 0 then "ok" else failures.show ++ " failures"))

fun main()
  val pool = list(-20, 20) ++ [100, -100, 1000, 12345]
  check-map(pool)
  check-set(pool)
