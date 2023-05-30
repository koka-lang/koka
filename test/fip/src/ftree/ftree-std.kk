// Adapted from "Finger Trees Explained Anew, and Slightly Simplified (Functional Pearl)", Claessen
import std/num/int32
import std/os/env

// Originally "some" which is a reserved keyword in Koka
type afew<a>
  One(a : a)
  Two(a : a, b : a)
  Three(a : a, b : a, c : a)

type tuple<a>
  Pair(a : a, b : a)
  Triple(a : a, b : a, c : a)

type seq<a>
  Empty // Nil is used for the empty list in Koka
  Unit(a : a)
  More(l : afew<a>, s : seq<tuple<a>>, r : afew<a>)

fun head(s : seq<a>) : <exn> a
  match s
    Unit(x) -> x
    More(One(x), _, _) -> x
    More(Two(x, _), _, _) -> x
    More(Three(x, _, _), _, _) -> x

fun cons(x : a, s : seq<a>) : seq<a>
  match s
    Empty -> Unit(x)
    Unit(y) -> More(One(x), Empty, One(y))
    More(One(y), q, u) -> More(Two(x, y), q, u)
    More(Two(y, z), q, u) -> More(Three(x, y, z), q, u)
    More(Three(y, z, w), q, u) -> More(Two(x, y), cons(Pair(z, w), q), u)

fun uncons(s : seq<a>) : <exn> (a, seq<a>)
  match s
    Unit(x) -> (x, Empty)
    More(Three(x, y, z), q, u) -> (x, More(Two(y, z), q, u))
    More(Two(x, y), q, u) -> (x, More(One(y), q, u))
    More(One(x), q, u) -> (x, more0(q, u))

// we inline chop and map1 for better reuse behaviour
fun more0(q : seq<tuple<a>>, u : afew<a>) : seq<a>
  match q
    Empty -> match u
      One(y) -> Unit(y)
      Two(y, z) -> More(One(y), Empty, One(z))
      Three(y, z, w) -> More(One(y), Empty, Two(z, w))
    Unit(p) -> match p
      Pair(x, y) -> More(Two(x, y), Empty, u)
      Triple(x, y, z) -> More(One(x), Unit(Pair(y,z)), u)
    More(One(p), q1, u1) -> match p
      Pair(x, y) -> More(Two(x, y), more0(q1, u1), u)
      Triple(x, y, z) -> More(One(x), More(One(Pair(y,z)), q1, u1), u)
    More(Two(p, y1), q1, u1) -> match p
      Pair(x, y) -> More(Two(x, y), More(One(y1), q1, u1), u)
      Triple(x, y, z) -> More(One(x), More(Two(Pair(y,z), y1), q1, u1), u)
    More(Three(p, y1, z1), q1, u1) -> match p
      Pair(x, y) -> More(Two(x, y), More(Two(y1, z1), q1, u1), u)
      Triple(x, y, z) -> More(One(x), More(Three(Pair(y,z), y1, z1), q1, u1), u)

fun snoc(s : seq<a>, x : a)  : seq<a>
  match s
    Empty -> Unit(x)
    Unit(y) -> More(One(y), Empty, One(x))
    More(u, q, One(y)) -> More(u, q, Two(y, x))
    More(u, q, Two(y, z)) -> More(u, q, Three(y, z, x))
    More(u, q, Three(y, z, w)) -> More(u, snoc(q, Pair(y, z)), Two(w, x))

fun to-list(u : afew<a>) : list<a>
  match u
    One(x) -> [x]
    Two(x,y) -> [x,y]
    Three(x,y,z) -> [x,y,z]

fun to-tuples(xs : list<a>) : list<tuple<a>>
  match xs
    Cons(x, Cons(y, Nil)) -> [Pair(x,y)]
    Cons(x, Cons(y, Cons(z, Cons(w, Nil)))) -> [Pair(x,y), Pair(z,w)]
    Cons(x, Cons(y, Cons(z, xs))) -> Cons(Triple(x,y,z), to-tuples(xs))
    _ -> [] // only if xs == Nil

fun append(q1 : seq<a>, q2 : seq<a>) : <div> seq<a>
   glue(q1, Nil, q2)

fun glue(q1 : seq<a>, xs : list<a>, q2 : seq<a>) : <div> seq<a>
  match(q1, q2)
    (Empty, _) -> xs.foldr(q2, cons)
    (_, Empty) -> xs.foldl(q1, snoc)
    (Unit(x), _) -> (Cons(x,xs)).foldr(q2, cons)
    (_, Unit(x)) -> (xs ++ [x]).foldl(q1, snoc)
    (More(u1, q1, v1), More(u2, q2, v2)) ->
      More(u1, glue(q1, to-tuples(to-list(v1) ++ xs ++ to-list(u2)), q2), v2)

fun iterate(s : seq<a>, n : int32) : <div,exn> seq<a>
  if n <= 0.int32 then s
  else
    val (x, s') = uncons(s)
    iterate(snoc(s', x), n - 1.int32)

fun build(n : int32, s : seq<int32>) : <div> seq<int32>
  if n <= 0.int32 then s else build(n - 1.int32, snoc(s, n))

fun test(n : int32)
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val s = build(n, Empty)
            acc + head(iterate(s, n * 3.int32))
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)