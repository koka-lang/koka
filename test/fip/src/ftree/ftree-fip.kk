// Adapted from "Finger Trees Explained Anew, and Slightly Simplified (Functional Pearl)", Claessen
import std/num/int32
import std/os/env

ref type pad
  Pad

type reuse3
  Reuse3(a : pad, b : pad, c : pad)

type afew<a>
  One(a : a, b : pad, c : pad)
  Two(a : a, b : a, c : pad)
  Three(a : a, b : a, c : a)

type tuple<a>
  Pair(a : a, b : a, c : pad)
  Triple(a : a, b : a, c : a)

type seq<a>
  Empty 
  Unit(a : a, b : pad, c : pad)
  More0(l : a, s : seq<tuple<a>>, r : afew<a>)
  More(l : tuple<a>, s : seq<tuple<a>>, r : afew<a>)

type buffer
  BNil
  BCons(next : buffer, b : pad, c : pad)

value type bseq<a> 
  BSeq(s : seq<a>, q : buffer)

// Isomorphic to (,,,) but unboxed
value type tuple4<a,b,c,d> 
  Tuple4(fst:a,snd:b,thd:c,field4:d) 

fun bhead(^bs : bseq<a>) : exn a
  match bs 
    BSeq(s, _) -> head(s)

fun head(^s : seq<a>) : exn a
  match s
    Unit(x) -> x
    More0(x, _, _) -> x
    More(Pair(x, _, _), _, _) -> x
    More(Triple(x, _, _), _, _) -> x

fip fun bcons(x : a, u3 : reuse3, bs : bseq<a>) : exn bseq<a>
  val BSeq(s, b) = bs
  val (s', b') = cons(x, u3, s, b)
  BSeq(s', b')

fip fun cons(x : a, u3 : reuse3, s : seq<a>, b : buffer) : exn (seq<a>, buffer)
  match s
    Empty                     -> (Unit(x, Pad, Pad), b)
    Unit(y, _, _)             -> (More0(x, Empty, One(y, Pad, Pad)), b)
    More0(y, q, u)            -> (More(Pair(x, y, Pad), q, u), b)
    More(Pair(y, z, _), q, u) ->
      (More(Triple(x, y, z), q, u), BCons(b, Pad, Pad))
    More(Triple(y, z, w), q, u) ->
      val BCons(b', _, _) = b
      val (q', b'') = cons(Pair(z, w, Pad), u3, q, b')
      (More(Pair(x, y, Pad), q', u), b'')

fip fun buncons(bs : bseq<a>) : exn (a, reuse3, bseq<a>)
  val BSeq(s, b) = bs
  val Tuple4(x, u3, s', b') = uncons(s, b)
  (x, u3, BSeq(s', b'))

fip fun uncons(s : seq<a>, b : buffer) : exn tuple4<a, reuse3, seq<a>, buffer>
  match s
    Unit(x, _, _) ->
      Tuple4(x, Reuse3(Pad,Pad,Pad), Empty, b)
    More(Triple(x, y, z), q, u) ->
      val BCons(b', _, _) = b
      Tuple4(x, Reuse3(Pad,Pad,Pad), More(Pair(y, z, Pad), q, u), b')
    More(Pair(x, y, _), q, u) ->
      Tuple4(x, Reuse3(Pad,Pad,Pad), More0(y, q, u), b)
    More0(x, q, u) ->
      val (q', b') = more0(q, u, b)
      Tuple4(x, Reuse3(Pad,Pad,Pad), q', b')

fip fun more0(q : seq<tuple<a>>, u : afew<a>, b : buffer) : exn (seq<a>, buffer)
  match q
    Empty ->
      match u
        One(x, y, z) -> (Unit(x, y, z), b)
        Two(y, z, _) ->
          val BCons(b', _, _) = b
          (More0(y, Empty, One(z, Pad, Pad)), b')
        Three(y, z, w) ->
          val BCons(b', _, _) = b
          (More0(y, Empty, Two(z, w, Pad)), b')
    Unit(p, _, _) ->
      match p
        Pair(x, y, _)   -> (More(Pair(x, y, Pad), Empty, u), b)
        Triple(x, y, z) ->
          val BCons(b', _, _) = b
          (More0(x, Unit(Pair(y,z,Pad),Pad,Pad), u), b')
    More0(p, q1, u1) ->
      match p
        Pair(x, y) ->
          val (q1', b') = more0(q1, u1, b)
          (More(Pair(x, y, Pad), q1', u), b')
        Triple(x, y, z) ->
          val BCons(b', _, _) = b
          (More0(x, More0(Pair(y,z,Pad), q1, u1), u), b')
    More(Pair(p, y1), q1, u1) ->
      match p
        Pair(x, y)      -> (More(Pair(x, y, Pad), More0(y1, q1, u1), u), b)
        Triple(x, y, z) ->
          val BCons(b', _, _) = b
          (More0(x, More(Pair(Pair(y,z,Pad), y1, Pad), q1, u1), u), b')
    More(Triple(p, y1, z1), q1, u1) ->
      val BCons(b', _, _) = b
      match p
        Pair(x, y) ->
          (More(Pair(x, y, Pad), More(Pair(y1, z1, Pad), q1, u1), u), b')
        Triple(x, y, z) ->
          (More0(x, More(Triple(Pair(y,z,Pad), y1, z1), q1, u1), u), b')

fip fun bsnoc(bs : bseq<a>, u3 : reuse3, x : a) : exn bseq<a>
  val BSeq(s, b) = bs
  val (s', b') = snoc(s, b, u3, x)
  BSeq(s', b')

fip fun snoc(s : seq<a>, b : buffer, u3 : reuse3, x : a) : exn (seq<a>, buffer)
  match s
    Empty                     -> (Unit(x, Pad, Pad), b)
    Unit(y, _, _)             -> (More0(y, Empty, One(x, Pad, Pad)), b)
    More0(u, q, One(y, _, _)) -> (More0(u, q, Two(y, x, Pad)), BCons(b, Pad, Pad))
    More (u, q, One(y, _, _)) -> (More (u, q, Two(y, x, Pad)), BCons(b, Pad, Pad))
    More0(u, q, Two(y, z, _)) -> (More0(u, q, Three(y, z, x)), BCons(b, Pad, Pad))
    More (u, q, Two(y, z, _)) -> (More (u, q, Three(y, z, x)), BCons(b, Pad, Pad))
    More0(u, q, Three(y, z, w)) ->
      val BCons(b', _, _) = b
      val (q', b'') = snoc(q, b', u3, Pair(y, z, Pad))
      (More0(u, q', Two(w, x, Pad)), b'')
    More(u, q, Three(y, z, w)) ->
      val BCons(b', _, _) = b
      val (q', b'') = snoc(q, b', u3, Pair(y, z, Pad))
      (More(u, q', Two(w, x, Pad)), b'')

// append

type list3<a>
  Cons3(x : a, xx : list3<a>, c : pad)
  Nil3

fip fun reverse3(xs : list3<a>) : list3<a>
  reverse-append3( xs, Nil3 )

fip fun reverse-acc(acc : list3<a>, ys : list3<a> ) : list3<a>
  match ys
    Cons3(x,xx,pad) -> reverse-acc(Cons3(x,acc,pad),xx)
    _          -> acc

fip fun reverse-append3( xs : list3<a>, tl : list3<a> ) : list3<a>
  reverse-acc(tl,xs)

fip fun (++)(xs : list3<a>, ys : list3<a> ) : list3<a>
  append3(xs, ys)

fip fun append3(xs : list3<a>, ys : list3<a> ) : list3<a>
  match xs
    Cons3(x,xx,pad) -> Cons3(x,append3(xx,ys),pad)
    Nil3 -> ys

fip fun foldl3(xs,z1,z2,^f)
  match xs
    Cons3(x,xx) ->
      val (z1', z2') = f(z1,z2,Reuse3(Pad,Pad,Pad),x)
      foldl3(xx,z1',z2',f)
    Nil3        -> (z1,z2)

// foldl3 specialized to the `flip` function
fip fun foldl3_flipped(xs,z1,z2,^f)
  match xs
    Cons3(x,xx) ->
      val (z1', z2') = f(x,Reuse3(Pad,Pad,Pad),z1,z2)
      foldl3_flipped(xx,z1',z2',f)
    Nil3        -> (z1,z2)

fip fun foldr3(xs,z1,z2,^f)
  xs.reverse3.foldl3_flipped(z1,z2,f)

fip fun (++)( xs : buffer, ys : buffer ) : buffer
  append-buffers(xs, ys)

fip fun append-buffers(b1 : buffer, b2 : buffer) : buffer
  match b1
    BNil -> b2
    BCons(b', _, _) -> BCons(append-buffers(b', b2), Pad, Pad)

fip fun afew-to-list(u : afew<a>, b : buffer) : exn (list3<a>, buffer)
  match u
    One(x) -> (Cons3(x, Nil3, Pad), b)
    Two(x,y) ->
      match b
        BCons(b', _, _) -> (Cons3(x, Cons3(y, Nil3, Pad), Pad), b')
    Three(x,y,z) ->
      match b
        BCons(BCons(b', _, _), _, _) -> 
          (Cons3(x, Cons3(y, Cons3(z, Nil3, Pad), Pad), Pad), b')

fip fun tuple-to-list(u : tuple<a>, b : buffer) : exn (list3<a>, buffer)
  match u
    Pair(x,y) ->
     match b
        BCons(b', _, _) -> (Cons3(x, Cons3(y, Nil3, Pad), Pad), b')
    Triple(x,y,z) ->
      match b
        BCons(BCons(b', _, _), _, _) -> 
          (Cons3(x, Cons3(y, Cons3(z, Nil3, Pad), Pad), Pad), b')

fip fun to-tuples(xs : list3<a>, b : buffer) : (list3<tuple<a>>, buffer)
  match xs
    Cons3(x, Cons3(y, Nil3)) ->
      (Cons3(Pair(x,y,Pad), Nil3, Pad), b)
    Cons3(x, Cons3(y, Cons3(z, Cons3(w, Nil3)))) ->
      (Cons3(Pair(x,y,Pad), Cons3(Pair(z,w,Pad),Nil3,Pad), Pad), b)
    Cons3(x, Cons3(y, Cons3(z, xs))) ->
      val (xs', b') = to-tuples(xs, b)
      (Cons3(Triple(x,y,z), xs', Pad), BCons(b', Pad, Pad))
    _ -> (Nil3, b) // only if xs == Nil3

fip fun append(q1 : bseq<a>, q2 : bseq<a>) : pure bseq<a>
  match (q1, q2)
    (BSeq(q1, b1), BSeq(q2, b2)) ->
      val (q, b) = glue(q1, b1, Nil3, BNil, q2, b2)
      BSeq(q, b)

fip fun glue(q1 : seq<a>, b1 : buffer, xs : list3<a>, bs0 : buffer, q2 : seq<a>, b2 : buffer) : pure (seq<a>, buffer)
  match(q1, q2)
    (Empty, q2) -> xs.foldr3(q2, (bs0 ++ b1 ++ b2), cons)
    (q1, Empty) -> xs.foldl3(q1, (bs0 ++ b2 ++ b1), snoc)
    (Unit(x,_,_), q2) -> (Cons3(x,xs,Pad)).foldr3(q2, (bs0 ++ b1 ++ b2), cons)
    (q1, Unit(x,_,_)) -> append3(xs,Cons3(x,Nil3,Pad)).foldl3(q1, (bs0 ++ b2 ++ b1), snoc)
    (More(u1, q1, v1), More(u2, q2, v2)) ->
      val (v1', bs1) = afew-to-list(v1, BCons(bs0, Pad, Pad))
      val (u2', bs2) = tuple-to-list(u2, bs1)
      val (ts, bs3) = to-tuples(v1' ++ xs ++ u2', bs2)
      val (q, b) = glue(q1, b1, ts, bs3, q2, b2)
      (More(u1, q, v2), b)
    (More0(u1, q1, v1), More(u2, q2, v2)) ->
      val (v1', bs1) = afew-to-list(v1, BCons(bs0, Pad, Pad))
      val (u2', bs2) = tuple-to-list(u2, bs1)
      val (ts, bs3) = to-tuples(v1' ++ xs ++ u2', bs2)
      val (q, b) = glue(q1, b1, ts, bs3, q2, b2)
      (More0(u1, q, v2), b)
    (More(u1, q1, v1), More0(u2, q2, v2)) ->
      val (v1', bs1) = afew-to-list(v1, bs0)
      val (u2', bs2) = (Cons3(u2, Nil3, Pad), bs1)
      val (ts, bs3) = to-tuples(v1' ++ xs ++ u2', bs2)
      val (q, b) = glue(q1, b1, ts, bs3, q2, b2)
      (More(u1, q, v2), b)
    (More0(u1, q1, v1), More0(u2, q2, v2)) ->
      val (v1', bs1) = afew-to-list(v1, bs0)
      val (u2', bs2) = (Cons3(u2, Nil3, Pad), bs1)
      val (ts, bs3) = to-tuples(v1' ++ xs ++ u2', bs2)
      val (q, b) = glue(q1, b1, ts, bs3, q2, b2)
      (More0(u1, q, v2), b)

// benchmark

fun iterate(s : bseq<a>, n : int32) : <div,exn> bseq<a>
  if n <= 0.int32 then s
  else
    val (x, u3, s') = buncons(s)
    iterate(bsnoc(s', u3, x), n - 1.int32)

fun build(n : int32, s : bseq<int32>) : <div,exn> bseq<int32>
  if n <= 0.int32 then s else build(n - 1.int32, bsnoc(s, Reuse3(Pad,Pad,Pad), n))

fun test(n : int32)
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val s = build(n, BSeq(Empty, BNil))
            acc + bhead(iterate(s, n * 3.int32))
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)
