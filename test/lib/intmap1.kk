// Patricia tree maps and sets: std/num/i64map, i64set, i32map, i32set (big-endian)
// and std/data/imap, iset (unbounded `int` keys, little-endian). Same operations for all three.
// `union` is right-biased: for a key in both maps, the value from the second wins.
import std/num/i64map
import std/num/i64set
import std/num/i32map
import std/num/i32set
import std/data/imap
import std/data/iset
import std/num/int64
import std/num/int32

fun test64()
  val m = i64map/empty().insert(3.int64, "three").insert(1.int64, "one").insert(2.int64, "two")
  println(m.lookup(2.int64).default("?"))
  println(m.member(4.int64))
  println(m.insert(2.int64, "TWO").lookup(2.int64).default("?"))
  println(m.remove(2.int64).size)
  println(m.list.map(fn((k, v)) k.show ++ ":" ++ v).join(","))
  val u = i64map/from-list([(10.int64, 1), (20.int64, 2)]).union(i64map/from-list([(20.int64, 22), (30.int64, 3)]))
  println(u.list.map(fn((k, v)) k.show ++ ":" ++ v.show).join(","))
  val s = i64set/from-list([5.int64, 1.int64, 3.int64])
  println(s.member(3.int64))
  println(s.remove(3.int64).list.map(show).join(","))
  println(s.intersection(i64set/from-list([3.int64, 5.int64])).list.map(show).join(","))
  println(i64set/from-list([1.int64]).is-subset-of(s))

fun test32()
  val m = i32map/empty().insert(3.int32, "three").insert(1.int32, "one").insert(2.int32, "two")
  println(m.lookup(2.int32).default("?"))
  println(m.member(4.int32))
  println(m.remove(2.int32).size)
  println(m.list.map(fn((k, v)) k.show ++ ":" ++ v).join(","))
  val s = i32set/from-list([5.int32, 1.int32, 3.int32])
  println(s.member(3.int32))
  println(s.union(i32set/from-list([7.int32])).size)

fun test-int()
  val big = 0x1_0000_0000_0000_0000
  val m = imap/empty().insert(big, "big").insert(negate(big), "-big").insert(1, "one")
  println(m.lookup(big).default("?"))
  println(m.lookup(big + 1).default("?"))
  println(m.remove(big).size)
  val s = iset/from-list([big, 1, negate(big)])
  println(s.member(negate(big)))
  println(s.intersection(iset/from-list([big, 2])).list.map(show).join(","))

fun main()
  test64()
  test32()
  test-int()
  println("done")
