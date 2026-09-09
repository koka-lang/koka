// std/data/int-map, int-set (int64) and int32-map, int32-set (int32):
// big-endian Patricia trees. Same operations at both widths; the int32
// variants exist for the JS backend where int64 lowers to BigInt.
import std/data/int-map
import std/data/int-set
import std/data/int32-map
import std/data/int32-set
import std/num/int64
import std/num/int32

fun test64()
  val m = int-map/empty().set(3.int64, "three").set(1.int64, "one").set(2.int64, "two")
  println(m.lookup(2.int64).default("?"))
  println(m.member(4.int64))
  println(m.set(2.int64, "TWO").lookup(2.int64).default("?"))
  println(m.remove(2.int64).size)
  println(m.list.map(fn((k, v)) k.show ++ ":" ++ v).join(","))
  val u = int-map/from-list([(10.int64, 1), (20.int64, 2)]).union(int-map/from-list([(20.int64, 22), (30.int64, 3)]))
  println(u.list.map(fn((k, v)) k.show ++ ":" ++ v.show).join(","))
  val s = int-set/from-list([5.int64, 1.int64, 3.int64])
  println(s.member(3.int64))
  println(s.remove(3.int64).list.map(show).join(","))
  println(s.intersection(int-set/from-list([3.int64, 5.int64])).list.map(show).join(","))
  println(int-set/from-list([1.int64]).is-subset-of(s))

fun test32()
  val m = int32-map/empty().set(3.int32, "three").set(1.int32, "one").set(2.int32, "two")
  println(m.lookup(2.int32).default("?"))
  println(m.member(4.int32))
  println(m.remove(2.int32).size)
  println(m.list.map(fn((k, v)) k.show ++ ":" ++ v).join(","))
  val s = int32-set/from-list([5.int32, 1.int32, 3.int32])
  println(s.member(3.int32))
  println(s.union(int32-set/from-list([7.int32])).size)

fun main()
  test64()
  test32()
  println("done")
