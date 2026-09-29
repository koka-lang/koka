import std/core/bslice

// `length` and `byte-at` on a slice, including offsets out of range
fun main()
  val s = "hello".bytes.slice.advance(1)
  println(s.length)
  println(s.byte-at(0))
  println(s.byte-at(3))
  println(s.byte-at(4))
  println(s.byte-at(-1))
  println(["ab".bytes, "cd".bytes].join.string)
