import std/num/float64

// `default/maximum` beside the `int` and `float64` versions: a call without a default still
// resolves to those, and of equal largest elements the first is returned
fun main()
  println([3,1,2].maximum)
  println([1.5,0.5].maximum)
  println(["b","c","a"].maximum(""))
  println(([] : list<string>).maximum("none"))
  println([(1,"x"),(2,"y"),(2,"z")].maximum((0,""), ?cmp = fn((a,_),(b,_)) a.cmp(b)).snd)
