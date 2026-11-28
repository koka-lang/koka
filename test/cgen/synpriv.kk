import synpriv-lib

// the inferred type of `go` mentions synpriv-lib's private synonym `pairs`
noinline fun go(xs)
  swap-all(xs)

fun main()
  go([(1,2),(3,4)]).map(fn((a,b)) a + b).sum.println
