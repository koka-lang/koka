fun test-many()
  var i := 0
  for(1,10000000) fn(j)
    i := i + 1
  println(i)


fun main()
  test-many()
