fun count(start : int, end : int) : console ()
  if start == end then
    return ()
  
  start.println
  count(unsafe-decreasing(start - 1), end)

fun main()
  count(10, 0)