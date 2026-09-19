import mpat-lib

// `cross` is inlined from mpat-lib's .kki; swapped scrutinees make
// `cross(South, North)` return True.
fun main()
  println( cross(South, North) )
  println( cross(North, South) )
  println( cross(East(1), West(2)) )
  println( cross(East(1), East(2)) )
