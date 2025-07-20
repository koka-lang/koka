// expand _ in application position, see issue #756

fun main()
  val fs = [int/(+), (-), (*), (/), (%)]

  fs.map( _(5, 3) ).println
  // fs.map(fn(f) f(5, 3)).println

  fs.map( 5._(3) ).println
  // fs.map(fn(f) 5.f(3)).println