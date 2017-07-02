/*
  seconds of days: 9 223 372 035 908 035 200
    + 55808
 =  9 223 372 035 908 091 008
*/

import std/num/ddouble

fun main() {
  val i = 9223372035908091008
  val x : ddouble = i.ddouble
  val y = ddouble(9223372035908091000.0) + ddouble(8.0)
  val z = ddouble(9223372035908090880.0) + ddouble(128.0)
  println( x.show-sum(20) )
  println( y.show-sum )
  println( z.show-sum )

  println( i.show )
  println( x.show-fixed )
  println( y.int.show )
  println( z.int.show )
}