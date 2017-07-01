/*
  from the posits paper: http://www.johngustafson.net/pdfs/BeatingFloatingPoint.pdf
*/

import std/num/ddouble

fun main() {
  val a = ddouble(7)
  val b = ddouble(7)/ddouble(2) + ddouble(3)*pow2(-111)
  val c = b
  val s = (a + b + c)/ddouble(2)
  val x = sqrt(s*(s - a)*(s - b)*(s - c))
  println("result       : " + x.show(32))
  println("exact        : 3.14784204874900425235885265494550774498...e-16")
  println("128-bit posit: 3.14784204874900425235885265494550774439e-16")
  println("128-bit ieee : 3.63481490842332134725920516158057682788e-16")
}