/*
  from the posits paper: http://www.johngustafson.net/pdfs/BeatingFloatingPoint.pdf
*/

import std/num/ddouble
import std/num/double

fun main() {
  val a = ddouble(7)
  val b = ddouble(7)/ddouble(2) + ddouble(3)*pow2(-111)
  val c = b
  val s = (a + b + c)/ddouble(2)
  val x = sqrt(s*(s - a)*(s - b)*(s - c))
  println("exact          : 3.14784204874900425235885265494550774498...e-16")
  println("128-bit ddouble: " + x.show(32))
  println("128-bit posit  : 3.14784204874900425235885265494550774439e-16")
  println("128-bit ieee   : 3.63481490842332134725920516158057682788e-16")
  println("")
  val ddx1 = 27.ddouble / 10.ddouble - dd-e
  val ddx2 = dd-pi - (sqrt(2.ddouble) + sqrt(3.ddouble))
  val ddx3 = 67.ddouble / 16.ddouble
  val ddx  = pow( ddx1 / ddx2, ddx3)
  println("exact X        : 302.882719655469549250146446201116105977446...")
  println("ddouble X      : " + ddx.show(32))
  val dx1 = 27.0 / 10.0 - dbl-e
  val dx2 = pi - (sqrt(2.0) + sqrt(3.0))
  val dx3 = 67.0 / 16.0
  val dx  = pow( dx1 / dx2, dx3)
  println("double X       : " + dx.show())
  println("32-bit ieee    : 302.912...")
}