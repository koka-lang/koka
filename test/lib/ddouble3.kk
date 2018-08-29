/*
  from the posits paper: http://www.johngustafson.net/pdfs/BeatingFloatingPoint.pdf
*/

public import std/num/ddouble
public import std/num/double
public import std/num/decimal

fun dd-area(a : ddouble, b : ddouble, c : ddouble = b ) : ddouble {
  val s = (a + b + c)/2.ddouble
  sqrt(s*(s - a)*(s - b)*(s - c))
}

fun dd-areax(a : ddouble, b : ddouble, c : ddouble = b) : ddouble {
  val s = (a+(b+c))*(c - (a - b))*(c+(a - b))*(a+(b - c))
  sqrt(s)/4.ddouble
}

fun dd-area2(a : ddouble, b : ddouble, c : ddouble = b) : (ddouble,ddouble) {
  (dd-area(a,b,c),dd-areax(a,b,c))
}

fun d-area(a : double, b : double, c : double = b) : double {
  val s = (a + b + c)/2.0
  sqrt(s*(s - a)*(s - b)*(s - c))
}

fun d-areax(a : double, b : double, c : double = b) : double {
  val s = (a+(b+c))*(c - (a - b))*(c+(a - b))*(a+(b - c))
  sqrt(s)/4.0
}

fun d-area2(a : double, b : double, c : double = b) : (double,double) {
  (d-area(a,b,c),d-areax(a,b,c))
}

fun main() {
  val (x1,x2) = dd-area2(7.ddouble, 7.ddouble/2.ddouble + ldexp(3.ddouble,-111))
  println("exact           : 3.14784204874900425235885265494550774498...e-16")
  println("128-bit ddouble : " + x1.show(32))
  println("128-bit ddoublex: " + x2.show(32))
  println("128-bit posit   : 3.14784204874900425235885265494550774439e-16")
  println("128-bit ieee    : 3.63481490842332134725920516158057682788e-16")
  println("128-bit ieeex   : 3.14784204874900425235885265494550792210e-16")
  println("")
  val (dda,ddax) = dd-area2("99999.99996".ddouble, "99999.99994".ddouble, "0.00003".ddouble)
  val (da,dax)   =  d-area2(99999.99996, 99999.99994, 0.00003)
  println("area a exact   : 1.118033988190877853817061527883619442305255...")
  println("area a ddouble : " + dda.show)
  println("area a ddoublex: " + ddax.show)
  println("area a double  : " + da.show)
  println("area a doublex : " + dax.show)
  println("area 128 ieee  : 1.1180339881908778538170602531005730214932430451")
  println("area 128 ieee x: 1.1180339881908778538170615316768102579240155369")
  println("")
  val ddb  = dd-area("31622.77662".ddouble, "0.0155555".ddouble, "31622.77661".ddouble)
  val db   =  d-area(31622.77662, 0.0155555, 31622.77661)
  println("area b exact   : 245.9539999948024911172384469609351921552528...")
  println("area b ddouble : " + ddb.show)
  println("area b double  : " + db.show)
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
