module ddouble1

public import std/num/ddouble

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}

val max-int = 9007199254740991.0
val mx = 123456789012345.0

public fun test-show() {
  check("show1","0.1000000000000000055511151231258"){ 0.1.ddouble.show-prec }
  check("show2","0.1"){ (one / ten).show-prec }
  val x1 = 5.123456.ddouble
  check("prec1a","5.123456000000000010174971976085"){ x1.show-prec() }
  check("prec1b","5.1235"){ x1.show-prec(5) }
  check("prec1c","5.1"){ x1.show-prec(2) }
  check("prec1d","5"){ x1.show-prec(1) }
  val x2 = 0.000123.ddouble
  check("prec2","0.0001230000000000000081983031474664"){ x2.show-prec() }
  check("prec2","0.00012300"){ x2.show-prec(5) }
  check("prec2","0.00012"){ x2.show-prec(2) }
}

public fun main() {
  check("add1","9007199254740992.0 vs. 9007199254740993") { (max-int + 2.0).show + " vs. " + (max-int.ddouble + ddouble(2.0)).show }
  check("pow1","1e+31") { pow(ddouble(10.0),31).show }
  check("mul1a","1.5241578753238668e+28 vs. 1.5241578753238669120562399025000e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show-prec(ddouble-max-prec) }
  check("mul1b","1.5241578753238668e+28 vs. 1.5241578753238669120562399025e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show-prec() }
  test-show()
  test-e()
  test-pi-machin()
  test-log2()
}

public fun test-e(){
  /* Use Taylor series
       e = 1 + 1 + 1/2! + 1/3! + 1/4! + ...
     To compute e.
  */
  check("e","approx-e: 2.718281828459045235360287471353"){ 
      val approx = expand-e()
      val delta  = (approx - dd-e).abs
      trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
      trace(" e: " + dd-e.show)
      ("approx-e: " + approx.show)
  }
}

fun expand-e( approx : ddouble = 2.0.ddouble, t0 : ddouble = one, n0 : ddouble = one ) : div ddouble  {
  if (t0 <= dd-epsilon) then approx else {
    val n = n0 + one
    val t = t0 / n
    expand-e( approx + t, t, n )
  }
}

public fun test-pi-machin() {
  /* Use the Machin's arctangent formula:
       pi / 4  =  4 arctan(1/5) - arctan(1/239)
     The arctangent is computed based on the Taylor series expansion
       arctan(x) = x - x^3 / 3 + x^5 / 5 - x^7 / 7 + ...
  */
  check("pi-machin","approx-pi: 3.141592653589793238462643383279") {
    val approx = 4.ddouble * ((4.ddouble * arctan(one / 5.ddouble )) - arctan( one / 239.ddouble ))
    val delta  = (approx - dd-pi).abs
    trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    trace(" pi: " + dd-pi.show )
    ("approx-pi: " + approx.show)
  }
}

public fun arctan( x : ddouble ) : div ddouble {
  expand-arctan(x,x.sqr)
}

fun expand-arctan( t : ddouble, xsqr : ddouble, n : ddouble = one, acc : ddouble = zero, pos : bool = True ) : div ddouble {
  if (t <= dd-epsilon) then acc else {
    val acc1 = if (pos) then acc + (t / n) else acc - (t / n)
    val n1 = n + 2.0.ddouble
    val t1 = t * xsqr
    expand-arctan( t1, xsqr, n1, acc1, !pos ) 
  }
}


public fun test-log2() {
  check("log2","approx-log2: 0.6931471805599453094172321214582") {
    val approx = log2()
    val delta  = (approx - dd-log2).abs
    trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    trace(" log2: " + dd-log2.show )
    ("approx-log2: " + approx.show)
  }
}

public fun log2() : div ddouble {
  /* Use the Taylor series
      -log(1-x) = x + x^2/2 + x^3/3 + x^4/4 + ...
     with x = 1/2 to get  log(1/2) = -log 2.
  */
  expand-log2(0.5.ddouble,0.5.ddouble)
}

fun expand-log2( acc : ddouble, t : ddouble, n : ddouble  = one ) : div ddouble {
  if (t <= dd-epsilon) then acc else {
    val n1 = n + one
    val t1 = t * 0.5.ddouble
    val acc1 = acc + (t1 / n1)
    expand-log2( acc1, t1, n1 )
  }
}