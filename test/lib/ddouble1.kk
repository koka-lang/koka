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
}