module ddouble1

public import std/num/ddouble

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(8,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}


public fun main() {
  test-big()
  test-show()
  test-e()
  test-pi-machin()
  test-pi-borwein()
  test-pi-salmanbrent()
  test-log2()
  test-exp()
  check("add1","9007199254740992 vs. 9007199254740993") { (max-int + 2.0).show + " vs. " + (max-int.ddouble + ddouble(2.0)).show }
  check("pow1","1e+31") { pow(ddouble(10.0),31).show }
  check("mul1a","1.5241578753238668e+28 vs. 1.524157875323866912056239902500e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show-prec(dd-max-prec) }
  check("mul1b","1.5241578753238668e+28 vs. 1.5241578753238669120562399025e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show-prec() }
  check("max","1.797693134862315807937289714053e+308"){ dd-max.show }
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

public fun test-e(){
  /* Use Taylor series
       e = 1 + 1 + 1/2! + 1/3! + 1/4! + ...
     To compute e.
  */
  check("e",dd-e.show + ", err: 0 ulp") {
      val approx = expand-e()
      val delta  = (approx - dd-e).abs
      val err    = delta / dd-epsilon    
      //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
      //trace(" e: " + dd-e.show)
      (approx.show + ", err: " + err.show(3) + " ulp")
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
  check("pi-mach", dd-pi.show + ", err: 2.00 ulp") {
    val approx = 4.ddouble * ((4.ddouble * arctan(one / 5.ddouble )) - arctan( one / 239.ddouble ))
    val delta  = (approx - dd-pi).abs
    val err    = delta / dd-epsilon
    //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    //trace(" pi: " + dd-pi.show )
    (approx.show + ", err: " + err.show(3) + " ulp")
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
  check("log2",dd-log2.show + ", err: 0.375 ulp") {
    val approx = log2()
    val delta  = (approx - dd-log2).abs
    val err    = delta / dd-epsilon    
    //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    //trace(" log2: " + dd-log2.show )
    (approx.show + ", err: " + err.show(3) + " ulp")
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


public fun test-big() {
  val digits= dd-max-prec - 1
  val pi290 = dd-pi * pow10(290)
  val s     = dd-pi.show-exp(digits)
  //trace( "s: " + s + ", len: " + s.count.show)
  list(0,17).foldl(pi290) fun(x:ddouble,i:int) {
    val expect = s + "e+" + (i + 290).show
    check("big" + i.show, expect) { x.show-exp(digits)}
    x*ten
  }
  list(0,17).foldl(~pi290) fun(x:ddouble,i:int) {
    val expect = "-" + s + "e+" + (i + 290).show
    check("small" + i.show, expect) { x.show-exp(digits)}
    x*ten
  }
  ()
}


public fun test-exp() {
  // check:  
  //   e^2 = exp(2)
  //       = exp(-13/4) * exp(-9/4) * exp(-5/4) * exp(-1/4) *
  //          exp(3/4) * exp(7/4) * exp(11/4) * exp(15/4)
  
  val expect = (dd-e * dd-e).show

  check("exp1",expect) { dd-e.sqr.show }
  check("exp2",expect) { exp(2.ddouble).show }
  check("exp3",expect) {
    list(~3,4).map( fun(i) { 
      exp((4*i).dec.ddouble / 4.ddouble)
    }).foldl(one,(*)).show
  }
}

public fun test-pi-borwein() {
  check("borwein", dd-pi.show-exp(30)) {
    val a = 6.ddouble - (4.ddouble * sqrt(2.ddouble))
    val y = sqrt(2.ddouble) - one
    val m = 2.ddouble
    val p = one / a
    val approx = expand-pi( a, y, m, p )
    approx.show-exp(30)
  }
}

fun expand-pi( a : ddouble, y : ddouble, m : ddouble,
               p : ddouble, i : int = 0 ) : div ddouble 
{
  val m1 = m * 4.ddouble
  val r  = nroot( one - y.sqr.sqr, 4 )
  val y1 = (one - r) / (one + r)
  val a1 = (a * sqr(sqr(one + y1))) - (m1*y1*(one + y1 + y1.sqr))

  val p1 = one / a1
  if (i >= 20 || ((p1 - p).abs < 4.ddouble * dd-epsilon))
   then p1
   else expand-pi(a1,y1,m1,p1,i.inc)
}

public fun test-pi-salmanbrent() {
  check("pi-sb",dd-pi.show) {
    val a = one
    val b = sqrt(0.5.ddouble)
    val s = 0.5.ddouble
    val m = one
    val p = (2.0.ddouble * sqr(a)) / s
    val approx = expand-pi-sb(a,b,s,m,p)
    approx.show
  }
}

public fun showx( x : ddouble ) : string {
  x.show-exp(dd-max-prec+1)
}

fun expand-pi-sb( a : ddouble, b : ddouble, s : ddouble,
                  m : ddouble, p : ddouble, i : int = 0 ) : div ddouble 
{
  val m1 = m * 2.0.ddouble
  val a1 = 0.5.ddouble * (a + b)
  val b1 = a * b
  val s1 = s - (m1 * (a1.sqr - b1))
  val b2 = sqrt(b1)
  //trace(" a: " + a.showx + ", b: " + b.showx)
  //trace(" b2: " + b2.showx + ", b1: " + b1.showx )
  val p1 = (2.0.ddouble * a1.sqr) / s1
  //trace("i: " + i.show + ": p: " + p1.show-exp(dd-max-prec+1))
  if (i >= 20 || ((p1 - p).abs <= 64.ddouble*dd-epsilon))
   then p1
   else expand-pi-sb(a1,b2,s1,m1,p1,i.inc)
}

