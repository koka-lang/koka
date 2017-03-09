module ddouble1

public import std/num/ddouble

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(8,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
  if (got != res) then error("failed") else ()
}

fun check-rt( name : string, expect : string, tst : () -> io ddouble ) : io () {
  check(name,expect) {
    check-round-trip(name,tst()).show
  }    
}

fun check-rte( name : string, expect : string, tst : () -> io ddouble ) : io () {
  check(name,expect) {
    check-round-trip(name,tst()).show-exp
  }    
}

fun check-round-trip( name : string, x : ddouble ) : io ddouble {
  //val y = x.show(33).parse-ddouble.default(dd-nan)
  val y = x.show-sum.parse-ddouble.default(dd-nan)
  if (y==x) then () else {
      println("failed round-trip parse! (" + name + ")\n x : " + 
            x.show-exp + "\n     " + x.show-sum +  "\n y : " + y.show-exp + "\n     " + y.show-sum)
  }
  x
} 


public fun main() {
  test-big()
  test-int10()
  test-int99()
  test-show()
  test-e()
  test-pi-machin()
  test-pi-borwein()
  test-pi-salmanbrent()
  test-log2()
  test-exp()
  check("add1","9007199254740992 vs. 9007199254740993") { (max-int + 2.0).show + " vs. " + (max-int.ddouble + ddouble(2.0)).show }
  check("pow1","1e+31") { pow(ddouble(10.0),31).show }
  check("mul1a","1.5241578753238668e+28 vs. 1.524157875323866912056239902500e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show(dd-max-prec) }
  check("mul1b","1.5241578753238668e+28 vs. 1.5241578753238669120562399025e+28") { (mx * mx).show + " vs. " + mx.ddouble.sqr.show }
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
  val x30 = "1e30".parse-int.default(0)
  check("int1","1000000000000000000000000000000"){ x30.ddouble.show-fixed }
  check("int2", "999999999999999999999999999999"){ x30.dec.ddouble.show-fixed }
}


/*-------------------------------------------------------
  Iterate over interesting numbers
-------------------------------------------------------*/

public fun test-int10() {
  for(2,308) fun(i){
    val expect = "1e+" + i.show
    check("int" + i.show, expect) { ddouble/pow10(i).show-exp }
  }
}

public fun test-int99() {
  for(2,30) fun(i) {
    val expect8 = "9." + repeat("9",i - 2) + "8e+" + i.dec.show
    check-rte("int" + i.show, expect8) { ddouble/pow10(i).dec.dec }    
    val expect9 = "9." + repeat("9",i.dec) + "e+" + i.dec.show
    check-rte("int" + i.show, expect9) { ddouble/pow10(i).dec }    
    val expect1 = "1." + repeat("0",i.dec) + "1e+" + i.show
    check-rte("int" + i.show, expect1) { ddouble/pow10(i).inc }    
    val expect2 = "1." + repeat("0",i.dec) + "2e+" + i.show
    check-rte("int" + i.show, expect2) { ddouble/pow10(i).inc.inc } 
    if (i < 30) {   
      val expect99 = "1" + repeat("9",i)// + "e+" + i.show
      check("int" + i.show, expect99) { (ddouble/pow10(i) + ddouble/pow10(i).dec).show-fixed() }        
    }
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


/*-------------------------------------------------------
  Various formulas to approximate known numbers
-------------------------------------------------------*/

public fun test-e(){
  /* Use Taylor series
       e = 1 + 1 + 1/2! + 1/3! + 1/4! + ...
     To compute e.
  */
  check-rt("e",dd-e.show) {
      val approx = expand-e()
      val delta  = (approx - dd-e).abs
      val err    = delta / dd-epsilon    
      //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
      //trace(" e: " + dd-e.show)
      // (approx.show + ", err: " + err.show(3) + " ulp")
      approx
  }
}

fun expand-e( approx : ddouble = 2.0.ddouble, t0 : ddouble = one, n0 : ddouble = one ) : div ddouble  {
  if (t0 <= dd-epsilon) then approx else {
    val n = n0 + one
    val t = t0 / n
    expand-e( approx + t, t, n )
  }
}


public fun test-log2() {
  check-rt("log2",dd-log2.show) {
    val approx = log2()
    val delta  = (approx - dd-log2).abs
    val err    = delta / dd-epsilon    
    //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    //trace(" log2: " + dd-log2.show )
    //(approx.show + ", err: " + err.show(3) + " ulp")
    approx
  }
}

fun log2() : div ddouble {
  // Use the Taylor series
  //  -log(1-x) = x + x^2/2 + x^3/3 + x^4/4 + ...
  // with x = 1/2 to get  log(1/2) = -log 2.
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


// check exponent equalities:  
//   e^2 = exp(2)
//       = exp(-13/4) * exp(-9/4) * exp(-5/4) * exp(-1/4) *
//          exp(3/4) * exp(7/4) * exp(11/4) * exp(15/4)
public fun test-exp() {  
  val expect = (dd-e * dd-e).show

  check-rt("exp1",expect) { dd-e.sqr }
  check-rt("exp2",expect) { exp(2.ddouble) }
  check-rt("exp3",expect) {
    list(~3,4).map( fun(i) { 
      exp((4*i).dec.ddouble / 4.ddouble)
    }).foldl(one,(*))
  }
}

/* Check sin/cos equalities:
  sin(x) = sin(5x/7)cos(2x/7) + cos(5x/7)sin(2x/7)
  cos(x) = cos(5x/7)cos(2x/7) - sin(5x/7)sin(2x/7)
*/
public fun test-sincos() {
  [1,2,4,3,5,6,7,8,9,10,1000,10000000000]
    .foreach-indexed(fun(i,r) { 
      check-sincos(i, dd-twopi / r.ddouble) 
      check-sincos(i, ~dd-twopi / r.ddouble) 
    })
}

public fun check-sincos( i : int, x : ddouble ) {
  val x1 = (5.ddouble * x) / 7.ddouble
  val x2 = (2.ddouble * x) / 7.ddouble

  val s1 = sin(x)
  val c1 = cos(x)
  val s2 = sin(x1) * cos(x2) + cos(x1) * sin(x2)
  val c2 = cos(x1) * cos(x2) - sin(x1) * sin(x2)
  check("sin" + i.show, s1.show(30)) { s2.show(30) }
  check("cos" + i.show, c1.show(30)) { c2.show(30) }
  
  // check not too much error with respect to the double computation
  val e1 = (s1 - sin(x.double).ddouble).abs.double
  val e2 = (c1 - cos(x.double).ddouble).abs.double
  if (e1 > 3.0e-16) then error("sin does not match double: " + e1.show)
  if (e2 > 3.0e-16) then error("cos does not match double: " + e2.show)    
}


/*-------------------------------------------------------
  Various formulas to approximate pi
-------------------------------------------------------*/

//-----------------------------------------------
// Use the Machin's arctangent formula:
//    pi / 4  =  4 arctan(1/5) - arctan(1/239)
// The arctangent is computed based on the Taylor series expansion
//   arctan(x) = x - x^3 / 3 + x^5 / 5 - x^7 / 7 + ...
public fun test-pi-machin() {
  check-rt("pi-mach", dd-pi.show) {
    val approx = 4.ddouble * ((4.ddouble * arctan(one / 5.ddouble )) - arctan( one / 239.ddouble ))
    val delta  = (approx - dd-pi).abs
    val err    = delta / dd-epsilon
    //trace(" delta: " + delta.show + " = " + (delta / dd-epsilon).show(3) + " eps")
    //trace(" pi: " + dd-pi.show )
    // (approx.show + ", err: " + err.show(3) + " ulp")
    approx
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


// --------------------------------
// Borwein Quartic algorithm for pi
//
public fun test-pi-borwein() {
  check("borwein", dd-pi.show(30)) {
    val a = 6.ddouble - (4.ddouble * sqrt(2.ddouble))
    val y = sqrt(2.ddouble) - one
    val m = 2.ddouble
    val p = one / a
    val approx = expand-pi-borwein( a, y, m, p )
    approx.show(30)
  }
}

fun expand-pi-borwein( a : ddouble, y : ddouble, m : ddouble,
                       p : ddouble, i : int = 0 ) : div ddouble 
{
  val m1 = m * 4.ddouble
  val r  = nroot( one - y.sqr.sqr, 4 )
  val y1 = (one - r) / (one + r)
  val a1 = (a * sqr(sqr(one + y1))) - (m1*y1*(one + y1 + y1.sqr))

  val p1 = one / a1
  if (i >= 20 || ((p1 - p).abs < 4.ddouble * dd-epsilon))
   then p1
   else expand-pi-borwein(a1,y1,m1,p1,i.inc)
}


// --------------------------------
// Salman Brent approximation to pi
//
public fun test-pi-salmanbrent() {
  check-rt("pi-sb",dd-pi.show) {
    val a = one
    val b = sqrt(0.5.ddouble)
    val s = 0.5.ddouble
    val m = one
    val p = (2.0.ddouble * sqr(a)) / s
    val approx = expand-pi-sb(a,b,s,m,p)
    approx
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

