module ddouble1

public import std/num/ddouble
public import std/num/double

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(8,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
  if (got != res && host() != "dotnet") then error("failed") else ()
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
  val y = x.show-sum(20).parse-ddouble.default(dd-nan)
  if (y==x) then () else {
      println("failed round-trip parse! (" + name + ")\n x : " + 
            x.show-exp + "\n     " + x.show-sum +  "\n y : " + y.show-exp + "\n     " + y.show-sum)
  }
  x
} 

fun check-prec( name : string, expect : ddouble, res : ddouble, eps : ddouble = ddouble-exp(1,-15) ) : io () {
  val pre = name.pad-right(8,' ') + ": "
  if (expect.finite?) then {  
    val delta  = (res - expect).abs
    if (delta < eps) then {
      println(pre + "ok: " + res.show + ", dt: " + delta.show(6)  )
    }
    else {
      val relerr = (delta / expect).abs
      val releps = if (expect.abs < ten) then eps else eps * 1000.ddouble
      if (relerr < releps) then {
        println(pre + "ok: " + res.show + ", derr: " + relerr.show(6)  )
      }
      else {
        println(pre + "FAILED:\n expect: " + expect.show + "\n gotten: " + res.show + "\n delta : " + delta.show + "\n error: " + relerr.show )
        error("failed " + name)
      }
    }
  }
  else {
    if (expect == res) then {
      println( pre + "ok: " + res.show )
    }
    else {
      println(pre + "FAILED:\n expect: " + expect.show + "\n gotten: " + res.show )
      error("failed " + name)
    }
  } 
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
  test-sincos()
  test-sin()
  test-atan()
  test-trig()
  test-atrig()
  
  check("add1","9.007199254740992e+15 vs. 9.007199254740993e+15") { (max-int + 2.0).show-exp(15) + " vs. " + (max-int.ddouble + ddouble(2.0)).show-exp }
  check("pow1","1e+31") { ddouble-exp(1,31).show }
  check("mul1a","1.5241578753238668e+28 vs. 1.524157875323866912056239902500e+28") { (mx * mx).show-exp(16) + " vs. " + mx.ddouble.sqr.show(dd-max-prec) }
  check("mul1b","1.5241578753238668e+28 vs. 1.5241578753238669120562399025e+28") { (mx * mx).show-exp(16) + " vs. " + mx.ddouble.sqr.show }
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
  //check("prec2","0.00012300"){ x2.show-prec(5) }
  //check("prec2","0.00012"){ x2.show-prec(2) }
  val x30 = "1e+30".parse-int.default(0)
  check("int1","1000000000000000000000000000000"){ x30.ddouble.show-fixed }
  check("int2", "999999999999999999999999999999"){ x30.dec.ddouble.show-fixed }
}


/*-------------------------------------------------------
  Iterate over interesting numbers
-------------------------------------------------------*/

public fun test-int10() {
  for(2,308) fun(i){
    val expect = "1e+" + i.show
    check("int" + i.show, expect) { ddouble-exp(1,i).show-exp }
  }
}

public fun test-int99() {
  for(2,30) fun(i) {
    val expect8 = "9." + repeat("9",i - 2) + "8e+" + i.dec.show
    check-rte("int" + i.show, expect8) { ddouble-exp(1,i).dec.dec }    
    val expect9 = "9." + repeat("9",i.dec) + "e+" + i.dec.show
    check-rte("int" + i.show, expect9) { ddouble-exp(1,i).dec }    
    val expect1 = "1." + repeat("0",i.dec) + "1e+" + i.show
    check-rte("int" + i.show, expect1) { ddouble-exp(1,i).inc }    
    val expect2 = "1." + repeat("0",i.dec) + "2e+" + i.show
    check-rte("int" + i.show, expect2) { ddouble-exp(1,i).inc.inc } 
    if (i < 30) {   
      val expect99 = "1" + repeat("9",i)// + "e+" + i.show
      check("int" + i.show, expect99) { (ddouble-exp(1,i) + ddouble-exp(1,i).dec).show-fixed() }        
    }
  }
}


public fun test-big() {
  val digits= dd-max-prec - 1
  val pi290 = dd-pi * ddouble-exp(1,290)
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
    list(-3,4).map( fun(i) { 
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
  check-prec("sin(" + x.show + ")", s1, s2, ddouble-exp(1,-31))
  check-prec("cos(" + x.show + ")", c1, c2, ddouble-exp(1,-31))
  
  // check not too much error with respect to the double computation
  check-prec( "dsin" + i.show, sin(x.double).ddouble, s1, ddouble-exp(1,-15) )
  check-prec( "dcos" + i.show, cos(x.double).ddouble, c1, ddouble-exp(1,-15) )
}

public fun test-sin() {
  val exact-sin : list<ddouble> =
    ["0.000099999999833333333416666666646825396828152557318973",
    "0.0009999998333333416666664682539710097001513147348086",
    "0.0099998333341666646825424382690997290389643853601692",
    "0.099833416646828152306814198410622026989915388017982",
    "0.84147098480789650665250232163029899962256306079837",
    "-0.54402111088936981340474766185137728168364301291622",
    "-0.50636564110975879365655761045978543206503272129066",
    "0.82687954053200256025588742910921814121272496784779",
    "-0.30561438888825214136091003523250697423185004386181"]
    .map(ddouble)
  exact-sin.foreach-indexed( fun(i : int, esin : ddouble) {
    val x = ddouble("0.0001") * ddouble-exp(1,i)
    val s = sin(x)
    check-prec( "esin" + i.show + ": sin(" + x.show  + "): ", esin, s, ddouble-exp(1,-31) * (if (x <= one) then one else 1000.ddouble) )
  })
  check( "sin-atan", "0.7071067811865475027519429562175" /* "0.7071067811865475244008443621049" */) { sin(atan(1.0).ddouble).show }
}                     


public fun test-atan() {
  val exact-atan : list<ddouble> =
   ["0.000099999999666666668666666652380952492063491154401162",
    "0.00099999966666686666652380963492054401162093455426801",
    "0.0099996666866652382063401162092795485613693525443766",
    "0.099668652491162027378446119878020590243278322504315",
    "0.78539816339744830961566084581987572104929234984378",
    "1.4711276743037345918528755717617308518553063771832",
    "1.5607966601082313810249815754304718935372153471432",
    "1.5697963271282297525647978820048308980869637651333",
    "1.5706963267952299525626550249873704896065212085332"]
   .map(ddouble)
  exact-atan.foreach-indexed( fun(i : int, eatan : ddouble) {
    val x = ddouble("0.0001") * ddouble-exp(1,i)
    val a = atan(x)
    check-prec( "eatan" + i.show + ": atan(" + x.show  + "): ", eatan, a, ddouble-exp(1,-31) * (if (x <= one) then one else 1000.ddouble) )
  })
}

val posnums   = [1.0e-12,1.0e-8,1.0e-6,0.1,0.25,0.5,0.75,0.9,0.99999999,1.0,1.00000001,1.1,1.5,2.0,2.5,5.0,10.0,100.0,1.0e6,1.0e8,1.0e12]
val posexotic = [0.5*sqrt(2.0),atan(1.0),sqrt(2.0)]
val exotic    = posexotic.map((~)).reverse + posexotic
val nums      = posnums.map((~)).reverse + [0.0] + posnums + exotic
val pinums    = nums.map( fun(d){ d*pi } ) + exotic
  
public fun test-trig() {
  test-double-fun( "tan", tan, tan, pinums )
  test-double-fun( "sin", sin, sin, pinums )
  test-double-fun( "cos", cos, cos, pinums ) 
  test-double-fun( "tanh", tanh, tanh, pinums ) 
  test-double-fun( "sinh", sinh, sinh, pinums ) 
  test-double-fun( "cosh", cosh, cosh, pinums ) 
}

public fun test-atrig() {
  test-double-fun( "atan", atan, atan, nums )
  test-double-fun( "asin", asin, asin, nums )
  test-double-fun( "acos", acos, acos, nums )
  test-double-fun( "atanh", atanh, atanh, nums )
  test-double-fun( "asinh", asinh, asinh, nums )
  test-double-fun( "acosh", acosh, acosh, nums )
}

public fun test-double-fun( name : string, fd : double -> double, fdd : ddouble -> ddouble, nums : list<double> ) : io () {
  nums.foreach-indexed( fun( i:int, x : double ) {
    val d = x.fd
    val dd = x.ddouble.fdd
    check-prec( name + "(" + x.show(3) + ")", d.ddouble, dd, 
               if (host()=="dotnet") then ddouble-exp(1,-4) // dotnet does not have good trigonometry :-(
                                     else ddouble-exp(1,-14))
  })
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

