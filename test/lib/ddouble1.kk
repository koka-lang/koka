module ddouble1

pub import std/num/ddouble
pub import std/num/float64

pub fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst().replace-all("e+0","e+")
  println(name.pad-right(8,' ') ++ ": "
    ++ (if (got == res) then "ok: " ++ res
                       else "FAILED!:\n expect: " ++ res ++ "\n gotten: " ++ got ++ "\n"))
  if (got != res) then throw("failed") else ()
}

pub fun check-rt( name : string, expect : string, tst : () -> io ddouble ) : io () {
  check(name,expect) {
    check-round-trip(name,tst()).show
  }
}

pub fun check-rte( name : string, expect : string, tst : () -> io ddouble ) : io () {
  check(name,expect) {
    check-round-trip(name,tst()).show-exp
  }
}

pub fun check-round-trip( name : string, x : ddouble ) : io ddouble {
  //val y = x.show(33).parse-ddouble.default(dd-nan)
  val y = x.show-hex.parse-ddouble.default(dd-nan)
  if (y==x) then () else {
      println("failed round-trip parse! (" ++ name ++ ")\n x : " ++
            x.show-exp ++ "\n     " ++ x.show-hex ++  "\n y : " ++ y.show-exp ++ "\n     " ++ y.show-hex)
  }
  x
}

pub fun check-prec( name : string, expect : ddouble, res : ddouble, eps : ddouble = ddouble-exp(1,-15) ) : io () {
  val pre = name.pad-right(8,' ') ++ ": "
  if (expect.is-finite) then {
    val delta  = (res - expect).abs
    if (delta < eps) then {
      println(pre ++ "ok: " ++ res.show(-30) /* ++ ", dt: " ++ delta.show(6) */ )
    }
    else {
      val relerr = (delta / expect).abs
      val releps = if (expect.abs < ten) then eps else eps * 1000.ddouble
      if (relerr < releps) then {
        println(pre ++ "ok: " ++ res.show /* ++ ", derr: " ++ relerr.show(6) */ )
      }
      else {
        println(pre ++ "FAILED:\n expect: " ++ expect.show ++ "\n gotten: " ++ res.show ++ "\n delta : " ++ delta.show ++ "\n error: " ++ relerr.show )
        //error("failed " ++ name)
      }
    }
  }
  else {
    if (expect == res || (expect.is-nan && res.is-nan)) then {
      println( pre ++ "ok: " ++ res.show )
    }
    else {
      println(pre ++ "FAILED:\n expect: " ++ expect.show ++ "\n gotten: " ++ res.show )
      throw("failed " ++ name)
    }
  }
}


pub fun main() {
  test-big()
  test-int10()
  test-int99()
  test-show()

  test-sin()
  test-atan()
  test-trig()
  test-atrig()

  check("add1","9.007199254740992e+15 vs. 9.007199254740993e+15") { (max-int + 2.0).show-exp(15) ++ " vs. " ++ (max-int.ddouble + ddouble(2.0)).show-exp }
  check("pow1","1e+31") { ddouble-exp(1,31).show }
  check("mul1a","1.5241578753238668e+28 vs. 1.5241578753238669120562399025000e+28") { (mx * mx).show-exp(16) ++ " vs. " ++ mx.ddouble.sqr.show-exp(dd-max-prec) }
  check("mul1b","1.5241578753238668e+28 vs. 1.5241578753238669120562399025e+28") { (mx * mx).show-exp(16) ++ " vs. " ++ mx.ddouble.sqr.show-exp }
  check("max","1.797693134862315807937289714053e+308"){ dd-max.show(30) }
}

val max-int = 9007199254740991.0
val mx = 123456789012345.0

pub fun test-show() {
  check("show1","0.1000000000000000055511151231258"){ 0.1.ddouble.show }
  check("show2","0.1"){ (one / ten).show }
  val x1 = 5.123456.ddouble
  check("prec1a","5.1234560000000000101749719760846"){ x1.show() }
  check("prec1b","5.1235"){ x1.show(4) }
  check("prec1c","5.1"){ x1.show(1) }
  check("prec1d","5"){ x1.show(0) }
  val x2 = 0.000123.ddouble
  check("prec2","0.0001230000000000000081983031475"){ x2.show-fixed() }
  check("prec2","0.00012300"){ x2.show-fixed(8) }
  check("prec2","0.00012"){ x2.show-fixed(5) }
  val x30 = "1e+30".parse-int.default(0)
  check("int1","1000000000000000000000000000000"){ x30.ddouble.show-fixed }
  check("int2", "999999999999999999999999999999"){ x30.dec.ddouble.show-fixed }
}


/*-------------------------------------------------------
  Iterate over interesting numbers
-------------------------------------------------------*/

pub fun test-int10() {
  for(2,308) fn(i){
    val expect = "1e+" ++ i.show
    check("int" ++ i.show, expect) { ddouble-exp(1,i).show-exp(-28) }
  }
}

pub fun test-int99() {
  for(2,30) fn(i) {
    val expect8 = "9." ++ repeat("9",i - 2) ++ "8e+" ++ i.dec.show
    check-rte("int" ++ i.show, expect8) { ddouble-exp(1,i).dec.dec }
    val expect9 = "9." ++ repeat("9",i.dec) ++ "e+" ++ i.dec.show
    check-rte("int" ++ i.show, expect9) { ddouble-exp(1,i).dec }
    val expect1 = "1." ++ repeat("0",i.dec) ++ "1e+" ++ i.show
    check-rte("int" ++ i.show, expect1) { ddouble-exp(1,i).inc }
    val expect2 = "1." ++ repeat("0",i.dec) ++ "2e+" ++ i.show
    check-rte("int" ++ i.show, expect2) { ddouble-exp(1,i).inc.inc }
    if (i < 30) then {
      val expect99 = "1" ++ repeat("9",i)// ++ "e+" ++ i.show
      check("int" ++ i.show, expect99) { (ddouble-exp(1,i) + ddouble-exp(1,i).dec).show-fixed(-9) }
    }
  }
}


pub fun test-big() {
  val digits= dd-max-prec - 3
  val pi290 = dd-pi * ddouble-exp(1,290)
  val s     = dd-pi.show-exp(digits)
  //trace( "s: " ++ s ++ ", len: " ++ s.count.show)
  list(0,17).foldl(pi290) fn(x:ddouble,i:int) {
    val expect = s ++ "e+" ++ (i + 290).show
    check("big" ++ i.show, expect) { x.show-exp(digits)}
    x*ten
  }
  list(0,17).foldl(~pi290) fn(x:ddouble,i:int) {
    val expect = "-" ++ s ++ "e+" ++ (i + 290).show
    check("small" ++ i.show, expect) { x.show-exp(digits)}
    x*ten
  }
  ()
}



pub fun test-sin() {
  val exact-sin : list<ddouble> = [
    "0.000099999999833333333416666666646825396828152557318973",
    "0.0009999998333333416666664682539710097001513147348086",
    "0.0099998333341666646825424382690997290389643853601692",
    "0.099833416646828152306814198410622026989915388017982",
    "0.84147098480789650665250232163029899962256306079837",
    "-0.54402111088936981340474766185137728168364301291622",
    "-0.50636564110975879365655761045978543206503272129066",
    "0.82687954053200256025588742910921814121272496784779",
    "-0.30561438888825214136091003523250697423185004386181"]
    .map(ddouble)
  exact-sin.foreach-indexed( fn(i : int, esin : ddouble) {
    val x = ddouble("0.0001") * ddouble-exp(1,i)
    val s = sin(x)
    check-prec( "esin" ++ i.show ++ ": sin(" ++ x.show  ++ "): ", esin, s, ddouble-exp(1,-30) * (if (x <= one) then one else 1000.ddouble) )
  })
  // check( "sin-atan", "0.7071067811865475027519429562175" /* "0.7071067811865475244008443621049" */) { sin(atan(1.0).ddouble).show }
}


pub fun test-atan() {
  val exact-atan : list<ddouble> = [
    "0.000099999999666666668666666652380952492063491154401162",
    "0.00099999966666686666652380963492054401162093455426801",
    "0.0099996666866652382063401162092795485613693525443766",
    "0.099668652491162027378446119878020590243278322504315",
    "0.78539816339744830961566084581987572104929234984378",
    "1.4711276743037345918528755717617308518553063771832",
    "1.5607966601082313810249815754304718935372153471432",
    "1.5697963271282297525647978820048308980869637651333",
    "1.5706963267952299525626550249873704896065212085332"]
   .map(ddouble)
  exact-atan.foreach-indexed( fn(i : int, eatan : ddouble) {
    val x = ddouble("0.0001") * ddouble-exp(1,i)
    val a = atan(x)
    check-prec( "eatan" ++ i.show ++ ": atan(" ++ x.show  ++ "): ", eatan, a, ddouble-exp(1,-31) * (if (x <= one) then one else 1000.ddouble) )
  })
}

val posnums   = [1.0e-12,1.0e-8,1.0e-6,0.1,0.25,0.5,0.75,0.9,0.99999999,1.0,1.00000001,1.1,1.5,2.0,2.5,5.0,10.0,100.0,1.0e6,1.0e8,1.0e12]
val posexotic = [0.5*sqrt(2.0),atan(1.0),sqrt(2.0)]
val exotic    = posexotic.map((~)).reverse ++ posexotic
val nums      = posnums.map((~)).reverse ++ [0.0] ++ posnums ++ exotic
val pinums    = nums.map( fn(d){ d*pi } ) ++ exotic

pub fun test-trig() {
  test-float64-fn( "tan", tan, tan, pinums )
  test-float64-fn( "sin", sin, sin, pinums )
  test-float64-fn( "cos", cos, cos, pinums )
  test-float64-fn( "tanh", tanh, tanh, pinums )
  test-float64-fn( "sinh", sinh, sinh, pinums )
  test-float64-fn( "cosh", cosh, cosh, pinums )
}

pub fun test-atrig() {
  test-float64-fn( "atan", atan, atan, nums )
  test-float64-fn( "asin", asin, asin, nums )
  test-float64-fn( "acos", acos, acos, nums )
  test-float64-fn( "atanh", atanh, atanh, nums )
  test-float64-fn( "asinh", asinh, asinh, nums )
  test-float64-fn( "acosh", acosh, acosh, nums )
}

pub fun test-float64-fn( name : string, fd : float64 -> float64, fdd : ddouble -> ddouble, nums : list<float64> ) : io () {
  nums.foreach-indexed( fn( i:int, x : float64 ) {
    val d = x.fd
    val dd = x.ddouble.fdd
    check-prec( name ++ "(" ++ x.show(3) ++ ")", d.ddouble, dd,
               if (host()=="dotnet") then ddouble-exp(1,-4) // dotnet does not have good trigonometry :-(
                                     else ddouble-exp(1,-14))
  })
}
