/*

*/

public import std/num/double


fun check(name : string, res : string, got : string ) : io () {
  println(name.pad-right(7,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
  // if (got != res) then error("failed") else ()
}

fun check2( name : string, res : string, tst : (double, int) ) : io () {
  check(name,res, tst.fst.show + ", " + tst.snd.show)
}

fun check1(name : string, res : string, tst : double ) : io () {
  check(name,res,tst.show)
}

fun check-fl(name : string, res : string, tst : double ) : io () {
  val (frac,exp) = frexp(tst)
  val r = ldexp(frac,exp)
  check(name,res,tst.show(20) + (if(r == tst) then " == " else " != ") + 
          "ldexp(" + frac.show(15) + ", " + exp.show() + " )" )
}

public fun show( x : (double,int), prec : int = -1) : string {
  "(" + x.fst.show(prec) + ", " + x.snd.show + " )"
}

fun main() {
  check2( "frexp1", "1.25, 3", frexp(10.0))
  check-fl( "fr/ld 1", "10.000000000000000000 == ldexp(1.25000000000000, 3 )", 10.0 )
  check-fl( "fr/ld 1", "0.10000000000000000555 == ldexp(0.800000000000000, -3 )", 0.1 )
  check-fl( "fr/ld 3", "1.4821969375237396325e-323 == ldexp(0.750000000000000, -1072 )", 1.4821969375237396325e-323 )
  check-fl( "fr/ld 4", "4.9406564584124654418e-324 == ldexp(1.00000000000000, -1074 )", dbl-denorm-min )
  check-fl( "fr/ld 5", "2.2250738585072013831e-308 == ldexp(1.00000000000000, -1022 )", dbl-min )
  check-fl( "fr/ld 6", "1.7976931348623157081e+308 != ldexp(1.00000000000000, 1024 )", dbl-max )
  check-fl( "fr/ld 7", "-1.7976931348623157081e+308 != ldexp(-1.00000000000000, 1024 )", ~dbl-max )
  check1("kahan", "2000", [1.0e3,1.0e97,1.0e3,-1.0e97].sum) //foldr(0.0,(+)))
}