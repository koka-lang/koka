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
  check(name,res,tst.show-full + (if(r == tst) then " == " else " != ") + 
          "ldexp(" + frac.show + ", " + exp.show + " )" )
}

public fun show( x : (double,int), prec : int = -1) : string {
  "(" + x.fst.show(prec) + ", " + x.snd.show + " )"
}

fun main() {
  check2( "frexp1", "0.625, 4", frexp(10.0))
  check-fl( "fr/ld 1", "1.00000000000000000000e+1 == ldexp(0.625, 4 )", 10.0 )
  check-fl( "fr/ld 1", "1.00000000000000005551e-1 == ldexp(0.80000000000000004, -3 )", 0.1 )
  check-fl( "fr/ld 3", "1.48219693752373963253e-323 == ldexp(0.75, -1072 )", 1.4821969375237396325e-323 )
  check-fl( "fr/ld 4", "4.94065645841246544177e-324 == ldexp(0.5, -1073 )", dbl-true-min )
  check-fl( "fr/ld 5", "2.22507385850720138309e-308 == ldexp(0.5, -1021 )", dbl-min )
  check-fl( "fr/ld 6", "1.79769313486231570815e+308 == ldexp(0.99999999999999989, 1024 )", dbl-max )
  check-fl( "fr/ld 7","-1.79769313486231570815e+308 == ldexp(-0.99999999999999989, 1024 )", ~dbl-max )
  check1("kahan", "2000", [1.0e3,1.0e97,1.0e3,-1.0e97].sum) //foldr(0.0,(+)))
}