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

fun check-logp(name : string, res : string, x : double ) : io () {
  check(name,res, log(x + 1.0).show + ", " + log1p(x).show + ", " + jslog1p(x).show)
}

extern jslog1p( x : double ) : double {
  js "Math.log1p"
}

fun main() {
  check-logp("logp 1", "0.6931471805599453, 0.6931471805599453, 0.6931471805599453", 1.0)
  check-logp("logp 2", "1.332267629550187e-15, 1.2344999999999995e-15, 1.2344999999999993e-15", 1.2345e-15)
  check-logp("logp 3", "2.2204460492503128e-16, 1.2344999999999996e-16, 1.2345e-16", 1.2345e-16)
  check-logp("logp 4", "0, 1.2345e-32, 1.2345e-32", 1.2345e-32)
  check-logp("logp 5", "0, 1.2345e-64, 1.2345e-64", 1.2345e-64)
  check-logp("logp 6", "709.782712893384, 709.782712893384, 709.782712893384", 1.7976931348622732e+308)
}