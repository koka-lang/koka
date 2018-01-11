/*

*/

public import std/num/double
public import std/num/int32

fun check(name : string, res : string, got : string ) : io () {
  println(name.pad-right(7,' ') + ": "
    + (if (got == res) then "ok: " + res
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
  // if (got != res) then error("failed") else ()
}

struct ldexp( frac : double, exp : int, exp2 : int, expect : string );

fun main() {
  val tests = [test-basic, test-zero, test-inf,
               test-ovf, test-underfl, test-denormal,
               test-denormal-big].concat

  tests.foreach-indexed fun(i,t) {
    match(t) {
      Ldexp(frac,exp,exp2,expect) -> {
        val x0 = ldexp(frac,exp)
        val x  = if (exp2==skip) then x0 else ldexp(x0,exp2)
        val s  = x.show-hex
        val se = expect.parse-double.default(nan).show-hex
        check(i.show + ":" + exp.show + "," + exp2.show, s, se);
      }
    }
  }
}


val skip = 9999

val test-basic = [
  Ldexp( 1.0,  5,  skip, "32" ),
  Ldexp( 1.0,  1022, skip, "4.4942328371557897693233e+307" ),
  Ldexp( 1.0,  1023, -1, "4.4942328371557897693233e+307" ),
  Ldexp( 1.0,  1023, skip, "8.9884656743115795386465e+307" ),
  Ldexp( 1.0,  1022, 1,  "8.9884656743115795386465e+307" ),
  Ldexp( 1.0,  -1022,  2045, "8.9884656743115795386465e+307" ),
  Ldexp( 1.0,  -5, skip, "                0.03125" ),
  Ldexp( 1.0,  -1021,  skip, "4.4501477170144027661805e-308" ),
  Ldexp( 1.0,  -1022,  1,  "4.4501477170144027661805e-308" ),
  Ldexp( 1.0,  -1022,  skip, "2.2250738585072013830902e-308" ),
  Ldexp( 1.0,  -1021,  -1, "2.2250738585072013830902e-308" ),
  Ldexp( 1.0,  1023, -2045,  "2.2250738585072013830902e-308" ),
  Ldexp( 1.0,  1023, -1023,  "1" ),
  Ldexp( 1.0,  -1022,  1022, "1" )
]

val test-zero = [
  Ldexp( 0.0,  -1, skip, "0" ),
  Ldexp( 0.0,  0,  skip, "0" ),
  Ldexp( 0.0,  1,  skip, "0" ),
  Ldexp( 0.0,  1024, skip, "0" ),
  Ldexp( 0.0,  1025, skip, "0" ),
  Ldexp( 0.0,  -1023,  skip, "0" ),
  Ldexp( 0.0,  -1024,  skip, "0" )
]

val test-inf = [
  Ldexp( 1.0,  1024, -1, "Infinity" ),
  Ldexp( 1.0,  1024, 0,  "Infinity" ),
  Ldexp( 1.0,  1024, 1,  "Infinity" ),
  Ldexp( -1.0, 1024, -1, "-Infinity" ),
  Ldexp( -1.0, 1024, 0,  "-Infinity" ),
  Ldexp( -1.0, 1024, 1,  "-Infinity" )
]

val test-ovf = [
  Ldexp( 1.0,  1024, skip, "Infinity" ),
  Ldexp( 1.0,  1023, 1,  "Infinity" ),
  Ldexp( 1.0,  -1022,  2046, "Infinity" ),
  Ldexp( 1.0,  1025, skip, "Infinity" ),
  Ldexp( 2.0,  9999999,skip, "Infinity" ),
  Ldexp( -1.0, 1024, skip, "-Infinity" ),
  Ldexp( -1.0, 1023, 1,  "-Infinity" ),
  Ldexp( -1.0, -1022,  2046, "-Infinity" ),
  Ldexp( -1.0, 1025, skip, "-Infinity" ),
  Ldexp( -2.0, 9999999,skip, "-Infinity" )
]

val test-denormal = [
  Ldexp( 1.0,  -1023,  skip, "1.1125369292536006915451e-308" ),
  Ldexp( 1.0,  -1022,  -1, "1.1125369292536006915451e-308" ),
  Ldexp( 1.0,  1023, -2046,  "1.1125369292536006915451e-308" ),
  Ldexp( 1.0,  -1024,  skip, "5.5626846462680034577256e-309" ),
  Ldexp( 1.0,  -1074,  skip, "4.9406564584124654417657e-324" ),
  Ldexp( -1.0, -1023,  skip, "-1.1125369292536006915451e-308" ),
  Ldexp( -1.0, -1022,  -1, "-1.1125369292536006915451e-308" ),
  Ldexp( -1.0, 1023, -2046,  "-1.1125369292536006915451e-308" ),
  Ldexp( -1.0, -1024,  skip, "-5.5626846462680034577256e-309" ),
  Ldexp( -1.0, -1074,  skip, "-4.9406564584124654417657e-324" )
]

val test-underfl = [
  Ldexp( 1.0,  -1075,  skip, "0" ),
  Ldexp( 1.0,  -1074,  -1, "0" ),
  Ldexp( 1.0,  1023, -2098,  "0" ),
  Ldexp( 1.0,  -1076,  skip, "0" ),
  Ldexp( -1.0, -1075,  skip, "-0" ),
  Ldexp( -1.0, -1074,  -1, "-0" ),
  Ldexp( -1.0, 1023, -2098,  "-0" ),
  Ldexp( -1.0, -1076,  skip, "-0" )
]

val test-denormal-big = [
  Ldexp( 1.0,  -1028,  1024, "0.0625" ),
  Ldexp( 1.0,  -1028,  1025, "0.125" ),
  Ldexp( 1.0,  -1028,  1026, "0.25" ),
  Ldexp( 1.0,  -1028,  1027, "0.5" ),
  Ldexp( 1.0,  -1028,  1028, "1" ),
  Ldexp( 1.0,  -1028,  1029, "2" ),
  Ldexp( 1.0,  -1028,  1030, "4" ),
  Ldexp( 1.0,  -1028,  1040, "4096" ),
  Ldexp( 1.0,  -1028,  1050, "4194304" ),
  Ldexp( 1.0,  -1028,  1060, "4294967296" ),
  Ldexp( 1.0,  -1028,  1100, "4722366482869645213696" ),
  Ldexp( 1.0,  -1028,  1200, "5.9863107065073783529623e+51" ),
  Ldexp( 1.0,  -1028,  1300, "7.5885503602567541832791e+81" ),
  Ldexp( 1.0,  -1028,  1400, "9.6196304190416209014353e+111" ),
  Ldexp( 1.0,  -1028,  1500, "1.2194330274671844653834e+142" ),
  Ldexp( 1.0,  -1028,  1600, "1.5458150092069033378781e+172" ),
  Ldexp( 1.0,  -1028,  1700, "1.9595533242629369747791e+202" ),
  Ldexp( 1.0,  -1028,  1800, "2.4840289476811342962384e+232" ),
  Ldexp( 1.0,  -1028,  1900, "3.1488807865122869393369e+262" ),
  Ldexp( 1.0,  -1028,  2000, "3.9916806190694396233127e+292" ),
  Ldexp( 1.0,  -1028,  2046, "2.808895523222368605827e+306" ),
  Ldexp( 1.0,  -1028,  2047, "5.6177910464447372116541e+306" ),
  Ldexp( 1.0,  -1028,  2048, "1.1235582092889474423308e+307" ),
  Ldexp( 1.0,  -1028,  2049, "2.2471164185778948846616e+307" ),
  Ldexp( 1.0,  -1028,  2050, "4.4942328371557897693233e+307" ),
  Ldexp( 1.0,  -1028,  2051, "8.9884656743115795386465e+307" )
]
