/*

*/

pub import std/num/float64


fun check(name : string, res : string, got : string ) : io () {
  println(name.pad-right(7,' ') ++ ": " 
    ++ (if (got == res) then "ok: " ++ res 
                       else "FAILED!:\n expect: " ++ res ++ "\n gotten: " ++ got ++ "\n"))
  // if (got != res) then error("failed") else ()
}

fun check2( name : string, res : string, tst : (float64, int) ) : io () {
  check(name,res, tst.fst.show ++ ", " ++ tst.snd.show)
}

fun check1(name : string, res : string, tst : float64 ) : io () {
  check(name,res,tst.show)
}

fun check-logp(name : string, res : string, x : float64 ) : io () {
  check(name,res, ln(x + 1.0).show(15) ++ ", " ++ ln1p(x).show(15) ++ ", " ++ system-log1p(x).show(15))
}

extern system-log1p( x : float64 ) : float64 {
  c "log1p"
  js "Math.log1p"
}

// The area hyperbolic tangent of `d`
inline extern system-atanh( d : float64 ) : float64 {
  c "atanh"
  js "Math.atanh"
}

// The area hyperbolic cosine of `d`
inline extern system-acosh( d : float64 ) : float64 {
  c "acosh"
  js "Math.acosh"
}

// The area hyperbolic sine of `d`
inline extern system-asinh( d : float64 ) : float64 {
  c "asinh"
  js "Math.asinh"
}

fun check-eq( name : string, nums : list<float64>, f : float64 -> float64, g : float64 -> float64) {
  val xs = nums ++ nums.map((~))
  xs.foreach fn(x:float64) {
    val fx = f(x)
    val gx = g(x)
    check(name ++ ": " ++ x.show, fx.show(13), gx.show(13))
  }
}

fun main() {
  check-logp("logp 1", "0.693147180559945, 0.693147180559945, 0.693147180559945", 1.0)
  check-logp("logp 2", "1.332267629550187e-15, 1.234499999999999e-15, 1.234499999999999e-15", 1.2345e-15)
  check-logp("logp 3", "2.220446049250313e-16, 1.234500000000000e-16, 1.234500000000000e-16", 1.2345e-16)
  check-logp("logp 4", "0.000000000000000, 1.234500000000000e-32, 1.234500000000000e-32", 1.2345e-32)
  check-logp("logp 5", "0.000000000000000, 1.234500000000000e-64, 1.234500000000000e-64", 1.2345e-64)
  check-logp("logp 6", "709.782712893383973, 709.782712893383973, 709.782712893383973", 1.7976931348622732e+308)
  check-eq("atanh", [0.0,1.23e-11,1.23e-10,1.23e-3,0.123,0.5,flt-ln2,1.0,2.0,posinf,nan], atanh, system-atanh)
  check-eq("asinh", [0.0,1.23e-16,1.23e-15,1.23e-3,0.122,0.5,flt-ln2,1.0,2.0,1.23e3,1.23e8,1.23e15,1.23e16,posinf,nan], asinh, system-asinh)
  check-eq("acosh", [0.0,1.23e-16,1.23e-15,1.23e-3,0.122,0.5,flt-ln2,1.0,2.0,1.23e3,1.23e8,1.23e15,1.23e16,posinf,nan], acosh, system-acosh)

}