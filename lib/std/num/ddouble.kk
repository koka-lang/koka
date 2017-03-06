/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Operations on _double double_ 128-bit floating point numbers.

The `:double` type implements [double double][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:double` values. This extends the precision to 31 decimal digits
(versus 15 for `:double`), but keeps the same range as
a `:double` with a maximum  value of about 1.8&times;10^308^. Because
`:double`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

Note that despite the extra precision, underflow/overflow and machine precision are 
not as well-defined as with a regular `:double` [@Dekker;@Hida:qd;@Hida:qdlib].

The implementation is based closely on the [QD] library and assumes proper IEEE `:double`s 
with correct rounding.


## References {-}

~ Bibliography { caption:"0" }

~~ BibItem { #Hida:qd; searchterm:"Hida+Quad-Double+Arithmetic:+Algorithms,+Implementation,+and+Application" }
Yozo Hida,\ X.\ Li, and D.\ H.\ Bailey.
_Quad-Double Arithmetic: Algorithms, Implementation, and Application_.
Lawrence Berkeley National Laboratory Technical Report LBNL-46996 (2000). [pdf](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.5769).
~~

~~ BibItem { #Hida:qdlib; searchterm:"Hida+Library+for+double-double+and+quad-double+arithmetic" }
Yozo Hida,\ X.\ Li, and D.\ H.\ Bailey.
_Library for double-double and quad-double arithmetic._ 
(2007). [pdf](http://www.jaist.ac.jp/~s1410018/papers/qd.pdf).
~~

~~ BibItem { #dekker; searchterm:"Dekker+A+Floating-Point+Technique+for+Extending+the+Available+Precision" }
T.\ Dekker.
_A Floating-Point Technique for Extending the Available Precision_.
Numerische\ Mathematik **18**(3), June 1971, 224--242.
~~
~

[ddwiki]: https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format#Double-double_arithmetic
[qd]:     http://crd-legacy.lbl.gov/~dhbailey/mpdist

\/
*/
module std/num/ddouble

/* The `:double` type implements [double double][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:double` values. This extends the precision to 31 decimal digits
(versus 15 for `:double`), but keeps the same range as
a `:double` with a maximum  value of about 1.8&times;10^308^. Because
`:double`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

Internally a `:ddouble` _d_ is represented as a pair of `:double`s, _hi_ and _lo_
such that the number represented by _d_ is _hi_+_lo_, where \|_lo_\| &le; 0.5&times;ulp(_hi_).
*/
abstract struct ddouble(
  hi : double,
  lo : double
)

public fun ddouble( d : double ) : ddouble {
  Ddouble(d,0.0)
}


val maxprecise : int = 9007199254740991
val minprecise : int = ~maxprecise

fun precise?(i : int ) : bool {
  (i >= minprecise && i <= maxprecise)
}

public fun ddouble( i : int ) : ddouble {
  if (i.precise?) then ddouble( i.double ) else {
    val pd = i.count-digits
    val p  = pd - 15
    val hi = i.cdiv-pow10(p)
    val r  = i - hi.mul-pow10(p)
    ddouble(hi.double)*pow10(p) + ddouble(r.double)
  }
}


public val dd-pi      = Ddouble(3.141592653589793116e+00,1.224646799147353207e-16)
public val dd-twopi   = Ddouble(6.283185307179586232e+00,2.449293598294706414e-16);
public val dd-pi2     = Ddouble(1.570796326794896558e+00,6.123233995736766036e-17);
public val dd-pi4     = Ddouble(7.853981633974482790e-01,3.061616997868383018e-17);
public val dd-e       = Ddouble(2.718281828459045091e+00,1.445646891729250158e-16)
public val dd-log2    = Ddouble(6.931471805599452862e-01,2.319046813846299558e-17);
public val dd-log10   = Ddouble(2.302585092994045901e+00,~2.170756223382249351e-16);

public val dd-epsilon = ddouble(4.93038065763132e-32)
public val dd-max     = Ddouble(1.79769313486231570815e+308, 9.97920154767359795037e+291)
public val dd-min     = ddouble(2.0041683600089728e-292)  // normalized = 2^(-1022 + 53)

public val dd-nan     = Ddouble(nan,nan)
public val dd-pos-inf = Ddouble(pos-inf,pos-inf)

// maximal precision in decimal digits of a `:ddouble`.
public val dd-max-prec = 31


/*------------------------------------------------------
   Compare etc.
------------------------------------------------------*/

public val zero = ddouble(0.0)
public val one  = ddouble(1.0)
public val ten  = ddouble(10.0)

public fun zero?( x : ddouble ) : bool {
  x.hi.zero?
}

public fun neg?( x : ddouble ) : bool {
  x.hi.neg?
}

public fun pos?( x : ddouble ) : bool {
  x.hi.pos?
}

public fun sign?( x : ddouble ) : order {
  if (x.hi==0.0) then Eq
  elif (x.hi < 0.0) then Lt
  else Gt
}

public fun nan?( x : ddouble ) : bool {
  x.hi.nan?
}

public fun finite?( x : ddouble ) : bool {
  x.hi.finite?
}

public fun inf?( x : ddouble ) : bool {
  x.hi.inf?
}

public fun posinf?( x : ddouble ) : bool {
  x.hi.inf?
}

public fun neginf?( x : ddouble ) : bool {
  x.hi.inf?
}


public fun compare( x : ddouble, y : ddouble ) : order {
  match(compare(x.hi,y.hi)) {
    Eq  -> compare(x.lo,y.lo)
    ord -> ord
  }
}

public fun (>) (x : ddouble, y : ddouble) : bool { compare(x,y) == Gt }
public fun (>=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Lt }
public fun (==)(x : ddouble, y : ddouble) : bool { compare(x,y) == Eq }
public fun (!=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Eq }
public fun (<) (x : ddouble, y : ddouble) : bool { compare(x,y) == Lt }
public fun (<=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Gt }

// The minimum of `x` and `y`.
public fun min( x : ddouble, y : ddouble ) : ddouble {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
public fun max( x : ddouble, y : ddouble ) : ddouble {
  if (x >= y) then x else y
}

/*------------------------------------------------------
   Addition
------------------------------------------------------*/

struct edouble(
  num : double,
  err : double              
)

fun sum( x : double, y : double ) : edouble {
  val z    = x + y
  val diff = z - x
  val err  = (x - (z - diff)) + (y - diff)
  Edouble(z,err)
}

fun quicksum( x : double, y : double ) : edouble {
  val z   = x + y
  val err = y - (z - x)
  Edouble(z,err)
}

fun qquicksum( x : double, y : double ) : ddouble {
  val z   = x + y
  val err = y - (z - x)
  Ddouble(z,err)
}

public fun (+)( x : ddouble, y : ddouble ) : ddouble {
  val z1 = sum(x.hi,y.hi)
  val lo = sum(x.lo,y.lo)
  val e1 = z1.err + lo.num
  val z2 = quicksum(z1.num,e1)
  val e2 = z2.err + lo.err
  qquicksum(z2.num,e2)
}

public fun (~)( x : ddouble ) : ddouble {
  Ddouble(~x.hi,~x.lo)
}

public fun (-)( x : ddouble, y : ddouble ) : ddouble {
  x + (~y)
}

public fun abs( x : ddouble ) : ddouble {
  if (x.neg?) then ~x else x
}

public fun inc( x : ddouble ) :ddouble {
  x + one
}

public fun dec( x : ddouble ) :ddouble {
  x - one
}

public fun sum( xs : list<ddouble> ) : ddouble {
  xs.foldl(zero,(+))
}

/*------------------------------------------------------
   Multiply and divide
------------------------------------------------------*/
val splitter = 134217729.0;              // = 2^27 + 1
val splitbound  = 6.69692879491417e+299; // = 2^996
val two28 = 268435456.0;                 // = 2^28

fun split( x : double ) : (double,double) {
  if (x > splitbound || x < ~splitbound) {
    val y = x * 3.7252902984619140625e-09  // 2^-28
    val t = y * splitter
    val hi = t - (t - y)
    val lo = y - hi
    (hi*two28, lo*two28)
  }
  else {
    val t = x * splitter
    val hi = t - (t - x)
    val lo = x - hi
    (hi,lo)
  }
}

fun prod( x : double, y : double ) : edouble {
  val z = x*y
  val (xhi,xlo) = split(x)
  val (yhi,ylo) = split(y)
  val err = ((xhi*yhi - z) + (xhi*ylo) + (xlo*yhi)) + (xlo*ylo)
  Edouble(z,err)
}

fun prodsqr( x : double ) : edouble {
  val z = x*x
  val (h,l) = split(x)
  val err = ((h*h - z) + (2.0*h*l)) + (l*l)
  Edouble(z,err)             
}

// `prod` & `prodsqr` can also be implemented using 'fused multiply add'
/* 
fun prod( x : double, y : double ) : edouble {
  val z = x*y
  err = fmadd(x,y,~z)
  Edouble(z,err)
}

fun prodsqr( x : double ) : edouble {
  val z = x*x
  val err = fmadd(x,x,~z)
  Edouble(z,err)
}
*/

public fun (*)( x : ddouble, y : ddouble ) : ddouble {
  val z  = prod(x.hi, y.hi);
  val e  = z.err + (x.hi*y.lo + x.lo*y.hi);
  qquicksum(z.num, e);
}

public fun sqr( x : ddouble ) : ddouble {
  val z = prodsqr(x.hi)
  val e = (z.err + (2.0*x.hi*x.lo)) + (x.lo*x.lo)
  qquicksum(z.num,e)
}

public fun (/)( x : ddouble, y : ddouble ) : ddouble {
  val q1 = ddouble(x.hi / y.hi);
  val r1 = x - (y * q1);
  val q2 = ddouble(r1.hi / y.hi);
  val r2 = r1 - (y * q2); 
  val q3 = ddouble(r2.hi / y.hi);
  val q  = qquicksum(q1.hi,q2.hi);
  q + q3
}

public fun (%)( x : ddouble, y : ddouble ) : ddouble {
  val n = (x / y).round
  x - (n*y)
}

public fun divrem( x : ddouble, y : ddouble ) : (ddouble,ddouble) {
  val n = (x / y).round
  (n, x - (n*y))
}

/*------------------------------------------------------
   Rounding
------------------------------------------------------*/

public fun double( x : ddouble ) : double {
  x.hi
}

public fun int( x : ddouble ) : int {
  val r = x.round
  if (!r.finite?) then 0 else {
    val i = r.hi.int
    if (i.precise?) then i else {
      // todo: optimize?
      r.show-fixed().parse-int-default(0)
    }
  }
}

public fun round( x : ddouble ) : ddouble {
  val r = x.hi.round
  if (r == x.hi) {
    qquicksum(r,x.lo.round)
  }
  elif(abs(r - x.hi) == 0.5 && x.lo.neg?) {
    Ddouble(r - 1.0,0.0)
  }
  else {
    Ddouble(r,0.0)
  }
}

public fun floor( x : ddouble ) : ddouble {
  val r = x.hi.floor
  if (r == x.hi) 
   then qquicksum(r,x.lo.floor)
   else Ddouble(r,0.0)
}

public fun ceiling( x : ddouble ) : ddouble {
  val r = x.hi.ceiling
  if (r == x.hi) 
   then qquicksum(r,x.lo.ceiling)
   else Ddouble(r,0.0)
}

public fun trunc( x : ddouble ) : ddouble {
  if (x.neg?) then x.ceiling else x.floor
}

public fun fraction( x : ddouble ) : ddouble {
  x - x.trunc
}

public fun ffraction( x : ddouble ) : ddouble {
  x - x.floor
}


// Round a `:ddouble` to a specified precision
public fun round-to-prec( x : ddouble, prec : int  ) : ddouble {
  if (prec <= 0) then x.round 
  elif (prec > dd-max-prec) then x 
  else {
    val p : ddouble = pow10(prec)
    (x * p).round / p
  }
}

/*------------------------------------------------------
   Exponents (needed for showing ddouble's)
------------------------------------------------------*/
fun ldexp( x : double, exp : double ) : double {
  x*pow2(exp)
}
public fun ldexp( x : ddouble, exp : double ) : ddouble {
  Ddouble(ldexp(x.hi,exp),ldexp(x.lo,exp))
}

fun npwr-acc( x : ddouble, acc : ddouble, n : int ) : ddouble {
  if (n <= 0) then acc 
  elif (n.even?) then npwr-acc( sqr(x), acc, unsafe-decreasing(n/2) )
  else npwr-acc( x, x*acc, unsafe-decreasing(n.dec))
}

fun npwr( x : ddouble, n : int ) : ddouble {
  if (n==0) then (if (x.zero?) then dd-nan else one)
  elif (n==1) then x
  else x.npwr-acc(one,n)
}

public fun pow( x : ddouble, n : int ) : ddouble {
  val p = npwr(x,n.abs)
  if (n.neg?) then (one / p) else p
}

public fun pow10( exp : int ) : ddouble {
  pow(ten,exp)
}

public fun ddouble-exp( i : int, exp : int ) : ddouble {
  i.ddouble * pow10(exp)
}


/*------------------------------------------------------
   Show
------------------------------------------------------*/

public fun show-debug( q : ddouble ) : string {
  "ddouble(" + q.hi.show +
    (if (q.hi.finite?) then " + " + q.lo.show else "") +
    ")"   
}

public fun show( x : ddouble, prec : int = ~1 ) : string {
  x.show-prec(prec)
}

public fun show-prec( x : ddouble, prec : int = ~1 ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return "0"
  // guess exponent
  val e = x.hi.abs.log10.floor.int  
  val p = if (prec.neg?) then dd-max-prec else max(1,prec)
  if (p > e && e >= ~4 && e < 27)
   then show-fixed(x, (p - 1) - e, prec.neg? )
   else show-exp(x, prec)
}

public fun show-fixed( x : ddouble, dprec : int = 0, trim-zeros : bool = False ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return ("0" + (if (dprec.pos? && !trim-zeros) then "." + repeat("0",dprec) else ""))
  val (ds,e) = digits(x, dd-max-prec + 1 )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  

  // don't print too large a number; 10^28 and larger is problematic anyway
  if (e.abs > 27) return show-exp-digits(x,dd-max-prec,ds,e)
  show-fixed-digits(x,dprec,ds,e,trim-zeros)
}

fun show-fixed-digits( x : ddouble, dprec : int, ds0 : list<int>, e0 : int, trim-zeros : bool ) : string {
  // round
  val (ds,e) = fixed-round( x.neg?, ds0, e0, dprec )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  

  // use fixed
  val (whole,frac) = if (e.neg?) then ([0], replicate(0, ~e - 1 ) + ds) else ds.split(e+1)
  val pre  = (whole + replicate(0,e + 1 - whole.length)).map(show).join
  val frac0= if (dprec.zero?) then []
             elif (dprec.pos?) then frac.take(dprec) + replicate(0, dprec - frac.length)
             else frac
  val frac1= frac0.map(show).join
  val frac2= if (trim-zeros) then frac1.trim-right("0") else frac1
  val post = if (frac2.empty?) then "" else "." + frac2             
  val num  = pre + post
  val sign = if (x.neg? && num!="0") then "-" else ""
  sign + num
}

fun fixed-round( isneg : bool, ds : list<int>, e:int, dprec : int ) : (list<int>,int) {
  val pred = if (isneg) then odd? else even?
  val (pre,post) = ds.split( e + 1 + dprec )
  // trace( " fixed-round: " + pre.show-list(show) + " . " + post.show-list(show))
  match(post) {
    Cons(p) | p > 5 || (p==5 && pre.last.default(0).pred) -> {
      if (pre.nil?) then ([1],e.inc) 
      else pre.reverse.digits-adjust(1).adjust10(e)
    }
    _ -> (pre,e)
  }
}  

public fun show-exp( x : ddouble, prec : int = ~1 ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return ("0" + (if (prec.pos?) then "." + repeat("0",prec) else ""))
  val (ds,e) = digits(x, (if (prec.neg?) then dd-max-prec else prec)  )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  
  show-exp-digits(x,prec,ds,e)
}

fun show-exp-digits( x : ddouble, prec : int, ds : list<int>, e : int ) : string {    
  val pre  = ds.head.default(0).show
  val frac0= (if (!prec.zero?) then ds.tail else []).map(show).join
  val frac = if (!prec.neg?) then frac0 else frac0.trim-right("0")
  val post = if (frac.empty?) then "" else "." + frac
  val sign = if (x.neg?) then "-" else ""
  val exp  = if (e.zero?) then "" else ("e" + (if (e.pos?) then "+" else "") + e.show)
  sign + pre + post + exp
}


/*------------------------------------------------------
   Extract digits
------------------------------------------------------*/

// Extract significant digits and exponent from a `:ddouble`.
fun digits( x : ddouble, prec : int = dd-max-prec ) : (list<int>,int) {
  val n = prec + 1  // number of digits
  val (e,r) = digits-toexp(x)
  // trace(" digits-toexp: e: " + e.show + ", r: " + r.show-debug )
  val ds = r.digits-extract(n).take(prec)
  //trace(" digits-extract: " + ds.show-list(show))
  ds.adjust10(e)
}

// adjust if starts with a digit > 9
fun adjust10( ds : list<int>, e : int ) : (list<int>,int) {
  match(ds) {
    Cons(d,ds1) | d > 9 -> {
      ( Cons(1,Cons(0,ds1)), e.inc )
    }
    _ -> (ds,e)
  }
}

// Return an exponent and normalized ddouble
fun digits-toexp( x : ddouble ) : (int,ddouble) {
  // guess approximate exponent
  val e1 = x.hi.abs.log10.floor.int  
  // guess exponential
  val r0 = x.abs
  val r1 = if (e1 < ~300) then (r0*pow10(300)) / pow10(e1+300)
           elif (e1 > 300) then ldexp(ldexp(r0,~53.0) / pow10(e1), 53.0)
           else r0 / pow10(e1)
  // fix off by one
  val ten = ddouble(10.0)
  if (r1 >= ten) then (e1.inc, r1 / ten)
  elif (r1 < one) then (e1.dec, r1 * ten)
  else (e1,r1)
}
 
// extract the first n digits from a normalized ddouble (in reverse)
fun digits-extract( r : ddouble, n : int, acc : list<int> = [] ) : list<int> {
  if (n <= 0) then digits-round(acc).digits-adjust else {
    val d = r.hi.trunc.int
    digits-extract( (r - ddouble(d))*ten, 
                    unsafe-decreasing(n.dec), Cons(d,acc) )
  }
}

// round on (and discard) the last digit before doing overflow correction
fun digits-round( rds : list<int> ) : list<int> {
  match(rds) {
    Cons(d0,Cons(d1,rds1) as rds0) 
      -> if (d0 >= 5) then Cons(d1.inc,rds1) else rds0
    _ -> rds
  }
}

// adjust overflow digits in a reversed list of digits
fun digits-adjust( rds : list<int>, adj : int = 0, acc : list<int> = [] ) : list<int> {
  match(rds) {
    Cons(d0,ds) {
      // propagate carry while adjusting
      val d = d0 + adj
      if (d < 0)   then digits-adjust( ds, ~1, Cons(d + 10, acc))
      elif (d > 9) then digits-adjust( ds, 1, Cons(d - 10, acc))
      else digits-adjust( ds, 0, Cons(d,acc) )
    }
    Nil -> acc
  }
}

/*------------------------------------------------------
  Parsing
------------------------------------------------------*/

public fun parse-ddouble( s  : string ) : maybe<ddouble> {
  parse-double(s).map(ddouble)
}



/*------------------------------------------------------
  Advanced operations
------------------------------------------------------*/

// The square root of a non-negative `:ddouble` `x`.
// For negative `x`, `dd-nan` is returned.
public fun sqrt( x : ddouble ) : ddouble {
  // From the QD library:
  // Use Karp's strategy: if a is an approximation to sqrt(x), then
  // sqrt(x) = x*a + (x - (x*a)^2) * a/2   
  if (x.zero?) return zero
  if (x.neg?)  return dd-nan
  val a = 1.0 / sqrt(x.hi)
  val t1 = ddouble( x.hi * a )
  val t2 = (x - sqr(t1)) * ddouble(a * 0.5)
  t1 + t2
}

// The `n`-th root of a `:ddouble` number `x`.
// `n` must be positive, and if `n` is even, then
// `x` must not be negative.
public fun nroot( x : ddouble, n : int ) : ddouble {
  if (n==2) then x.sqrt
  elif (n==1) then x
  elif (n<=0 || (n.even? && x.neg?)) then dd-nan
  elif (x.zero?) then zero
  else {
    val r = x.abs
    val a0 = exp( ~log(r.hi) / n.double ).ddouble
    val a1 = a0 + ((a0 * (one - r * pow(a0,n))) / n.ddouble)
    (if (x.hi.neg?) then ~one else one) / a1
  }
} 

// Multiply `x` by a `:double` `p` where `p` must be a power of 2.
fun mul-pwr2( x : ddouble, p : double ) : ddouble {
  Ddouble(x.hi * p, x.lo * p)
}

// Return _e_ (`dd-e`) to the power of `x`.
public fun exp( x : ddouble ) : ddouble {
  /* From the QD library:
     Strategy:  We first reduce the size of x by noting that
     
          exp(kr + m * log(2)) = 2^m * exp(r)^k

     where m and k are integers.  By choosing m appropriately
     we can make |kr| <= log(2) / 2 = 0.347.  Then exp(r) is 
     evaluated using the familiar Taylor series.  Reducing the 
     argument substantially speeds up the convergence.  
  */
  val k : double = 512.0
  val inv-k = 1.0 / k

  if (x.hi <= ~709.0) then zero 
  elif (x.hi >= 709.0) then dd-pos-inf
  elif (x.zero?) then one
  elif (x==one) then dd-e
  else {
    val m : double = floor( (x.hi / dd-log2.hi) + 0.5 )
    val r = mul-pwr2( x - (m.ddouble * dd-log2), inv-k )
    val p = r.sqr
    val t = r + mul-pwr2(p, 0.5)
    var acc := exp-approx( p, t, r, inv-k * dd-epsilon.hi, exp-factors )
    repeat(9) {
      acc := mul-pwr2(acc,2.0) + acc.sqr
    }
    acc := acc + one
    ldexp( acc, m )
  }
}

val exp-factors : list<ddouble> = [
  Ddouble(1.66666666666666657e-01,  9.25185853854297066e-18),
  Ddouble(4.16666666666666644e-02,  2.31296463463574266e-18),
  Ddouble(8.33333333333333322e-03,  1.15648231731787138e-19),
  Ddouble(1.38888888888888894e-03, ~5.30054395437357706e-20),
  Ddouble(1.98412698412698413e-04,  1.72095582934207053e-22),
  Ddouble(2.48015873015873016e-05,  2.15119478667758816e-23)
]

fun exp-approx( p : ddouble, t : ddouble, 
                r : ddouble,
                eps : double,
                fs : list<ddouble>,
                s : ddouble = zero ) : ddouble {
  match(fs) {
    Nil -> s + t
    Cons(f,fs1) -> {
      val s1 = s + t
      val p1 = p * r
      val t1 = p1 * f
      if (t1.double.abs <= eps) 
       then s + t
       else exp-approx( p1, t1, r, eps, fs1, s1 )
    }
  }
}



// The natural logarithm (in base _e_) of `x`.
public fun log( x : ddouble ) : ddouble {
  /* From QD Library:
     Strategy.  The Taylor series for log converges much more
     slowly than that of exp, due to the lack of the factorial
     term in the denominator.  Hence this routine instead tries
     to determine the root of the function
         f(x) = exp(x) - a
     using Newton iteration.  The iteration is given by
         x' = x - f(x)/f'(x) 
            = x - (1 - a * exp(-x))
            = x + a * exp(-x) - 1.         
     Only one iteration is needed, since Newton's iteration
     approximately doubles the number of digits per iteration. 
  */
  if (x == one) then zero
  elif (x <= zero) then dd-nan
  elif (x == dd-e) then one
  else {
    val a0 = log(x.hi).ddouble // approximate
    val a1 = a0 + (x * exp(~a0) - one)
    a1
  }
}

// The logarithm in base 10 of `x`.
public fun log10( x : ddouble ) : ddouble {
  return log(x) / dd-log10
}


// The logarithm in base 2 of `x`.
public fun log2( x : ddouble ) : ddouble {
  return log(x) / dd-log2
}
