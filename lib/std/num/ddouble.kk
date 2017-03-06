/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on _double double_ 128-bit floating point numbers.
//
module std/num/ddouble

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

/*------------------------------------------------------
   Multiply and divide
------------------------------------------------------*/
val splitter = 134217729.0;              // = 2^27 + 1
val splitbound  = 6.69692879491417e+299; // = 2^996
val two28 = 268435456.0;                 // = 2^28

fun split( x : double ) : (double,double) {
  if (x > splitbound || x < ~splitbound) {
    val y = x * 3.7252902984619140625e-09;  // 2^-28
    val t = y * splitter;
    val hi = t - (t - y);
    val lo = y - hi;
    (hi*two28, lo*two28);
  }
  else {
    val t = x * splitter;
    val hi = t - (t - x);
    val lo = x - hi;
    (hi,lo);
  }
}

fun prod( x : double, y : double ) : edouble {
  val z = x*y;
  val (xhi,xlo) = split(x);
  val (yhi,ylo) = split(y);
  val err = ((xhi*yhi - z) + (xhi*ylo) + (xlo*yhi)) + (xlo*ylo);
  Edouble(z,err)
}

// `prod` can also be implemented using 'fused multiply add'
/* 
fun prod( x : double, y : double ) : edouble {
  val z = x*y;
  err = fmadd(x,y,~z)
  Edouble(z,err)
}
*/

public fun (*)( x : ddouble, y : ddouble ) : ddouble {
  val z  = prod(x.hi, y.hi);
  val e  = z.err + (x.hi*y.lo + x.lo*y.hi);
  qquicksum(z.num, e);
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
  val i = r.hi.int
  if (i.precise?) then i else {
    // todo: optimize?
    r.show-fixed().parse-int-default(r.hi.int)
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
  elif (prec > ddouble-max-prec) then x 
  else {
    val p : ddouble = pow10(prec)
    (x * p).round / p
  }
}

/*------------------------------------------------------
   Various
------------------------------------------------------*/
fun ldexp( x : double, exp : double ) : double {
  x*pow2(exp)
}
public fun ldexp( x : ddouble, exp : double ) : ddouble {
  Ddouble(ldexp(x.hi,exp),ldexp(x.lo,exp))
}

public fun sqr( x : ddouble ) : ddouble {
  x * x
}

fun npwr-acc( x : ddouble, acc : ddouble, n : int ) : ddouble {
  if (n <= 0) then acc 
  elif (n.even?) then npwr-acc( sqr(x), acc, unsafe-decreasing(n/2) )
  else npwr-acc( x, x*acc, unsafe-decreasing(n.dec))
}

fun npwr( x : ddouble, n : int ) : ddouble {
  if (n==0) {
    if (x.zero?) then ddouble(nan) else one
  }
  elif (n==1) {
    x
  }
  else {
    x.npwr-acc(one,n)
  }
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
  val p = if (prec.neg?) then ddouble-max-prec else max(1,prec)
  if (p > e && e >= ~4 && e < 27)
   then show-fixed(x, (p - 1) - e, prec.neg? )
   else show-exp(x, prec)
}

public fun show-fixed( x : ddouble, dprec : int = 0, trim-zeros : bool = False ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return ("0" + (if (dprec.pos? && !trim-zeros) then "." + repeat("0",dprec) else ""))
  val (ds,e) = digits(x, ddouble-max-prec + 1 )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  

  // don't print too large a number; 10^28 and larger is problematic anyway
  if (e.abs > 27) return show-exp-digits(x,ddouble-max-prec,ds,e)
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
  val (ds,e) = digits(x, (if (prec.neg?) then ddouble-max-prec else prec) + 1 )
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
public val ddouble-max-prec = 31

// Extract significant digits and exponent from a `:ddouble`.
fun digits( x : ddouble, prec : int = ddouble-max-prec ) : (list<int>,int) {
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

public fun parse-ddouble( s  : string ) : maybe<ddouble> {
  parse-double(s).map(ddouble)
}