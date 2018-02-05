/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision decimal numbers.
module std/num/decimal

import std/text/regex
import std/num/double

// Type of a decimal number. Decimals have arbitrary precision and range and
// do exact decimal arithmetic and are well suited for financial calculations for
// example.
abstract struct decimal (
	num: int,
	exp: int
)

// The decimal zero.
public val zero : decimal = Decimal(0,0)

// round exponents to specific intervals (7) to avoid too much rescaling
fun round-exp( exp : int ) : int {
  if (exp.zero?) then 0 else 7*(exp / 7)
}

// Create a decimal from an integer `i` with an optional
// exponent `exp` (=`0`) such that the result equals `i`&times;10^`exp`^.
public fun decimal( i : int, exp : int = 0 ) : decimal {
	decimal-exp(i,exp)
}


// Create a decimal from an integer `i` with an optional
// exponent `exp` (=`0`) such that the result equals `i`&times;10^`exp`^.
fun decimal-exp( i : int, exp : int = 0 ) : decimal {
  // use exponents only at specific intervals to avoid too much re-scaling
  val x = round-exp(exp)  // always x <= exp
  val diff = exp - x
  //trace("decimal-exp: " + i.show + "x" + exp.show + ", diff: " + diff.show + ", x: " + x.show)
  if (diff.zero?) then Decimal(i,exp) else Decimal(i.mul-exp10(diff),x)
}

/* Create a decimal from a `:double` with a specified maximal precision (=`-1`).
Use a negative maximal precision to create a decimal that precisely represents the `:double`.
Note: creating a `:decimal` from a `:double` may lose precision and give surprising results as many decimal
fractions cannot be represented precisely by a `:double`.
Also, `decimal(i,exp)` is more efficient and better when when exact representations
are required. For example:
```
> decimal(1.1)
1.100000000000000088817841970012523233890533447265625
> decimal(1.1,17)
1.10000000000000008
> decimal(11,-1)
1.1
```
.
*/
public fun decimal( d : double, max-prec : int = -1 ) : decimal {
	val (man,exp)	= d.decode
	if (exp >= 0) then (man * (2^exp)).decimal else {
		val prec = if (max-prec < 0)  then ~exp else min(max-prec,~exp)
		div( man.decimal, pow(2.decimal, ~exp), prec )
	}
}


// Parse a `:decimal` number.
public fun parse-decimal( s : string ) : maybe<decimal> {
	match(s.trim.find(rx-decimal)) {
		Nothing -> Nothing
		Just(cap) -> {
			val sign	= cap.groups[1]
			val whole	= cap.groups[2]
			val frac	= cap.groups[3]
			val exp 	= ~frac.count + cap.groups[4].parse-int.default(0)
			val f	= decimal-exp( (whole + frac).parse-int.default(0), exp )
			if (sign=="-") then Just(~f) else Just(f)
		}
	}
}
val rx-decimal = regex(@"^([\-\+]?)(\d+)(?:\.(\d*))?(?:[eE]([\-\+]?\d+))?$")


// Ensure a decimal `x` has an exponent such that `x.exp <= e`.
fun expand( x : decimal, e : int ) : decimal {
  if (x.exp <= e) then x else {
    //trace("expand: " + x.show + " to exp " + p.show)
    decimal-exp( x.num.mul-exp10( x.exp - e ), e )
  }
}

// Choose an exponent that minimizes memory usage.
public fun reduce( x : decimal ) : decimal {
  val p   = x.num.exp10?
  if (!p.pos?) return x
  val expp= x.exp + p
  // trace("reduce: x:" + x.showx + ", p:" + p.show + ", expp:" + expp.show)
  if (round-exp(expp)==x.exp) then x else decimal-exp(x.num.cdiv-exp10(p), expp)
}

// Add two decimals.
public fun (+)( x : decimal, y : decimal ) : decimal {
  val e  = min(x.exp,y.exp)
  val xx = x.expand(e)
  val yy = y.expand(e)
  Decimal(xx.num + yy.num, e)
}

// Negate a decimal.
public fun (~)( x : decimal ) : decimal {
  Decimal(~x.num, x.exp)
}

// Subtract two decimals.
public fun (-)( x : decimal, y : decimal ) : decimal {
  x + (~y)
}

// Increment a decimal
public fun inc( x : decimal ) : decimal {
  Decimal(x.num.inc, x.exp)
}

// Decrement a decimal
public fun dec( x : decimal ) : decimal {
  Decimal(x.num.dec, x.exp)
}

// Is this an even decimal?
public fun even?(x : decimal ) : bool {
  x.num.even?
}

// Is this an odd decimal?
public fun odd?( x :decimal ) : bool {
  x.num.odd?
}


// Is this decimal zero?
public fun zero?( x : decimal ) : bool {
  x.num.zero?
}

// Is the decimal positive?
public fun pos?( x : decimal ) : bool {
  x.num.pos?
}

// Is the decimal negative?
public fun neg?( x : decimal ) : bool {
  x.num.neg?
}

// Multiply two decimals with full precision.
public fun (*)( x : decimal, y : decimal ) : decimal {
  val z = decimal-exp(x.num * y.num, x.exp + y.exp)
  if (z.exp.neg?) then z.reduce else z
}

// Decimal to the power of `n`
public fun pow( x : decimal, n : int  ) : decimal {
	val m = n.abs
	val y = decimal-exp(x.num ^ m, x.exp * m )
	if (n.neg?) then div( 1.decimal, y, 3 + m) else y
}

// Rounding modes.
public type round {
  // Round to neareast integer, round to the even number in case of a tie
  con Half-even
  // Round to nearest integer, round towards infinity in case of a tie
  con Half-ceiling
  // Round to nearest integer, round towards negative infinity in case of a tie
  con Half-floor
  // Round to nearest integer, round towards zero in case of a tie
  con Half-truncate
  // Round to nearest integer, round away from zero in case of a tie
  con Half-away-from-zero
  // Round to the minimum integer that is larger or equal
  con Ceiling
  // Round to the maximum integer that is lower or equal
  con Floor
  // Round to the nearest integer towards zero (i.e. _truncate_)
  con Truncate
  // Round to the nearest integer away from zero, i.e. toward negative infinity for negative numbers, and positive infinity for positive numbers.
  con Away-from-zero
}

// Round a `:decimal` number to an integer an optional rounding mode `rnd` (=`Half-even`).
public fun int( x : decimal, rnd : round = Half-even) : int {
  val y = x.round(rnd)  // y.exp >= 0
  if (y.exp.pos?) then y.num.mul-exp10(y.exp) else y.num
}

// Round a `:decimal` number to a whole number with an optional rounding mode (=`Half-even`).
public fun round( x : decimal, rnd : round = Half-even ) : decimal {
  x.round-to-prec(0,rnd)
}

// Round the decimal-point number `x` to
// to a specified number of digits behind the dot `prec` (=`0`) with an optional
// rounding mode `rnd` (=`Half-even`). The precision can be negative.\
// `decimal(1,485).round-to-prec(2).show == "1.48"` \
// `decimal(112,49).round-to-prec(-1).show == "110"`
public fun round-to-prec( x : decimal, prec : int = 0, rnd : round = Half-even ) : total decimal {
  if (x.exp >= ~prec) return x
  val cx = x.reduce
  val p  = ~cx.exp - prec
  if (!p.pos?) return cx  // already less than prec precision

  val (q,r) = cx.num.divmod-exp10(p)

  fun round-half( keep-on-eq : bool ) {
    val half = p.exp10 / 2
    match(compare(r,half)) {
      Eq -> if (keep-on-eq) then q else q.inc
      Gt -> q.inc
      Lt -> q
    }
  }

  val q1 = if (r.zero?) then q else match(rnd) {
	Half-even	-> round-half(q.even?)
	Half-floor	-> round-half(True)
	Half-ceiling	-> round-half(False)
	Half-truncate	-> round-half(q.pos?)
	Half-away-from-zero	-> round-half(q.neg?)
	Floor	-> q
	Ceiling	-> q.inc
	Truncate	-> if (!q.neg?) then q else q.inc
	Away-from-zero	-> if (!q.pos?) then q else q.inc
}

  //trace("round: " + x.showx + ", q: " + q.show + ", r: " + r.show + ", q1: " + q1.show + "e-" + prec.show )
  decimal-exp(q1,~prec)
}

// Optimize: Use double division when within precision bounds.
val maxexp     : int = 308
val maxpd      : double = 1.0e15
val maxprecise : int = int(maxpd)
val minprecise : int = ~maxprecise

fun precise?(i : int ) : bool {
  (i > minprecise && i < maxprecise)
}


/* Divide two decimals with a given extra precision `min-prec` (=`15`).
The `min-prec` is the number of extra digits used to calculate inexact
divisions.

Note: the division uses up to `min-prec` precision using `Floor` rounding
for the last digit if the result is  inexact. To round differently, you can
for example divide with larger precision and use `round-to-prec`.
```
> div( decimal(2), decimal(3), 0 )
0
> div( decimal(2), decimal(3), 1 )
0.6
> div( decimal(2), decimal(3) )  // default precision is 15
0.6666666666666666
> div( decimal(2), decimal(3) ).round-to-prec(6)
0.666667
```
.
*/
public fun div( x : decimal, y : decimal, min-prec : int = 15 ) : decimal {
  if (x.zero? || y.zero?) then return zero
	val e = x.exp - y.exp
	/*
  // use double division if within precision bounds..
  if (min-prec <= 15 && e.abs <= 15 && x.num.precise? && y.num.precise?) {
	val r = x.num.double / y.num.double
  	if (e.zero?) then decimal(r) else decimal(r * exp10(e.double))
  }
  else {
	*/
	// otherwise use long integer division
	val xdigits	= x.num.count-digits
	val ydigits	= y.num.count-digits
	val extra	= max(0,ydigits - xdigits) + min-prec
	if (extra > 0)
		then decimal-exp( x.num.mul-exp10(extra) / y.num, e - extra ).reduce
		else decimal-exp( x.num / y.num, e - extra ).reduce
}

// Divide two decimals using 15 digits of extra precision.
public fun (/)( x : decimal, y : decimal ) : decimal {
  div(x,y)
}


// Compare decimals.
public fun compare( x : decimal, y : decimal ) : order {
  // todo: optimize further to avoid allocations.
  val e  = min(x.exp,y.exp)
  val xx = x.expand(e)
  val yy = y.expand(e)
  compare(xx.num,yy.num)
}


public fun (>) (x : decimal, y : decimal) : bool { compare(x,y) == Gt }
public fun (>=)(x : decimal, y : decimal) : bool { compare(x,y) != Lt }
public fun (==)(x : decimal, y : decimal) : bool { compare(x,y) == Eq }
public fun (!=)(x : decimal, y : decimal) : bool { compare(x,y) != Eq }
public fun (<) (x : decimal, y : decimal) : bool { compare(x,y) == Lt }
public fun (<=)(x : decimal, y : decimal) : bool { compare(x,y) != Gt }

// The sign of a decimal number.
public fun sign( x : decimal ) : order {
  x.num.sign
}

// The minimum of `x` and `y`.
public fun min( x : decimal, y : decimal ) : decimal {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
public fun max( x : decimal, y : decimal ) : decimal {
  if (x >= y) then x else y
}

// Take the sum of a list of decimal numbers (0 for the empty list).
public fun sum( ds : list<decimal> ) : decimal {
  ds.foldr(zero,(+))
}

// The absolute value of a decimal
public fun abs( x : decimal ) : decimal {
  if (x.neg?) then ~x else x
}


// Round a `:decimal` using to the largest integer that is not larger than `x`.
public fun floor( x : decimal ) : decimal {
  x.round(Floor)
}

// Round a `:decimal` to the smallest integer that is not less than `x`.
public fun ceiling( x : decimal ) : decimal {
  x.round(Ceiling)
}

// Truncate a `:decimal` to an integer by rounding towards zero.
public fun truncate( x : decimal ) : decimal {
  x.round(Truncate)
}


// Convert a decimal to a `:double`. This may lose precision.
public fun double( x : decimal ) : double {
  // todo: handle decimals with many digits better
  if (!x.exp.neg?) {
    x.num.double * pow(10.0,x.exp.double)
  }
  else {
    val (q,r) = x.num.divmod-exp10(~x.exp)
    q.double + r.double * exp10(x.exp.double)
  }
}

// Return the 'truncated' fraction, always in the range (`-1.0`,`1.0`).
// `x.truncate + x.fraction == x`
public fun fraction( x : decimal ) : decimal {
  if (!x.exp.neg?) then zero else  (x - x.truncate)
}

// Return the 'floored' fraction, always in the range [`0`,`1.0`).
// `x.floor + x.ffraction == x`
public fun ffraction( x : decimal ) : decimal {
  if (!x.exp.neg?) then zero else  (x - x.floor)
}


/* --------------------------------------------------------------------------------------
    Show
 -------------------------------------------------------------------------------------- */

// Show a decimal `d` with a given precision `prec` (=`-1000`).
// The precision specifies the  number of digits after the dot (in either scientific of fixed-point notation).
// If the precision is negative, _at most_ `prec` digits are displayed, while for a positive
// precision, exactly `prec` digits behind the dot are displayed.
// This uses `show-fixed` when the exponent of `d` in scientific notation is larger than -5
// and smaller than the precision (or 15 in case of a negative precision), otherwise it uses `show-exp`.
public fun show( d : decimal, prec : int = -1000 ) : string {
	val exp = d.exponent?
	if (exp > -5 && exp < (if (prec.neg?) then  15 else prec))
		then show-fixed( d, prec )
		else show-exp( d, prec )
}

// The exponent of a decimal if displayed in scientific notation.\
// `11.2e-1.decimal.exponent? == 0`
public fun exponent?( d : decimal ) : int {
	d.num.count-digits + d.exp - 1
}

/* Show a decimal `d` with a given precision `prec` (=`-1000`) in fixed-point notation.
The precision specifies the  number of digits after the dot.
If the precision is negative, _at most_  `prec` digits after the dot are displayed,
while for a positive precision, exactly `prec` digits are used.
```
> decimal(1,-1).show-fixed
"0.1"
> 0.1.decimal.show-fixed
"0.1000000000000000055511151231257827021181583404541015625"
> 0.1.decimal.show-fixed(20)
"0.1000000000000000555"
> 0.1.decimal.show-fixed(-20)
"0.1000000000000000555"
> decimal(1,-1).show-fixed(20)
"0.1000000000000000000"
```
.
*/
public fun show-fixed( d : decimal, prec : int = -1000 ) : string {
	val x = d.round-to-prec(prec.abs)
	if (x.exp >= 0) {
		val frac = if (prec<=0) then "" else "." + repeat("0",prec)
		x.num.show + repeat("0",x.exp) + frac
	}
	else {
		val digits	= ~x.exp
		val sign	= if (x.neg?) then "-" else ""
		val i         	= x.num.abs
		val man	= cdiv-exp10(i,digits)
		val frac	= i - mul-exp10(man,digits)
		sign + man.show + frac.show.pad-left(digits,'0').show-frac(prec)
	}
}

/* Show a decimal `d` with a given precision `prec` (=`-1000`) in scientific notation.
The precision specifies the  number of digits after the dot, i.e.
the number of significant digits is `prec+1`.
If the precision is negative, _at most_ `prec` digits are displayed, and if
it is positive exactly `prec` digits are used.
```
> decimal(1,-1).show-exp
"1e-1"
> 1.1.decimal.show-exp
"1.100000000000000088817841970012523233890533447265625"
> 1.1.decimal.show-exp(-20)
"1.10000000000000008882"
> 0.125.decimal.show-exp(-20)
"1.25e-1"
> 0.125.decimal.show-exp(20)
"1.25000000000000000000e-1"
```
.
*/
public fun show-exp( d : decimal, prec : int = -1000 ) : string {
	val x 	= d.round-to-prec(prec.abs - d.exponent?)
	val s	= x.num.abs.show
	val digits	= s.count
	val exp	= x.exp + digits - 1
	val sign	= if (x.neg?) then "-" else ""
	val exponent	= if (exp==0) then "" else "e" + (if (exp.pos?) then "+" else "") + exp.show
	sign + s.head + s.tail.show-frac(prec) + exponent
}

fun show-frac( frac : string, prec : int ) : string {
	val frac-trim	= frac.trim-right("0")
	val frac-full	=if (prec >= 0) then frac-trim.pad-right(prec,'0') else frac-trim
	if (frac-full.empty?) then "" else "." + frac-full
}
