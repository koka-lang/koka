public import std/num/int32
public import std/num/double
public import std/num/decimal


fun decode( d : double ) : (int,int) {
	val (lo,hi)	= double-to-bits(d)
	val sign 	= if (hi.neg?) then -1 else 1
	val exp0	= (hi.shr(20) & 0x7FF.int32).int
	val man0	= (hi & 0xFFFFF.int32).int
	val exp	= if (exp0==0) then -1042 else (exp0  - 1043)
	val man	= if (exp0==0) then man0 else man0 + 0x100000
	if (lo != zero)
		then (sign*(man * 0x100000000 + lo.uint), exp - 32)
		else (sign*man, exp)
}

fun encode( man : int, exp : int ) : double {
	ldexp(man.double,exp)
}

fun decimal-exact( d : double, max-prec : int = 17 ) : decimal {
	val (man,exp) = d.decode
	if (exp.neg?)
		then div( man.decimal, pow(2.decimal, ~exp), min(max-prec,~exp) )
		else (man * (2^exp)).decimal
}

fun show( xy : (int,int) ) : string {
	xy.fst.show + "p" + xy.snd.show
}

fun main() {
	val pnums	= [	-0.0, 0.0, dbl-true-min, dbl-min, 1.0e-308,
			0.1, 0.1+0.2, 0.3, 1.0, 2.0, 1.0e308, exp2(1020.0), dbl-max,posinf,nan ]
	val nums	= pnums + pnums.map((~))
	nums.foreach fun(n){
		val (man,exp) = n.decode
		println( n.show.pad-left(25) + " = " + decode(n).show + " -> " + encode(man,exp).show + "\n  digits: " + n.decimal-exact.show + "\n")
	}
}

fun negzero?( d : double ) : bool {
	(d==0.0 && neginf?(1.0 / d))
}
