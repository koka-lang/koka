public import std/num/int32
public import std/num/double
public import std/num/decimal

fun denormal?( d :double ) : bool  {
	(d.abs < dbl-min)
}

fun show-hex( d : double, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string {
	if (!d.finite?) then d.show else {
		val (m,e)	= d.decode
		val man	= m.abs.show-hex(1,use-capitals,"")
		val (hd,frac0)	= (man.head,man.tail)
		val exp0    	= if (m==0) then 0 else e + 4*frac0.count
		val exp	= (if (exp0 >= 0) then "+" else "") + exp0.show
		val frac	= frac0.trim-right("0").pad-right(max(1,width),'0')
		val sign	= if (d.neg? || d.negzero?) then "-" else ""
		sign + pre + hd + "." + frac + "p" +  exp
	}
}

fun encodex( man : int, exp : int ) : double {
	ldexp(man.double,exp)
}

fun show( xy : (int,int) ) : string {
	xy.fst.show + "p" + xy.snd.show
}

fun main() {
	val pnums	= [	-0.0, 0.0, dbl-true-min, dbl-min, 1.0e-308,
			0.1, 0.1+0.2, 0.3, 1.0, 2.0, 1.0e308, exp2(1020.0), dbl-max,posinf,nan ]
	val nums	= pnums + pnums.map((~))
	nums.foreach fun(d){
		val (man,exp) = d.decode
		println( d.show.pad-left(25) + " = " + d.decode.show + " -> " + encode(man,exp).show + "\n  digits: " + d.decimal.show + "\n")
	}
}

fun negzero?( d : double ) : bool {
	(d==0.0 && neginf?(1.0 / d))
}
