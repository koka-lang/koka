public import std/num/int32
public import std/num/double
public import std/num/decimal


fun show-hex( d : double, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string {
	if (!d.finite?) then d.show else {
		val (m,e)	= d.decode
		val man	= m.abs.show-hex(1,use-capitals,"")
		val exp0    	= e + 4*(man.count - 1)
		val exp	= (if (exp0 >= 0) then "+" else "") + exp0.show
		val frac	= man.tail.trim-right("0").pad-right(max(1,width),'0')
		val sign	= if (d.neg? || d.negzero?) then "-" else ""
		sign + pre + man.head + "." + frac + "p" +  exp
	}
}

fun show( xy : (int,int) ) : string {
	xy.fst.show-hex + "p" + xy.snd.show
}

fun show( xy : (double,int) ) : string {
	xy.fst.show + "e" + xy.snd.show
}
fun main() {
	val pnums	= [	-0.0, 0.0, dbl-true-min, dbl-min, 1.0e-308,
			0.1, 0.1+0.2, 0.3, 1.0, 2.0, 1.0e308, exp2(1020.0), dbl-max,posinf,neginf,nan ]
	val nums	= pnums // + pnums.map((~))
	nums.foreach fun(d){
		val (man,exp) = d.decode
		println( d.show + "= " + d.show-hex + " == " + d.decode.show + " ->" + encode(man,exp).show)
	}
	// nums.foreach fun(d) { println( d.show + ", digits: " + d.decimal.show + "\n")	}
}
