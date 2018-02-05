public import std/num/int32
public import std/num/double
public import std/num/decimal




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
