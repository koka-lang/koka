// Test initially
effect amb {
	fun flip() : bool
}

val amb = handler {
	return x -> [x]
	fun flip() -> resume(True) + resume(False)
}

fun bar() {
	println("action: " + flip().show );
	42
}

fun foo()
{
	handle(bar) (l : int) {
		initially -> {
			match(l) {
				Nothing	-> { println("initially"); 0 }
				Just(i)	-> { println("initially: " + i.show); i+1 }
			}
		}
		return x -> {
			println("return, local=" + l.show)
			x
		}
		finally  -> {
			println("finally, local=" + l.show)
		}
	}
}

fun main() {
	amb(foo).map(show).join(",").println
}
