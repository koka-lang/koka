// Test initially
effect amb {
	fun flip() : bool
}

val amb = handler {
	return x -> [x]
	fun flip() -> resume(True) + resume(False)
}

effect state {
	fun inc() : int
}

fun bar() : _ int {
	println("action: " + flip().show );
	inc();
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
			println("return: " + (x:int).show + ", local=" + l.show)
			x
		}
		finally  -> {
			println("finally, local=" + l.show)
		}
		fun inc() -> {
			resume(l,l+1)
		}
	}
}

fun main() {
	amb(foo).map(show).join(",").println
}
