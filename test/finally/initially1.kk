// Test initially

fun bar() {
	println("action");
	42
}

fun foo()
{
	handle(bar) (l : int) {
		initially -> {
			println("initially")
			0
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
	foo().println
}
