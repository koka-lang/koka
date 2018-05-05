public module http-server1

public import std/async
public import std/net/http
public import std/time/duration

fun main() : <async,io> () {
	try {
		timeout(15.seconds) {
			http-server(80,"localhost",3) {
				println("request received!")
				//error("ouch!")
				wait(4.seconds)
				html-response( "<html><body>Hello world!</body></html>" )
			}
		}
		println("end of program")
		()
	}
	fun(exn) {
		println("exn happened: " + exn.show)
	}
}
