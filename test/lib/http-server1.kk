module http-server1

pub import std/async
pub import std/net/http
pub import std/time/duration

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
	fn(exn) {
		println("exn happened: " ++ exn.show)
	}
}
