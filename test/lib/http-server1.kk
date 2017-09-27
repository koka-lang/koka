public module http-server1

public import std/async
public import std/net/http
public import std/time/duration

fun main() : <async,io> () {
	catch {
		timeout(10.seconds) {
			http-server(80,"localhost",10) {
				println("request received!")
				error("ouch!")
				html-response( "<html><body>Hello world!</body></html>" )
			}
		}
		()
	}
	fun(exn) {
		println("exn happened: " + exn.show)
	}
}
