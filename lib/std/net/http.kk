/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* HTTP functionality

*/
module std/net/http

import std/async

external import {
	js "http"
}

external include {
	js file "http-inline.js"
}

// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------

abstract type http-server
abstract type request-object
abstract type response-object

alias http-server-channel = channel<either<exception,(request-object,response-object)>>

public fun http-server( port : int = 0, hostname : string = "", concurrency : int = 100, action : () -> <asyncx,net,div,console|e> http-response ) : <asyncx,net,div,console|e> () {
	with-http-server fun(server) {
		server.serve-to-channel(port,hostname).listen( action, concurrency )
	}
}

public fun with-http-server( action : (server : http-server) -> <net,ndet|e> a  ) : <net,ndet|e> a {
	val server = http-create-server()
	on-exit { server-close(server) } { action(server) }
}

// Listen to an HTTP server on an optional port and hostname.
public fun serve-to-channel( server : http-server, port : int = 0, hostname : string = "") : <asyncx,net> channel<either<exception,(request-object,response-object)>> {
	val ch = channel()
	fun setup(  cb : (either<exception,(request-object,response-object)>,bool) -> io () ) : io maybe<() -> io ()>  {
		fun cbx( reqn : null<request-object>, res ) {
			match(reqn.maybe) {
				Nothing	-> { println("close or cancel the http server channel"); cb(Left(cancel-exn), True) }
				Just(req)	-> cb(Right((req,res)), False)
			}
		}
		server-listen( server, port.null, hostname.null, 0.null, cbx )
		Nothing
	}
	await-to-channel( setup, ch, id )
}

external server-listen( server : http-server, port : null<int>, hostname : null<string>, backlog : null<int>, cb : (null<request-object>,response-object) -> io () ) : io () {
	js "_server_listen"
}

// Create an HTTP server.
external http-create-server() : <net,ndet> http-server {
	js "http.createServer"
}

external server-close( server : http-server ) : <net> () {
	js "_server_destroy"
}

public struct http-response(
	status-code : int = 200,
	status-message : string = "",
	headers : list<(string,string)> = [],
	body : string = ""
)

public fun html-response( body : string ) : http-response {
	text-response(body).add([("Content-Type","text/html; charset=utf-8")])
}

public fun error-response( status-code : int, message : string ) {
	(text-response(message))(status-code = status-code )
}

public fun text-response( body : string ) : http-response {
	val headers = [
		("Content-Type","text/plain; charset=utf-8"),
		("Content-Length",utf8-length(body).show),
		("Content-Encoding","UTF-8")
	]
	Http-response( headers = headers, body = body )
}

public fun add( res : http-response, extra-headers : list<(string,string)> ) : http-response {
	res( headers = res.headers + extra-headers )
}


public fun listen( ch : channel<either<exception,(request-object,response-object)>>, action : () -> <asyncx, net, div,console|e> http-response, concurrency : int = 100 ) : <asyncx,net,div,console|e> () {
	val strands = list(1,max(1,min(500,concurrency))).map fun(idx) {
		return fun() {
			while { True } {
				match(ch.receive) {
					Left(exn) -> {
						println("exn on " + idx.show + ": " + exn.show)
						throw(exn)
					}
					Right(resreq) -> {
						println("request on " + idx.show)
						resreq.handle-request( action )
					}
				}
			}
		}
	}
	interleaved(strands)
	()
}

public fun handle-request( request : (request-object,response-object), action : () -> <asyncx, net, console| e> http-response ) : <asyncx,net,console | e> () {
	val (_reqobj,resobj) = request
	val res =	catch(action) fun(exn) {
			println("exn thrown in request handler: " + exn.show)
			error-response( 500, exn.show )
		}
	res.headers.foreach fun(header) {
		resobj.response-write-header( header.fst, header.snd )
	}
	resobj.response-write-head(res.status-code, res.status-message.null )
	if (res.body.notempty?) resobj.response-write-text( res.body )
	resobj.response-end()
}


external response-write-head( res : response-object, status : int, message : null<string> ) : <net,exn> () {
	js inline "res.writeHead(status,message)"
}

external response-write-header( res : response-object, header : string, content : string ) : <net,exn> () {
	js inline "res.setHeader(header,content)"
}

external response-write-text( res : response-object, chunk : string ) : <net,exn> () {
	js inline "res.write(chunk)"
}

external utf8-length( s : string ) : int {
	js inline "Buffer.byteLength(s)"
}

external response-end( res : response-object ) : <net,exn> () {
	js inline "res.end()"
}
