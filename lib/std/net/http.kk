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
type status-code {
	Inform-continue	//100
	Success-ok	//200
	Success-created	//201
	Success-accepted	//202
	Success-no-content	//204
	Success-reset-content	//205
	Success-partial-content	//206
	Redirect-multiple-choices	//300
	Redirect-see-other	//303
	Redirect-not-modified	//304
	Redirect-temporary	//307
	Redirect-permanent 	//308
	Error-bad-request	//400
	Error-unauthorized	//401
	Error-payment-required	//402
	Error-forbidden	//403
	Error-not-found	//404
	Error-method-not-allowed	//405
	Error-not-acceptable	//406
	Error-proxy-authenticate	//407
	Error-request-timeout	//408
	Error-conflict	//409
	Error-gone	//410
	Error-length-required	//411
	Error-precondition	//412
	Error-payload-too-large	//413
	Error-uri-too-long	//414
	Error-unsupported-media	//415
	Error-range	//416
	Error-expectation	//417
	Error-misdirected	//421
	Error-upgrade	//426
	Error-too-many-requests	//429
	Error-legal	//451
	Server-internal	//500
	Server-not-implemented	//501
	Server-bad-gateway	//502
	Server-unavailable	//503
	Server-gateway-timeout	//504
	Server-http-version	//505
	Server-authentication	//511
	Status-any( : int )
}


fun int( status-code : status-code ) : int {
	match(status-code) {
		Inform-continue	-> 100
		Success-ok	-> 200
		Success-created	-> 201
		Success-accepted	-> 202
		Success-no-content	-> 204
		Success-reset-content	-> 205
		Success-partial-content	-> 206
		Redirect-multiple-choices	-> 300
		Redirect-see-other	-> 303
		Redirect-not-modified	-> 304
		Redirect-temporary	-> 307
		Redirect-permanent 	-> 308
		Error-bad-request	-> 400
		Error-unauthorized	-> 401
		Error-payment-required	-> 402
		Error-forbidden	-> 403
		Error-not-found	-> 404
		Error-method-not-allowed	-> 405
		Error-not-acceptable	-> 406
		Error-proxy-authenticate	-> 407
		Error-request-timeout	-> 408
		Error-conflict	-> 409
		Error-gone	-> 410
		Error-length-required	-> 411
		Error-precondition	-> 412
		Error-payload-too-large	-> 413
		Error-uri-too-long	-> 414
		Error-unsupported-media	-> 415
		Error-range	-> 416
		Error-expectation	-> 417
		Error-misdirected	-> 421
		Error-upgrade	-> 426
		Error-too-many-requests	-> 429
		Error-legal	-> 451
		Server-internal	-> 500
		Server-not-implemented	-> 501
		Server-bad-gateway	-> 502
		Server-unavailable	-> 503
		Server-gateway-timeout	-> 504
		Server-http-version	-> 505
		Server-authentication	-> 511
		Status-any( code ) 	-> code
	}
}

fun status-code( code : int ) : status-code {
	match(code) {
		100	-> Inform-continue
		200	-> Success-ok
		201	-> Success-created
		202	-> Success-accepted
		204	-> Success-no-content
		205	-> Success-reset-content
		206	-> Success-partial-content
		300	-> Redirect-multiple-choices
		303	-> Redirect-see-other
		304	-> Redirect-not-modified
		307	-> Redirect-temporary
		308	-> Redirect-permanent
		400	-> Error-bad-request
		401	-> Error-unauthorized
		402	-> Error-payment-required
		403	-> Error-forbidden
		404	-> Error-not-found
		405	-> Error-method-not-allowed
		406	-> Error-not-acceptable
		407	-> Error-proxy-authenticate
		408	-> Error-request-timeout
		409	-> Error-conflict
		410	-> Error-gone
		411	-> Error-length-required
		412	-> Error-precondition
		413	-> Error-payload-too-large
		414	-> Error-uri-too-long
		415	-> Error-unsupported-media
		416	-> Error-range
		417	-> Error-expectation
		421	-> Error-misdirected
		426	-> Error-upgrade
		429	-> Error-too-many-requests
		451	-> Error-legal
		500	-> Server-internal
		501	-> Server-not-implemented
		502	-> Server-bad-gateway
		503	-> Server-unavailable
		504	-> Server-gateway-timeout
		505	-> Server-http-version
		511	-> Server-authentication
		_ 	-> Status-any(code)
	}
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
public fun serve-to-channel( server : http-server, port : int = 0, hostname : string = "") : <asyncx,net> channel<try<(request-object,response-object)>> {
	val ch = channel()
	fun setup(  cb : (try<(request-object,response-object)>,bool) -> io () ) : io maybe<() -> io ()>  {
		fun cbx( reqn : null<request-object>, res ) {
			match(reqn.maybe) {
				Nothing  	-> { println("close or cancel the http server channel"); cb(Exn(unsafe-cancel-exn()), True) }
				Just(req)	-> cb(Ok((req,res)), False)
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


public fun listen( ch : channel<try<(request-object,response-object)>>, action : () -> <asyncx, net, div,console|e> http-response, concurrency : int = 100 ) : <asyncx,net,div,console|e> () {
	var current := 0
	val strands = list(1,max(1,min(500,concurrency))).map fun(idx) {
		return fun() {
			while { True } {
				val resreq = ch.receive.untry
				println("request on " + idx.show + ", current concurrency: " + current.show)
				resreq.handle-request {
					if (current >= concurrency - 1) {
						println("request denied, too many connections")
						error-response(503, "Too many connections")
					}
					else {
						current := current + 1
						finally(inject-st(action)) { current := current - 1 }
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
