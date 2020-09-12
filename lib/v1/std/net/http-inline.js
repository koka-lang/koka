/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* HTTP functionality

*/
var _socket_id = 0;

function _server_listen( server, port, hostname, backlog, cb ) {
	server.on('close' ,function(){ cb(null,null); });
	server.on('request', cb);
	server._sockets = {};
	server.on('connection', function(socket) {
		if (server._sockets) {
			const id = _socket_id++;
			server._sockets[id] = socket;
			socket.on('close', function() {
				if (server._sockets) delete server._sockets[id];
			});
		}
	});
	server.listen(port,hostname,backlog,null);
}

function _server_close( server ) {
	server.close( function() { server._sockets = null; } );
}

function _server_destroy( server ) {
	server.close();
	if (server._sockets) {
		for (var id in server._sockets ) {
			server._sockets[id].destroy();
		}
	}
}
