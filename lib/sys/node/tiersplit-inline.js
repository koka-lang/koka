/*---------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
  We define these functions for both a server and client
---------------------------------------------------------------------------*/

var $prim_nodejs_socket = function(){};
var $prim_nodejs_rpc = function(){};
var $prim_nodejs_moebius = function(){};
var domReady = function(f) { f(); };

/*---------------------------------------------------------------------------
  Defaults
---------------------------------------------------------------------------*/
var defaultLocal = "./out/lib";
var defaultPort = 8080;
var defaultUrl = '127.0.0.1';

/*---------------------------------------------------------------------------
  Code that runs on the server (nodejs)
---------------------------------------------------------------------------*/
var onserver = (typeof window === 'undefined' || !window.document);
if (onserver) 
{
  var http = require('http');
  var node_static = require('node-static');
  var websocket = require('ws');

  domReady = function(f) { f(); };

  $prim_nodejs_socket = function(node_global, node_session, node_client, nodeLocal, nodeUrl, nodePort)
  { 
    if (nodeLocal==null) nodeLocal = defaultLocal;
    if (nodeUrl == null) nodeUrl = defaultUrl;
    if (nodePort == null) nodePort = defaultPort;
    var node_global_return = node_global();

    // Setting up serving the client part via normal http requests
    var fileServer = new(node_static.Server)(nodeLocal);
    var httpServer = http.createServer( 
      function (request, response) {
        fileServer.serve(request, response);
      });
    httpServer.listen(nodePort,nodeUrl);

    // Setting up the websocket connection handler
    var serverPort = (parseInt(nodePort) + 1).toString();
    var socketServer = new websocket.Server( { port: serverPort } );
    var sessionHandler = function(websocket) {
      var messageHandler = { handler: function(msg) {} };
      websocket.on( 'message', function(msg) { messageHandler.handler(msg); } );
      var socket = { 
        send: function(obj) { websocket.send(JSON.stringify(obj)); },
        receive: function() { 
          var async = { on: function(val) {} };
          messageHandler.handler = function(msg) {
            if(async.value == null) {
              async.value = JSON.parse(msg);
              async.on(async.value);
            } 
          };
          return async;
        },
        set_onclose: function(handler) {}
      };
      node_session(socket, node_global_return);
    };
    socketServer.on( 'connection', sessionHandler );
  } 

  $prim_nodejs_rpc = function(server_code, client_code, nodeLocal, nodeUrl, nodePort )
  { 
    if (nodeLocal==null) nodeLocal = defaultLocal;
    if (nodeUrl == null) nodeUrl = defaultUrl;
    if (nodePort == null) nodePort = defaultPort;
    
    var server_handlers = server_code();
    var init            = server_handlers.serverSessionInit;
    var interact        = server_handlers.serverSessionInteract;

    // Setting up serving the client part via normal http requests
    var fileServer = new(node_static.Server)(nodeLocal);
    var httpServer = http.createServer( 
      function (request, response) {
        fileServer.serve(request, response);
      });
    httpServer.listen(nodePort,nodeUrl);

    // Setting up the websocket connection handler
    var serverPort = (parseInt(nodePort) + 1).toString();
    var socketServer = new websocket.Server( { port: serverPort } );
    var sessionHandler = function(websocket) {
      var server_remote = init();
      websocket.on( 'message', function(msg) { server_remote(JSON.parse(msg)); } );
      interact( function(cmd) { websocket.send(JSON.stringify(cmd)); } );
    };
    socketServer.on( 'connection', sessionHandler );
  }

  $prim_nodejs_moebius = function(server_code, client_code, nodeLocal, nodeUrl, nodePort )
  { 
    if (nodeLocal==null) nodeLocal = defaultLocal;
    if (nodeUrl == null) nodeUrl = defaultUrl;
    if (nodePort == null) nodePort = defaultPort;
    var server_session = server_code();

    // Setting up serving the client part via normal http requests
    var fileServer = new(node_static.Server)(nodeLocal);
    var httpServer = http.createServer( 
      function (request, response) {
        fileServer.serve(request, response);
      });
    httpServer.listen(nodePort,nodeUrl);

    // Setting up the websocket connection handler
    var serverPort = (parseInt(nodePort) + 1).toString();
    var socketServer = new websocket.Server( { port: serverPort } );                              
    var sessionHandler = function(websocket) {
      var client_remote = function(cmd) { websocket.send(JSON.stringify(cmd)); };
      var server_remote = server_session(client_remote);
      websocket.on( 'message', function(msg) { server_remote(JSON.parse(msg)); } );
    };
    socketServer.on( 'connection', sessionHandler );
  }
}

/*---------------------------------------------------------------------------
  Code that runs on the client (browser)
---------------------------------------------------------------------------*/
else {

  // create client side web sockets
  var websocket = function( port ) 
  {
    if (port==null) port = defaultPort;
    var serverPort = (parseInt(port)+1).toString();
    var url = "ws://" + window.location.hostname + ":" + serverPort;
    // console.log("websocket: trying to connect to " + url);we
    var socket;
    if(window.WebSocket) {
      // console.log("websocket: assuming standard websocket object");
      socket = new WebSocket(url);
    } 
    else if(window.MozWebSocket) {
      // console.log("websocket: assuming mozilla websocket object");
      socket = new MozWebSocket(url);
    } 
    else {
      throw "this browser version does not support websockets"
    }

    return { 
      send: function(obj) { socket.send(JSON.stringify(obj)); },
      receive: function() { 
        var async = { on: function() { console.log("websocket: dropped message (missing handler)"); } };
        socket.onmessage = function(msg) {
          // console.log("websocket: socket received message");
          // console.log(msg);
          if(async.value==null) {
            async.value = JSON.parse(msg.data);
            async.on(async.value);
          }
        };
        return async;
      },                                     
      set_onopen: function(f) { socket.onopen = function(x) { f(x); } },
      set_onclose: function(f) { socket.onclose = function(x) { f(x); } },
      listen: function(f) { socket.onmessage = function(msg) { f(JSON.parse(msg.data)); } },
    }
  };

  var domReady = (function() {
    var onReady = [];
    var isReady = false;

    function domDone() {
      if (isReady) return;
      isReady = true;
      for( var i in onReady) {
        onReady[i]();
      }
      onReady = [];
    }

    function domReady(f) {
      if (isReady) {
        f();
      }
      else {
        onReady.push(f);
      }
    }

    if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", function() { alert("hi") }, false);
      window.addEventListener("load", domDone, false);
    } 
    else if (window.attachEvent) {
      window.attachEvent("onload", domDone);
    }
    else {
      // just pretend the dom is loaded...
      domDone;
    }
    // check if the document is already loaded, 
    if (document.readyState === "complete") {
        domDone();
    }

    return domReady;
  })();

  $prim_nodejs_socket = function(node_global, node_session, node_client, nodeLocal, nodeUrl, nodePort ) 
  { 
    if (nodePort==null) nodePort = defaultPort;
    var socket = websocket(nodePort);
    socket.set_onopen( function() { node_client(socket); } );
  }

  $prim_nodejs_rpc = function(server_code, client_code, nodeLocal, nodeUrl, nodePort ) 
  { 
    if (nodePort==null) nodePort = defaultPort;
    domReady( function() {
      var client_handlers = client_code();
      var init            = client_handlers.clientSessionInit;
      var interact        = client_handlers.clientSessionInteract;
      var client_remote   = init();
      var socket          = websocket(nodePort);
      socket.set_onopen( function() { 
        socket.listen( function(obj) { client_remote(obj); });
        var server_remote = socket.send;
        interact(server_remote);
      });
    });
  }

  $prim_nodejs_moebius = function(server_code, client_code, nodeLocal, nodeUrl, nodePort ) 
  { 
    if (nodePort==null) nodePort = defaultPort;
    domReady( function() {
      var client_session  = client_code(); // (server-remote) -> client-ctx client-remote 
      var socket = websocket(nodePort);
      socket.set_onopen( function() {
        var server_remote = socket.send;
        var client_remote = client_session(server_remote);
        socket.listen( function(obj) {
          client_remote(obj);
        });
      });
    });
  }
} 
