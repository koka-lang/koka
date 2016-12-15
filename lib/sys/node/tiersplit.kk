/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// An experimental module to try out tierSplitted applications.
module tiersplit

import sys/dom/types

external include {
  js file "tiersplit-inline.js"
}

public alias nodeServer       = io
public alias nodeClient       = <dom, div>

// Exit the process with a specified exit code.
public external exit : (code : int) -> io () {
  js inline "process.exit(#1)"
}

// A socket that sends messages of type ":a" and receives messages of type ":b"
public type socket<a,b>

// Send data over a socket
public external inline send : forall<a,b>( socket<a,b>, a  ) -> net ()
{ 
  js inline "(#1).send(#2)"
}

// Receive data from a socket
public external inline receive : forall<a,b,e>( socket<a,b> ) -> net async<b>
{ 
  js inline "(#1).receive()"         
}

// A channel to broadcast messages over
public type broadcast<a>

// we don't use 'external import' here so it is only imported on the node server
// but 'ignored' on the browser
external include {
  js "var events = (typeof window === 'undefined' ? require('events') : null);"
}

// Create a broadcast channel
public external createBroadcast : forall<a> () -> broadcast<a>
{ js inline "new events.EventEmitter()" }

public external emit             : forall<a>   ( broadcast<a>, a )           ->   ()
{ js inline "#1.emit('broadcast',#2)" }

public external listen           : forall<a,e> ( broadcast<a>, (a) -> e () ) -> e ()
{ js inline "#1.on('broadcast',#2)" }


// Socket based tiersplitting
public external nodejsSocket( nodeGlobal: () -> io c,
                               nodeSession: ( socket<a,b>, c ) -> nodeServer (),
                               nodeClient:  ( socket<b,a> )    -> nodeClient (),
                               nodeLocal : string = "./out/lib",
                               nodeUrl: string = "127.0.0.1",
                               nodePort: string = "8080" ) : io () 
{ js "$prim_nodejs_socket" } 

// RPC based tierSplitting

public struct serverHandlers<s,c>( 
  serverSessionInit: () -> nodeServer ( s -> nodeServer () ),
  serverSessionInteract: ( c -> nodeServer () )  -> nodeServer ()
)

public struct clientHandlers<s,c>( 
  clientSessionInit: () -> nodeClient ( c -> nodeClient () ),
  clientSessionInteract: ( s -> nodeClient () ) -> nodeClient ()
)

// RPC based tierSplitting
public external nodejsRpc(  serverCode: () -> nodeServer serverHandlers<s,c>,
                             clientCode: () -> nodeClient clientHandlers<s,c>,
                             nodeLocal : string = "./out/lib",
                             nodeUrl: string = "127.0.0.1",
                             nodePort: string = "8080" ) : io () 
{  js "$prim_nodejs_rpc" } 


// moebius tierSplitting
public external nodejsMoebius(  serverCode: () -> nodeServer (( c -> nodeServer () ) -> nodeServer ( s -> nodeServer () )),
                                 clientCode: () -> nodeClient (( s -> nodeClient () ) -> nodeClient ( c -> nodeClient () )),
                                 nodeLocal : string = "./out/lib",
                                 nodeUrl: string = "127.0.0.1",
                                 nodePort: string = "8080" ) : io () 
{ js "$prim_nodejs_moebius" } 
