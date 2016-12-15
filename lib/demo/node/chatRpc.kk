/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

import sys/node/tiersplit


type clientCommand { 
  Display( message: string ) 
}

type serverCommand { 
  Utter( message : string ) 
  SetUsername( username : string ) 
}

fun log( s : string ) {
  trace( "[log] " + s );
}

public fun main(): io ()
{ 
  fun serverCode() : nodeServer serverHandlers<serverCommand, clientCommand>           
  { 
    log("rpc server listening on localhost:8080")
    val broadcast = createBroadcast()
        
    // exit node server if the user presses enter
    println("press enter to exit..")
    readln() fun(s) {
      exit(0);
    }

    fun init()
    { 
      log("`serverCode.init`") 
      var username := "anonymous"

      // return session handler
      return fun(c) {
        match(c) { 
          Utter(message) -> { 
            broadcast.emit( username + ": " + message )
          }
          SetUsername(name)  -> { 
            broadcast.emit( "*** `" + username + "` is now known as `" + name + "`" )
            username := name
          }
        }
      }
    }

    fun interact(client)
    { 
      log("`serverCode.interact`")
      Display("|  Welcome").client
      Display("|  (use `/nick` to change your username)\n").client
      broadcast.listen fun(s) {
        Display(s).client 
      }
    }

    return ServerHandlers( init, interact )
  }

  fun clientCode() : nodeClient clientHandlers<serverCommand, clientCommand> 
  { 
    fun init() { 
      log("`clientCode.init`")      
      return fun(c) {
        match(c) {
          Display( message ) -> println( message ) 
        }
      }
    }

    fun interact(server) { 
      log("`clientCode.interact`")
      fun loop() 
      {
        readln() fun(input) { 
          if( input.startsWith("/") ) {
            if( input.startsWith("/nick ") ) {
              SetUsername( input.substr(6) ).server
            } 
            else {
              println( ">>> unknown command " )
            }
          } 
          else {
            Utter(input).server
          }
          loop()
        }
      }
      loop()
      return ()
    }

    return ClientHandlers( init, interact )
  }

  nodejsRpc( serverCode, clientCode )
}
