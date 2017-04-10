/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Read user input from the console.
*/
module std/os/readline

import std/async

external include {
  js file "readline-inline.js"
}

// Displays `query` to the user (with a space attached) and issues a `readline` for the user response.
public fun question( query : string ) : <async,console,exn> string {
  print(query + " ")
  readline()
}

// Read a line of input from the user.
public fun readline() : <async,console,exn> string {
  on-cancel( prim-cancel-readline, { unsafe-async-yield( fun(cb) { prim-readline(cb) } ) } )
}

extern prim-readline( cb : (string) -> <console|e> () ) : console () {
  js "_readline"
}

extern prim-cancel-readline() : exn () {
  js "_cancel_readline"
}

// Read a line of input from the user.
extern xreadline() : <async,console> string {
  js inline "(function(){ _readline( $std_core._async_callback1(_k) ); return $std_core.async_yield(); })()"
}