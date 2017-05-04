/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Read user input from the console. 

   test `std/async` and `module std/time/instant`.
*/
module std/os/readline

import std/async

external include {
  js file "readline-inline.js"
}

// Displays `query` to the user (with a space attached) and issues a `readline` for the user response.
public fun question( query : string ) : <async,exn,console> string {
  print(query + " ")
  readline()
}

// Read a line of input from the user.
public fun readline() : <async,exn,console> string {
  on-cancel( {
    trace("cancel readline")
    prim-cancel-readline()
  }, { 
    await-exn1( fun(resume) { 
      prim-readline(resume) 
    }) 
  })
}

extern prim-readline( cb : (null<exception>,string) -> io () ) : io () {
  js "_readline"
}

extern prim-cancel-readline() : exn () {
  js "_cancel_readline"
}