/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Read user input from the console.

   test `std/async` and `module std/time/instant`.
*/
module std/async/readline

import std/async

extern import {
  cs file "readline-inline.cs"
  js file "readline-inline.js" 
}

// Displays `query` to the user (with a space attached) and issues a `readline` for the user response.
pub fun question( query : string ) : <asyncx,console> string {
  print(query ++ " ")
  readline()
}

// Read a line of input from the user.
pub fun readline() : <asyncx,console> string {
  await-exn1 fn(cb) {
    val id = prim-readline(cb)
    Just( { prim-cancel-readline(id) })
  }
}

extern prim-readline( cb : (null<exception>,string) -> io-noexn () ) : io any {
  cs "_Readline.Readline"
  js "_readline"
}

extern prim-cancel-readline( id : any ) : io-noexn () {
  cs "_Readline.CancelReadline"
  js "_cancel_readline"
}
