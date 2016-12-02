/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Read user input from the console.
*/
module std/os/readline

external include {
  js file "readline-inline.js"
}

// Displays `query` to the user (with a space attached) and issues a `readline` for the user response.
public fun question( query : string ) : <async,console> string {
  print(query + " ")
  readline()
}

// Read a line of input from the user.
public extern readline() : <async,console> string {
  js inline "(function(){ _readline( $std_core._async_callback1(_k) ); return $std_core.async_yield(); })()"
}