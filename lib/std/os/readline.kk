/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/* Read input from stdin.
*/
module std/os/readline

extern import
  c file "readline-inline.c"
  js file "readline-inline.js" 


// Read a line of input synchronously from stdin (using UTF8 encoding).
// Read chararacters until either a newline is encountered (not included in the result),
// or 1023 characters have been read.
public fun readline() : <console,exn> string
  match readline-err() 
    Error(exn)  -> Error(exn.prepend("unable to read from stdin")).throw
    Ok(line)    -> line


private fun prepend( exn : exception, pre : string ) : exception
  Exception(pre ++ ": " ++ exn.message, exn.info)


extern readline-err() : console error<string>
  c "kk_os_read_line_error"  
  js "sync_readline_err"
