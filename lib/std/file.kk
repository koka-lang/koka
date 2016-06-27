/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File operations. 
*/
public module std/file

extern import {
  js "fs"
}

// Read a text file (using UTF8 encoding) asynchronously
extern read-text-file( fname : string ) : <async,exn> string {
  js inline "(function(){ fs.readFile(fname,'utf8', $std_core._async_callback_err1(_k)); return $std_core.async_yield(); })()"
}

// Try to read a text file; return a default value in case of an error.
fun try-read-text-file-syncdef( fileName : string, def : string ) : io string {
  match (fileName.try-read-text-file-sync()) {
    Left(_exn) -> def
    Right(content) -> content
  }
}

// Try to a text file synchronously (using UTF8 encoding)
fun try-read-text-file-sync( fileName : string ) : io either<exception,string> 
{
  catch( { Right(read-text-file-sync(fileName)) }, fun(exn) { Left(exn) }) 
}

// Read a text file synchronously (using UTF8 encoding)
extern read-text-file-sync( fileName : string ) : io string
{
  cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"
  js inline "fs.readFileSync(#1,{encoding: 'utf8'})"
}

// Try to write a text file synchronously (using UTF8 encoding). 
// Returns `True` if successful.
fun try-write-text-file( fileName : string, content : string )  : io maybe<exception>
{
  catch( { write-text-file-sync(fileName,content); Nothing }, fun(exn) { Just(exn) } )
}

// Write a text file synchronously (using UTF8 encoding)
extern write-text-file-sync( fileName : string, content : string )  : io () {
  cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
  js inline "fs.writeFileSync(#1,#2,{encoding: 'utf8'})"
}
