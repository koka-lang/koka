/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
File operations. 
*/
public module std/file

extern import {
  js "fs"
}

// Try to read a text file; return a default value in case of an error.
fun readTextFileDef( fileName : string, def : string ) : io string {
  match (fileName.tryReadTextFile()) {
    Left(_exn) -> def
    Right(content) -> content
  }
}

// Try to a text file synchronously (using UTF8 encoding)
fun tryReadTextFile( fileName : string ) : io either<exception,string> 
{
  catch( { Right(readTextFileSync(fileName)) }, fun(exn) { Left(exn) }) 
}

// Read a text file synchronously (using UTF8 encoding)
extern readTextFileSync( fileName : string ) : io string
{
  cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"
  js inline "fs.readFileSync(#1,{encoding: 'utf8'})"
}

// Try to write a text file synchronously (using UTF8 encoding). 
// Returns `True` if successful.
fun tryWriteTextFile( fileName : string, content : string )  : io bool 
{
  catch( { writeTextFileSync(fileName,content); True }, fun(_exn) { False } )
}

// Write a text file synchronously (using UTF8 encoding)
extern writeTextFileSync( fileName : string, content : string )  : io () {
  cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
  js inline "fs.writeFileSync(#1,#2,{encoding: 'utf8'})"
}
