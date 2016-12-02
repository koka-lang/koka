/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File operations. 
*/
public module std/os/file

import std/os/path
import std/num/fixed
import std/time/duration
import std/time/instant
import std/time/utc

extern import {
  js "fs"
}

extern include {
  js file "file-inline.js"
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
public extern read-text-file-sync( fileName : string ) : io string
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

public fun utimes( fname : path, atime : instant, mtime : instant ) : <async,file,exn> () {
  xfile-utimes( fname.string, atime.unix-timestamp*1000.0, mtime.unix-timestamp*1000.0 )
}

extern xfile-utimes( fname : string, atime : double, mtime : double ) : <async,file,exn> () {
  js inline "_file_utimes(fname, atime, mtime, _k)"  
}

public fun mod-time( fname : path ) : <async,file,exn> instant {
  xfile-mod-time( fname.string ).unix-instant
}

extern xfile-mod-time( fname : string ) : <async,file,exn> double {
  js inline "_file_stat(fname, function(stats){ return _k(stats.mtime.getTime() * 1e-3); })"  
}


public fun write-text( fname : path, text : string ) : <async,file,exn> () {
  xwrite-text(fname.string,text)
}

extern xwrite-text( fname : string, text : string ) : <async,file,exn> () {
  js inline "_file_write_text(fname, text, _k)"
}


public fun read-text( fname : path ) : <async,file,exn> string {
  xread-text(fname.string)
}

extern xread-text( fname : string ) : <async,file,exn> string {
  js inline "_file_read_text(fname, _k)"
}


public fun download-text( url : string, timeout : duration = duration(10) ) : <async,net,exn> string {
  xdownload-text(url,timeout.seconds.double*1000.0)
}

extern xdownload-text( url : string, timeout : double ) : <async,net,exn> string {
  js inline "_file_download_text(url, timeout, _k)"
}
