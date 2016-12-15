/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File operations. 
*/
module std/os/file

import std/os/path
import std/num/fixed
import std/time/duration
import std/time/instant
import std/time/utc
import std/async

extern include {
  js file "file-inline.js"
}


// Deprecated: Read a text file synchronously (using UTF8 encoding)
public extern read-text-file-sync( fileName : string ) : io string
{
  cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"
  js inline "fs.readFileSync(#1,{encoding: 'utf8'})"
}

// Deprecated: Write a text file synchronously (using UTF8 encoding)
public extern write-text-file-sync( fileName : string, content : string )  : io () {
  cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
  js inline "fs.writeFileSync(#1,#2,{encoding: 'utf8'})"
}


// Set the access time and modification time of a file.
public fun utimes( fname : path, atime : instant, mtime : instant ) : <async,file,exn> () {
  xfile-utimes( fname.string, atime.unix-timestamp*1000.0, mtime.unix-timestamp*1000.0 )
}

extern xfile-utimes( fname : string, atime : double, mtime : double ) : <async,file,exn> () {
  js inline "_file_utimes(fname, atime, mtime, _k)"  
}

// Return the modification time of a file.
public fun mod-time( fname : path ) : <async,file,exn> instant {
  xfile-mod-time( fname.string ).unix-instant
}

extern xfile-mod-time( fname : string ) : <async,file,exn> double {
  js inline "_file_stat(fname, function(stats){ return _k(stats.mtime.getTime() * 1e-3); })"  
}


// Write a text file (using UTF8 encoding) asynchronously
public fun write-text( fname : path, text : string ) : <async,file,exn> () {
  xwrite-text(fname.string,text)
}

extern xwrite-text( fname : string, text : string ) : <async,file,exn> () {
  js inline "_file_write_text(fname, text, _k)"
}


// Read a text file (using UTF8 encoding) asynchronously
public fun read-text( fname : path ) : <async,file,exn> string {
  unsafe-async-yield-err( fun(cb) { xread-text(fname.string,cb) } )
}

extern xread-text( fname : string, cb : string -> <async,file,exn> ()  ) : () {
  js "_file_read_text"
}

// Download a text file from `url`, timing out after `timeout` seconds (=`10.seconds`).
public fun download-text( url : string, timeout : duration = duration(10) ) : <async,net,exn> string {
  xdownload-text(url,timeout.seconds.double*1000.0)
}

extern xdownload-text( url : string, timeout : double ) : <async,net,exn> string {
  js inline "_file_download_text(url, timeout, _k)"
}
