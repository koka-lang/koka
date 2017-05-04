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
import std/num/ddouble
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
public fun utimes( fname : path, atime : instant, mtime : instant ) : <asyncx,file> () {
  await-exn0 fun(cb) { xfile-utimes( fname.string, atime.unix-timestamp*1000.0, mtime.unix-timestamp*1000.0, cb ) }
}

extern xfile-utimes( fname : string, atime : double, mtime : double, cb : (null<exception>) -> io () ) : io () {
  js "_file_utimes"  
}

// Return the modification time of a file.
public fun mod-time( fname : path ) : <asyncx,file> instant {
  await-exn1(fun(cb) { 
    xfile-mod-time( fname.string, cb ) 
  }).unix-instant
}

extern xfile-mod-time( fname : string, cb : (null<exception>,double) -> io () ) : io () {
  js "_file_mod_time" // (#1, function(stats){ return (#2)(stats.mtime.getTime() * 1e-3); })"  
}


// Write a text file (using UTF8 encoding) asynchronously
public fun write-text( fname : path, text : string ) : <asyncx,file> () {
  await-exn0 fun(cb) { 
    xwrite-text(fname.string,text, cb) 
  }
}

extern xwrite-text( fname : string, text : string, cb : (null<exception>) -> io () ) : io () {
  js "_file_write_text"
}


// Read a text file (using UTF8 encoding) asynchronously
public fun read-text( fname : path ) : <asyncx,file> string {
  await-exn1 fun(cb) { 
    xread-text(fname.string,cb) 
  }
}

extern xread-text( fname : string, cb : (null<exception>,string) -> io ()  ) : io () {
  js "_file_read_text"
}

// Download a text file from `url`, timing out after `timeout` seconds (=`10.seconds`).
public fun download-text( url : string, timeout : duration = duration(10) ) : <asyncx,net> string {
  await-exn1 fun(cb) { 
    xdownload-text(url,timeout.seconds.double*1000.0,cb) 
  }
}

extern xdownload-text( url : string, timeout : double, cb : (null<exception>,string) -> io () ) : io () {
  js "_file_download_text"
}
