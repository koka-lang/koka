/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var http = require("http");
var https= require("https");
var url  = require("url");

function _file_stat( fname, cb ) {
  fs.stat(fname,$std_core._async_callback_err1(cb)); 
  return $std_core.async_yield();
}

function _file_utimes( fname, atime, mtime, cb ) {
  fs.utimes(fname,atime,mtime,$std_core._async_callback_err0(cb));
  return $std_core.async_yield();
}

function _file_write_text( fname, text, cb ) {
  fs.writeFile(fname,text,{},$std_core._async_callback_err0(cb));
  return $std_core.async_yield();
}

function _file_read_text( fname, cb ) {
  fs.readFile(fname,"utf8",$std_core._async_callback_err1(cb));
  return $std_core.async_yield();
}

function _file_download_text(urlstr, timeout, cb) {
  var options = url.parse(urlstr);
  options.timeout = timeout;
  _download_text(options, $std_core._async_callback_err1(cb));
  return $std_core.async_yield();
}

function _download_text(options, cb, redirected) {
  var prot = (options.protocol === "http:" ? http : https);
  prot.get(options, function(response) {
    switch(response.statusCode) {
      case 200:
        var ctype = response.headers["content-type"];
        // console.log("got 200: " + ctype)
        if (!/^(?:text\/.*|application\/json|image\/svg\+xml)$/.test(ctype)) {
          return cb(new Error("Server sent binary file"));
        }
        var res = "";
        response.setEncoding("utf8");
        response.on('data', function(chunk){
          res += chunk;
        }).on('end', function(){
          // console.log(" download complete: " + res)
          return cb(null,res);
        }).on('error', function(err) {
          return cb(new Error("Server error: " + err.toString()));
        });
        break;
      case 301:
      case 302:
      case 303:
      case 307:
        console.log("redirection")
        if (redirected) return cb(new Error("Server redirected multiple times"));
        var newoptions = url.parse(response.headers.location);
        newoptions.timeout = options.timeout;
        return _download_text(newoptions, cb, true );
        break;
      default:
        return cb(new Error('Server responded with status code ' + response.statusCode));
    }
  })
  .on('error', function(err) {
    return cb(err);
  });
}