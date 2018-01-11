/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function notimp(name,fname) {
  throw new Error("cannot " + name + " file: " + fname);
}

function _cancel_entry(obj) {
  // nothing
}

var _file_mod_time = function(fname,cb) { notimp("mod_time",fname); };
var _file_utimes = function(fname,atime,mtime,cb) { notimp("utimes",fname); };
var _file_write_text = function(fname,text,cb) { notimp("write",fname); };
var _file_read_text = function(fname,cb) { notimp("read",fname); };
var _file_download_text = function(url,timeout,cb) { notimp("download", url); };

if ($std_core.host()=="node") 
{
  var fs   = require("fs");
  var http = require("http");
  var https= require("https");
  var url  = require("url");

  _file_mod_time = function( fname, cb ) {
    fs.stat(fname, function(err,stats) {
      if (err) return cb(err, 0.0);
      if (stats==null || stats.mtime==null) return cb(new Error("fs.stat: invalid stats"),0.0);
      return cb(err, stats.mtime.getTime() * 1e-3 );
    });
  };

  _file_utimes = function( fname, atime, mtime, cb ) {
    fs.utimes(fname,atime * 1000.0,mtime * 1000.0,cb);
  };

  _file_write_text = function( fname, text, cb ) {
    fs.writeFile(fname,text,{},cb);
  };

  _file_read_text = function( fname, cb ) {
    fs.readFile(fname,"utf8",cb);
  };

  _file_download_text = function(urlstr, timeout, cb) {
    var t = null;
    var timedOut = false;
    // start download
    var req = _download_text(urlstr, (timeout > 0), function(err,data) {
      if (timeout > 0 && t) {
        clearTimeout(t);      
      }
      if (err && timedOut) return cb(new Error("server is not responsive.\n  url: " + urlstr),data)
                      else return cb(err,data);
    });
    // set timeout
    if (timeout > 0) {
      t = setTimeout( function() { 
        timedOut = true;
        if (req) req.abort();
      }, timeout*1000.0 );
    }
  }

  function _download_text(urlstr, use_timeout, cb, redirected) {
    // Check that there is progress (some response every 2.5s) (do this only if timeout is set)
    var progress = 2500; 
    var req = null;
    var t   = null;
    function timeoutAbort() { 
      if (req != null) {
        console.log("abort request due to no progress")
        req.abort();
        req = null;
      }  
    };
    function timeoutClear() {
      if (t) { clearTimeout(t); t = 0; }
    }
    function timeoutReset(msecs) { 
      timeoutClear();
      if (use_timeout) { t = setTimeout(timeoutAbort, msecs); }
    }
    // Issue the request
    var prot = (/^http:\/\//.test(urlstr) ? http : https);
    req = prot.get(urlstr, function(response) {
      switch(response.statusCode) {
        case 200:
          timeoutReset(progress);
          // console.log("response headers: " + JSON.stringify(response.headers));
          var ctype = response.headers["content-type"];
          // console.log("got 200: " + ctype)
          if (!/^(?:text\/.*|application\/json|image\/svg\+xml)$/.test(ctype)) {
            return cb(new Error("Server sent binary file"));
          }
          var res = "";
          response.setEncoding("utf8");
          response.on('data', function(chunk){
            //console.log(" download chunk: " + chunk.length);
            timeoutReset(progress);
            res += chunk;
          }).on('end', function(){
            timeoutClear();
            // console.log(" download complete: " + res.length)
            return cb(null,res);
          }).on('error', function(err) {
            timeoutClear();
            return cb(new Error("Server error: " + err.toString()));
          });
          break;
        case 301:
        case 302:
        case 303:
        case 307:
          timeoutClear();
          // console.log("redirection");
          if (redirected) return cb(new Error("Server redirected multiple times"));
          return _download_text(response.headers.location, timeout, cb, true );
          break;
        default:
          timeoutClear();
          return cb(new Error('Server responded with status code ' + response.statusCode));
      }
    })
    .on('error', function(err) {
      timeoutClear();
      return cb(err);
    });
    return req;
  }
}