/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var _get_pathsep;
var _get_dirsep;
var _get_realpath;
var _get_homedir;
var _get_tempdir;

function _get_apppath() {
  if (typeof module === "undefined" || module==null) return "";
  var m = module; 
  while(m.parent != null) { m = m.parent; }; 
  return (m.filename != null ? m.filename : "");    
};


if ($std_core.host() === "node") {
  var _path = require("path");
  var _fs   = require("fs");
  var _os   = require("os");
  _get_dirsep  = function() { return _path.sep; };
  _get_pathsep = function() { return _path.delimiter; };
  _get_realpath = function(p) { return _fs.realpathSync(p); };
  _get_homedir  = function() { return _os.homedir(); }
  _get_tempdir  = function() { return _os.tmpdir(); }
}
else {
  // browser?
  _get_dirsep = function() { return "/"; };
  _get_pathsep = function() { return ":"; };
  _get_realpath = function(p) { return p; };
  _get_homedir  = function() { return "."; };
  _get_tempdir  = function() { return "."; };
}
