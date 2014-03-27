/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var onserver = ($std_core.getHost() === "nodejs");

var path;

if (onserver) {
  path = require("path");
}
else {
  path = {
    sep: "/",
    delimiter: ";",
    basename: $basename,
    dirname: $dirname,
    extname: $extname,
    normalize: $normalize,    
  };
}

function $basename(s) {
  if (!s) return "";
  var i = s.lastIndexOf("/");
  return (i < 0 ? s : s.substr(i+1));
}

function $dirname(s) {
  if (!s) return "";
  var i = s.lastIndexOf("/");
  return (i <= 0 ? "" : s.substr(0,i-1));
}

function $extname(s) {
  s = $basename(s);
  if (!s) return "";
  var i = s.lastIndexOf(".");
  return (i < 0 ? "" : s.substr(i));      
}

function $normalize(s) { 
  return s; // TODO: improve on the browser?
}    

