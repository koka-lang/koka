/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// make a shallow copy
function _dict_copy(obj) {
  var newobj = {};
  for( var prop in obj) {
    if (obj.hasOwnProperty(prop)) newobj[prop] = obj[prop];
  }
  return newobj;
}

// get the fields of an object
function _dict_keys(obj) {
  var props = [];
  for (var prop in obj) {
    if (prop[0]==="/") props.push(prop.substr(1));
  } 
  return props;
}

// clear 
function _dict_clear(obj) {
  for (var prop in obj) {
    if (prop[0]==="/") delete obj[prop];
  }
}

function _dict_assign(obj,key,val) {
  if (obj['/'+key] === undefined) obj._count++;
  obj['/'+key] = val;
}