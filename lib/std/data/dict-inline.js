/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// make a shallow copy
function $dictCopy(obj) {
  var newobj = {};
  for( var prop in obj) newobj[prop] = obj[prop];
  return newobj;
}

// get the fields of an object
function $dictKeys(obj) {
  var props = [];
  for (var prop in obj) {
    if (obj.hasOwnProperty(prop)) props.push(prop);
  } 
  return props;
}
