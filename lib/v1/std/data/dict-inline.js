/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


$std_data_dict = $std_core_types._export( $std_data_dict, {
                    "_dict_assign": _dict_assign,
                 });
                 
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
