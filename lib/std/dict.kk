/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module implements string dictionaries.
*/  
public module std/dict

external include {
  js file "dict-inline.js"
}

// ----------------------------------------------------------------------------
// Mutable Dictionaries
// ----------------------------------------------------------------------------

// The type of mutable string dictionaries
type mdict<h::H,a>

// Create a mutable string dictionary
external mdict() : alloc<h> mdict<h,a> {
  js inline "{}"
}

// Index into a string dictionary
external []( md : mdict<h,a>, s : string ) : read<h> maybe<a> {
  js inline "(#1[#2]!==undefined ? $std_core.Just(#1[#2]) : null)"
}

// Return the keys in a dictionary
external keys( d : mdict<h,a> ) : vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js inline "$dictKeys(#1)"
}

// Assign to a string dictionary
external inline []( md : mdict<h,a>, s : string, assigned : a ) : write<h> () {
  inline "(#1[#2] = #3)"
}

external copy( md : mdict<h,a> ) : <alloc<h>,read<h>> mdict<h,a> {
  js "$dictCopy"
}

// Freeze a mutable dictionary into a ":dict"
function freeze( md : mdict<h,a> ) : <alloc<h>,read<h>> dict<a> {
  md.copy.unsafeFreeze
}

// Freeze a mutable dictionary without making a copy first.
private external unsafeFreeze( md : mdict<h,a> ) : read<h> dict<a> {
  inline "#1"
}



// ----------------------------------------------------------------------------
// Dictionaries
// ----------------------------------------------------------------------------

// A dictionary is a map from strings to values
type dict<a>

private external inline isNull( x : a) : bool  = inline "(#1 == null)"

// Create a new empty dictionary
external dict() : dict<a> {
  cs inline "new System.Collections.Generic.Dictionary<string,##1>()"
  js inline "{}"
}  

private external unsafeDictAdd( d : dict<a>, key : string, value : a ) : () {
  inline "(#1)[#2] = #3"
}

private external unsafeDictGet( d : dict<a>, key : string ) : a {
 inline "(#1)[#2]"
}

private external dictCopy( d : dict<a> ) : dict<a> {
  cs inline "new System.Collections.Generic.Dictionary<string,##1>(#1)"
  js inline "$dictCopy(#1)"
}

// Convert a dictionary to a vector of key/value pairs
function vector( d : dict<a> ) : vector<(string,a)> {
  d.keys.map fun(key) {
    (key,unsafeDictGet(d,key))
  }
}

// Convert a dictionary to a list of key/value pairs
function list( d : dict<a> ) : list<(string,a)> {
  d.vector.list
}

// Return the keys in a
external keys( d : dict<a> ) : vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js inline "$dictKeys(#1)"
}

// Return the value at a specified "key"
external []( d : dict<a>, key : string ) : maybe<a> {
  js inline "(#1[#2]!==undefined ? $std_core.Just(#1[#2]) : null)"
}

// Create a new dictionary from a ":list" of key value pairs.
function dict( elems : list<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafeDictAdd(d,key,value)
  }
  return d
}

// Create a new dictionary from a ":vector" of key/value pairs.
function dict( elems : vector<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafeDictAdd(d,key,value)
  }
  return d
}

// Append two dictionaries.
function (+)( d1 : dict<a>, d2 : dict<a> ) : dict<a> {
  val dnew = dictCopy(d1);
  d2.foreach fun(key,value) {
    unsafeDictAdd(dnew,key,value)
  }
  return dnew
}


// Execute action for each key/value pair in a dictionary.
function foreach( d : dict<a>, action : (string,a) -> e () ) : e () {
  d.keys.foreach fun(k) {
    action(k,unsafeDictGet(d,k))
  }
}

// Map a function over the values in a dictionary.
function map( d : dict<a>, f : (string,a) -> e b ) : e dict<b> {
  d.keys.map( fun(k) { (k, f(k,unsafeDictGet(d,k))) } ).dict
}  

