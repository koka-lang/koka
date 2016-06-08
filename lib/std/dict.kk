/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module implements string dictionaries.
*/  
public module std/dict

extern include {
  js file "dict-inline.js"
}

// ----------------------------------------------------------------------------
// Mutable Dictionaries
// ----------------------------------------------------------------------------

// The type of mutable string dictionaries
type mdict<h::H,a>

// Create a mutable string dictionary
extern mdict : forall<h,a> () -> alloc<h> mdict<h,a> {
  cs inline "new Primitive.MDict<##1,##2>()"
  js inline "{}"
}

private extern inline unsafeIndex( md : mdict<h,a>, s : string ) : read<h> a {
  cs inline "((#1)[#2])"
  js inline "((#1)[#2])"  
}

extern inline containsKey( md : mdict<h,a>, s : string ) : read<h> bool {
  cs inline "((#1).ContainsKey(#2))"
  js inline "((#1)[#2]!==undefined)"
} 

// Index into a string dictionary
fun []( md : mdict<h,a>, s : string ) : read<h> maybe<a> {
  if (md.containsKey(s)) then Just(md.unsafeIndex(s)) else Nothing
}


// Return the keys in a dictionary
extern keys( d : mdict<h,a> ) : read<h> vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js inline "$dictKeys(#1)"
}

// Convert a dictionary to a vector of key/value pairs
fun vector( d : mdict<h,a> ) : read<h> vector<(string,a)> {
  d.keys.map fun(key) {
    (key,unsafeIndex(d,key))
  }
}

// Convert a dictionary to a list of key/value pairs
fun list( d : mdict<h,a> ) : read<h> list<(string,a)> {
  d.vector.list
}

// Assign to a string dictionary
extern inline []( md : mdict<h,a>, s : string, assigned : a ) : write<h> () {
  cs inline "(#1)[#2] = #3"
  js inline "(#1)[#2] = #3"
}

extern copy : forall<h,a> ( md : mdict<h,a> ) -> <alloc<h>,read<h>> mdict<h,a> {
  cs inline "new Primitive.MDict<##1,##2>(#1)"
  js "$dictCopy"
}

// Freeze a mutable dictionary into a ":dict"
extern freeze : forall<h,a> ( md : mdict<h,a> ) -> <alloc<h>,read<h>> dict<a> {
  cs inline "new Primitive.Dict<##2>(#1)"
  js "$dictCopy"
}



// ----------------------------------------------------------------------------
// Dictionaries
// ----------------------------------------------------------------------------

// A dictionary is a map from strings to values
type dict<a>

private extern inline isNull( x : a) : bool  { inline "(#1 == null)" }

// Create a new empty dictionary
extern dict() : dict<a> {
  cs inline "new Primitive.Dict<##1>()"
  js inline "{}"
}  

private extern inline unsafeDictAdd( d : dict<a>, key : string, value : a ) : () {
  inline "(#1)[#2] = (#3)"
}

private extern inline unsafeDictGet( d : dict<a>, key : string ) : a {
  inline "(#1)[#2]"
}


extern inline containsKey( d : dict<a>, key : string ) : bool {
  cs inline "((#1).ContainsKey(#2))"
  js inline "((#1)[#2]!==undefined)"
} 

private extern dictCopy( d : dict<a> ) : dict<a> {
  cs inline "new Primitive.Dict<##1>(#1)"
  js inline "$dictCopy(#1)"
}

// Convert a dictionary to a vector of key/value pairs
fun vector( d : dict<a> ) : vector<(string,a)> {
  d.keys.map fun(key) {
    (key,unsafeDictGet(d,key))
  }
}

// Convert a dictionary to a list of key/value pairs
fun list( d : dict<a> ) : list<(string,a)> {
  d.vector.list
}

// Return the keys in a
extern keys( d : dict<a> ) : vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js inline "$dictKeys(#1)"
}

// Index into a string dictionary
fun []( d : dict<a>, key : string ) : maybe<a> {
  if (d.containsKey(key)) then Just(d.unsafeDictGet(key)) else Nothing
}

// Create a new dictionary from a ":list" of key value pairs.
fun dict( elems : list<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafeDictAdd(d,key,value)
  }
  return d
}

// Create a new dictionary from a ":vector" of key/value pairs.
fun dict( elems : vector<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafeDictAdd(d,key,value)
  }
  return d
}

// Append two dictionaries.
fun (+)( d1 : dict<a>, d2 : dict<a> ) : dict<a> {
  val dnew = dictCopy(d1);
  d2.foreach fun(key,value) {
    unsafeDictAdd(dnew,key,value)
  }
  return dnew
}


// Execute action for each key/value pair in a dictionary.
fun foreach( d : dict<a>, action : (string,a) -> e () ) : e () {
  d.keys.foreach fun(k) {
    action(k,unsafeDictGet(d,k))
  }
}

// Map a fun over the values in a dictionary.
fun map( d : dict<a>, f : (string,a) -> e b ) : e dict<b> {
  d.keys.map( fun(k) { (k, f(k,unsafeDictGet(d,k))) } ).dict
}  

