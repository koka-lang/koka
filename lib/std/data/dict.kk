/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* String dictionaries.
*/  
public module std/data/dict

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
  js inline "{ _count: 0 }"
}

private extern inline unsafe-index( md : mdict<h,a>, s : string ) : read<h> a {
  cs inline "((#1)[#2])"
  js inline "((#1)['/' + #2])"  
}

extern inline contains-key( md : mdict<h,a>, s : string ) : read<h> bool {
  cs inline "((#1).ContainsKey(#2))"
  js inline "((#1)['/' + #2]!==undefined)"
} 

// Index into a string dictionary
fun []( md : mdict<h,a>, s : string ) : read<h> maybe<a> {
  if (md.contains-key(s)) then Just(md.unsafe-index(s)) else Nothing
}


// Return the keys in a dictionary
extern keys( d : mdict<h,a> ) : read<h> vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js inline "_dict_keys(#1)"
}

// Convert a dictionary to a vector of key/value pairs
fun vector( d : mdict<h,a> ) : read<h> vector<(string,a)> {
  d.keys.map fun(key) {
    (key,unsafe-index(d,key))
  }
}

// Convert a dictionary to a list of key/value pairs
fun list( d : mdict<h,a> ) : read<h> list<(string,a)> {
  d.vector.list
}

// Assign to a string dictionary
extern []( md : mdict<h,a>, s : string, assigned : a ) : write<h> () {
  cs inline "(#1)[#2] = #3"
  js "_dict_assign"
}

extern copy : forall<h,a> ( md : mdict<h,a> ) -> <alloc<h>,read<h>> mdict<h,a> {
  cs inline "new Primitive.MDict<##1,##2>(#1)"
  js "_dict_copy"
}

// Freeze a mutable dictionary into a `:dict`
extern freeze : forall<h,a> ( md : mdict<h,a> ) -> <alloc<h>,read<h>> dict<a> {
  cs inline "new Primitive.Dict<##2>(#1)"
  js "_dict_copy"
}


extern clear( md : mdict<h,a> ) : write<h> () {
  cs inline "(#1).Clear()"
  js "_dict_clear"
}

extern remove( md : mdict<h,a>, s : string ) : <read<h>,write<h>> () {
  cs inline "(#1).Remove(#2)"
  js inline "delete #1['/'+#2]"
}

extern count( md : mdict<h,a> ) : int {
  cs inline "(#1).Count"
  js inline "#1._count"
}

// ----------------------------------------------------------------------------
// Dictionaries
// ----------------------------------------------------------------------------

// A dictionary is a map from strings to values
type dict<a>

private extern inline null?( x : a) : bool  { inline "(#1 == null)" }

// Create a new empty dictionary
extern dict() : dict<a> {
  cs inline "new Primitive.Dict<##1>()"
  js inline "{ _count: 0 }"
}  

private extern inline unsafe-dict-add( d : dict<a>, key : string, value : a ) : () {
  cs inline "(#1)[#2] = (#3)"
  js "_dict_assign"
}

private extern inline unsafe-dict-get( d : dict<a>, key : string ) : a {
  cs inline "(#1)[#2]"
  js inline "((#1)['/' + #2])"
}


extern inline contains-key( d : dict<a>, key : string ) : bool {
  cs inline "((#1).ContainsKey(#2))"
  js inline "((#1)['/' + #2]!==undefined)"
} 

private extern dict-copy( d : dict<a> ) : dict<a> {
  cs inline "new Primitive.Dict<##1>(#1)"
  js "_dict_copy"
}

// Convert a dictionary to a vector of key/value pairs
fun vector( d : dict<a> ) : vector<(string,a)> {
  d.keys.map fun(key) {
    (key,unsafe-dict-get(d,key))
  }
}

// Convert a dictionary to a list of key/value pairs
fun list( d : dict<a> ) : list<(string,a)> {
  d.vector.list
}

// Return the keys in a
extern keys( d : dict<a> ) : vector<string> {
  cs inline "Primitive.DictKeys(#1)"
  js "_dict_keys"
}

// Index into a string dictionary
fun []( d : dict<a>, key : string ) : maybe<a> {
  if (d.contains-key(key)) then Just(d.unsafe-dict-get(key)) else Nothing
}

// Create a new dictionary from a `:list` of key value pairs.
fun dict( elems : list<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafe-dict-add(d,key,value)
  }
  return d
}

// Create a new dictionary from a `:vector` of key/value pairs.
fun dict( elems : vector<(string,a)> ) : dict<a> {
  val d = dict();
  elems.foreach fun(elem) {
    val (key,value) = elem
    unsafe-dict-add(d,key,value)
  }
  return d
}

// Append two dictionaries.
fun (+)( d1 : dict<a>, d2 : dict<a> ) : dict<a> {
  val dnew = dict-copy(d1);
  d2.foreach fun(key,value) {
    unsafe-dict-add(dnew,key,value)
  }
  return dnew
}


// Execute action for each key/value pair in a dictionary.
fun foreach( d : dict<a>, action : (string,a) -> e () ) : e () {
  d.foreach-while fun(k,x) { 
    action(k,x)
    Nothing
  }
  ()
}

// Execute action for each key/value pair in a dictionary until
// the action returns `Just`.
fun foreach-while( d : dict<a>, action : (string,a) -> e maybe<b> ) : e maybe<b> {
  d.keys.foreach-while fun(k) {
    action(k,unsafe-dict-get(d,k))
  }
}

// Map a fun over the values in a dictionary.
fun map( d : dict<a>, f : (string,a) -> e b ) : e dict<b> {
  d.keys.map( fun(k) { (k, f(k,unsafe-dict-get(d,k))) } ).dict
}  

