/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module provides a perfect hash for strings.

   These `:atom`s can be compared very efficiently. If you compare only
   seldomly, a straight string compare will probably be faster since 
   creating an atom takes some initial work.
*/
module std/atom

import std/vector

external include {
  cs file "atom-inline.cs"
  js file "atom-inline.js"
}

// An `atom` corresponding to the empty string
public val atom-null = atom("")

// An atom is an abstract representation of a string that can be compared quickly
abstract struct atom( value: int )  

// Create an `:atom`
extern atom-create : ( s : string ) -> int {
  cs "Atom.Create"
  js "$atomCreate"
}

// Convert an `:atom` back to a `:string`
extern atom-to-string : ( x : int ) -> string {
  cs "Atom.ToString"
  js "$atomToString"
}

// Create an `:atom`
public fun atom( s : string ) : atom {
  atom-create(s).Atom
}

// Convert an `:atom` back to a `:string`
public fun string( a : atom ) : string {
  atom-to-string(a.value)
}

// Compare two atoms for equality
public fun (==)(x : atom, y : atom) : bool  -> (x.value == y.value)

// Compare if two atoms are not equal.
public fun (!=)(x : atom, y : atom) : bool  -> (x.value != y.value)


extern atom-matcher : ( xs : vector<string>, ignoreCase: int ) -> any
{
  cs "Atom.CreateMatcher"
  js "$atomCreateMatcher"
}

extern atom-match : ( m : any, s : string, ignoreCase: int ) -> int
{
  cs "Atom.Match"
  js "$atomMatch"
}

// Create an efficient string matcher which given a list of strings returns a matching function.
public fun matcher( xs : list<string>, ignoreCase: bool = False ) : (( s : string ) -> bool)
{
  matcher(vector(xs), ignoreCase)
}

// Create an efficient string matcher which given a vector of strings returns a matching function.
public fun matcher( xs : vector<string>, ignoreCase: bool = False ) : (( s : string ) -> bool)
{
  val m = atom-matcher( xs, ignoreCase.int );
  return fun(s) { return bool(atom-match(m,s,int(ignoreCase))); }
}
