/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Perfect hashing for strings.

   These `:atom`s can be compared very efficiently. If you compare only
   seldomly, a straight string compare will probably be faster since 
   creating an atom takes some initial work.
*/
module std/text/atom

import std/num/int32

external include {
  cs file "atom-inline.cs"
  js file "atom-inline.js"
}

// An `atom` corresponding to the empty string
public val atom-null = atom("")

// An atom is an abstract representation of a string that can be compared quickly
abstract struct atom( value: int32 )  

// Create an `:atom`
extern atom-create : ( s : string ) -> int32 {
  cs "Atom.Create"
  js "$atomCreate"
}

// Convert an `:atom` back to a `:string`
extern atom-to-string : ( x : int32 ) -> string {
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


extern atom-matcher : ( xs : vector<string>, ignoreCase: int32 ) -> any
{
  cs "Atom.CreateMatcher"
  js "$atomCreateMatcher"
}

extern atom-match : ( m : any, s : string, ignoreCase: int32 ) -> int32
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
  val m = atom-matcher( xs, ignoreCase.int32 );
  return fun(s) { return bool(atom-match(m,s,ignoreCase.int32)); }
}
