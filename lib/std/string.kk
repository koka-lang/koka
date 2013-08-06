/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* This module implements extended string operations and a string builder.
*/  
module std/string


// Trim whitespace on the left and right side of a string
public function trim( s : string ) : string {
  s.trimLeft.trimRight
}

// Trim the starting white space of a string 
public external inline trimLeft( s : string ) : string {
  js inline "((#1).replace(/^\\s\\s*/,''))"
}

// Trim the ending white space of a string.
public external inline trimRight( s : string ) : string {
  js inline "((#1).replace(/\\s+$/,''))"
}

/* --------------------------------------
 String builder 
---------------------------------------- */

// A ":builder" is used to append strings in linear time.
public type builder<h ::H>

// Create a string builder
public external inline builder() : alloc<h> builder<h> {
  js inline "{ value: '' }"
}

// O(1). Append to a string builder
public function append( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitAppend(s)
  b
}


// O(1). Append a string in front of a string builder
public function prepend( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitPrepend(s)
  b
}

private external inline unitAppend( b : builder<h>, s : string ) : write<h> () {
  js inline "((#1).value += (#2))"
}

private external inline unitPrepend( b : builder<h>, s : string ) : write<h> () {
  js inline "((#1).value = (#2) + (#1).value)"
}

// O(n). Read the current string from a string builder.
public external inline string( b : builder<h> ) : read<h> string {
  js inline "(#1).value"
}
