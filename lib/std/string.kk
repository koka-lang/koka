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
  cs inline "(#1).TrimStart()"
  js inline "((#1).replace(/^\\s\\s*/,''))"
}

// Trim the ending white space of a string.
public external inline trimRight( s : string ) : string {
  cs inline "(#1).TrimEnd()"
  js inline "((#1).replace(/\\s+$/,''))"
}

public function indent( s : string, n : int = 2, c : char = ' ' ) {
  if (n<=0) return s
  val pre = string(n,c)
  s.lines.map( fun(line) { pre + line } ).unlines
}

/* --------------------------------------
 String builder 
---------------------------------------- */

// A `:builder` is used to append strings in linear time.
public type builder<h ::H>

// Create a string builder
public external inline builder() : alloc<h> builder<h> {
  cs inline "new System.Text.StringBuilder()"
  js inline "{ value: '' }"
}

// O(1). Append to a string builder
public function append( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitAppend(s)
  b
}


// O(1). Prepend a string in front of a string builder
public function prepend( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitPrepend(s)
  b
}

private external inline unitAppend( b : builder<h>, s : string ) : write<h> () {
  cs inline "(#1).Append(#2)"
  js inline "((#1).value += (#2))"
}

private external inline unitPrepend( b : builder<h>, s : string ) : write<h> () {
  cs inline "(#1).Insert(0,#2)"
  js inline "((#1).value = (#2) + (#1).value)"
}

// O(n). Read the current string from a string builder.
public external inline build( b : builder<h> ) : read<h> string {
  cs inline "(#1).ToString()"
  js inline "(#1).value"
}
