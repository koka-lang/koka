/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Extended string operations and a string builder.
*/  
module std/text/string

import std/data/array

// Indent all lines a string `s` by `n`. `fill` is used for indentation, by default `" "`.
public fun indent( s : string, n : int = 2, fill : string = " " ) {
  if (n<=0) return s
  val pre = fill.repeat(n)
  s.lines.map( fun(line) { pre + line } ).unlines
}

/* --------------------------------------
  Jaro-Winkler string distance
---------------------------------------- */

// Return the [Jaro-Winkler distance](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance) 
// between two strings. This is a number between `1.0` and `0.0` where `1.0` is a perfect
// match. . Works best for small words and names.
// _Jaro-Winkler_ is a variation of a plain `jaro-distance` developed by the U.S. census
// for name comparison and it gives a higher score to strings that have a common prefix.
//
// * Scores are only boosted if the `jaro-distance` is above the `boost-threshold` (= 0.7) 
// * The maximal common prefix length considered is given by `prefix-len` (= 4)
// * The scale modifies the boosted amount (= 0.1). It is adjusted dynamically depending on the `prefix-len` to
//   ensure no result can be higher then 1.0.
//
// `jaro-distance("martha","marhta") == 0.96`\
// `jaro-distance("abrams","abroms") == 0.92`
public fun jaro-winkler-distance( s : string, t : string, boost-threshold : double = 0.7, prefix-len : int = 4, scale : double = 0.1 ) : double {
  val d = jaro-distance(s,t)
  if (d <= boost-threshold || prefix-len <= 0) return d
  val l = common-prefix(s,t,prefix-len).count
  val sc = min( max(scale,0.0), 1.0 / prefix-len.double)
  d + (l.double * sc * (1.0 - d))
}

// Return the [Jaro distance](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance) 
// between two strings. This is a number between `1.0` and `0.0` where `1.0` is a perfect
// match. Works best for small words and names.
//
// `jaro-distance("martha","marhta") == 0.94`\
// `jaro-distance("abrams","abroms") == 0.89`
public fun jaro-distance( s : string, t : string ) : double {
  if (s==t) return 1.0
  if (s.empty? || t.empty?) return 0.0
  val v = s.vector
  val w = t.vector
  val lim = max(0,max(v.length,w.length)/2 - 1)
  val (mcount,tcount) = if (v.length > w.length) then jaro-match(w,v,lim) else jaro-match(v,w,lim)
  if (mcount<=0) then 0.0 else {
    val mc = mcount.double
    val tc = tcount.double
    ((mc / v.length.double) + (mc / w.length.double) + ((mc - tc) / mc)) / 3.0
  }
}

fun jaro-match( v : vector<char>, w : vector<char>, lim : int ) : (int,int) {
  try-default((0,0)) {
    val matched = array(w.length,False)
    var transposes := 0
    var matches    := 0
    var lastmatch  := 0
    v.foreach-indexed fun(c,i) {
      val start = max(0,i - lim)
      val end   = min(w.length - 1, i + lim)
      for-while(start,end) fun(j) {
        if (w[j] != c || matched[j]) Nothing
        else {
          matched[j] := True
          matches := matches + 1
          if (lastmatch > j) transposes := transposes + 1
          lastmatch := j
          Just(())
        }
      }
      ()
    }
    (matches,transposes)
  }
}

/* --------------------------------------
 String builder 
---------------------------------------- */

// A `:builder` is used to append strings in linear time.
public type builder<h ::H>

// Create a string builder
public extern inline builder() : alloc<h> builder<h> {
  cs inline "new System.Text.StringBuilder()"
  js inline "{ value: '' }"
}

// O(1). Append to a string builder
public fun append( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitAppend(s)
  b
}


// O(1). Prepend a string in front of a string builder
public fun prepend( b : builder<h>, s : string ) : write<h> builder<h> {
  b.unitPrepend(s)
  b
}

extern inline unitAppend( b : builder<h>, s : string ) : write<h> () {
  cs inline "(#1).Append(#2)"
  js inline "((#1).value += (#2))"
}

extern inline unitPrepend( b : builder<h>, s : string ) : write<h> () {
  cs inline "(#1).Insert(0,#2)"
  js inline "((#1).value = (#2) + (#1).value)"
}

// O(n). Read the current string from a string builder.
public extern inline build( b : builder<h> ) : read<h> string {
  cs inline "(#1).ToString()"
  js inline "(#1).value"
}

// Do `action` with a given `:builder` and return the `:string` that was built.
public fun with-builder( action : forall<h> (b : builder<h>) -> <st<h>|e> () ) : e string {
  val b = builder()
  action(b)
  b.build
}