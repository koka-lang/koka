/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time11

import std/num/ddouble
import std/time/timestamp
public import std/time


/*----------------------------------------------------------------------------
  Tests if reference count on boxed value types is maintained 
  correctly: here the `timezone` is a value type in a boxed field of the
  optional argument; without fix #da4ff3db this goes wrong
----------------------------------------------------------------------------*/

public fun test-dates(tz : timezone = tz-utc) 
{
  fun g(y:int){
    val t = time(y,tz=tz)
    ()
  }  
  g(1950)
  g(1951)
  //val t1 = time(starty,1,1,tz=tz,cal=cal,ts=ts)
  //val t2 = time(starty+1,1,1,tz=tz,cal=cal,ts=ts)
  ()
}

public fun main() {
  //test-dates()
  test-dates(tz=tz-fixed(1))  
  println("ok")
}

