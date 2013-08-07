/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

public module std/memo

import std/dict

function memo( f : string -> a ) : string -> a 
{
  unsafeTotal {
    var cache := mdict()
    return fun(s) {
      unsafeTotal {
        match(cache[s]) {
          Just(x) -> x
          Nothing -> {
            x = f(s)
            cache[s] := x
            x
          }
        }
      }
    }
  }
}

function memoInt( f : int -> a ) : int -> a {
  return fun(i) { memo( fun(s) { f(s.parseInt.maybe(0,id)) } )(i.show) }  
}