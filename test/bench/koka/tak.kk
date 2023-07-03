module tak

import std/num/int32

pub fun tak(x : int32, y : int32, z : int32 ) : div int32 
  if y < x
    then tak( tak(x - 1.int32, y, z), tak(y - 1.int32, z, x), tak(z - 1.int32, x, y) )
    else z

pub fun main()
  // tak(18,12,6).println    
  tak(36.int32,24.int32,14.int32).show.println    