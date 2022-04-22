module tak-int

pub fun tak(x : int, y : int, z : int ) : div int 
  if y < x
    then tak( tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y) )
    else z

pub fun main()
  // tak(18,12,6).println    
  tak(36,24,14).println    