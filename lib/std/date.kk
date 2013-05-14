/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

public module std/date

type date

external now : () -> io date { 
  js inline "new Date()" 
}

external hours : (date) -> int { 
  js inline "#1.getHours()" 
}

external minutes : (date) -> int { 
  js inline "#1.getMinutes()" 
}

external seconds : (date) -> int { 
  js inline "#1.getSeconds()" 
}

external time : (date) -> int { 
  js inline "#1.getTime()" 
}
