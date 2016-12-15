/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   copyan be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

function _local_timezone_name() {
  try {
    var name = Intl.DateTimeFormat().resolvedOptions().timeZone;
    return (typeof name === "string" ? name : ""); 
  }
  catch(exn) {
    return "";
  }
}