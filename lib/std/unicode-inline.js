/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// normalize a string
function _normalize(s,nf) {
  if (typeof s.normalize === "function") {
    return s.normalize(nf || "NFC");
  }
  else {
    // .. no unicode normalization support on this platform; let's hope for the best
    return s; 
  }
}
