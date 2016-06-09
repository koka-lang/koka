/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// combining marks
var _rx_combining = /([\0-\u02FF\u0370-\u1DBF\u1E00-\u20CF\u2100-\uD7FF\uDC00-\uFE1F\uFE30-\uFFFF]|[\uD800-\uDBFF][\uDC00-\uDFFF]|[\uD800-\uDBFF])([\u0300-\u036F\u1DC0-\u1DFF\u20D0-\u20FF\uFE20-\uFE2F]*)|([\s\S])/g;

// normalize a string
function _normalize(s,nf) {
  if (typeof s.normalize === "function") {
    return s.normalize(nf || "NFC");
  }
  else {
    // .. no unicode support on this platform; let's hope for the best
    return s; 
  }
}

// convert to a vector of grapheme strings
function _graphemes(s) {
  var gs = [];
  s.replace(_rx_combining, function(m) {
    gs.push(m);
  });
  return gs;
}