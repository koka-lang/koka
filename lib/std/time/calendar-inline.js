/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function _local_get_timezone() {
  return null; // not possible in javascript, it's a global :-(
}

function _local_utc_delta(tz,i) {
  var d = new Date(i*1000);
  var abbrv = "";
  try {
    var dt = Intl.DateTimeFormat("en-US",{timeZoneName:"short"});
    abbrv = dt.format(Date.now()).replace(/^.*\s([\w\+\-:]{3,6})$/,"$1");
  }
  catch(exn) { }; //ignore
  return $std_core._tuple2_( (d.getTimezoneOffset())*-60, abbrv );
}

