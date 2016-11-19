/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function _local_get_timezone() {
  return null; // not possible in javascript, it's a global :-(
}

var dtformat = undefined;

function _local_get_timezone_abbrv(d) {
  // try to get the timezone abbreviation; expensive to initialize so do that once only;
  // even with static initialization, getTimezoneOffset and format are pretty slow...
  if (typeof dtformat === "undefined") {
    try {
      dtformat = Intl.DateTimeFormat("en-US",{timeZoneName:"short"});
    }
    catch(exn) {
      dtformat = null;
    }
  }
  return (dtformat == null ? "" : dtformat.format(d).replace(/^.*\s([\w\+\-:]{3,6})$/,"$1"));
}

function _local_utc_delta(tz,i) {
  var d = new Date(i*1000);
  
  // if we have a datetime format, we get the timezone abbreviation
  var abbrv = _local_get_timezone_abbrv(d);
  
  // Return the timezone offset (switch sign!) and the timezone abbreviation
  return $std_core._tuple2_( (d.getTimezoneOffset())*-60, abbrv );
}

