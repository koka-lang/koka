/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

var sync_readline_err = function(){ 
  return $std_core._error_from_exception(new Error("readline is not supported")); 
}

if ($std_core.host()=="node")
{
  var fs = await import("fs");
  sync_readline_err = function(){
    try {
      const buffer = Buffer.alloc(1024);
      const n = fs.readSync(0, buffer);
      const input = (n <= 0 ? "" : buffer.toString("utf8", 0, n-1));  // no ending newline
      // console.log("read: " + input);
      return $std_core.Ok( input );
    }
    catch(exn) {
      return $std_core._error_from_exception(exn);
    }
  }  
}
