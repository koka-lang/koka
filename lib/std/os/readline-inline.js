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
      var   input = buffer.toString("utf8", 0, n); 
      const len = input.length;
      if (input.endsWith("\r\n")) {
        input = input.substr(0,len-2);
      }
      else if (input.endsWith("\n")) {
        input = input.substr(0,len-1);
      }
      // console.log("read: " + input);
      return $std_core.Ok( input );
    }
    catch(exn) {
      return $std_core._error_from_exception(exn);
    }
  }  
}
