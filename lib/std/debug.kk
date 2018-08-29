/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Debugging support

*/
module std/debug



public extern breakpoint() : ndet () {
  cs "System.Diagnostics.Debugger.Break"
  js inline "(function(){ debugger; })()"
}

public extern launch() : ndet () {
  cs "System.Diagnostics.Debugger.Launch"
  js inline "(function(){ debugger; })()"
}