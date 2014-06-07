/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Utility function to access the DOM inside a browser.
module sys/dom

import sys/dom/types

// Execute an action that gains access to the global ":window" object. 
public function withWindow( action: (window) -> <dom|e> () ) : <dom|e> ()
{
  val success = applyWindow(action);
  if (!success) error("this application can only run inside a browser")
}

external applyWindow : forall<e> ( action: (window) -> <dom|e> () ) -> <dom|e> bool { 
  js inline "(typeof window !== 'undefined' ? (function(){ (#1)(window); return true; })() : false)" 
}


// Execute an action after a specified time out.
public external timeout : forall<e>( msecs: int, action: () -> <dom|e> () ) -> <dom|e> ()
{ 
  js inline "window.setTimeout(#2,#1)" 
}

// Execute an action on a specified interval.
public external interval : forall<e>( msecs: int, action: () -> <dom|e> () ) -> <dom|e> ()
{ 
  js inline "window.setInterval(#2,#1)" 
}
