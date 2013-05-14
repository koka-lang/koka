/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Access to the program environment and commmand line arguments.
*/
module std/env

external isNull : string -> bool  = inline "(#1 == null)"
external getEnvironmentVariable : string -> string  {
  cs "System.Environment.GetEnvironmentVariable"
  js inline "(typeof process !== 'undefined' ? process.env[#1] : null)"
}

// Returns the value of an environment variable "name".
// Returns "default" (= "") if the environment variable was not present.
public function getEnv( name : string, default : string = "" ) : string
{
  s = getEnvironmentVariable(name)
  return (if (isNull(s)) then default else s)
}

// Returns the unprocessed command line that was used to start this program.
public external argv() : vector<string>  {
  cs "System.Environment.GetCommandLineArgs"
  js inline "(typeof process !== 'undefined' ? process.argv : [])"
}

