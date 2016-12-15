/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Access to the program environment and commmand line arguments.

Print the environment: `get-env().list.map(fun(p) { p.fst + "=" + p.snd }).join("\n").print`
*/
module std/os/env

import std/os/path
import std/data/dict

extern xget-env() : ndet dict<string>  {
  cs inline "Primitive.DictFromStringCollection(System.Environment.GetEnvironmentVariables())"
  js inline "(typeof process !== 'undefined' ? process.env : {})"
}

private val env : delayed<<ndet>,dict<string>> = delay(xget-env)

// Get the environment variables for this program
public fun get-env() : ndet dict<string>  {
  env.force
}

// Get the environment variables for this program
fun get-env-list() : ndet list<(string,string)>  {
  get-env().list
}

// Returns the value of an environment variable `name`.
// Returns `default` (= `""`) if the environment variable was not present.
public fun get-env( name : string, default : string = "" ) : ndet string
{
  match(get-env()[name]) {
    Nothing -> default
    Just(s) -> s
  }
}

extern xget-argv() : ndet vector<string>  {
  cs "System.Environment.GetCommandLineArgs"
  js inline "(typeof process !== 'undefined' ? process.argv : [])"
}

private val argv : delayed<<ndet>,list<string>> = delay{ xget-argv().list }

// The unprocessed command line that was used to start this program.
// On ''Node'' the first arguments will often be of the form `["node","interactive.js",...]`.
public fun get-argv() : ndet list<string> {
  argv.force
}


// Return the arguments that were passed to program itself.
// Strips off the initial program from the unprocessed command line.
// i.e. If a program started as:
// ````
// > node myprogram.js --flag bla
// ````
// The `arguments` list will be `["--flag","bla"]`
public fun get-args() : ndet list<string> {
  match(get-argv()) {
    Cons(x,xx) | x.path.stemname == "node" -> xx.drop(1)
    xs -> xs.drop(1)
  }
}

