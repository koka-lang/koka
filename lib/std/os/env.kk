/*---------------------------------------------------------------------------
  Copyright 2012-2016,2020, Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Access to the program environment and commmand line arguments.

Print the environment: `get-env().map(fun(p) { p.fst + "=" + p.snd }).join("\n").print`
*/
module std/os/env

import std/os/path

public alias env = list<(string,string)>

val environ : delayed<<ndet>,env> = delay{ os-get-env().list.to-tuples }

fun to-tuples( xs : list<string> ) : env {
  match(xs) {
    Cons(name,Cons(value,xx)) -> Cons((name,value), xx.to-tuples)
    Cons(name,Nil)            -> [(name,"")] 
    Nil                       -> []
  }
}

extern os-get-env() : ndet vector<string>  {   // vector as [name1,value1,...,nameN,valueN]
  c "kk_os_get_env"  
  js inline "(typeof process !== 'undefined' ? (function(){ var env = []; Object.keys(process.env).forEach(function(name){ env.push(name); env.push(process.env[name]); }); return env; })() : [])"
}

// Get the environment variables for this program
public fun get-env() : ndet env  {
  environ.force
}


// Returns the value of an environment variable `name` (or `Nothing` if not present)
public fun get-env( name : string ) : ndet maybe<string>
{
  get-env().lookup(fun(nm){ nm == name })
}

val argv : delayed<<ndet>,list<string>> = delay{ os-get-argv().list }

extern os-get-argv() : ndet vector<string>  {
  c  "kk_os_get_argv"
  cs "System.Environment.GetCommandLineArgs"
  js inline "(typeof process !== 'undefined' ? process.argv : [])"
}


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
  val is-node = (host() == "node")
  match(get-argv()) {
    Cons(x,xx) | is-node && x.path.stemname == "node" -> xx.drop(1)
    xs -> xs.drop(1)
  }
}
