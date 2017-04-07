/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Log messages
public module std/log 

import std/data/dict

private val log-dict : mdict<global,string> = unsafe-total { mdict() }

// Encloses a possibly total `action` and returns its result together
// with the contents of a log named `log-name`. The contents of the log
// are non-deterministically determined. However, in practice, they contain
// any messages that were recorded in the `action` by calls to `log`.
// However, if `nolog` was called, the log is always empty.
fun with-log( log-name : string, action : () -> <ndet|e> a ) : <ndet|e> (string, a) {
  unsafe-total {
    val old-log = log-dict[log-name]
    log-dict[log-name] := ""
    val x = unsafe-total(action)
    val l = log-dict[log-name].maybe("")
    match(old-log) {
      Nothing -> ()
      Just(old) -> log-dict[log-name] := old
    }
    (l,x)
  }
}

// Get the current log. Since this is in the `io` effect, `with-log` is preferred.
fun get-log( log-name : string ) : io string {
  log-dict[log-name].maybe("")
}

// Total fun that logs a message in a certain log `log-name`.
// Has no observable effect but see `withLog`.
fun log( log-name : string, message : string ) : () {
  unsafe-total {
    if (!(log-dict.contains-key("nolog")) && log-dict.contains-key(log-name)) {
      log-dict[log-name] := log-dict[log-name].maybe("") + (message + "\n")      
    }
  }
}

// Disable logging completely.
fun nolog() : st<global> () {
  log-dict["nolog"] := ""
}


// Log an error (to log `"error"`). This should be used in particular
// to log errors in pure functions that are otherwise ignored.
fun log-error( message : string ) : () {
  log("error", message)
}

// Log a warning (to log `"warning"`). This should be used in particular
// to log warnings in pure functions that are otherwise ignored.
fun log-warning( message : string ) : () {
  log("warning", message)
}