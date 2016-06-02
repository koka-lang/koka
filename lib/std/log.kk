// Log messages
public module std/log 

import std/dict

private val logDict : mdict<global,string> = unsafeTotal { mdict() }

// Encloses a possibly total `action` and returns its result together
// with the contents of a log named `logName`. The contents of the log
// are non-deterministically determined. However, in practice, they contain
// any messages that were recorded in the `action` by calls to `log`.
// However, if `nolog` was called, the log is always empty.
fun withLog( logName : string, action : () -> <ndet|e> a ) : <ndet|e> (string, a) {
  unsafeTotal {
    val oldLog = logDict[logName]
    logDict[logName] := ""
    val x = unsafeTotal(action)
    val l = logDict[logName].maybe("",id)
    match(oldLog) {
      Nothing -> ()
      Just(old) -> logDict[logName] := old
    }
    (l,x)
  }
}

// Get the current log. Since this is in the `io` effect, `withLog` is preferred.
fun getLog( logName : string ) : io string {
  logDict[logName].maybe("",id)
}

// Total fun that logs a message in a certain log `logName`.
// Has no observable effect but see `withLog`.
fun log( logName : string, message : string ) : () {
  unsafeTotal {
    if (!(logDict.containsKey("nolog")) && logDict.containsKey(logName)) {
      logDict[logName] := logDict[logName].maybe("",id) + (message + "\n")      
    }
  }
}

// Disable logging completely.
fun nolog() : st<global> () {
  logDict["nolog"] := ""
}
