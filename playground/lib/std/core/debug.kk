/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Core debugging functions.
module std/core/debug

import std/core/types
import std/core/int
import std/core/unsafe
import std/core/hnd
import std/core/string
import std/core/console

extern import
  c file "inline/debug.c"

// ------------------------------------------------------------------------------
// File locations
// ------------------------------------------------------------------------------

// Compilation constant that is replaced with the current file's module name
pub val file/kk-module : string = ""

// Compilation constant that is replaced with the current line number
pub val file/kk-line : int = 0

// Compilation constant that is replaced with the current file name
pub val file/kk-file : string = ""

pub fip fun file/kk-file-line( ?kk-file : string, ?kk-line : int ) : string
  ?kk-file ++ "(" ++ show(?kk-line) ++ ")"

// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------

fip extern xtrace : ( message : string ) -> ()
  c  "kk_trace"
  cs "Primitive.Trace"
  js "$std_core_console._trace"

fip extern xtrace-any : forall<a> ( message: string, x : a ) -> ()
  c  "kk_trace_any"
  cs "Primitive.TraceAny"
  js "$std_core_console._trace_any"

val trace-enabled : ref<global,bool> = unsafe-total{ ref(True) }

// Trace a message used for debug purposes.
// The behavior is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.
pub fip fun trace( message : string ) : ()
  unsafe-total
    if !trace-enabled then xtrace(message)

pub fip fun trace-info( message : string, ?kk-file-line : string ) : ()
  trace(?kk-file-line ++ ": " ++ message)

pub fip fun trace-show( x : a, ?show : a -> e string, ?kk-file-line : string ) : e ()
  trace-info(x.show,?kk-file-line=?kk-file-line)

pub fip fun trace-any( message : string, x : a ) : ()
  unsafe-total
    if !trace-enabled then xtrace-any(message,x)

// Disable tracing completely.
pub noinline fun notrace() : st<global> ()
  trace-enabled := False


noinline extern unsafe-abort( msg : string ) : a
  c "kk_abort"
  js inline "function() { throw new Error(\"fatal error: \" + #1) }()"

pub noinline fip fun impossible( message : string = "?", ?kk-file-line : string ) : a
  unsafe-abort(kk-file-line ++ ": impossible:" ++ message)

pub fip fun assert( message : string, condition : bool, ?kk-file-line : string ) : ()   // Compiler removes assert calls in optimized builds
  if !condition then unsafe-abort(kk-file-line ++ ": assertion failed: " ++ message)


// Explicitly trigger a breakpoint
pub fip extern breakpoint() : ndet () {
  c "kk_debugger_break"
  cs "System.Diagnostics.Debugger.Break"
  js inline "(function(){ debugger; })()"
}

