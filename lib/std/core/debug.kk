/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Core debugging functions.
module std/core/debug

import std/core/types
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
pub val file/kk-line : string = ""

// Compilation constant that is replaced with the current file name
pub val file/kk-file : string = ""

pub fun file/kk-file-line( ?kk-file, ?kk-line )
  ?kk-file ++ "(" ++ ?kk-line ++ ")"

// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------

extern xtrace : ( message : string ) -> ()
  c  "kk_trace"
  cs "Primitive.Trace"
  js "$std_core_console._trace"

extern xtrace-any : forall<a> ( message: string, x : a ) -> ()
  c  "kk_trace_any"
  cs "Primitive.TraceAny"
  js "$std_core_console._trace_any"

val trace-enabled : ref<global,bool> = unsafe-total{ ref(True) }

// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.
pub fun trace( message : string ) : ()
  unsafe-total
    if !trace-enabled then xtrace(message)

pub fun trace-info( message : string, ?kk-file-line : string ) : ()
  trace(?kk-file-line ++ ": " ++ message)

pub fun trace-show( x : a, ?show : a -> string, ?kk-file-line : string ) : ()
  trace-info(x.show)

pub fun trace-any( message : string, x : a ) : ()
  unsafe-total
    if !trace-enabled then xtrace-any(message,x)

// Disable tracing completely.
pub noinline fun notrace() : st<global> ()
  trace-enabled := False

noinline extern unsafe-assert-fail( msg : string ) : ()
  c "kk_assert_fail"
  js inline "function() { throw new Error(\"assertion failed: \" + #1) }()"

pub fun assert( message : string, condition : bool, ?kk-file-line : string ) : ()   // Compiler removes assert calls in optimized builds
  if !condition then unsafe-assert-fail(kk-file-line ++ ": " ++ message)

// Explicitly trigger a breakpoint
pub extern breakpoint() : ndet () {
  c "kk_debugger_break"
  cs "System.Diagnostics.Debugger.Break"
  js inline "(function(){ debugger; })()"
}