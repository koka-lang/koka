/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard exception (`:exn`) effect.
module std/core/exn

import std/core/types
import std/core/hnd

extern import
  c  file "inline/exn"
  js file "inline/exn.js"


// Exceptions
pub effect exn
  // Throw an exception
  final ctl throw-exn( exn : exception ) : a

// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.
pub fun error-pattern(location : string, definition : string) : exn a
  throw(location ++ ": " ++ definition ++ ": pattern match failure",
         ExnPattern(location,definition))

// The exception data type
pub value struct exception( message :string, info :exception-info )

// Exception information
pub open type exception-info
  ExnError      // Generic error
  ExnAssert
  ExnTodo
  ExnRange
  ExnPattern( location : string, definition : string )
  ExnSystem( errno : int )
  ExnInternal( name : string )

// Show the exception message
pub fun show( exn : exception ) : string
  exn.message

// Throw an exception with a specified message.
pub fun throw( message: string, info : exception-info = ExnError ) : exn a
  throw-exn(Exception(message,info))

// Catch any exception raised in `action` and handle it.
// Use `on-exit` when appropriate.
pub fun handle/try( action : () -> <exn|e> a, hndl: exception -> e a ) : e a
  with final ctl throw-exn(exn) hndl(exn)
  action()

// Catch an exception raised by `throw` and handle it.
// This is `try` but with the arguments reversed, and is
// often convenient to use as `with catch fn(e) ...`
pub inline fun catch(hndl: exception -> e a, action : () -> <exn|e> a) : e a
  try(action,hndl)

// An `:error<a>` type represents a first-class exception result as a `:result<a,exception>`.
pub alias error<a> = result<a,exception>

// Transform an exception effect to an  `:error` type.
pub fun try( action : () -> <exn|e> a ) : e error<a>
  try({ Ok(action()) }, fn(exn){ Error(exn) })

// Transform an exception result type back to an `exn` effect (by throwing on an `Error`)
pub fun untry( err : error<a> ) : exn a
  match err
    Error(exn) -> throw-exn(exn)
    Ok(x)      -> x

// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
pub fun on-exit( hndler : () -> e (), action : () -> e a ) : e a
  finally(hndler,action)

pub fun exn-error-range() : exn a
  throw("index out-of-range", ExnRange)

// Prepend `exn`'s message with `pre`.
pub fun prepend( exn : exception, pre : string ) : exception
  Exception(pre ++ ": " ++ exn.message, exn.info)

