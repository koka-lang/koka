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
// Use `on-exn` or `on-exit` when appropriate.
pub fun exn/try( action : () -> <exn|e> a, hndl: exception -> e a ) : e a
  with final ctl throw-exn(exn) hndl(exn)
  action()

// _Deprecated_; use `try` instead. Catch an exception raised by `throw` and handle it.
// Use `on-exn` or `on-exit` when appropriate.
pub fun catch( action : () -> <exn|e> a, hndl: exception -> e a) : e a
  try(action,hndl)

// An `:error` type represents a first-class exception result.
pub value type error<a>
  Error( exception : exception )
  Ok( result : a )

// Transform an exception effect to an  `:error` type.
pub fun try( action : () -> <exn|e> a ) : e error<a>
  try({ Ok(action()) }, fn(exn){ Error(exn) })

// Transform an `:error` type back to an `exn` effect.
pub fun untry( err : error<a> ) : exn a
  match err
    Error(exn) -> throw-exn(exn)
    Ok(x)      -> x

// Transform an `:error` type back to an `exn` effect.
pub fun exn( err : error<a> ) : exn a
  untry(err)

// Use default value `def` in case of an error.
pub fun default( t : error<a>, def : a ) : a
  match t
    Error  -> def
    Ok(x)  -> x

// Transform an `:error` type to a `:maybe` value.
pub fun maybe( t : error<a> ) : maybe<a>
  match t
    Error  -> Nothing
    Ok(x)  -> Just(x)

// Transform an `:error` type to an `:either` value.
pub fun either( t : error<a> ) : either<exception,a>
  match t
    Error(exn) -> Left(exn)
    Ok(x)      -> Right(x)

/*
// Set a `hndler` that is called only when an exception is raised in the `action` block.
pub fun on-exn( hndler : exception -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a
  // prim-try-some( action, fn(exn){ hndler(exn); throw(exn) })
  match mask<exn>{ try(action) }
    TryOk(x)    { x
    TryExn(exn) { hndler(exn); throw(exn)
*/

// Set a `hndler` that is always called when the `action` finishes (either normally or with an exception).
pub fun on-exit( hndler : () -> e (), action : () -> e a ) : e a
  finally(hndler,action)

pub fun exn-error-range() : exn a
  throw("index out-of-range", ExnRange)


// // _Unsafe_. This function removes the exception effect (`:exn`) from the effect of an action
// pub fun unsafe-no-exn( action : () -> <exn|e> a ) : e a
//   unsafe-total( action )

