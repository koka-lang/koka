// ---------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting.
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
// ---------------------------------------------------------------------------
// Updated as of 7/23/24: Commit 48b9d26

module compiler/common/failure
pub import compiler/platform/config

// ------------------------------------------
// Internal errors and assertions
// ------------------------------------------

pub fun failure/fail(s: string): exn a
  raise(s)

pub fun assert(msg: string, test: bool, x: a): exn a
  if test then x else failure/fail(msg)

pub fun todo(msg: string): exn a
  failure/fail("todo: " ++ msg)

pub fun match-failure(msg: string, ?kk-file-line: string): exn a
  failure/fail("unmatched pattern: " ++ msg ++ " at " ++ kk-file-line)

// Upstream's `raise` also prints a stack trace in a debug build; Koka has no stack traces to print.
pub fun raise(s: string): exn a
  throw(s)

// Upstream's `catchIO` strips GHC's "user error" wrappers from the message; a Koka exception carries the message as raised.
pub fun catch(hndl: string -> _ a, f: () -> _ a): _ a
  with handler final ctl throw-exn(e)
    hndl(e.message)
  f()
