//---------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// Updated as of 7/24/26: Commit 32ada87

module compiler/lib/trace

import std/core/unsafe
pub import pretty/printer
import pretty/pprint

pub fun color/trace(s: string, a: a, ?kk-file-line: string): a
  ctrace(DarkGray, s, a)

pub fun unit/trace(s: string, ?kk-file-line: string): ()
  trace(s, ())

pub fun trace-id(s: string, ?kk-file-line: string): string
  trace(s, s)

pub fun trace-doc(d: doc, a: a, ?kk-file-line: string): div a
  trace(d.show, a)

pub fun unit/trace-show(a: a, ?show: a -> e string, ?kk-file-line: string): e ()
  color/trace(a.show, ())

pub fun id/trace-show(a: a, ?show: a -> e string, ?kk-file-line: string): e a
  color/trace(a.show, a)

pub fun trace-eq(s: string, a: a, ?show: (a) -> string, ?kk-file-line: string): a
  trace(s ++ " = " ++ a.show, a)

// Traces go to stderr and without color: the language server uses stdout for its JSON-RPC protocol.
// `c` is accepted so call sites can keep naming the color they would like.
pub fun ctrace(c: color, message: string, a: a, ?kk-file-line: string): a
  std/core/debug/trace(?kk-file-line ++ ": " ++ message)
  a
