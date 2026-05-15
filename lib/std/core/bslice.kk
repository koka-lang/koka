/*---------------------------------------------------------------------------
  Copyright 2024, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Byte slices
module std/core/bslice
import std/core/bytes
import std/core/types
import std/core/int
import std/core/exn

extern import 
  c file "inline/bslice" 

// A byte slice
pub value struct bslice 
  backing-bytes: bytes
  start: int
  len: int // The length of the slice
  total-len: int // Cached length of the backing byte array

// Create a byte slice from bytes
pub fun slice(b: bytes): bslice
  val len = b.length.int
  Bslice(b, 0, len, len)

extern external/bytes(b: bslice): bytes
  c "kk_bslice_bytes"

// Get the bytes referenced by the slice
pub fun get/bytes(b: bslice): bytes
  external/bytes(b)

// If the slice is not empty,
// return the first byte, and a new slice that is advanced by 1.
pub fun next(b : bslice): maybe<(int8, bslice)>
  val Bslice(bts, start, len, tl) = b
  if start >= tl then Nothing
  else Just((bts.unsafe-index(start.ssize_t), Bslice(bts, start + 1, len - 1, tl)))

// O(1). Advance the start position of a byte slice by `count` bytes
// up to the end of the byte array.
// A negative `count` advances the start position backwards up to index 0.
pub fun advance(b: bslice, n: int): bslice
  val Bslice(bts, start, len, tl) = b
  if n > 0 then
    if n > len then // If advancing past the current length, advance to the end
      Bslice(bts, start + len, 0, tl)
    else
      Bslice(bts, start + n, len - n, tl)
  elif n < 0 then
    if start + n < 0 then // If advancing past the beginning, advance to the beginning
      Bslice(bts, 0, len + start, tl)
    else 
      Bslice(bts, start + n, len - n, tl)
  else 
    Bslice(bts, start, len, tl)

// O(1). Drop the first `n` bytes of the slice
// If there is not `n` bytes to drop returns the empty slice
pub fun drop(b: bslice, n : int): bslice
  val Bslice(bts, start, len, tl) = b 
  if n >= len then Bslice(bts, tl, 0, tl)
  else Bslice(bts, start + n, len - n, tl)

// O(1). Get the subslice from `slice.start + start` to `slice.start + end`. 
// If the new start is past the end of the slice, returns the empty slice
// If the end is past the end of the slice, return just to the end.
pub fun subslice(b : bslice, start: int, end: int): bslice
  val Bslice(bts, s0, l, tl) = b
  if start > l then Bslice(bts, tl, 0, tl)
  else if end >= l then Bslice(bts, s0 + start, l - start, tl)
  else Bslice(bts, s0 + start, end - start, tl)

// O(1). Extend a byte slice by `count` bytes up to the end of the byte slice.
// A negative `count` shrinks the slice up to the empty slice.
pub fun extend(b: bslice, n: int): bslice
  val Bslice(bts, start, len, tl) = b
  if n > 0 then
    if n > (tl - len) then // If extending past total end of bytes, extend to end of bytes
      Bslice(bts, start, tl - start, tl)
    else
      Bslice(bts, start, len + n, tl)
  elif n < 0 then
    if len + n < 0 then // If extending past start of slice, extend to start point
      Bslice(bts, start, 0, tl)
    else
      Bslice(bts, start, len + n, tl)
  else 
    Bslice(bts, start, len, tl)

// Truncates the slice to length 0
pub fun truncate(b: bslice): bslice 
  val Bslice(bts, start, _, tl) = b
  Bslice(bts, start, 0, tl)

// O(1). Return the byte slice from the start of the byte array up to the
// start of the `slice` argument.
pub fun before(slice: bslice): bslice
  val Bslice(bts, start, _, tl) = slice
  Bslice(bts, 0, start, tl)

// O(1). Return the byte slice from the end of the `slice` argument
// to the end of the byte array.
pub fun after(slice: bslice): bslice
  val Bslice(bts, start, len, tl) = slice
  val new-start = start + len
  Bslice(bts, new-start, tl - new-start, tl)

// Get's the byte at the offset `i`
pub fun @index( ^b : bslice, i : int ) : exn int8
  val Bslice(bts, start, len, _) = b
  if i < 0 || i >= len then throw("index out of bounds", ExnRange)
  bts.unsafe-index((start + i).ssize_t)

// Assigns the bytes starting at offset `i` to `new-value`, throws if there is not enough space or the index is invalid
pub fun assign( b : bytes, i : int, new-value : bslice ) : exn bytes
  val l = b.length.int
  val size = new-value.len
  if i < 0 || i > l then 
    throw("index out of bounds", ExnRange)
  elif size + i > l then
    throw("size overflow", ExnRange)
  else
    assign(b, i.ssize_t, new-value)

extern slice/assign(b : bytes, i : ssize_t, new-value : bslice ): bytes
  c "kk_bslice_assign"