/*---------------------------------------------------------------------------
  Copyright 2024-2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Byte slices
//
// Todo: add iteration (foreach, prev, next etc)
module std/core/bslice

import std/core/debug
import std/core/bytes
import std/core/types
import std/core/int
import std/core/exn
import std/core/list

extern import 
  c file "inline/bslice" 

// A byte slice represents a range of bytes in a `:bytes` type.
abstract value struct bslice 
  backing-bytes : bytes
  start : int
  len   : int 
  

// An empty byte slice
pub val empty = Bslice(empty,0,0)

// Is a slice empty?
pub fun is-empty( slice : bslice ) : bool
  slice.len <= 0

// Create a byte slice from bytes
pub fun slice( b : bytes ) : bslice
  Bslice(b, 0, b.length)

// View of the first `n` bytes
pub fun first( b : bytes, n : int = 1 ) : bslice
  Bslice(b, 0, n.clamp(0,b.length) )

// View of the last `n` bytes
pub fun last( b : bytes, n : int = 1 ) : bslice
  val total = b.length
  val count = n.clamp(0,total)
  val start = total - count
  Bslice(b, start, count )

// Get the bytes referenced by the slice
pub fun bytes( Bslice(b,start,len) : bslice ) : bytes
  b.subbytes( start, len )

// If the slice is not empty,
// return the first byte, and a new slice that is advanced by 1.
pub fun next( Bslice(b,start,len) : bslice ) : maybe2<int, bslice>
  if len <= 0
    then Nothing2
    else Just2(b.unsafe-index(start.ssize_t).int, Bslice(b, start + 1, len - 1))
  
// O(1). Advance the start position of a byte slice by `count` bytes
// up to the end of the byte array.
// A negative `count` advances the start position backwards up to index 0.
pub fun advance( Bslice(b,start,len) : bslice, count : int) : bslice
  val total    = b.length
  val newstart = clamp(start + count, 0, total)
  val newlen   = max(len, total - newstart)
  Bslice(b,newstart,newlen)

// O(1). Extend a byte slice range by `count` bytes up to the end of the byte slice.
// A negative `count` shrinks the slice up to the empty slice.
pub fun extend( Bslice(b,start,len) : bslice, count : int ) : bslice
  val total    = b.length
  val newlen   = clamp( len + count, 0, total - start )
  Bslice(b,start,newlen)

// Truncates a slice to length 0
pub fun truncate( Bslice(b,start,_): bslice ): bslice
  Bslice(b,start,0)

// O(1). Drop the first `n` bytes of the slice. Identity for `n<=0`.
pub fun drop( Bslice(b,start,len) : bslice, n : int ) : bslice
  val count    = min(n,0)
  val total    = b.length
  val newstart = clamp(start + count, 0, total)
  val newlen   = clamp(len - count, 0, total - newstart)
  Bslice(b,newstart,newlen)

// O(1). Take the first `n` bytes of the slice.
pub fun take( Bslice(b,start,len) : bslice, n : int ) : bslice
  val newlen   = n.clamp(0,len)
  Bslice(b,start,newlen)

// O(1). Get a subslice of `len` bytes from `start` in a given byte slice.
pub fun subslice( Bslice(b,bstart,blen) : bslice, start : int, len : int ) : bslice
  val newstart = start.clamp(0,blen)
  val newlen   = len.clamp(0,blen - newstart)
  Bslice(b,bstart + newstart, newlen)
  
// O(1). Return the byte slice from the start of the bytes up to the
// start of the `slice` argument.
pub fun before( Bslice(b,start,_) : bslice ) : bslice
  Bslice(b, 0, start)

// O(1). Return the byte slice from the end of the `slice` argument
// to the end of the byte array.
pub fun after( Bslice(b,start,len) : bslice ) : bslice
  val newstart = start + len
  val newlen   = max(0,b.length - newstart)  // assert: b.length - newstart >= 0
  Bslice(b, newstart, newlen)

// Get's the byte at the offset `i`
pub fun @index( ^(Bslice(b,start,len)) : bslice, i : int ) : exn int
  if i < 0 || i >= len then throw("index out of bounds", ExnRange)
  b.unsafe-index((start + i).ssize_t).int



/*
// Assigns the bytes starting at offset `i` to `new-value`, throws if there is not enough space or the index is invalid
pub fun assign( b : bytes, i : int, new-value : bslice ) : exn bytes
  val l = b.length
  val size = new-value.len
  if i < 0 || i > l then 
    throw("index out of bounds", ExnRange)
  elif size + i > l then
    throw("size overflow", ExnRange)
  else
    assign(b, i.ssize_t, new-value)

extern slice/assign(b : bytes, i : ssize_t, new-value : bslice ): bytes
  c "kk_bslice_assign"
*/  