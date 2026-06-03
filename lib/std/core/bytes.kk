/*---------------------------------------------------------------------------
  Copyright 2024, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Byte arrays
module std/core/bytes
import std/core/types
import std/core/exn
import std/core/bool
import std/core/int
import std/core/vector

extern import 
  js file "inline/bytes.js"

// Create a new empty array of bytes
extern prim-empty() : bytes
  c  inline "kk_bytes_empty()"
  js inline "new Uint8Array(0)"

pub val empty : bytes = prim-empty()  

// Allocate a new zero initialized array of `n` bytes.
extern prim-alloc( n : ssize_t ) : bytes
  c  inline "kk_bytes_zalloc_buf(#1, NULL, kk_context())"
  js inline "new Uint8Array(#1).fill(0)"

// Allocate a array of `n` uninitialized bytes.
extern prim-unsafe-alloc( n : ssize_t ) : bytes
  c  inline "kk_bytes_alloc_buf(#1, NULL, kk_context())"
  js inline "new Uint8Array(#1)"

// Gets the length of the byte array
extern prim-length( ^b : bytes ) : ssize_t
  c  "kk_bytes_len_borrow"
  js inline "#1.length"

// Adjusts the byte array to be the size of the `len` argument
extern prim-adjust-length(b: bytes, len: ssize_t): bytes
  c  "kk_bytes_adjust_length"
  js "kk_bytes_adjust_length"

extern prim-append(b1: bytes, b2: bytes): bytes
  c  "kk_bytes_cat"
  js "kk_bytes_cat"

extern prim-join(bss: vector<bytes> ) : bytes
  c  "kk_bytes_join"
  js "kk_bytes_join"


// Get's the byte at the offset `i`, without checking the offset is valid
pub extern unsafe-index( ^b : bytes, i : ssize_t ) : int8
  c  "kk_bytes_at"
  js inline "#1[#2]"

// Assigns the byte at offset `i` to `new-value`. Does not check the bounds!
pub extern unsafe-assign( b : bytes, i : ssize_t, new-value : int8 ) : bytes
  c  "kk_bytes_set"
  js "kk_bytes_assign"

// The count of bytes
pub fun length( bs : bytes ) : int
  bs.prim-length.int

// Are the bytes empty?
pub fun is-empty( bs : bytes ) : bool
  bs.length == 0 

// Create `len` new zero-initialized bytes.
pub fun bytes( len : int ) : bytes
  prim-alloc(len.ssize_t)  

// Create `len` uninitialized bytes.
pub fun unsafe-bytes( len : int ) : bytes
  prim-alloc(len.ssize_t)  

// Concatenates two byte arrays
pub fun (++)(b1 : bytes, b2: bytes): bytes
  prim-append(b1, b2)

// Concatenate a vector of bytes
pub fun vector/join(bss : vector<bytes> ) : bytes
  prim-join(bss)

// Concatenate a list of bytes
pub fun list/join(bss : list<bytes> ) : bytes
  bss.vector.join

// Adjust the length of the bytes. If enlarged, the added bytes are initialized to zero.
pub fun adjust-length( bs : bytes, newlen : int ) : bytes
  prim-adjust-length(bs,newlen.ssize_t)
  

// Converts an array of bytes to a string.
pub extern string( bytes : bytes ) : string
  c  "kk_string_convert_from_qutf8"
  js "kk_string_from_bytes"

// Gets a view of a string as raw bytes
pub extern string/bytes( s : string ) : bytes
  c  inline "#1.bytes"
  js inline "(new TextEncoder()).encode(#1)"

// Get's the byte at the offset `i`
pub fun @index( ^b : bytes, i : int): exn int
  if i < 0 || i >= b.length then throw("index out of bounds", ExnRange)
  b.unsafe-index(i.ssize_t).int

// Assign a (clamped) byte `new-byte` at index `i`. Throws on out-of-bound.
pub fun assign( bs : bytes, i : int, new-byte : int ) : exn bytes
  if i < 0 || i >= bs.length then throw("index out of bounds", ExnRange)
  bs.unsafe-assign(i.ssize_t,new-byte.int8)