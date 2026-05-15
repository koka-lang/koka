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

extern import 
  js file "inline/bytes.js"

// Create a new empty array of bytes
pub extern empty(): bytes
  c  inline "kk_bytes_empty()"
  js inline "new Uint8Array(0)"

// Allocate a new zero initialized array of `n` bytes.
pub extern alloc( n : ssize_t ) : bytes
  c  inline "kk_bytes_alloc_buf(#1, NULL, kk_context())"
  js inline "new Uint8Array(#1).fill(0)"

// Converts an array of bytes to a string. (Unsafe, ensure that the bytes have a zero terminated string)
pub extern string( bytes : bytes ) : string
  c  "kk_string_convert_from_qutf8"
  js inline "String.fromCharCode.apply(null, #1)"

// Gets a view of the string as raw bytes
pub extern string/bytes( s : string ) : bytes
  c  inline "#1.bytes"
  js inline "(new TextEncoder()).encode(#1)"

// Gets the length of the byte array
pub extern length( ^b : bytes ) : ssize_t
  c  "kk_bytes_len_borrow"
  js inline "#1.length"

// Adjusts the byte array to be the size of the `len` argument
pub extern adjust-length(b: bytes, len: ssize_t): bytes
  c  "kk_bytes_adjust_length"
  js "#1"

extern extern-append(b1: bytes, b2: bytes): bytes
  c "kk_bytes_cat"
    
// Concatenates two byte arrays
pub fun (++)(b1 : bytes, b2: bytes): bytes
  extern-append(b1, b2)

// Get's the byte at the offset `i`
pub fun @index(^b: bytes, i: int): exn int8 
  if i < 0 || i >= b.length.int then throw("index out of bounds", ExnRange)
  b.unsafe-index(i.ssize_t)

// Get's the byte at the offset `i`, without checking the offset is valid
// !! Attention !! Unsafe API
pub extern unsafe-index( ^b : bytes, i : ssize_t ) : int8
  c  "kk_bytes_at"
  js inline "#1[#2]"

// Assigns the byte at offset `i` to `new-value`
pub extern byte/assign( b : bytes, i : ssize_t, new-value : int8 ) : bytes
  c  "kk_bytes_set"
  js "kk_bytes_assign"
