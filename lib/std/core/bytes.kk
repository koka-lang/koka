/*---------------------------------------------------------------------------
  Copyright 2024-2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Byte sequences.
module std/core/bytes

import std/core/types
import std/core/exn
import std/core/bool
import std/core/int
import std/core/vector
import std/core/string
import std/core/list

extern import 
  js file "inline/bytes.js"

// Create a new empty array of bytes
extern prim-empty() : bytes
  c  inline "kk_bytes_empty()"
  js inline "new Uint8Array(0)"

pub val empty : bytes = prim-empty()  

// Allocate a new zero initialized array of `n` bytes.
extern prim-bytes( n : ssize_t ) : bytes
  c  inline "kk_bytes_zalloc_buf(#1, NULL, kk_context())"
  js inline "new Uint8Array(#1).fill(0)" 

// Allocate a array of `n` uninitialized bytes.
extern prim-unsafe-bytes( n : ssize_t ) : bytes
  c  inline "kk_bytes_alloc_buf(#1, NULL, kk_context())"
  js inline "new Uint8Array(#1)"

// Create `len` new zero-initialized bytes.
pub fun bytes( len : int ) : bytes
  prim-bytes(len.ssize_t)  

// Create `len` uninitialized bytes.
pub fun unsafe-bytes( len : int ) : bytes
  prim-unsafe-bytes(len.ssize_t)  


// Gets the length of the byte array
extern prim-length( ^b : bytes ) : ssize_t
  c  "kk_bytes_len_borrow"
  js inline "#1.length"

// The count of bytes
pub fun length( bs : bytes ) : int
  bs.prim-length.int

// Are the bytes empty?
pub fun is-empty( bs : bytes ) : bool
  bs.length == 0 


// Adjusts the byte array to be the size of the `len` argument
extern prim-adjust-length(b: bytes, len: ssize_t): bytes
  c  "kk_bytes_adjust_length"
  js "kk_bytes_adjust_length"

extern prim-append(b1: bytes, b2: bytes): bytes
  c  "kk_bytes_cat"
  js "kk_bytes_cat"  

extern prim-join(bss : vector<bytes> ) : bytes
  c  "kk_bytes_join"
  js "kk_bytes_join"

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


extern prim-subbytes(bs : bytes, start : ssize_t, len : ssize_t ) : bytes
  c  "kk_bytes_subbytes"
  js inline "#1.slice(#2,#3)"

// Return a range of `len` bytes starting at `start`.
// If `len < 0`, it copies all bytes from `start` to the end (`bs.length - start`).
pub fun subbytes( bs : bytes, start : int = 0, len : int = -1 ) : bytes
  val total = bs.length
  if start >= total then return empty
  val ofs   = if start < 0 then 0 else start
  val count = if len < 0 || ofs + len > total then total - ofs else len
  if count <= 0 then return empty
  prim-subbytes(bs,ofs.ssize_t,count.ssize_t)



// Converts bytes to a vector
extern prim-subvector( bytes : bytes, start : ssize_t, len : ssize_t ) : vector<int>
  c  "kk_bytes_subvector"
  js "kk_bytes_subvector"
  
// Convert a range of bytes to vector of integers.
pub fun subvector( bytes : bytes, start : int = 0, len : int = -1 ) : vector<int>
  val total = bytes.length
  if start >= total then return empty()
  val ofs   = max(0,start)
  val count = if len < 0 || ofs + len > total then total - ofs else len
  if count <= 0 then empty()
                else prim-subvector(bytes,ofs.ssize_t,count.ssize_t)

// Convert a vector of integers to bytes (by clamping between -128 and 127)
pub extern vector/bytes( v : vector<int> ) : bytes
  c  "kk_bytes_from_vector"  
  js "kk_bytes_from_vector"

// Convert a string to a sequence of utf-8 encoded bytes
pub extern string/bytes( s : string ) : bytes
  c  inline "#1.bytes"
  js inline "(new TextEncoder()).encode(#1)"


// Get's the byte at the offset `i`, without checking the offset is valid
pub extern unsafe-index( ^b : bytes, i : ssize_t ) : int8
  c  "kk_bytes_at"
  js inline "#1[#2]"

// Assigns the byte at offset `i` to `new-value`. Does not check the bounds!
pub extern unsafe-assign( b : bytes, i : ssize_t, new-value : int8 ) : bytes
  c  "kk_bytes_set"
  js "kk_bytes_assign"

// Get's the byte at the offset `i`
pub fun @index( ^b : bytes, i : int): exn int
  if i < 0 || i >= b.length then throw("index out of bounds", ExnRange)
  b.unsafe-index(i.ssize_t).int

// Assign a (clamped) byte `new-byte` at index `i`. Throws on out-of-bound.
pub fun assign( bs : bytes, i : int, new-byte : int ) : exn bytes
  if i < 0 || i >= bs.length then throw("index out of bounds", ExnRange)
  bs.unsafe-assign(i.ssize_t,new-byte.int8)



  
// Converts a sequence of qutf-8 bytes to a string.
pub extern string( bytes : bytes ) : string
  c  "kk_string_convert_from_qutf8"
  js "kk_string_from_bytes"


extern prim-substring( b : bytes, start : ssize_t, len : ssize_t ) : string
  c  "kk_string_convert_from_qutf8_slice"
  js "kk_bytes_substring"

// Return a range of utf-8 encoded bytes as qutf-8
pub fun substring( b : bytes, start : int = 0, len : int = -1 ) : string
  val total = b.length
  val ofs   = start.clamp(0,total)
  val count = if len < 0 || ofs + len > total then total - ofs else len
  if count <= 0 then "" else prim-substring(b,ofs.ssize_t,len.ssize_t)

// Return initial `n<=3` utf-8 continuation bytes (`>=0x80`)
extern utf8-partial-pre( ^b : bytes ) : ssize_t
  c  "kk_bytes_utf8_partial_pre_borrow"
  js "kk_bytes_utf8_partial_pre"

// Return final `n<=3` bytes for an incomplete utf-8 code point encoding. 
extern utf8-partial-post( ^b : bytes ) : ssize_t
  c  "kk_bytes_utf8_partial_post_borrow"
  js "kk_bytes_utf8_partial_post"

fun partial-strings( b : bytes, previous-post : bytes = empty ) : (list<string>,bytes)
  val pre-len  = utf8-partial-pre(b).int
  val post-len = utf8-partial-post(b).int
  val mid-len  = max(0,b.length - pre-len - post-len)
  val partial  = if pre-len == 0 then previous-post else previous-post ++ b.subbytes(0,pre-len)
  if (mid-len == 0 && post-len == 0)
    then ([],partial)
    else ( (if partial.is-empty() then [] else [partial.string]) ++ [b.substring(pre-len,mid-len)]
         , b.subbytes(pre-len + mid-len, post-len) )

// Convert a sequence of qutf-8 bytes to a string. Take a potential `previous-post` previous 
// uncompleted utf-8 code point (of at most 3 bytes) (in case the full byte sequence was split 
// over a utf-8 code point). Returns the qutf-8 decoded string and also potential left-over
// bytes for a final uncompleted utf-8 code point (of at most 3 bytes). 
pub fun partial-string( b : bytes, previous-post : bytes = empty ) : (string,bytes)
  val (parts,post) = b.partial-strings(previous-post)
  (parts.join,post)


// Join a list of utf-8 bytes into a string.
// Ensures that utf-8 sequences that are split over different bytes are handled correctly.
pub fun list/string( bs : list<bytes> ) : string
  tail fun collect( previous-post : bytes, xs : list<bytes> ) : list<list<string>>
    match xs 
      Nil -> [[previous-post.string]]
      Cons(b,bb) ->
        val (part,post) = b.partial-strings(previous-post)
        Cons(part,collect(post,bb))
  collect(empty,bs).concat.join
