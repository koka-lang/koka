
// ----------------------------------------------------------------------------
// Bytes
// ----------------------------------------------------------------------------

pub struct bslice 
  bytes: bytes
  start: int
  len: int
  total-len: int

// Create a byte slice from bytes
pub fun slice(b: bytes): bslice
  val len = b.length.int
  Bslice(b, 0, len, len)

// Move the start position forward or backward
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

pub fun truncate(b: bslice): bslice 
  val Bslice(bts, start, _, tl) = b
  Bslice(bts, start, 0, tl)

pub fun before(b: bslice): bslice
  val Bslice(bts, start, _, tl) = b
  Bslice(bts, 0, start, tl)

pub fun after(b: bslice): bslice
  val Bslice(bts, start, len, tl) = b
  val new-start = start + len
  Bslice(bts, new-start, tl - new-start, tl)

pub extern empty(): bytes
  c  inline "kk_bytes_empty()"
  cs "Primitive.BytesEmpty"
  js inline "new Uint8Array(0)"

pub extern alloc( n : ssize_t ) : bytes
  c  inline "kk_bytes_alloc_buf(#1, NULL, kk_context())"
  cs "Primitive.BytesAlloc"
  js inline "new Uint8Array(#1).fill(#2)"

pub extern string( n : bytes ) : string
  c  "kk_string_convert_from_qutf8"
  cs "Primitive.BytesToString"
  js inline "String.fromCharCode.apply(null, #1)"

pub extern string/bytes( s : string ) : bytes
  c  inline "#1.bytes"
  cs "Primitive.BytesFromString"
  js inline "new TextEncoder().encode(#1)"

pub extern length( ^b : bytes ) : ssize_t
  c  "kk_bytes_len_borrow"
  cs "Primitive.BytesLen"
  js inline "#1.length"

pub extern adjust-length(b: bytes, len: ssize_t): bytes
  c  "kk_bytes_adjust_length"

pub extern index( ^b : bytes, ^i : ssize_t ) : int8
  c  "kk_bytes_at"
  cs "Primitive.BytesAt"
  js inline "#1[#2]"

pub extern byte/assign( ^b : bytes, ^i : ssize_t, x : int8 ) : ()
  c  "kk_bytes_set"
  cs "Primitive.BytesSet"
  js inline "#1[#2] = #3"
