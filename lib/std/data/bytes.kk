
// ----------------------------------------------------------------------------
// Bytes
// ----------------------------------------------------------------------------

pub extern bytes/empty(): bytes
  c  inline "kk_bytes_empty()"
  cs "Primitive.BytesEmpty"
  js inline "new Uint8Array(0)"

pub extern bytes/alloc( n : ssize_t ) : bytes
  c  inline "kk_bytes_alloc_buf(#1, NULL, kk_context())"
  cs "Primitive.BytesAlloc"
  js inline "new Uint8Array(#1).fill(#2)"

pub extern bytes/string( n : bytes ) : string
  c  "kk_string_convert_from_qutf8"
  cs "Primitive.BytesToString"
  js inline "String.fromCharCode.apply(null, #1)"

pub extern string/bytes( s : string ) : bytes
  c  inline "#1.bytes"
  cs "Primitive.BytesFromString"
  js inline "new TextEncoder().encode(#1)"

pub extern bytes/length( ^b : bytes ) : ssize_t
  c  "kk_bytes_len_borrow"
  cs "Primitive.BytesLen"
  js inline "#1.length"

pub extern bytes/adjust-length(b: bytes, len: ssize_t): bytes
  c  "kk_bytes_adjust_length"

pub extern bytes/advance(b: bytes, count: ssize_t): bytes
  c  "kk_bytes_advance"

pub extern bytes/index( ^b : bytes, ^i : ssize_t ) : int8
  c  "kk_bytes_at"
  cs "Primitive.BytesAt"
  js inline "#1[#2]"

pub extern bytes/int8/assign( ^b : bytes, ^i : ssize_t, x : int8 ) : ()
  c  "kk_bytes_set"
  cs "Primitive.BytesSet"
  js inline "#1[#2] = #3"
