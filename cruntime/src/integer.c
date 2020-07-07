/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdint.h>
#include <string.h> // memcpy
#include <ctype.h>
#include <math.h>   // INFINITY
#include "runtime.h"
#include "runtime/integer.h"

/*----------------------------------------------------------------------
Big integers. For our purposes, we need an implementation that does not have to be the fastest 
possible; we instead aim for portable, simple, well performing, and with fast conversion to/from decimal strings. 
Still, it performs quite respectable and does have various optimizations including Karatsuba multiplication.

  Big integers are arrays of `digits` with a `count` and `is_neg` flag. For a number `n` we have:
  
  n = (is_neg ? -1 : 1) * (digits[0]*(BASE^0) + digits[1]*(BASE^1) + ... + digits[count-1]*(BASE^(count-1)))

  For any `count>0`, we have `digits[count-1] != 0`.
  We use a decimal representation for efficient conversion of numbers to strings and back.
  We use 32-bit integers for the digits, this way:
  - we can use base 10^9 (which uses 29.9 bits of the 32 available).
  - it can hold `2*BASE + 1` which allows for efficient addition.
  - a double digit `ddigit_t` of 64-bit can hold a full multiply of `BASE*BASE + BASE + 1` 
    which allows efficient multiplication.
----------------------------------------------------------------------*/

typedef int32_t   digit_t;     // 2*BASE + 1 < digit_t_max
typedef int64_t   ddigit_t;    // (BASE*BASE + BASE) + 1 < ddigit_t_max
#define BASE        ((intx_t)1000000000UL)  
#define LOG_BASE    (9)

typedef uint16_t extra_t;
#define MAX_EXTRA           (UINT16_MAX / 2)  // we use 1 bit for the negative bool

typedef struct bigint_s {
  block_t  _block;
#if INTPTR_SIZE>=8
  uint8_t  is_neg: 1;      // negative
  extra_t  extra :15;      // extra digits available: `sizeof(digits) == (count+extra)*sizeof(digit_t)`
  uint64_t count :48;      // count of digits in the number
#else
  uint8_t  is_neg;
  extra_t  extra;
  uint32_t count;
#endif
  digit_t  digits[1];      // digits from least-significant to most significant. 
} bigint_t;


static bool bigint_is_neg_(const bigint_t* b) {
  return (b->is_neg != 0);
}

static intx_t bigint_sign_(const bigint_t* b) {
  return (bigint_is_neg_(b) ? -1 : 1);
}

static size_t bigint_count_(const bigint_t* b) {
  return (b->count);
}

static size_t bigint_available_(const bigint_t* b) {
  return (b->count + b->extra);
}

static digit_t bigint_last_digit_(const bigint_t* b) {
  return b->digits[b->count-1];
}

static integer_t integer_bigint(bigint_t* x, context_t* ctx);

/*----------------------------------------------------------------------
  allocation, ref counts, trim
  functions ending in "_" do not change reference counts of input arguments
----------------------------------------------------------------------*/

static ptr_t bigint_ptr_(bigint_t* x) {
  return &x->_block;
}

static bool bigint_is_unique_(bigint_t* x) {
  return block_is_unique(bigint_ptr_(x));
}

static bigint_t* dup_bigint(bigint_t* x) {
  return dup_datatype_as(bigint_t*, x);
}

static void drop_bigint(bigint_t* x, context_t* ctx) {
  drop_datatype(x,ctx);
}


static size_t bigint_roundup_count(size_t count) {
  if (count < 4) return 4;                      // minimal size of 4 digits (128-bit)
  else if ((count & 1) == 1) return (count+1);  // always even 
  else return count;
}

static bigint_t* bigint_alloc(size_t count, bool is_neg, context_t* ctx) {
  size_t dcount = bigint_roundup_count(count);
  bigint_t* b = (bigint_t*)block_alloc(sizeof(bigint_t) - sizeof(digit_t) + dcount*sizeof(digit_t), 0, TAG_BIGINT, ctx);
  b->is_neg = (is_neg ? 1 : 0);
  b->extra = (extra_t)(dcount - count);
  b->count = count;
  return b;
}

static bigint_t* bigint_alloc_zero(size_t count, bool is_neg, context_t* ctx) {
  bigint_t* b = bigint_alloc(count, is_neg, ctx);
  memset(b->digits, 0, sizeof(digit_t)* bigint_available_(b));
  return b;
}

static bigint_t* bigint_trim_realloc_(bigint_t* x, size_t count, context_t* ctx) {
  assert_internal(bigint_is_unique_(x));
  size_t dcount = bigint_roundup_count(count);
  size_t xcount = bigint_available_(x);
  bigint_t* b;
  if ((dcount <= xcount) && (xcount-dcount) < 4) {
    b = x; // avoid realloc if shrinking by less than 4 digits.
    dcount = xcount;
  }
  else {
    b = (bigint_t*)block_realloc(bigint_ptr_(x), sizeof(bigint_t) - sizeof(digit_t) + dcount*sizeof(digit_t), ctx);
  }
  b->count = count;
  b->extra = (extra_t)(dcount - count);
  return b;
}

static bigint_t* bigint_trim_to(bigint_t* x, size_t count, bool allow_realloc, context_t* ctx) {
  ptrdiff_t d = bigint_available_(x) - count;
  assert_internal(d >= 0 && bigint_is_unique_(x));
  if (d < 0) {
    return x;
  }
  else if (d > MAX_EXTRA) {
    if (allow_realloc) {
      return bigint_trim_realloc_(x, count, ctx);
    }
    else {
      x->count = count;
      x->extra = MAX_EXTRA;  // if we cannot realloc and extra > MAX_EXTRA, we may lose space :(
      return x;
    }
  }
  else {
    x->count = count;
    x->extra = (uint16_t)d;
    return x;
  }
}

static bigint_t* bigint_trim(bigint_t* x, bool allow_realloc, context_t* ctx) {
  assert_internal(bigint_is_unique_(x));
  size_t i = bigint_count_(x);
  while ((i > 0) && (x->digits[i-1] == 0)) { i--; }  // skip top zero's
  return bigint_trim_to(x, i, allow_realloc, ctx);
}

static bigint_t* bigint_alloc_reuse_(bigint_t* x, size_t count, context_t* ctx) {
  ptrdiff_t d = bigint_available_(x) - count;
  if (d >= 0 && d <= MAX_EXTRA && bigint_is_unique_(x)) {   // reuse?
    return bigint_trim_to(x, count, false /* no realloc */, ctx);    
  }
  else {
    return bigint_alloc(count, bigint_is_neg_(x), ctx); 
  }
}

static bigint_t* bigint_copy(bigint_t* x, size_t extra, context_t* ctx) {
  assert_internal(extra <= MAX_EXTRA);
  if (extra > MAX_EXTRA) extra = MAX_EXTRA;
  bigint_t* z = bigint_alloc(x->count + extra, bigint_is_neg_(x), ctx);
  z->is_neg = x->is_neg;
  z = bigint_trim_to(z, x->count, false, ctx);
  memcpy(z->digits, x->digits, x->count * sizeof(digit_t) );
  drop_bigint(x,ctx);
  return z;
}

static bigint_t* bigint_ensure_unique(bigint_t* x, context_t* ctx) {
  return (bigint_is_unique_(x) ? x : bigint_copy(x,0,ctx));
}

static bigint_t* bigint_push(bigint_t* x, digit_t d, context_t* ctx) {
  if (x->extra == 0) { x = bigint_copy(x, MAX_EXTRA, ctx); }
  x->digits[x->count] = d;
  x->count++;
  x->extra--;
  return x;
}

/*----------------------------------------------------------------------
  Conversion from numbers
----------------------------------------------------------------------*/

// Bigint to integer. Possibly converting to a small int.
static integer_t integer_bigint(bigint_t* x, context_t* ctx) {
  if (x->count==1 && x->digits[0] <= SMALLINT_MAX) {
    // make it a small int
    intx_t i = x->digits[0];
    if (x->is_neg) i = -i;
    drop_bigint(x,ctx);
    return integer_from_small(i);
  }
  else {
    return box_ptr(bigint_ptr_(x));
  }
}

// create a bigint from an int_t
static bigint_t* bigint_from_int(intx_t i, context_t* ctx) {
  bool is_neg = (i < 0);
  if (is_neg) i = -i;
  bigint_t* b = bigint_alloc(0, is_neg, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

// create a bigint from an int64_t
static bigint_t* bigint_from_int64(int64_t i, context_t* ctx) {
  bool is_neg = (i < 0);
  if (is_neg) i = -i;
  bigint_t* b = bigint_alloc(0, is_neg, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

// create a bigint from a uint64_t
static bigint_t* bigint_from_uint64(uint64_t i, context_t* ctx) {
  bigint_t* b = bigint_alloc(0, false, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}


// unpack to a bigint always
static bigint_t* integer_to_bigint(integer_t x, context_t* ctx) {
  assert_internal(is_integer(x));
  if (is_bigint(x)) {
    return block_as_assert(bigint_t*, unbox_ptr(x), TAG_BIGINT);
  }
  else {
    assert_internal(is_smallint(x));
    return bigint_from_int(unbox_smallint_t(x), ctx);
  }
}

integer_t integer_from_bigu64(uint64_t i, context_t* ctx) {
  return box_ptr(bigint_ptr_(bigint_from_uint64(i, ctx)));
}

integer_t integer_from_big64(int64_t i, context_t* ctx) {
  return box_ptr(bigint_ptr_(bigint_from_int64(i,ctx)));
}

integer_t integer_from_big(intx_t i, context_t* ctx) {
  return box_ptr(bigint_ptr_(bigint_from_int(i, ctx)));
}


/*----------------------------------------------------------------------
  To string
----------------------------------------------------------------------*/

// Convert a digit to LOG_BASE characters.
// note: gets compiled without divisions on clang and GCC.
static intx_t digit_to_str_full(digit_t d, char* buf) {
  for (intx_t i = LOG_BASE - 1; i >= 0; i--, d /= 10) {
    buf[i] = '0' + (d % 10);
  }
  return LOG_BASE;
}
// convert digit to characters but skip leading zeros. No output if `d==0`.
static intx_t digit_to_str_partial(digit_t d, char* buf) {
  char tmp[LOG_BASE];
  if (d==0) return 0;
  digit_to_str_full(d, tmp);
  intx_t i = 0;
  while (i < LOG_BASE && tmp[i]=='0') { i++; }
  for (intx_t j = i; j < LOG_BASE; j++) {
    buf[j - i] = tmp[j];
  }
  return (LOG_BASE - i);
}

// Efficient conversion to a string buffer. Use `buf == NULL` to get the required size.
static size_t bigint_to_buf_(const bigint_t* b, char* buf, size_t buf_size) {
  assert_internal(b != NULL);
  const intx_t count = bigint_count_(b);
  const size_t needed = (count*LOG_BASE) + (bigint_is_neg_(b) ? 1 : 0) + 1; // + (sign and terminator);
  if (buf==NULL || buf_size==0 || needed > buf_size) return needed;
  intx_t j = 0;  // current output position
  // sign
  if (bigint_is_neg_(b)) {
    buf[j++] = '-';
  }
  // skip leading zeros
  intx_t i = count-1;
  while (i > 0 && b->digits[i]==0) {
    assert_internal(false); // we should never have leading zeros
    i--;
  }
  // output leading digit
  if (i >= 0) {
    j += digit_to_str_partial(b->digits[i], &buf[j]);
    i--;
  }
  // and output the rest of the digits
  while (i >= 0) {
    j += digit_to_str_full(b->digits[i], &buf[j]);
    i--;
  }
  buf[j++] = 0;
  return j;
}

static string_t bigint_to_string(bigint_t* b, context_t* ctx) {
  size_t needed = bigint_to_buf_(b, NULL, 0);
  string_t s = string_alloc_buf(needed,ctx);
  bigint_to_buf_(b, (char*)string_cbuf_borrow(s), needed);
  drop_bigint(b,ctx);
  return s;
}

// int_t to string
string_t int_to_string(intx_t n, context_t* ctx) {
  assert_internal(INTPTR_SIZE <= 26);
  char buf[64];  // enough for 2^212
  bool neg = (n < 0);
  if (neg) n = -n;
  // output to buf in reverse order
  intx_t i = 0;
  if (n == 0) {
    buf[i++] = '0';
  }
  else {
    for (; i < 63 && n != 0; i++, n /= 10) {
      buf[i] = '0' + n%10;
    }
    if (neg) {
      buf[i++] = '-';
    }
  }
  // write to the allocated string
  string_t s = string_alloc_buf(i + 1,ctx);
  char* p = (char*)string_cbuf_borrow(s);
  intx_t j;
  for (j = 0; j < i; j++) {
    p[j] = buf[i - j - 1];
  }
  p[j] = 0;
  return s;
}

/*----------------------------------------------------------------------
  Parse an integer
----------------------------------------------------------------------*/

integer_t integer_parse(const char* s, context_t* ctx) {
  assert_internal(s!=NULL);
  // parse
  bool is_neg = false;
  size_t sig_digits = 0; // digits before the fraction
  size_t i = 0;
  // sign
  if (s[i] == '+') { i++; }
  else if (s[i] == '-') { is_neg = true; i++; }
  if (!isdigit(s[i])) return box_null;  // must start with a digit
  // significant 
  for (; s[i] != 0; i++) {
    char c = s[i];
    if (isdigit(c)) {
      sig_digits++;
    }
    else if (c=='_' && isdigit(s[i+1])) { // skip underscores
    }
    else if ((c == '.' || c=='e' || c=='E') && isdigit(s[i+1])) { // found fraction/exponent
      break;
    }
    else return box_null; // error    
  }
  // const char* sigend = s + i;
  // fraction
  size_t frac_digits = 0;
  size_t frac_trailing_zeros = 0;
  if (s[i]=='.') {
    i++;
    for (; s[i] != 0; i++) {
      char c = s[i];
      if (isdigit(c)) {
        if (c != '0') {
          frac_trailing_zeros = 0;
        }
        else {
          frac_trailing_zeros++;
        }                
        frac_digits++;
      }
      else if (c=='_' && isdigit(s[i+1])) { // skip underscores
      }
      else if ((c=='e' || c=='E') && (isdigit(s[i+1]) || (s[i+1]=='+' && isdigit(s[i+2])))) { // found fraction/exponent
        break;
      }
      else return box_null; // error    
    }
  }
  frac_digits -= frac_trailing_zeros; // ignore trailing zeros
  const char* end = s + i;
  // exponent 
  size_t exp = 0;
  if (s[i]=='e' || s[i]=='E') {
    i++;
    if (s[i] == '+') i++;        // optional '+'
    for (; s[i] == '0'; i++) {}  // skip leading zeros
    for (; s[i] != 0; i++) {
      char c = s[i];
      if (isdigit(c)) {
        exp = 10*exp + ((size_t)c - '0');
        if (exp > BASE) return box_null; // exponents must be < 10^9
      }
      else return box_null;
    }
  }
  if (exp < frac_digits) return box_null; // fractional number
  const size_t zero_digits = exp - frac_digits;
  const size_t dec_digits = sig_digits + frac_digits + zero_digits;  // total decimal digits needed in the bigint

  // parsed correctly, ready to construct the number
  // construct an `int_t` if it fits.
  if (dec_digits < LOG_BASE) {   // must be less than LOG_BASE to avoid overflow
    assert_internal(INTPTR_SIZE >= sizeof(digit_t));
    intx_t d = 0;
    size_t digits = 0;
    for (const char* p = s; p < end && digits < dec_digits; p++) {
      char c = *p;
      if (isdigit(c)) {
        digits++;
        d = 10*d + ((intx_t)c - '0');
      }
    }
    for (size_t z = 0; z < zero_digits; z++) {
      d *= 10;
    }
    if (is_neg) d = -d;
    return integer_from_int(d,ctx);
  }

  // otherwise construct a big int
  const size_t count = ((dec_digits + (LOG_BASE-1)) / LOG_BASE); // round up
  bigint_t* b = bigint_alloc(count, is_neg, ctx);
  size_t k     = count;
  size_t chunk = dec_digits%LOG_BASE; if (chunk==0) chunk = LOG_BASE; // initial number of digits to read
  const char* p = s;
  size_t digits = 0;
  while (p < end && digits < dec_digits) {   
    digit_t d = 0;
    // read a full digit
    for (size_t j = 0; j < chunk; ) {
      char c = (p < end ? *p++ : '0'); // fill out with zeros
      if (isdigit(c)) {
        digits++;
        j++;
        d = 10*d + ((digit_t)c - '0'); assert_internal(d<BASE);
      }
    }
    // and store it
    assert_internal(k > 0);
    if (k > 0) { b->digits[--k] = d; }    
    chunk = LOG_BASE;  // after the first digit, all chunks are full digits
  }
  // set the final zeros
  assert_internal(zero_digits / LOG_BASE == k);
  for (size_t j = 0; j < k; j++) { b->digits[j] = 0; }
  return integer_bigint(b, ctx);
}

integer_t integer_from_str(const char* num, context_t* ctx) {
  integer_t i = integer_parse(num,ctx);
  assert_internal(i.box != box_null.box);
  return i;
}

/*----------------------------------------------------------------------
  negate, compare
----------------------------------------------------------------------*/

static bigint_t* bigint_neg(bigint_t* x, context_t* ctx) {
  bigint_t* z = bigint_ensure_unique(x,ctx);
  z->is_neg = !z->is_neg;
  return z;
}


static int bigint_compare_abs_(bigint_t* x, bigint_t* y) {
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  if (cx > cy) return 1;
  if (cx < cy) return -1;
  for (size_t i = cx; i > 0; ) {
    i--;
    if (x->digits[i] != y->digits[i]) return (x->digits[i] > y->digits[i] ? 1 : -1);
  }
  return 0;
}

static int bigint_compare_(bigint_t* x, bigint_t* y) {
  if (x->is_neg != y->is_neg) {
    return (y->is_neg ? 1 : -1);
  }
  else {
    return (int)(bigint_sign_(x)* bigint_compare_abs_(x, y));
  }
}

/*----------------------------------------------------------------------
  add absolute
----------------------------------------------------------------------*/

static bigint_t* bigint_add(bigint_t* x, bigint_t* y, bool y_isneg, context_t* ctx);
static bigint_t* bigint_sub(bigint_t* x, bigint_t* y, bool yneg, context_t* ctx);


static bigint_t* bigint_add_abs(bigint_t* x, bigint_t* y, context_t* ctx) {   // x.count >= y.count
  // assert_internal(bigint_sign_(x) == bigint_sign_(y));  
  // ensure x.count >= y.count
  const size_t cx = bigint_count_(x);
  const size_t cy = bigint_count_(y);
  assert_internal(cx >= cy);
  
  // allocate result bigint
  const size_t cz = ((intx_t)bigint_last_digit_(x) + (intx_t)bigint_last_digit_(y) + 1 >= BASE ? cx + 1 : cx);
  bigint_t* z = bigint_alloc_reuse_(x, cz, ctx); // if z==x, we reused x.
  //z->is_neg = x->is_neg;

  assert_internal(cx>=cy);
  assert_internal(bigint_count_(z) >= cx);
  digit_t carry = 0;
  digit_t sum = 0;
  // add y's digits
  size_t i;
  for (i = 0; i < cy; i++) {
    sum = x->digits[i] + y->digits[i] + carry;
    if (unlikely(sum >= BASE)) {
      carry = 1;
      sum -= BASE;
    }
    else {
      carry = 0;
    }
    z->digits[i] = sum;
  }
  // propagate the carry
  for (; carry != 0 && i < cx; i++) {
    sum = x->digits[i] + carry;
    if (unlikely(sum >= BASE)) {
      assert_internal(sum==BASE && carry==1);  // can only be at most BASE
      // carry stays 1;
      sum -= BASE;
    }
    else {
      carry = 0;
    }
    z->digits[i] = sum;
  }
  // copy the tail
  if (i < cx && z != x) {
    // memcpy(&z->digits[i], &x->digits[i], (cx - i)*sizeof(digit_t));
    for (; i < cx; i++) {
      z->digits[i] = x->digits[i];
    }
  }
  else {
    i = cx;
  }
  // carry flows into final extra digit?
  if (carry) {
    z->digits[i++] = carry;
  }
  assert_internal(i == bigint_count_(z) || i+1 == bigint_count_(z));
  if (z != x) drop_bigint(x,ctx);
  drop_bigint(y,ctx);
  return bigint_trim_to(z, i, true /* allow realloc */, ctx);
}

/*
static bigint_t* bigint_add_abs_small(bigint_t* x, int_t y) {
  assert_internal(y >= 0 && y < BASE);  
  // assert_internal(bigint_sign_(x) == bigint_sign_(y));  
 // ensure x.count >= y.count
  const size_t cx = bigint_count_(x);
 
  // allocate result bigint
  const size_t cz = ((int_t)bigint_last_digit_(x) + y + 1 >= BASE ? cx + 1 : cx);
  bigint_t* z = bigint_alloc_reuse_(x, cz); // if z==x, we reused x.
  assert_internal(bigint_count_(z) >= cx);
  digit_t carry = (digit_t)y;
  digit_t sum = 0;
  // add y do the digits of x
  size_t i;
  for (i = 0; carry!=0 && i < cx; i++) {
    sum = x->digits[i] + carry;
    if (unlikely(sum >= BASE)) {
      carry = 1;
      sum -= BASE;
    }
    else {
      carry = 0;
    }
    z->digits[i] = sum;
  }  
  // copy the tail
  if (i < cx && z != x) {
    assert_internal(carry == 0);
    // memcpy(&z->digits[i], &x->digits[i], (cx - i)*sizeof(digit_t));
    for (; i < cx; i++) {
      z->digits[i] = x->digits[i];
    }
  }
  else {
    i = cx;
    // carry flows into final extra digit?
    if (carry) {
      z->digits[i++] = carry;
    }
  }  
  assert_internal(i == bigint_count_(z) || i + 1 == bigint_count_(z));
  if (z != x) bigint_decref(x);
  return bigint_trim_to(z, i, true );
}
*/

/*----------------------------------------------------------------------
  subtract absolute
----------------------------------------------------------------------*/

static bigint_t* bigint_sub_abs(bigint_t* x, bigint_t* y, context_t* ctx) {  // |x| >= |y|
  assert_internal(bigint_compare_abs_(x, y) >= 0);
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  assert_internal(cx>=cy);
  bigint_t* z = bigint_alloc_reuse_(x, cx, ctx);
  //z->is_neg = x->is_neg;
  assert_internal(bigint_count_(z) >= cx);
  digit_t borrow = 0;
  digit_t diff = 0;
  // subtract y digits
  size_t i;
  for (i = 0; i < cy; i++) {
    diff = x->digits[i] - borrow - y->digits[i];
    if (unlikely(diff < 0)) {
      borrow = 1;
      diff += BASE; assert_internal(diff >= 0);
    }
    else {
      borrow = 0;
    }
    z->digits[i] = diff;
  }
  // propagate borrow
  for (; borrow != 0 && i < cx; i++) {
    diff = x->digits[i] - borrow;
    if (unlikely(diff < 0)) {
      // borrow stays 1;
      assert_internal(diff==-1);
      diff += BASE;
    }
    else {
      borrow = 0;
    }
    z->digits[i] = diff;
  }
  assert_internal(borrow==0);  // since x >= y.
  // copy the tail
  if (z != x) {
    // memcpy(&z->digits[i], &x->digits[i], (cx - i)*sizeof(digit_t));
    for (; i <= cx; i++) {
      z->digits[i] = x->digits[i];
    }
    drop_bigint(x,ctx);
  }
  drop_bigint(y,ctx);
  return bigint_trim(z,true,ctx);
}

/*----------------------------------------------------------------------
  Multiply & Sqr. including Karatsuba multiplication
----------------------------------------------------------------------*/

static bigint_t* bigint_mul(bigint_t* x, bigint_t* y, context_t* ctx) {
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  uint8_t is_neg = (bigint_is_neg_(x) != bigint_is_neg_(y) ? 1 : 0);
  size_t cz = cx+cy;
  bigint_t* z = bigint_alloc_zero(cz,is_neg,ctx);  
  int64_t carry = 0;
  int64_t prod = 0;  
  for (size_t i = 0; i < cx; i++) {
    int64_t dx = x->digits[i];
    for (size_t j = 0; j < cy; j++) {
      int64_t dy = y->digits[j];
      prod = (dx * dy) + z->digits[i+j];
      carry = prod/BASE;
      z->digits[i+j]    = (digit_t)(prod - (carry*(int64_t)BASE));
      z->digits[i+j+1] += (digit_t)carry;
    }
  }   
  drop_bigint(x,ctx);
  drop_bigint(y,ctx);  
  return bigint_trim(z, true,ctx);
}

static bigint_t* bigint_mul_small(bigint_t* x, intx_t y, context_t* ctx) {
  assert_internal(y > -BASE && y < BASE);
  size_t cx = bigint_count_(x);
  uint8_t is_neg = (bigint_is_neg_(x) && y<0 ? 1 : 0);
  size_t cz = cx+1;
  bigint_t* z = bigint_alloc_reuse_(x, cz, ctx);
  if (y < 0) y = -y;
  int64_t carry = 0;
  int64_t prod = 0;
  size_t i;
  for (i = 0; i < cx; i++) {
    prod  = (x->digits[i] * (int64_t)y) + carry;
    carry = prod/BASE;
    z->digits[i] = (digit_t)(prod - (carry*BASE));
  }
  while (carry > 0) {
    assert_internal(i < bigint_count_(z));
    z->digits[i++] = carry % BASE;
    carry /= BASE;
  }
  if (z != x) { drop_bigint(x,ctx); }
  if (is_neg && !bigint_is_neg_(z)) { z = bigint_neg(z,ctx); }
  return bigint_trim_to(z, i, true, ctx);
}

static bigint_t* bigint_sqr(bigint_t* x, context_t* ctx) {
  dup_bigint(x);
  return bigint_mul(x, x, ctx);
}

static bigint_t* bigint_shift_left(bigint_t* x, intx_t digits, context_t* ctx) {
  if (digits <= 0) return x;
  size_t cx = x->count;
  bigint_t* z = bigint_alloc_reuse_(x, x->count + digits, ctx);
  memmove(&z->digits[digits], &x->digits[0], sizeof(digit_t)*cx);
  memset(&z->digits[0], 0, sizeof(digit_t)*digits);
  if (z != x) drop_bigint(x, ctx);
  return z;
}

static bigint_t* bigint_slice(bigint_t* x, size_t lo, size_t hi, context_t* ctx) {
  if (lo == 0 && bigint_is_unique_(x)) {
    return bigint_trim_to(x, hi, false, ctx);
  }
  if (lo >= x->count) lo = x->count;
  if (hi > x->count)  hi = x->count;
  const size_t cz = hi - lo;
  bigint_t* z = bigint_alloc(cz, x->is_neg, ctx);
  if (cz==0) {
    z->digits[0] = 0;
    z->count = 1;
    z->extra--;
  }
  else if (lo < x->count) {
    memcpy(&z->digits[0], &x->digits[lo], sizeof(digit_t)*cz);
  }
  return z;
}

static bigint_t* bigint_mul_karatsuba(bigint_t* x, bigint_t* y, context_t* ctx) {
  intx_t n = (x->count >= y->count ? x->count : y->count);
  if (n <= 25) return bigint_mul(x, y, ctx);
  n = ((n + 1) / 2);

  bigint_t* b = bigint_slice(dup_bigint(x), n, x->count, ctx);
  bigint_t* a = bigint_slice(x, 0, n, ctx);
  bigint_t* d = bigint_slice(dup_bigint(y), n, y->count, ctx);
  bigint_t* c = bigint_slice(y, 0, n, ctx);

  bigint_t* ac = bigint_mul_karatsuba(dup_bigint(a), dup_bigint(c), ctx);
  bigint_t* bd = bigint_mul_karatsuba(dup_bigint(b), dup_bigint(d), ctx);
  bigint_t* abcd = bigint_mul_karatsuba( bigint_add(a, b, b->is_neg, ctx), 
                                         bigint_add(c, d, d->is_neg, ctx), ctx);
  bigint_t* p1 = bigint_shift_left(bigint_sub(bigint_sub(abcd, dup_bigint(ac), ac->is_neg, ctx), 
                                              dup_bigint(bd), bd->is_neg, ctx), n, ctx);
  bigint_t* p2 = bigint_shift_left(bd, 2 * n, ctx);
  bigint_t* prod = bigint_add(bigint_add(ac, p1, p1->is_neg, ctx), p2, p2->is_neg, ctx);
  return bigint_trim(prod,true, ctx);
}


/*----------------------------------'------------------------------------
  Pow
----------------------------------------------------------------------*/

integer_t integer_pow(integer_t x, integer_t p, context_t* ctx) {
  if (is_smallint(p)) {
    if (box_as_intptr(p) == box_as_intptr(integer_zero)) return integer_one;
  }
  if (is_smallint(x)) {
    if (box_as_intptr(x) == box_as_intptr(integer_zero)) {
      drop_integer_t(p,ctx);  return integer_zero;
    }
    if (box_as_intptr(x) == box_as_intptr(integer_one)) {
      drop_integer_t(p,ctx);  return integer_one;
    }
    if (box_as_intptr(x) == box_as_intptr(integer_min_one)) {
      return (integer_is_even(p,ctx) ? integer_one : integer_min_one);
    }
  }
  dup_integer_t(p);
  if (integer_signum(p,ctx)==-1) {
    drop_integer_t(p,ctx); return integer_zero;
  }
  integer_t y = integer_one;
  if (is_bigint(p)) {
    while (1) {
      dup_integer_t(p);
      if (integer_is_odd(p,ctx)) {
        dup_integer_t(x);
        y = integer_mul(y, x, ctx);
        p = integer_dec(p, ctx);
      }
      if (is_smallint(p)) break;
      p = integer_div(p, integer_from_small(2), ctx);
      x = integer_sqr(x, ctx);
    }
  }
  assert_internal(is_smallint(p));
  intx_t i = unbox_smallint_t(p);
  while (1) {
    if ((i&1)!=0) {
      dup_integer_t(x);
      y = integer_mul(y, x, ctx);
      i--;
    }
    if (i==0) break;
    i /= 2;
    x = integer_sqr(x, ctx);
  }
  drop_integer_t(x, ctx);
  return y;  
}


/*----------------------------------------------------------------------
  Division
----------------------------------------------------------------------*/

static bigint_t* bigint_div_mod_small(bigint_t* x, intx_t y, intx_t* pmod, context_t* ctx) {
  size_t cx = bigint_count_(x);
  // uint8_t is_neg = (bigint_is_neg_(x) != (y<0) ? 1 : 0);
  bigint_t* z = bigint_alloc_reuse_(x, cx, ctx);
  int64_t mod = 0;
  for (size_t i = cx; i > 0; i--) {
    int64_t div = mod*BASE + x->digits[i-1];
    int64_t q = div / y;
    mod = div - (q*y);
    z->digits[i-1] = (digit_t)q;
  }
  if (pmod != NULL) {
    *pmod = (intx_t)mod;
  }
  if (z != x) drop_bigint(x, ctx);
  return bigint_trim(z, true, ctx);
}
  

static bigint_t* bigint_div_mod(bigint_t* x, bigint_t* y, bigint_t** pmod, context_t* ctx) {
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  assert_internal(cx >= cy);
  uint8_t is_neg = (bigint_is_neg_(x) != bigint_is_neg_(y) ? 1 : 0);
  bigint_t* z = bigint_alloc_zero(cx - cy + 1, is_neg, ctx);
  // normalize
  intx_t divisorHi = bigint_last_digit_(y);
  intx_t lambda = ((int64_t)BASE + 2*divisorHi - 1)/(2*divisorHi);
  bigint_t* rem = bigint_mul_small(x, lambda, ctx);
  if (rem->count <= cx) { rem = bigint_push(rem, 0, ctx); }
  bigint_t* div = bigint_mul_small(y, lambda, ctx);
  divisorHi = bigint_last_digit_(div); // todo: check more
  div = bigint_push(div, 0, ctx);
  for (intx_t shift = cx - cy; shift >= 0; shift--) {
    int64_t qd = BASE - 1;
    assert_internal(rem->count > shift + cy);
    if (rem->digits[shift + cy] != divisorHi) {
      assert_internal(rem->count > 1);
      int64_t rem_hi = (rem->digits[shift + cy]*(int64_t)BASE) + rem->digits[shift + cy - 1];
      qd = (rem_hi / divisorHi);
    }
    assert_internal(qd <= (BASE - 1));
    int64_t carry = 0;
    int64_t borrow = 0;
    size_t cd = div->count;
    for (size_t i = 0; i < cd; i++) {
      carry += qd * div->digits[i];
      int64_t q = carry / BASE;
      borrow += rem->digits[shift + i] - (carry - (q*BASE));
      carry = q;
      if (borrow < 0) {
        rem->digits[shift + i] = (digit_t)(borrow + BASE);
        borrow = -1;
      }
      else {
        rem->digits[shift + i] = (digit_t)borrow;
        borrow = 0;
      }
    }
    while (borrow != 0) {
      qd--;
      carry = 0;
      for (size_t i = 0; i < cd; i++) {
        carry += rem->digits[shift + i] - (int64_t)BASE + div->digits[i];
        if (carry < 0) {
          rem->digits[shift + i] = (digit_t)(carry + BASE);
          carry = 0;
        }
        else {
          rem->digits[shift + i] = (digit_t)carry;
          carry = 1;
        }
      }
      borrow += carry;
    }
    z->digits[shift] = (digit_t)qd;
  }
  drop_bigint(div, ctx);
  if (pmod != NULL) {
    *pmod = bigint_div_mod_small(rem, lambda, NULL, ctx); // denormalize remainder
  }
  else {
    drop_bigint(rem, ctx);
  }
  return bigint_trim(z,true, ctx);
}


/*----------------------------------------------------------------------
  Addition and substraction
----------------------------------------------------------------------*/

static bigint_t* bigint_add(bigint_t* x, bigint_t* y, bool yneg, context_t* ctx) {
  if (bigint_is_neg_(x) != yneg) {
    return bigint_sub(x, y, !yneg, ctx);
  }
  bigint_t* z;
  if (bigint_count_(x) < bigint_count_(y)) {
    z = bigint_add_abs(y, x, ctx);
  }
  else {
    z = bigint_add_abs(x, y, ctx);
  }
  assert_internal(bigint_is_unique_(z));
  z->is_neg = yneg;
  return z;
}

static bigint_t* bigint_sub(bigint_t* x, bigint_t* y, bool yneg, context_t* ctx) {
  if (bigint_is_neg_(x) != yneg) {
    return bigint_add(x, y, !yneg, ctx);
  }
  if (bigint_compare_abs_(x,y) >= 0) {
    return bigint_sub_abs(x, y, ctx);
  }
  else {
    bigint_t* z = bigint_sub_abs(y, x, ctx);
    assert_internal(bigint_is_unique_(z));
    z->is_neg = !yneg;
    return z;
  }
}



/*----------------------------------------------------------------------
  Integer interface
----------------------------------------------------------------------*/

 integer_t integer_neg_generic(integer_t x, context_t* ctx) {
  assert_internal(is_integer(x));
  bigint_t* bx = integer_to_bigint(x,ctx);
  return integer_bigint(bigint_neg(bx, ctx), ctx);
}

 integer_t integer_sqr_generic(integer_t x, context_t* ctx) {
  assert_internal(is_integer(x));
  bigint_t* bx = integer_to_bigint(x,ctx);
  return integer_bigint(bigint_sqr(bx, ctx), ctx);
}

 int integer_signum_generic(integer_t x, context_t* ctx) {
  assert_internal(is_integer(x));
  bigint_t* bx = integer_to_bigint(x, ctx);
  int signum = (bx->is_neg ? -1 : ((bx->count==0 && bx->digits[0]==0) ? 0 : 1));
  drop_integer_t(x, ctx);
  return signum;
}

 bool integer_is_even_generic(integer_t x, context_t* ctx) {
  assert_internal(is_integer(x));
  if (is_smallint(x)) return ((box_as_intptr(x)&0x08)==0);
  bigint_t* bx = integer_to_bigint(x,ctx);
  bool even = ((bx->digits[0]&0x1)==0);
  drop_integer_t(x,ctx);
  return even;
}

int integer_cmp_generic(integer_t x, integer_t y, context_t* ctx) {
  assert_internal(is_integer(x)&&is_integer(y));
  bigint_t* bx = integer_to_bigint(x, ctx);
  bigint_t* by = integer_to_bigint(y, ctx);
  int sign = bigint_compare_(bx, by);
  drop_bigint(bx, ctx);
  drop_bigint(by, ctx);
  return sign;
}

integer_t integer_add_generic(integer_t x, integer_t y, context_t* ctx) {
  assert_internal(is_integer(x)&&is_integer(y));
  bigint_t* bx = integer_to_bigint(x, ctx);
  bigint_t* by = integer_to_bigint(y, ctx);
  return integer_bigint(bigint_add(bx, by, by->is_neg, ctx), ctx);
}

integer_t integer_sub_generic(integer_t x, integer_t y, context_t* ctx) {
  assert_internal(is_integer(x)&&is_integer(y));
  bigint_t* bx = integer_to_bigint(x, ctx);
  bigint_t* by = integer_to_bigint(y, ctx);
  return integer_bigint(bigint_sub(bx, by, by->is_neg, ctx), ctx);
}

static bool use_karatsuba(size_t i, size_t j) {
  return ((0.000012*(i*j) - 0.0025*(i+j)) >= 0);
}

integer_t integer_mul_generic(integer_t x, integer_t y, context_t* ctx) {
  assert_internal(is_integer(x)&&is_integer(y));
  bigint_t* bx = integer_to_bigint(x, ctx);
  bigint_t* by = integer_to_bigint(y, ctx);
  bool usek = use_karatsuba(bx->count, by->count);
  return integer_bigint((usek ? bigint_mul_karatsuba(bx,by, ctx) : bigint_mul(bx, by, ctx)), ctx);
}


/*----------------------------------------------------------------------
  Division and modulus
----------------------------------------------------------------------*/

integer_t integer_div_mod_generic(integer_t x, integer_t y, integer_t* mod, context_t* ctx) {
  assert_internal(is_integer(x)&&is_integer(y));
  if (is_smallint(y)) {
    intx_t ay = unbox_smallint_t(y);
    if (ay == 0) return box_null; // raise div-by-zero
    if (ay == 1) {
      if (mod!=NULL) *mod = integer_zero;
      return x;
    }
    if (ay == -1) {
      if (mod!=NULL) *mod = integer_zero;
      return integer_neg(x, ctx);
    }
    bool ay_neg = ay < 0;
    if (ay_neg) ay = -ay;
    if (ay < BASE) {
      // small division
      intx_t imod;
      bigint_t* bx = integer_to_bigint(x, ctx);
      bool     xneg = bigint_is_neg_(bx);
      bigint_t* bz = bigint_div_mod_small(bx, ay, &imod, ctx);
      if (xneg) imod = -imod;
      bz->is_neg = (xneg != ay_neg);
      if (mod != NULL) *mod = integer_from_int(imod, ctx);
      return integer_bigint(bz, ctx);
    }
    // fall through to full division
  }
  bigint_t* bx = integer_to_bigint(x, ctx);
  bigint_t* by = integer_to_bigint(y, ctx);
  int cmp = bigint_compare_abs_(bx, by);
  if (cmp < 0) {
    if (mod) {
      *mod = x;
    }
    else {
      drop_integer_t(x, ctx);
    }
    drop_integer_t(y, ctx);
    return integer_zero;
  }
  if (cmp==0) {
    if (mod) *mod = integer_zero;
    intx_t i = (bigint_is_neg_(bx) == bigint_is_neg_(by) ? 1 : -1);
    drop_integer_t(x, ctx);
    drop_integer_t(y, ctx);
    return integer_from_small(i);
  }
  bool qneg = (bigint_is_neg_(bx) != bigint_is_neg_(by));
  bool mneg = bigint_is_neg_(bx);
  bigint_t* bmod = NULL;
  bigint_t* bz = bigint_div_mod(bx, by, (mod!=NULL ? &bmod : NULL), ctx);
  bz->is_neg = qneg;
  if (mod!=NULL && bmod != NULL) {
    bmod->is_neg = mneg;
    *mod = integer_bigint(bmod, ctx);
  }
  return integer_bigint(bz, ctx);
}

integer_t integer_div_generic(integer_t x, integer_t y, context_t* ctx) {
  return integer_div_mod_generic(x, y, NULL, ctx);
}

integer_t integer_mod_generic(integer_t x, integer_t y, context_t* ctx) {
  integer_t mod = box_null;
  integer_t div = integer_div_mod_generic(x, y, &mod, ctx);
  drop_integer_t(div, ctx);
  return mod;
}

/*----------------------------------------------------------------------
  Conversion, printing
----------------------------------------------------------------------*/


string_t integer_to_string(integer_t x, context_t* ctx) {
  if (is_smallint(x)) {
    return int_to_string(unbox_smallint_t(x), ctx);
  }
  else {
    return bigint_to_string(integer_to_bigint(x, ctx), ctx);
  }
}

decl_export string_t integer_to_hex_string(integer_t x, bool use_capitals, context_t* ctx) {
  // TODO
  UNUSED(use_capitals);
  return integer_to_string(x, ctx);
}

void integer_fprint(FILE* f, integer_t x, context_t* ctx) {
  string_t s = integer_to_string(x, ctx);
  fprintf(f, "%s", string_buf_borrow(s));
  drop_string_t(s, ctx);  
}

void integer_print(integer_t x, context_t* ctx) {
  integer_fprint(stdout, x, ctx);
}

/*----------------------------------------------------------------------
  Operations for efficient fixed point arithmetic.
  Count trailing zeros, count digits, mul_pow10, div_pow10
----------------------------------------------------------------------*/

// count trailing decimal zeros
static intx_t int_ctz(intx_t x) {
  intx_t count = 0;
  for (; x != 0 && (x%10) == 0; x /= 10) {
    count++;
  }
  return count;
}

static intx_t bigint_ctz(bigint_t* x, context_t* ctx) {
  size_t i;
  for (i = 0; i < (x->count-1); i++) {
    if (x->digits[i] != 0) break;
  }
  assert_internal(x->digits[i]!=0);
  intx_t ctz = (int_ctz(x->digits[i]) + LOG_BASE*i);
  drop_bigint(x, ctx);
  return ctz;
}

integer_t integer_ctz(integer_t x, context_t* ctx) {
  if (is_smallint(x)) {
    return integer_from_small(int_ctz(unbox_smallint_t(x)));
  }
  else {
    return integer_from_int(bigint_ctz(integer_to_bigint(x, ctx), ctx), ctx);
  }
}

static intx_t int_count_digits(intx_t x) {
  // make positive
  uintx_t u;
  if (x < 0) {
    u = (uintx_t)(x == INTX_MIN ? INTX_MAX : -x);  // careful for overflow
  }
  else {
    u = (uintx_t)x;
  }
  return bits_digits(u);
}

static intx_t bigint_count_digits(bigint_t* x) {
  assert_internal(x->count > 0);
  return bits_digits32(x->digits[x->count-1]) + LOG_BASE*(x->count - 1);
}

integer_t integer_count_digits(integer_t x, context_t* ctx) {
  if (is_smallint(x)) {
    return integer_from_small(int_count_digits(unbox_smallint_t(x)));
  }
  else {
    return integer_from_int(bigint_count_digits(integer_to_bigint(x, ctx)), ctx);
  }
}

static intx_t powers_of_10[LOG_BASE+1] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };

integer_t integer_mul_pow10(integer_t x, integer_t p, context_t* ctx) {
  if (integer_is_zero(dup_integer_t(p),ctx)) {
    drop_integer_t(p, ctx);
    return x;
  }
  if (integer_is_zero(dup_integer_t(x),ctx)) {
    drop_integer_t(p, ctx); // x is small
    return integer_zero;
  }  
  if (!is_smallint(p)) {
    // TODO: raise error
    return integer_zero;
  }
  intx_t i = unbox_smallint_t(p);

  // negative?
  if (i < 0) {
    return integer_div_pow10(x, integer_from_small(-i), ctx);
  }

  // small multiply?
  if (is_smallint(x) && i < LOG_BASE) {
    return integer_mul(x, integer_from_int(powers_of_10[i], ctx), ctx);
  }

  // multiply a bigint
  intx_t large = i / LOG_BASE;  // number of zero digits to shift in
  intx_t ismall = i % LOG_BASE;  // small multiply the left over
  bigint_t* b = integer_to_bigint(x, ctx);
  if (ismall > 0) {
    b = bigint_mul_small(b, powers_of_10[ismall], ctx);
  }
  if (large > 0) {
    size_t bcount = b->count;
    size_t ccount = bcount + large;
    bigint_t* c = bigint_alloc_reuse_(b, ccount, ctx);
    memmove(&c->digits[large], &b->digits[0], bcount * sizeof(digit_t)); 
    memset(&c->digits[0], 0, large * sizeof(digit_t));
    assert_internal(c->count == ccount);
    if (b != c) drop_bigint(b, ctx);
    b = c;
  }
  return integer_bigint(b, ctx);
}


integer_t integer_div_pow10(integer_t x, integer_t p, context_t* ctx) {
  if (integer_is_zero(dup_integer_t(p),ctx)) {
    drop_integer_t(p, ctx);
    return x;
  }
  if (integer_is_zero(dup_integer_t(x),ctx)) {
    drop_integer_t(p, ctx); // x is small
    return integer_zero;
  }
  if (!is_smallint(p)) {
    // TODO: raise error
    return integer_zero;
  }
  intx_t i = unbox_smallint_t(p);

  // negative?
  if (i < 0) {
    return integer_mul_pow10(x, integer_from_small(-i), ctx);
  }

  // small divide?
  if (is_smallint(x) && i < LOG_BASE) {
    return integer_div(x, integer_from_int(powers_of_10[i], ctx), ctx);
  }

  // divide a bigint
  intx_t large = i / LOG_BASE;  // number of zero digits to shift out
  intx_t ismall = i % LOG_BASE;  // small divide the left over
  bigint_t* b = integer_to_bigint(x, ctx);
  size_t bcount = b->count;
  if (large > 0) {
    if (large >= (intx_t)bcount) { 
      drop_bigint(b, ctx);
      return integer_zero;
    }
    size_t ccount = bcount - large;
    if (bigint_is_unique_(b)) {
      memmove(&b->digits[0], &b->digits[large], ccount * sizeof(digit_t));
      b = bigint_trim_to(b, ccount, true, ctx);
    }
    else {
      bigint_t* c = bigint_alloc(ccount, b->is_neg, ctx);
      memcpy(&c->digits[0], &b->digits[large], bcount * sizeof(digit_t));
      drop_bigint(b, ctx);
      b = c;
    }    
  }
  if (ismall > 0) {
    b = bigint_div_mod_small(b, powers_of_10[ismall], NULL, ctx);
  }
  return integer_bigint(b, ctx);
}

int32_t integer_clamp32_generic(integer_t x, context_t* ctx) {
  bigint_t* bx = integer_to_bigint(x, ctx);
  int32_t i = bx->digits[0];
  if (bx->count > 1) i += (int32_t)(bx->digits[1]*BASE);
  if (bx->is_neg) i = -i;
  drop_bigint(bx,ctx);
  return i;
}

int64_t integer_clamp64_generic(integer_t x, context_t* ctx) {
  bigint_t* bx = integer_to_bigint(x, ctx);
  int64_t i = bx->digits[0];
  if (bx->count > 1) i += ((int64_t)bx->digits[1])*BASE;
  if (bx->count > 2) i += ((int64_t)bx->digits[2])*BASE*BASE;
  if (bx->is_neg) i = -i;
  drop_bigint(bx, ctx);
  return i;
}

double integer_as_double_generic(integer_t x, context_t* ctx) {
  bigint_t* bx = integer_to_bigint(x, ctx);
  if (bx->count > ((310/LOG_BASE) + 1)) return (bx->is_neg ? -INFINITY : INFINITY);
  double base = (double)BASE;
  double d = 0.0;
  for (size_t i = bx->count; i > 0; i--) {
    d = (d*base) + ((double)bx->digits[i-1]);
  }
  if (bx->is_neg) d = -d;
  drop_bigint(bx, ctx);
  return d;
}

static inline double double_round_even(double d) {
  double r = round(d);
  if (fabs(d-r) == 0.5) {
    // exactly in-between, round to even
    r = 2.0*round(d/2.0);
  }
  return r;
}

integer_t integer_from_double(double d, context_t* ctx) {
  char buf[32];
  d = double_round_even(d);
  if (!isnormal(d)) {
    return integer_zero;
  }
  else {
    snprintf(buf, 32, "%.20e", d);
    integer_t i = integer_parse(buf, ctx);
    return (i.box==box_null.box ? integer_zero : i);
  }
}

