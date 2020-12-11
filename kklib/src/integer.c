/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#define  __USE_MINGW_ANSI_STDIO 1  // so %z is valid on mingw

#include <stdio.h>
#include <stdint.h>
#include <string.h> // memcpy
#include <ctype.h>
#include <math.h>   // INFINITY
#include "kklib.h"
#include "kklib/integer.h"


/*----------------------------------------------------------------------
Big integers. For our purposes, we need an implementation that does not
have to be the fastest possible; we instead aim for portable, simple,
well performing, and with fast conversion to/from decimal strings.
Still, it performs quite respectable and does have various optimizations
including Karatsuba multiplication.

  Big integers are arrays of `digits` with a `count` and `is_neg` flag.
  For a number `n` we have:

  n = (is_neg ? -1 : 1) * (digits[0]*(BASE^0) + digits[1]*(BASE^1) + ... + digits[count-1]*(BASE^(count-1)))

  For any `count>0`, we have `digits[count-1] != 0`.
  We use a decimal representation for efficient conversion of numbers
  to strings and back. We use 32-bit or 64-bit integers for the digits
  depending on the platform, this way:
  - we can use base 10^9 or 10^18  (which uses 29.9 / 59.8 bits of the 32/64 available).
  - it can hold `2*BASE + 1` which allows for efficient addition with
    portable overflow detection.
  - a double digit `kk_ddigit_t` of 64/128-bit can hold a full multiply
    of `BASE*BASE + BASE + 1` which allows efficient multiplication with
    portable overflow detection.
----------------------------------------------------------------------*/

#if (KK_INTPTR_SIZE>=8) && defined(_MSC_VER) && (_MSC_VER >= 1920) && !defined(__clang_msvc__) /* not clang-cl or we get link errors */
// Use 64-bit digits on Microsoft VisualC
#define BASE          KI64(1000000000000000000)
#define LOG_BASE      (18)
#define DIGIT_BITS    (64)
#define BASE_HEX      KU64(0x100000000000000)  // largest hex base < BASE  
#define LOG_BASE_HEX  (14)                     // hex digits in BASE_HEX
#define PRIxDIGIT     "%llx"
#define PRIXDIGIT     "%llX"
typedef uint64_t      kk_digit_t;     // 2*BASE + 1 < kk_digit_t_max

typedef struct kk_ddigit_s {
  uint64_t hi;
  uint64_t lo;
} kk_ddigit_t;

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  kk_assert_internal(divisor >= 0);
  kk_digit_t tmp;
  if (rem==NULL) rem = &tmp;
  if (d.hi==0 && d.lo < divisor) {  // common case
    *rem = (kk_digit_t)d.lo;
    return 0;
  }
  return _udiv128(d.hi, d.lo, divisor, rem);
}

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  kk_ddigit_t r;
  r.lo = _umul128(x, y, &r.hi);
  if (z > 0) {
    if (r.lo > (UINT64_MAX - z)) {
      r.hi++;
    }
    r.lo += z;
  }
  return r;
}

#elif (KK_INTPTR_SIZE >= 8) && defined(__GNUC__) 
// Use 64-bit digits with gcc/clang/icc
#define BASE          KI64(1000000000000000000)
#define LOG_BASE      (18)
#define DIGIT_BITS    (64)
#define BASE_HEX      KU64(0x100000000000000)  // largest hex base < BASE  
#define LOG_BASE_HEX  (14)                     // hex digits in BASE_HEX
typedef uint64_t      kk_digit_t;     // 2*BASE + 1 < kk_digit_t_max

#include <inttypes.h>
#define PRIxDIGIT   "%" PRIx64
#define PRIXDIGIT   "%" PRIX64

__extension__ typedef unsigned __int128 kk_ddigit_t;

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  if (d < divisor) {
    if (rem!=NULL) *rem = (kk_digit_t)d;
    return 0;
  }
  if (rem!=NULL) *rem = (kk_digit_t)(d%divisor);
  return (kk_digit_t)(d/divisor);
}

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  return ((kk_ddigit_t)x * y) + z;
}

#else
// Default: use 32-bit digits
#if KK_INTPTR_SIZE > 4
#pragma message("using 32-bit digits for large integer arithmetic")
#endif

#define BASE          KI32(1000000000)
#define LOG_BASE      (9)
#define DIGIT_BITS    (32)
#define BASE_HEX      KU32(0x10000000)  // largest hex base < BASE  
#define LOG_BASE_HEX  (7)               // hex digits in BASE_HEX
typedef uint32_t      kk_digit_t;       // 2*BASE + 1 < kk_digit_t_max
#define PRIxDIGIT     "%x"
#define PRIXDIGIT     "%X"

typedef uint64_t    kk_ddigit_t;    // double digit for multiplies

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  return ((kk_ddigit_t)x * y) + z;
}

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  if (d < divisor) {
    if (rem!=NULL) *rem = (kk_digit_t)d;
    return 0;
  }
  if (rem!=NULL) *rem = (kk_digit_t)(d%divisor);
  return (kk_digit_t)(d/divisor);
}

#endif

#define KK_LOG16_DIV_LOG10  (1.20411998266)
#define KK_LOG10_DIV_LOG16  (0.83048202372)

typedef uint16_t kk_extra_t;
#define MAX_EXTRA           (UINT16_MAX / 2)  // we use 1 bit for the negative bool

typedef struct kk_bigint_s {
  kk_block_t  _block;
#if KK_INTPTR_SIZE>=8
  uint8_t  is_neg: 1;      // negative
  kk_extra_t  extra :15;      // extra digits available: `sizeof(digits) == (count+extra)*sizeof(kk_digit_t)`
  uint64_t count :48;      // count of digits in the number
#else
  uint8_t  is_neg;
  kk_extra_t  extra;
  uint32_t count;
#endif
  kk_digit_t  digits[1];      // digits from least-significant to most significant.
} kk_bigint_t;


static bool bigint_is_neg_(const kk_bigint_t* b) {
  return (b->is_neg != 0);
}

static kk_intx_t kk_bigint_sign_(const kk_bigint_t* b) {
  return (bigint_is_neg_(b) ? -1 : 1);
}

static size_t bigint_count_(const kk_bigint_t* b) {
  return (b->count);
}

static size_t bigint_available_(const kk_bigint_t* b) {
  return (b->count + b->extra);
}

static kk_digit_t bigint_last_digit_(const kk_bigint_t* b) {
  return b->digits[b->count-1];
}

static kk_integer_t integer_bigint(kk_bigint_t* x, kk_context_t* ctx);

/*----------------------------------------------------------------------
  allocation, ref counts, trim
  functions ending in "_" do not change reference counts of input arguments
----------------------------------------------------------------------*/

static kk_ptr_t bigint_ptr_(kk_bigint_t* x) {
  return &x->_block;
}

static kk_integer_t bigint_as_integer_(kk_bigint_t* x) {
  kk_assert_internal(((uintptr_t)x&3) == 0);
  return _kk_new_integer((intptr_t)(bigint_ptr_(x)));
}

static bool bigint_is_unique_(kk_bigint_t* x) {
  return kk_block_is_unique(bigint_ptr_(x));
}

static kk_bigint_t* dup_bigint(kk_bigint_t* x) {
  return kk_basetype_dup_as(kk_bigint_t*, x);
}

static void drop_bigint(kk_bigint_t* x, kk_context_t* ctx) {
  kk_basetype_drop(x,ctx);
}


static size_t bigint_roundup_count(size_t count) {
  if (count*sizeof(kk_digit_t) < 16) return (16/sizeof(kk_digit_t));    // minimal size of 128-bit (= 16 bytes)
  else if ((count & 1) == 1) return (count+1);  // always even
  else return count;
}

static kk_bigint_t* bigint_alloc(size_t count, bool is_neg, kk_context_t* ctx) {
  size_t dcount = bigint_roundup_count(count);
  kk_bigint_t* b = (kk_bigint_t*)kk_block_alloc_any(sizeof(kk_bigint_t) - sizeof(kk_digit_t) + dcount*sizeof(kk_digit_t), 0, KK_TAG_BIGINT, ctx);
  b->is_neg = (is_neg ? 1 : 0);
  b->extra = (kk_extra_t)(dcount - count);
  b->count = count;
  return b;
}

static kk_bigint_t* bigint_alloc_zero(size_t count, bool is_neg, kk_context_t* ctx) {
  kk_bigint_t* b = bigint_alloc(count, is_neg, ctx);
  memset(b->digits, 0, sizeof(kk_digit_t)* bigint_available_(b));
  return b;
}

static kk_bigint_t* kk_bigint_trim_realloc_(kk_bigint_t* x, size_t count, kk_context_t* ctx) {
  kk_assert_internal(bigint_is_unique_(x));
  size_t dcount = bigint_roundup_count(count);
  size_t xcount = bigint_available_(x);
  kk_bigint_t* b;
  if ((dcount <= xcount) && (xcount-dcount) < (16/sizeof(kk_digit_t))) {
    b = x; // avoid realloc if shrinking by less than 128 bits
    dcount = xcount;
  }
  else {
    b = (kk_bigint_t*)kk_block_realloc(bigint_ptr_(x), sizeof(kk_bigint_t) - sizeof(kk_digit_t) + dcount*sizeof(kk_digit_t), ctx);
  }
  b->count = count;
  b->extra = (kk_extra_t)(dcount - count);
  return b;
}

static kk_bigint_t* kk_bigint_trim_to(kk_bigint_t* x, size_t count, bool allow_realloc, kk_context_t* ctx) {
  kk_ssize_t d = (kk_ssize_t)(bigint_available_(x) - count);
  kk_assert_internal(d >= 0 && bigint_is_unique_(x));
  if (d < 0) {
    return x;
  }
  else if (d > MAX_EXTRA) {
    if (allow_realloc) {
      return kk_bigint_trim_realloc_(x, count, ctx);
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

static kk_bigint_t* kk_bigint_trim(kk_bigint_t* x, bool allow_realloc, kk_context_t* ctx) {
  kk_assert_internal(bigint_is_unique_(x));
  size_t i = bigint_count_(x);
  while ((i > 0) && (x->digits[i-1] == 0)) { i--; }  // skip top zero's
  return kk_bigint_trim_to(x, i, allow_realloc, ctx);
}

static kk_bigint_t* bigint_alloc_reuse_(kk_bigint_t* x, size_t count, kk_context_t* ctx) {
  kk_ssize_t d = (kk_ssize_t)(bigint_available_(x) - count);
  if (d >= 0 && d <= MAX_EXTRA && bigint_is_unique_(x)) {   // reuse?
    return kk_bigint_trim_to(x, count, false /* no realloc */, ctx);
  }
  else {
    return bigint_alloc(count, bigint_is_neg_(x), ctx);
  }
}

static kk_bigint_t* bigint_copy(kk_bigint_t* x, size_t extra, kk_context_t* ctx) {
  kk_assert_internal(extra <= MAX_EXTRA);
  if (extra > MAX_EXTRA) extra = MAX_EXTRA;
  kk_bigint_t* z = bigint_alloc(x->count + extra, bigint_is_neg_(x), ctx);
  z->is_neg = x->is_neg;
  z = kk_bigint_trim_to(z, x->count, false, ctx);
  memcpy(z->digits, x->digits, x->count * sizeof(kk_digit_t) );
  drop_bigint(x,ctx);
  return z;
}

static kk_bigint_t* bigint_ensure_unique(kk_bigint_t* x, kk_context_t* ctx) {
  return (bigint_is_unique_(x) ? x : bigint_copy(x,0,ctx));
}

static kk_bigint_t* bigint_push(kk_bigint_t* x, kk_digit_t d, kk_context_t* ctx) {
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
static kk_integer_t integer_bigint(kk_bigint_t* x, kk_context_t* ctx) {
  if (x->count==0) {
    return kk_integer_zero;
  }
  else if (x->count==1
#if (DIGIT_BITS >= KK_SMALLINT_BITS-2)
    && x->digits[0] <= KK_SMALLINT_MAX
#endif
  ) {

    // make it a small int
    kk_intx_t i = x->digits[0];
    if (x->is_neg) i = -i;
    drop_bigint(x,ctx);
    return kk_integer_from_small(i);
  }
  else {
    return bigint_as_integer_(x);
  }
}

// create a bigint from an kk_int_t
static kk_bigint_t* bigint_from_int(kk_intx_t i, kk_context_t* ctx) {
  bool is_neg = (i < 0);
  if (is_neg) i = -i;
  kk_bigint_t* b = bigint_alloc(0, is_neg, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

// create a bigint from an int64_t
static kk_bigint_t* bigint_from_int64(int64_t i, kk_context_t* ctx) {
  bool is_neg = (i < 0);
  if (is_neg) i = -i;
  kk_bigint_t* b = bigint_alloc(0, is_neg, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

// create a bigint from a uint64_t
static kk_bigint_t* bigint_from_uint64(uint64_t i, kk_context_t* ctx) {
  kk_bigint_t* b = bigint_alloc(0, false, ctx); // will reserve at least 4 digits
  do {
    b = bigint_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}


// unpack to a bigint always
static kk_bigint_t* kk_integer_to_bigint(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x));
  if (kk_is_bigint(x)) {
    return kk_block_assert(kk_bigint_t*, _kk_as_bigint(x), KK_TAG_BIGINT);
  }
  else {
    kk_assert_internal(kk_is_smallint(x));
    return bigint_from_int(kk_smallint_from_integer(x), ctx);
  }
}

kk_integer_t kk_integer_from_bigu64(uint64_t i, kk_context_t* ctx) {
  return bigint_as_integer_(bigint_from_uint64(i, ctx));
}

kk_integer_t kk_integer_from_big64(int64_t i, kk_context_t* ctx) {
  return bigint_as_integer_(bigint_from_int64(i,ctx));
}

kk_integer_t kk_integer_from_big(kk_intx_t i, kk_context_t* ctx) {
  return bigint_as_integer_(bigint_from_int(i, ctx));
}


/*----------------------------------------------------------------------
  To string
----------------------------------------------------------------------*/

// Convert a digit to LOG_BASE characters.
// note: gets compiled without divisions on clang and GCC.
static size_t kk_digit_to_str_full(kk_digit_t d, char* buf) {
  for (size_t i = LOG_BASE; i > 0; d /= 10) {
    i--;
    buf[i] = '0' + (d % 10);
  }
  return LOG_BASE;
}
// convert digit to characters but skip leading zeros. No output if `d==0`.
static size_t kk_digit_to_str_partial(kk_digit_t d, char* buf) {
  char tmp[LOG_BASE];
  if (d==0) return 0;
  kk_digit_to_str_full(d, tmp);
  size_t i = 0;
  while (i < LOG_BASE && tmp[i]=='0') { i++; }
  for (size_t j = i; j < LOG_BASE; j++) {
    buf[j - i] = tmp[j];
  }
  return (LOG_BASE - i);
}

// Efficient conversion to a string buffer. Use `buf == NULL` to get the required size.
static size_t kk_bigint_to_buf_(const kk_bigint_t* b, char* buf, size_t kk_buf_size) {
  kk_assert_internal(b != NULL);
  const size_t count  = bigint_count_(b);
  const size_t needed = (count*LOG_BASE) + (bigint_is_neg_(b) ? 1 : 0) + 1; // + (sign and terminator);
  if (buf==NULL || kk_buf_size==0 || needed > kk_buf_size) return needed;
  size_t j = 0;  // current output position
  // sign
  if (bigint_is_neg_(b)) {
    buf[j++] = '-';
  }
  if (count==0) {
    buf[j++] = '0';
  }
  else {
    // skip leading zeros
    size_t i = count-1;
    while (i > 0 && b->digits[i]==0) {
      kk_assert_internal(false); // we should never have leading zeros
      i--;
    }
    // output leading digit
    j += kk_digit_to_str_partial(b->digits[i], &buf[j]);
    
    // and output the rest of the digits
    while (i > 0) {
      i--;
      j += kk_digit_to_str_full(b->digits[i], &buf[j]);
    }
  }
  buf[j++] = 0;
  return j;
}

static kk_string_t kk_bigint_to_string(kk_bigint_t* b, kk_context_t* ctx) {
  size_t needed = kk_bigint_to_buf_(b, NULL, 0);
  kk_string_t s = kk_string_alloc_buf(needed-1,ctx); // don't count terminator
  size_t used = kk_bigint_to_buf_(b, (char*)kk_string_cbuf_borrow(s), needed);
  drop_bigint(b,ctx);
  s = kk_string_adjust_length(s, used-1, ctx);  // don't count the ending zero included in used
  return s;
}

// kk_int_t to string
static kk_string_t kk_int_to_string(kk_intx_t n, kk_context_t* ctx) {
  kk_assert_internal(KK_INTPTR_SIZE <= 26);
  char buf[64];  // enough for 2^212
  bool neg = (n < 0);
  if (neg) n = -n;
  // output to buf in reverse order
  size_t i = 0;
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
  kk_string_t s = kk_string_alloc_buf(i,ctx);
  char* p = (char*)kk_string_cbuf_borrow(s);
  size_t j;
  for (j = 0; j < i; j++) {
    p[j] = buf[i - j - 1];
  }
  p[j] = 0;
  return s;
}

/*----------------------------------------------------------------------
  Parse an integer
----------------------------------------------------------------------*/
kk_decl_export bool kk_integer_parse(const char* s, kk_integer_t* res, kk_context_t* ctx) {
  kk_assert_internal(s!=NULL && res != NULL);
  if (res==NULL) return false;
  *res = kk_integer_zero;
  if (s==NULL) return false;
  // parse
  bool is_neg = false;
  size_t sig_digits = 0; // digits before the fraction
  size_t i = 0;
  // sign
  if (s[i] == '+') { i++; }
  else if (s[i] == '-') { is_neg = true; i++; }
  // check if hexadecimal?
  if (s[i]=='0' && (s[i+1]=='x' || s[i+1]=='X')) {
    return kk_integer_hex_parse(s, res, ctx);
  }
  if (!kk_ascii_is_digit(s[i])) return false;  // must start with a digit
  // significant
  for (; s[i] != 0; i++) {
    char c = s[i];
    if (kk_ascii_is_digit(c)) {
      sig_digits++;
    }
    else if (c=='_' && kk_ascii_is_digit(s[i+1])) { // skip underscores
    }
    else if ((c == '.' || c=='e' || c=='E') && (s[i+1]=='+' || kk_ascii_is_digit(s[i+1]))) { // found fraction/exponent
      break;
    }
    else return false; // error
  }
  // const char* sigend = s + i;
  // fraction
  size_t frac_digits = 0;
  size_t kk_frac_trailing_zeros = 0;
  if (s[i]=='.') {
    i++;
    for (; s[i] != 0; i++) {
      char c = s[i];
      if (kk_ascii_is_digit(c)) {
        if (c != '0') {
          kk_frac_trailing_zeros = 0;
        }
        else {
          kk_frac_trailing_zeros++;
        }
        frac_digits++;
      }
      else if (c=='_' && kk_ascii_is_digit(s[i+1])) { // skip underscores
      }
      else if ((c=='e' || c=='E') && (kk_ascii_is_digit(s[i+1]) || (s[i+1]=='+' && kk_ascii_is_digit(s[i+2])))) { // found fraction/exponent
        break;
      }
      else return false; // error
    }
  }
  frac_digits -= kk_frac_trailing_zeros; // ignore trailing zeros
  const char* end = s + i;
  // exponent
  size_t exp = 0;
  if (s[i]=='e' || s[i]=='E') {
    i++;
    if (s[i] == '+') i++;        // optional '+'
    for (; s[i] == '0'; i++) {}  // skip leading zeros
    for (; s[i] != 0; i++) {
      char c = s[i];
      if (kk_ascii_is_digit(c)) {
        exp = 10*exp + ((size_t)c - '0');
        if (exp > BASE) return false; // exponents must be < 10^9
      }
      else return false;
    }
  }
  if (exp < frac_digits) return false; // fractional number
  const size_t zero_digits = exp - frac_digits;
  const size_t dec_digits = sig_digits + frac_digits + zero_digits;  // total decimal digits needed in the bigint

  // parsed correctly, ready to construct the number
  // construct an `kk_int_t` if it fits.
  if (dec_digits < LOG_BASE) {   // must be less than LOG_BASE to avoid overflow
    kk_assert_internal(KK_INTX_SIZE >= sizeof(kk_digit_t));
    kk_intx_t d = 0;
    size_t digits = 0;
    for (const char* p = s; p < end && digits < dec_digits; p++) {
      char c = *p;
      if (kk_ascii_is_digit(c)) {
        digits++;
        d = 10*d + ((kk_intx_t)c - '0');
      }
    }
    for (;  digits < dec_digits; digits++) {  // zero digits
      d *= 10;
    }
    if (is_neg) d = -d;
    *res = kk_integer_from_int(d,ctx);
    return true;
  }

  // otherwise construct a big int
  const size_t count = ((dec_digits + (LOG_BASE-1)) / LOG_BASE); // round up
  kk_bigint_t* b = bigint_alloc(count, is_neg, ctx);
  size_t k     = count;
  size_t chunk = dec_digits%LOG_BASE; if (chunk==0) chunk = LOG_BASE; // initial number of digits to read
  const char* p = s;
  size_t digits = 0;
  while (p < end && digits < dec_digits) {
    kk_digit_t d = 0;
    // read a full digit
    for (size_t j = 0; j < chunk; ) {
      char c = (p < end ? *p++ : '0'); // fill out with zeros
      if (kk_ascii_is_digit(c)) {
        digits++;
        j++;
        d = 10*d + ((kk_digit_t)c - '0'); kk_assert_internal(d<BASE);
      }
    }
    // and store it
    kk_assert_internal(k > 0);
    if (k > 0) { b->digits[--k] = d; }
    chunk = LOG_BASE;  // after the first digit, all chunks are full digits
  }
  // set the final zeros
  kk_assert_internal(zero_digits / LOG_BASE == k);
  for (size_t j = 0; j < k; j++) { b->digits[j] = 0; }
  *res = integer_bigint(b, ctx);
  return true;
}

kk_integer_t kk_integer_from_str(const char* num, kk_context_t* ctx) {
  kk_integer_t i;
  bool ok = kk_integer_parse(num, &i, ctx);
  kk_assert_internal(ok);
  return (ok ? i : kk_integer_zero);
}


/*----------------------------------------------------------------------
  Parse an integer as hexadecimal
----------------------------------------------------------------------*/

static kk_bigint_t* kk_bigint_mul_small(kk_bigint_t* x, kk_digit_t y, kk_context_t* ctx);
static kk_bigint_t* kk_bigint_add_abs_small(kk_bigint_t* x, kk_digit_t y, kk_context_t* ctx);

bool kk_integer_hex_parse(const char* s, kk_integer_t* res, kk_context_t* ctx) {
  kk_assert_internal(s!=NULL && res != NULL);
  if (res==NULL) return false;
  *res = kk_integer_zero;
  if (s==NULL) return false;
  // parse
  bool is_neg = false;
  size_t hdigits = 0; // digit count
  size_t i = 0;
  // sign
  if (s[i] == '+') { i++; }
  else if (s[i] == '-') { is_neg = true; i++; }
  // skip leading 0[xX]
  if (s[i] == '0' && (s[i+1]=='x' || s[i+1]=='X')) {
    i += 2;  
  }
  if (!kk_ascii_is_hexdigit(s[i])) return false;  // must start with a hex digit

  // significant
  const char* start = s+i;
  for (; s[i] != 0; i++) {
    char c = s[i];
    if (kk_ascii_is_hexdigit(c)) {
      hdigits++;
    }
    else if (c=='_' && kk_ascii_is_hexdigit(s[i+1])) { 
      // skip underscores
    }
    else {
      return false; // error
    }
  }
  const char* end = s+i;

  // parsed correctly, ready to construct the number
  // construct an `kk_intx_t` if it fits.
  if (hdigits < LOG_BASE_HEX) {   // must be less than LOG_BASE_HEX to avoid overflow
    kk_assert_internal(KK_INTX_SIZE >= sizeof(kk_digit_t));
    kk_intx_t d = 0;
    for (const char* p = s; p < end;  p++) {
      char c = *p;
      if (kk_ascii_is_hexdigit(c)) {
        const kk_intx_t hd = (kk_intx_t)(kk_ascii_is_digit(c) ? c - '0' : 10 + (kk_ascii_is_lower(c) ? c - 'a' : c - 'A'));
        d = 16*d + hd;
      }
    }
    if (is_neg) d = -d;
    *res = kk_integer_from_int(d, ctx);
    return true;
  }
  
  // otherwise construct a big int
  const size_t count = (size_t)(ceil((double)hdigits * KK_LOG16_DIV_LOG10)) + 1; // conservatively overallocate to max needed.
  kk_extra_t ecount = (count >= MAX_EXTRA ? MAX_EXTRA-1 : (kk_extra_t)count);
  kk_bigint_t* b = bigint_alloc(ecount, is_neg, ctx);
  b->extra += (ecount-1);
  b->count -= (ecount-1);
  b->digits[0] = 0;

  // create in chucks of LOG_BASE_HEX digits
  size_t chunk = hdigits%LOG_BASE_HEX; if (chunk==0) chunk = LOG_BASE_HEX; // initial number of digits to read
  const char* p = start;
  while (p < end) {
    kk_digit_t d = 0;
    // read a full digit
    for (size_t j = 0; j < chunk && p < end; ) {
      char c = *p++; // fill out with zeros
      if (kk_ascii_is_hexdigit(c)) {
        j++;
        kk_digit_t hd = (kk_digit_t)(kk_ascii_is_digit(c) ? c - '0' : 10 + (kk_ascii_is_lower(c) ? c - 'a' : c - 'A'));
        d = 16*d + hd; 
        kk_assert_internal(d<BASE);
      }
    }
    // and multiply-add
    b = kk_bigint_mul_small(b, BASE_HEX, ctx);
    b = kk_bigint_add_abs_small(b, d, ctx);
    chunk = LOG_BASE_HEX;  // after the first chunk, the chunk is always a full LOG_BASE_HEX
  }
  *res = integer_bigint(b, ctx);
  return true;
}


/*----------------------------------------------------------------------
  negate, compare
----------------------------------------------------------------------*/

static kk_bigint_t* bigint_neg(kk_bigint_t* x, kk_context_t* ctx) {
  kk_bigint_t* z = bigint_ensure_unique(x,ctx);
  z->is_neg = !z->is_neg;
  return z;
}


static int bigint_compare_abs_(kk_bigint_t* x, kk_bigint_t* y) {
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

static int bigint_compare_(kk_bigint_t* x, kk_bigint_t* y) {
  if (x->is_neg != y->is_neg) {
    return (y->is_neg ? 1 : -1);
  }
  else {
    return (int)(kk_bigint_sign_(x)* bigint_compare_abs_(x, y));
  }
}

/*----------------------------------------------------------------------
  add absolute
----------------------------------------------------------------------*/

static kk_bigint_t* bigint_add(kk_bigint_t* x, kk_bigint_t* y, bool y_isneg, kk_context_t* ctx);
static kk_bigint_t* kk_bigint_sub(kk_bigint_t* x, kk_bigint_t* y, bool yneg, kk_context_t* ctx);


static kk_bigint_t* bigint_add_abs(kk_bigint_t* x, kk_bigint_t* y, kk_context_t* ctx) {   // x.count >= y.count
  // kk_assert_internal(kk_bigint_sign_(x) == kk_bigint_sign_(y));
  // ensure x.count >= y.count
  const size_t cx = bigint_count_(x);
  const size_t cy = bigint_count_(y);
  kk_assert_internal(cx >= cy);

  // allocate result bigint
  const size_t cz = ((bigint_last_digit_(x) + bigint_last_digit_(y) + 1) >= BASE ? cx + 1 : cx);
  kk_bigint_t* z = bigint_alloc_reuse_(x, cz, ctx); // if z==x, we reused x.
  //z->is_neg = x->is_neg;

  kk_assert_internal(cx>=cy);
  kk_assert_internal(bigint_count_(z) >= cx);
  kk_digit_t carry = 0;
  kk_digit_t sum = 0;
  // add y's digits
  size_t i;
  for (i = 0; i < cy; i++) {
    sum = x->digits[i] + y->digits[i] + carry;
    if (kk_unlikely(sum >= BASE)) {
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
    if (kk_unlikely(sum >= BASE)) {
      kk_assert_internal(sum==BASE && carry==1);  // can only be at most BASE
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
    // memcpy(&z->digits[i], &x->digits[i], (cx - i)*sizeof(kk_digit_t));
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
  kk_assert_internal(i == bigint_count_(z) || i+1 == bigint_count_(z));
  if (z != x) drop_bigint(x,ctx);
  drop_bigint(y,ctx);
  return kk_bigint_trim_to(z, i, true /* allow realloc */, ctx);
}


static kk_bigint_t* kk_bigint_add_abs_small(kk_bigint_t* x, kk_digit_t y, kk_context_t* ctx) {
  kk_assert_internal(y >= 0 && y < BASE);  
  const size_t cx = bigint_count_(x);

  // allocate result bigint
  const size_t cz = ((bigint_last_digit_(x) + y + 1) >= BASE ? cx + 1 : cx);  // is overflow is possible?
  kk_bigint_t* z = bigint_alloc_reuse_(x, cz, ctx); // if z==x, we reused x.
  kk_assert_internal(bigint_count_(z) >= cx);
  kk_digit_t carry = y;
  kk_digit_t sum = 0;

  // add y do the digits of x
  size_t i;
  for (i = 0; carry!=0 && i < cx; i++) {
    sum = x->digits[i] + carry;
    if (kk_unlikely(sum >= BASE)) {
      carry = 1;
      sum -= BASE;
      kk_assert_internal(sum < BASE);
    }
    else {
      carry = 0;
    }
    z->digits[i] = sum;
  }
  // wrap up
  if (i == cx) {
    // carry overflows into the last digit?
    if (carry != 0) {
      z->digits[i++] = carry;
    }
  }
  else {
    kk_assert_internal(i < cx && carry==0);
    if (z != x) {
      // copy rest of digits if not in-place
      for (; i < cx; i++) {
        z->digits[i] = x->digits[i];
      }
    }
    else {
      // skip to the end
      i = cx;
    }
  }
  kk_assert_internal(i == bigint_count_(z) || i + 1 == bigint_count_(z));
  if (z != x) { drop_bigint(x, ctx); }
  return kk_bigint_trim_to(z, i, true, ctx );
}


/*----------------------------------------------------------------------
  subtract absolute
----------------------------------------------------------------------*/

static kk_bigint_t* kk_bigint_sub_abs(kk_bigint_t* x, kk_bigint_t* y, kk_context_t* ctx) {  // |x| >= |y|
  kk_assert_internal(bigint_compare_abs_(x, y) >= 0);
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  kk_assert_internal(cx>=cy);
  kk_bigint_t* z = bigint_alloc_reuse_(x, cx, ctx);
  //z->is_neg = x->is_neg;
  kk_assert_internal(bigint_count_(z) >= cx);
  kk_digit_t borrow = 0;
  kk_digit_t diff = 0;
  // subtract y digits
  size_t i;
  for (i = 0; i < cy; i++) {
    diff = x->digits[i] - borrow - y->digits[i];
    if (kk_unlikely(diff >= BASE)) {   // unsigned wrap around
      borrow = 1;
      diff += BASE; // kk_assert_internal(diff >= 0);
    }
    else {
      borrow = 0;
    }
    z->digits[i] = diff;
  }
  // propagate borrow
  for (; borrow != 0 && i < cx; i++) {
    diff = x->digits[i] - borrow;
    if (kk_unlikely(diff >= BASE)) {  // unsigned wrap around
      // borrow stays 1;
      kk_assert_internal(diff==~((kk_digit_t)0));
      diff += BASE;
    }
    else {
      borrow = 0;
    }
    z->digits[i] = diff;
  }
  kk_assert_internal(borrow==0);  // since x >= y.
  // copy the tail
  if (z != x) {
    // memcpy(&z->digits[i], &x->digits[i], (cx - i)*sizeof(kk_digit_t));
    for (; i < cx; i++) {
      z->digits[i] = x->digits[i];
    }
    drop_bigint(x,ctx);
  }
  drop_bigint(y,ctx);
  return kk_bigint_trim(z,true,ctx);
}

/*----------------------------------------------------------------------
  Multiply & Sqr. including Karatsuba multiplication
----------------------------------------------------------------------*/

static kk_bigint_t* bigint_mul(kk_bigint_t* x, kk_bigint_t* y, kk_context_t* ctx) {
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  uint8_t is_neg = (bigint_is_neg_(x) != bigint_is_neg_(y) ? 1 : 0);
  size_t cz = cx+cy;
  kk_bigint_t* z = bigint_alloc_zero(cz,is_neg,ctx);
  for (size_t i = 0; i < cx; i++) {
    kk_digit_t dx = x->digits[i];
    for (size_t j = 0; j < cy; j++) {
      kk_digit_t dy = y->digits[j];
      kk_ddigit_t prod = ddigit_mul_add(dx,dy,z->digits[i+j]);
      kk_digit_t rem;
      kk_digit_t carry = ddigit_cdiv(prod, BASE, &rem);
      z->digits[i+j]    = rem;
      z->digits[i+j+1] += carry;
    }
  }
  drop_bigint(x,ctx);
  drop_bigint(y,ctx);
  return kk_bigint_trim(z, true,ctx);
}

static kk_bigint_t* kk_bigint_mul_small(kk_bigint_t* x, kk_digit_t y, kk_context_t* ctx) {
  kk_assert_internal(y < BASE);
  size_t cx = bigint_count_(x);
  uint8_t is_neg = bigint_is_neg_(x);
  size_t cz = cx+1;
  kk_bigint_t* z = bigint_alloc_reuse_(x, cz, ctx);
  kk_digit_t carry = 0;
  size_t i;
  for (i = 0; i < cx; i++) {
    kk_ddigit_t prod = ddigit_mul_add(x->digits[i], y, carry);
    kk_digit_t rem;
    carry = ddigit_cdiv(prod, BASE, &rem);
    kk_assert_internal(rem < BASE);
    z->digits[i] = rem;
  }
  while (carry > 0) {
    kk_assert_internal(i < bigint_count_(z));
    z->digits[i++] = carry % BASE;
    carry /= BASE;
  }
  if (z != x) { drop_bigint(x,ctx); }
  if (is_neg && !bigint_is_neg_(z)) { z = bigint_neg(z,ctx); }
  return kk_bigint_trim_to(z, i, true, ctx);
}

static kk_bigint_t* kk_bigint_sqr(kk_bigint_t* x, kk_context_t* ctx) {
  dup_bigint(x);
  return bigint_mul(x, x, ctx);
}

static kk_bigint_t* kk_bigint_shift_left(kk_bigint_t* x, size_t digits, kk_context_t* ctx) {
  const size_t cx = x->count;
  kk_bigint_t* z = bigint_alloc_reuse_(x, x->count + digits, ctx);
  memmove(&z->digits[digits], &x->digits[0], sizeof(kk_digit_t)*cx);
  memset(&z->digits[0], 0, sizeof(kk_digit_t)*digits);
  if (z != x) drop_bigint(x, ctx);
  return z;
}

static kk_bigint_t* kk_bigint_slice(kk_bigint_t* x, size_t lo, size_t hi, kk_context_t* ctx) {
  if (lo == 0 && bigint_is_unique_(x)) {
    return kk_bigint_trim_to(x, hi, false, ctx);
  }
  if (lo >= x->count) lo = x->count;
  if (hi > x->count)  hi = x->count;
  const size_t cz = hi - lo;
  kk_bigint_t* z = bigint_alloc(cz, x->is_neg, ctx);
  if (cz==0) {
    z->digits[0] = 0;
    z->count = 1;
    z->extra--;
  }
  else if (lo < x->count) {
    memcpy(&z->digits[0], &x->digits[lo], sizeof(kk_digit_t)*cz);
  }
  return z;
}

static kk_bigint_t* bigint_mul_karatsuba(kk_bigint_t* x, kk_bigint_t* y, kk_context_t* ctx) {
  size_t n = (x->count >= y->count ? x->count : y->count);
  if (n <= 25) return bigint_mul(x, y, ctx);
  n = ((n + 1) / 2);

  kk_bigint_t* b = kk_bigint_slice(dup_bigint(x), n, x->count, ctx);
  kk_bigint_t* a = kk_bigint_slice(x, 0, n, ctx);
  kk_bigint_t* d = kk_bigint_slice(dup_bigint(y), n, y->count, ctx);
  kk_bigint_t* c = kk_bigint_slice(y, 0, n, ctx);

  kk_bigint_t* ac = bigint_mul_karatsuba(dup_bigint(a), dup_bigint(c), ctx);
  kk_bigint_t* bd = bigint_mul_karatsuba(dup_bigint(b), dup_bigint(d), ctx);
  kk_bigint_t* abcd = bigint_mul_karatsuba( bigint_add(a, b, b->is_neg, ctx),
                                         bigint_add(c, d, d->is_neg, ctx), ctx);
  kk_bigint_t* p1 = kk_bigint_shift_left(kk_bigint_sub(kk_bigint_sub(abcd, dup_bigint(ac), ac->is_neg, ctx),
                                              dup_bigint(bd), bd->is_neg, ctx), n, ctx);
  kk_bigint_t* p2 = kk_bigint_shift_left(bd, 2 * n, ctx);
  kk_bigint_t* prod = bigint_add(bigint_add(ac, p1, p1->is_neg, ctx), p2, p2->is_neg, ctx);
  return kk_bigint_trim(prod,true, ctx);
}


/*----------------------------------'------------------------------------
  Pow
----------------------------------------------------------------------*/

kk_integer_t kk_integer_pow(kk_integer_t x, kk_integer_t p, kk_context_t* ctx) {
  if (kk_is_smallint(p)) {
    if (p.value == kk_integer_zero.value) return kk_integer_one;
  }
  if (kk_is_smallint(x)) {
    if (x.value == kk_integer_zero.value) {
      kk_integer_drop(p,ctx);  return kk_integer_zero;
    }
    if (x.value == kk_integer_one.value) {
      kk_integer_drop(p,ctx);  return kk_integer_one;
    }
    if (x.value == kk_integer_min_one.value) {
      return (kk_integer_is_even(p,ctx) ? kk_integer_one : kk_integer_min_one);
    }
  }
  kk_integer_dup(p);
  if (kk_integer_signum(p,ctx)==-1) {
    kk_integer_drop(p,ctx); return kk_integer_zero;
  }
  kk_integer_t y = kk_integer_one;
  if (kk_is_bigint(p)) {
    while (1) {
      kk_integer_dup(p);
      if (kk_integer_is_odd(p,ctx)) {
        kk_integer_dup(x);
        y = kk_integer_mul(y, x, ctx);
        p = kk_integer_dec(p, ctx);
      }
      if (kk_is_smallint(p)) break;
      p = kk_integer_div(p, kk_integer_from_small(2), ctx);
      x = kk_integer_sqr(x, ctx);
    }
  }
  kk_assert_internal(kk_is_smallint(p));
  kk_intx_t i = kk_smallint_from_integer(p);
  while (1) {
    if ((i&1)!=0) {
      kk_integer_dup(x);
      y = kk_integer_mul(y, x, ctx);
      i--;
    }
    if (i==0) break;
    i /= 2;
    x = kk_integer_sqr(x, ctx);
  }
  kk_integer_drop(x, ctx);
  return y;
}


/*----------------------------------------------------------------------
  Division
----------------------------------------------------------------------*/

static kk_bigint_t* kk_bigint_cdiv_cmod_small(kk_bigint_t* x, kk_digit_t y, kk_digit_t* pmod, kk_context_t* ctx) {
  kk_assert_internal(y < BASE);
  size_t cx = bigint_count_(x);
  // uint8_t is_neg = (bigint_is_neg_(x) != (y<0) ? 1 : 0);
  kk_bigint_t* z = bigint_alloc_reuse_(x, cx, ctx);
  kk_digit_t mod = 0;
  for (size_t i = cx; i > 0; i--) {
    kk_ddigit_t div = ddigit_mul_add(mod, BASE, x->digits[i-1]);
    kk_digit_t q = ddigit_cdiv( div, y, &mod);
    z->digits[i-1] = q;
  }
  if (pmod != NULL) {
    *pmod = mod;
  }
  if (z != x) drop_bigint(x, ctx);
  return kk_bigint_trim(z, true, ctx);
}


static kk_bigint_t* bigint_cdiv_cmod(kk_bigint_t* x, kk_bigint_t* y, kk_bigint_t** pmod, kk_context_t* ctx) {
  size_t cx = bigint_count_(x);
  size_t cy = bigint_count_(y);
  kk_assert_internal(cx >= cy);
  uint8_t is_neg = (bigint_is_neg_(x) != bigint_is_neg_(y) ? 1 : 0);
  kk_bigint_t* z = bigint_alloc_zero(cx - cy + 1, is_neg, ctx);
  // normalize
  kk_digit_t divisorHi = bigint_last_digit_(y);
  kk_ddigit_t dlambda = ddigit_mul_add(divisorHi, 2, BASE - 1);
  kk_digit_t lambda = ddigit_cdiv(dlambda, 2*divisorHi, NULL);  //  ((int64_t)BASE + 2*divisorHi - 1)/(2*divisorHi);
  kk_bigint_t* rem = kk_bigint_mul_small(x, lambda, ctx);
  if (rem->count <= cx) { rem = bigint_push(rem, 0, ctx); }
  kk_bigint_t* div = kk_bigint_mul_small(y, lambda, ctx);
  divisorHi = bigint_last_digit_(div); // todo: check more
  div = bigint_push(div, 0, ctx);
  for (kk_ssize_t sshift = (kk_ssize_t)(cx - cy); sshift >= 0; sshift--) {
    const size_t shift = (size_t)sshift;
    kk_digit_t qd = BASE - 1;
    kk_assert_internal(rem->count > (size_t)shift + cy);
    if (rem->digits[shift + cy] != divisorHi) {
      kk_assert_internal(rem->count > 1);
      kk_assert_internal(rem->digits[(size_t)shift + cy] < BASE);
      kk_ddigit_t rem_hi = ddigit_mul_add(rem->digits[shift + cy], BASE, rem->digits[shift + cy - 1]);
      qd = ddigit_cdiv(rem_hi, divisorHi, NULL);
    }
    kk_assert_internal(qd < BASE);
    kk_digit_t carry = 0;
    kk_digit_t borrow = 0;
    size_t cd = div->count;
    for (size_t i = 0; i < cd; i++) {
      kk_ddigit_t dcarry = ddigit_mul_add( qd, div->digits[i], carry );
      kk_digit_t carry_rem;
      carry = ddigit_cdiv(dcarry, BASE, &carry_rem);
      borrow += (rem->digits[shift + i] - carry_rem);
      if (borrow >= BASE) {  // unsigned wrap
        kk_assert_internal(borrow + BASE < BASE);
        rem->digits[shift + i] = borrow + BASE;
        borrow = 0;
        borrow--;   // -1
      }
      else {
        kk_assert_internal(borrow < BASE);
        rem->digits[shift + i] = borrow;
        borrow = 0;
      }
    }
    while (borrow != 0) {
      qd--;
      carry = 0;
      for (size_t i = 0; i < cd; i++) {
        carry += rem->digits[shift + i] - BASE + div->digits[i];
        if (carry >= BASE) {   // unsigned wrap
          kk_assert_internal(carry + BASE < BASE);
          rem->digits[shift + i] = (carry + BASE);
          carry = 0;
        }
        else {
          kk_assert_internal(carry < BASE);
          rem->digits[shift + i] = carry;
          carry = 1;
        }
      }
      borrow += carry;
    }
    z->digits[shift] = qd;
  }
  drop_bigint(div, ctx);
  if (pmod != NULL) {
    *pmod = kk_bigint_cdiv_cmod_small(rem, lambda, NULL, ctx); // denormalize remainder
  }
  else {
    drop_bigint(rem, ctx);
  }
  return kk_bigint_trim(z,true, ctx);
}


/*----------------------------------------------------------------------
  Addition and substraction
----------------------------------------------------------------------*/

static kk_bigint_t* bigint_add(kk_bigint_t* x, kk_bigint_t* y, bool yneg, kk_context_t* ctx) {
  if (bigint_is_neg_(x) != yneg) {
    return kk_bigint_sub(x, y, !yneg, ctx);
  }
  kk_bigint_t* z;
  if (bigint_count_(x) < bigint_count_(y)) {
    z = bigint_add_abs(y, x, ctx);
  }
  else {
    z = bigint_add_abs(x, y, ctx);
  }
  kk_assert_internal(bigint_is_unique_(z));
  z->is_neg = yneg;
  return z;
}

static kk_bigint_t* kk_bigint_sub(kk_bigint_t* x, kk_bigint_t* y, bool yneg, kk_context_t* ctx) {
  if (bigint_is_neg_(x) != yneg) {
    return bigint_add(x, y, !yneg, ctx);
  }
  if (bigint_compare_abs_(x,y) >= 0) {
    return kk_bigint_sub_abs(x, y, ctx);
  }
  else {
    kk_bigint_t* z = kk_bigint_sub_abs(y, x, ctx);
    kk_assert_internal(bigint_is_unique_(z));
    z->is_neg = !yneg;
    return z;
  }
}



/*----------------------------------------------------------------------
  Integer interface
----------------------------------------------------------------------*/

 kk_integer_t kk_integer_neg_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x));
  kk_bigint_t* bx = kk_integer_to_bigint(x,ctx);
  return integer_bigint(bigint_neg(bx, ctx), ctx);
}

 kk_integer_t kk_integer_sqr_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x));
  kk_bigint_t* bx = kk_integer_to_bigint(x,ctx);
  return integer_bigint(kk_bigint_sqr(bx, ctx), ctx);
}

 int kk_integer_signum_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x));
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  int signum = (bx->is_neg ? -1 : ((bx->count==0 && bx->digits[0]==0) ? 0 : 1));
  kk_integer_drop(x, ctx);
  return signum;
}

 bool kk_integer_is_even_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x));
  if (kk_is_smallint(x)) return ((x.value&0x04)==0);
  kk_bigint_t* bx = kk_integer_to_bigint(x,ctx);
  bool even = ((bx->digits[0]&0x1)==0);
  kk_integer_drop(x,ctx);
  return even;
}

int kk_integer_cmp_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x)&&kk_is_integer(y));
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  kk_bigint_t* by = kk_integer_to_bigint(y, ctx);
  int sign = bigint_compare_(bx, by);
  drop_bigint(bx, ctx);
  drop_bigint(by, ctx);
  return sign;
}

kk_integer_t kk_integer_add_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x)&&kk_is_integer(y));
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  kk_bigint_t* by = kk_integer_to_bigint(y, ctx);
  return integer_bigint(bigint_add(bx, by, by->is_neg, ctx), ctx);
}

kk_integer_t kk_integer_sub_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x)&&kk_is_integer(y));
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  kk_bigint_t* by = kk_integer_to_bigint(y, ctx);
  return integer_bigint(kk_bigint_sub(bx, by, by->is_neg, ctx), ctx);
}

static bool use_karatsuba(size_t i, size_t j) {
  return ((0.000012*(double)(i*j) - 0.0025*(double)(i+j)) >= 0.0);
}

kk_integer_t kk_integer_mul_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x)&&kk_is_integer(y));
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  kk_bigint_t* by = kk_integer_to_bigint(y, ctx);
  bool usek = use_karatsuba(bx->count, by->count);
  return integer_bigint((usek ? bigint_mul_karatsuba(bx,by, ctx) : bigint_mul(bx, by, ctx)), ctx);
}


/*----------------------------------------------------------------------
  Division and modulus
----------------------------------------------------------------------*/

kk_integer_t kk_integer_cdiv_cmod_generic(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx) {
  kk_assert_internal(kk_is_integer(x)&&kk_is_integer(y));
  if (kk_is_smallint(y)) {
    kk_intx_t ay = kk_smallint_from_integer(y);
    if (ay == 0) return kk_integer_zero; // raise div-by-zero
    if (ay == 1) {
      if (mod!=NULL) *mod = kk_integer_zero;
      return x;
    }
    if (ay == -1) {
      if (mod!=NULL) *mod = kk_integer_zero;
      return kk_integer_neg(x, ctx);
    }
    bool ay_neg = ay < 0;
    if (ay_neg) ay = -ay;
    if (ay < BASE) {
      // small division
      kk_assert_internal(ay > 0 && ay < BASE);
      kk_digit_t dmod;
      kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
      bool     xneg = bigint_is_neg_(bx);
      kk_bigint_t* bz = kk_bigint_cdiv_cmod_small(bx, (kk_digit_t)ay, &dmod, ctx);
      kk_intx_t imod = (xneg ?  -(kk_intx_t)dmod : (kk_intx_t)dmod);
      bz->is_neg = (xneg != ay_neg);
      if (mod != NULL) *mod = kk_integer_from_int(imod, ctx);
      return integer_bigint(bz, ctx);
    }
    // fall through to full division
  }
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  kk_bigint_t* by = kk_integer_to_bigint(y, ctx);
  int cmp = bigint_compare_abs_(bx, by);
  if (cmp < 0) {
    if (mod) {
      *mod = x;
    }
    else {
      kk_integer_drop(x, ctx);
    }
    kk_integer_drop(y, ctx);
    return kk_integer_zero;
  }
  if (cmp==0) {
    if (mod) *mod = kk_integer_zero;
    kk_intx_t i = (bigint_is_neg_(bx) == bigint_is_neg_(by) ? 1 : -1);
    kk_integer_drop(x, ctx);
    kk_integer_drop(y, ctx);
    return kk_integer_from_small(i);
  }
  bool qneg = (bigint_is_neg_(bx) != bigint_is_neg_(by));
  bool mneg = bigint_is_neg_(bx);
  kk_bigint_t* bmod = NULL;
  kk_bigint_t* bz = bigint_cdiv_cmod(bx, by, (mod!=NULL ? &bmod : NULL), ctx);
  bz->is_neg = qneg;
  if (mod!=NULL && bmod != NULL) {
    bmod->is_neg = mneg;
    *mod = integer_bigint(bmod, ctx);
  }
  return integer_bigint(bz, ctx);
}

kk_integer_t kk_integer_cdiv_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  return kk_integer_cdiv_cmod_generic(x, y, NULL, ctx);
}

kk_integer_t kk_integer_cmod_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_integer_t mod = kk_integer_zero;
  kk_integer_t div = kk_integer_cdiv_cmod_generic(x, y, &mod, ctx);
  kk_integer_drop(div, ctx);
  return mod;
}


// Euclidean division: see <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf>
// Always preferred at it is more regular than C style truncated division. For example:
// - modulus is always positive     
// - x `div` 2^n == sar(x,n)        for any x, n
// - x `mod` 2^n == and(x,2^(n-1))  for any x, n
// - Euclidean division behaves identical to truncated division for positive dividends.
kk_integer_t kk_integer_div_mod_generic(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx) {
if (kk_integer_is_zero(kk_integer_dup(y),ctx)) {
    // div by zero
    if (mod!=NULL) {
      *mod = x;
    }
    else {
      kk_integer_drop(x,ctx);
    }
    kk_integer_drop(y, ctx);
    return kk_integer_zero;
  }
  else if (kk_integer_is_pos(kk_integer_dup(x),ctx)) {
    // positive x
    return kk_integer_cdiv_cmod_generic(x, y, mod, ctx);
  }
  else {
    // regular
    kk_integer_t m;
    kk_integer_t d = kk_integer_cdiv_cmod_generic(x, kk_integer_dup(y), &m, ctx);
    if (kk_integer_is_neg(kk_integer_dup(m), ctx)) {
      if (kk_integer_is_neg(kk_integer_dup(y), ctx)) {
        d = kk_integer_inc(d, ctx);
        if (mod!=NULL) { m = kk_integer_sub(m, y, ctx); }      
      }
      else {
        d = kk_integer_dec(d, ctx);
        if (mod!=NULL) { m = kk_integer_add(m, y, ctx); } 
      }
    }
    kk_integer_drop(y,ctx);
    if (mod==NULL) {
      kk_integer_drop(m, ctx);
    }
    else {
      *mod = m;
    }
    return d;
  }
}

kk_integer_t kk_integer_div_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  return kk_integer_div_mod_generic(x, y, NULL, ctx);
}

kk_integer_t kk_integer_mod_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_integer_t mod = kk_integer_zero;
  kk_integer_t div = kk_integer_div_mod_generic(x, y, &mod, ctx);
  kk_integer_drop(div, ctx);
  return mod;
}

/*----------------------------------------------------------------------
  Conversion, printing
----------------------------------------------------------------------*/


kk_string_t kk_integer_to_string(kk_integer_t x, kk_context_t* ctx) {
  if (kk_is_smallint(x)) {
    return kk_int_to_string(kk_smallint_from_integer(x), ctx);
  }
  else {
    return kk_bigint_to_string(kk_integer_to_bigint(x, ctx), ctx);
  }
}

static kk_string_t kk_int_to_hex_string(kk_intx_t i, bool use_capitals, kk_context_t* ctx) {
  kk_assert_internal(i >= 0);
  char buf[64];
  if (use_capitals) {
    snprintf(buf, 64, PRIXUX, (kk_uintx_t)i);
  }
  else {
    snprintf(buf, 64, PRIxUX, (kk_uintx_t)i);
  }
  return kk_string_alloc_dup(buf, ctx);
}

static size_t kk_bigint_to_hex_buf(kk_bigint_t* b, char* buf, size_t size, bool use_capitals, kk_context_t* ctx) {
  // TODO: can we improve the performance using the Chinese remainder theorem? 
  // and avoid the reversal? and per digit divide?
  kk_assert_internal(!b->is_neg);
  const char baseA = (use_capitals ? 'A' : 'a');
  size_t len = 0;
  while (len < size && ((b->count > 1) || (b->digits[0] != 0))) {
    // convert per BASE_HEX chunk in reverse order
    kk_digit_t mod;
    b = kk_bigint_cdiv_cmod_small(b, BASE_HEX, &mod, ctx);
    for (size_t i = 0; i < LOG_BASE_HEX && len < size; i++) {
      // convert the mod per hex digit in reverse order
      kk_digit_t d = mod % 16;
      mod /= 16;
      buf[len++] = (char)(d < 10 ? d + '0' : d - 10 + (kk_digit_t)baseA);
    }
  }
  if (len == 0) {
    buf[len++] = '0';
  }
  while (len > 0 && buf[len - 1] == '0') { // remove trailing zeros  
    len--;  
  }
  buf[len] = 0;

  // reverse the digits (careful with alignment restrictions if trying optimizing this)
  for (size_t i = 0; i < len/2; i++) {
    char c = buf[i];
    char d = buf[len - 1 - i];
    buf[len - 1 - i] = c;
    buf[i] = d;
  }

  drop_bigint(b,ctx);
  return len;
}

static kk_string_t kk_bigint_to_hex_string(kk_bigint_t* b, bool use_capitals, kk_context_t* ctx) {
  size_t dec_needed = kk_bigint_to_buf_(b, NULL, 0);   
  size_t needed = (size_t)(ceil((double)dec_needed * KK_LOG10_DIV_LOG16)) + 2; // conservative estimate
  kk_string_t s = kk_string_alloc_buf(needed, ctx);
  size_t len = kk_bigint_to_hex_buf(b, (char*)kk_string_cbuf_borrow(s), needed, use_capitals, ctx);
  kk_assert_internal(needed > len);
  return kk_string_adjust_length(s, len, ctx);
}

kk_decl_export kk_string_t kk_integer_to_hex_string(kk_integer_t x, bool use_capitals, kk_context_t* ctx) {
  if (kk_is_smallint(x)) {
    return kk_int_to_hex_string(kk_smallint_from_integer(x), use_capitals, ctx);
  }
  else {
    return kk_bigint_to_hex_string(kk_integer_to_bigint(x, ctx), use_capitals, ctx);
  }
}

void kk_integer_fprint(FILE* f, kk_integer_t x, kk_context_t* ctx) {
  kk_string_t s = kk_integer_to_string(x, ctx);
  fprintf(f, "%s", kk_string_buf_borrow(s));
  kk_string_drop(s, ctx);
}

void kk_integer_print(kk_integer_t x, kk_context_t* ctx) {
  kk_integer_fprint(stdout, x, ctx);
}

/*----------------------------------------------------------------------
  Operations for efficient fixed point arithmetic.
  Count trailing zeros, count digits, mul_pow10, div_pow10
----------------------------------------------------------------------*/

// count trailing decimal zeros
static kk_intx_t int_ctz(kk_intx_t x) {
  kk_intx_t count = 0;
  for (; x != 0 && (x%10) == 0; x /= 10) {
    count++;
  }
  return count;
}

static kk_intx_t bigint_ctz(kk_bigint_t* x, kk_context_t* ctx) {
  kk_intx_t i;
  for (i = 0; i < (kk_intx_t)(x->count-1); i++) {
    if (x->digits[i] != 0) break;
  }
  kk_assert_internal(x->digits[i]!=0);
  kk_intx_t ctz = (int_ctz(x->digits[i]) + LOG_BASE*i);
  drop_bigint(x, ctx);
  return ctz;
}

kk_integer_t kk_integer_ctz(kk_integer_t x, kk_context_t* ctx) {
  if (kk_is_smallint(x)) {
    return kk_integer_from_small(int_ctz(kk_smallint_from_integer(x)));
  }
  else {
    return kk_integer_from_int(bigint_ctz(kk_integer_to_bigint(x, ctx), ctx), ctx);
  }
}

static kk_intx_t int_count_digits(kk_intx_t x) {
  // make positive
  kk_uintx_t u;
  if (x < 0) {
    u = (kk_uintx_t)(x == KK_INTX_MIN ? KK_INTX_MAX : -x);  // careful for overflow
  }
  else {
    u = (kk_uintx_t)x;
  }
  return kk_bits_digits(u);
}

static kk_intx_t bigint_count_digits(kk_bigint_t* x, kk_context_t* ctx) {
  kk_assert_internal(x->count > 0);
  kk_intx_t count;
#if (DIGIT_BITS==64)
  count = kk_bits_digits64(x->digits[x->count-1]) + LOG_BASE*(x->count - 1);
#else
  count = kk_bits_digits32(x->digits[x->count-1]) + LOG_BASE*(x->count - 1);
#endif
  drop_bigint(x, ctx);
  return count;
}

kk_integer_t kk_integer_count_digits(kk_integer_t x, kk_context_t* ctx) {
  if (kk_is_smallint(x)) {
    return kk_integer_from_small(int_count_digits(kk_smallint_from_integer(x)));
  }
  else {
    return kk_integer_from_int(bigint_count_digits(kk_integer_to_bigint(x, ctx), ctx), ctx);
  }
}

static kk_digit_t powers_of_10[LOG_BASE+1] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
#if (LOG_BASE > 9)
                                          , 10000000000, 100000000000, 1000000000000, 10000000000000, 100000000000000
                                          , 1000000000000000, 10000000000000000, 100000000000000000, 1000000000000000000
#endif
                                          };

kk_integer_t kk_integer_mul_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx) {
  if (kk_integer_is_zero(kk_integer_dup(p),ctx)) {
    kk_integer_drop(p, ctx);
    return x;
  }
  if (kk_integer_is_zero(kk_integer_dup(x),ctx)) {
    kk_integer_drop(p, ctx); // x is small
    return kk_integer_zero;
  }
  if (!kk_is_smallint(p)) {
    // TODO: raise error
    return kk_integer_zero;
  }
  kk_intx_t i = kk_smallint_from_integer(p);

  // negative?
  if (i < 0) {
    return kk_integer_div_pow10(x, kk_integer_from_small(-i), ctx);
  }

  // small multiply?
  if (kk_is_smallint(x) && i < LOG_BASE) {
    return kk_integer_mul(x, kk_integer_from_int(powers_of_10[i], ctx), ctx);
  }

  // multiply a bigint
  size_t large = (size_t)i / LOG_BASE;  // number of zero digits to shift in
  size_t ismall = (size_t)i % LOG_BASE;  // small multiply the left over
  kk_bigint_t* b = kk_integer_to_bigint(x, ctx);
  if (ismall > 0) {
    b = kk_bigint_mul_small(b, powers_of_10[ismall], ctx);
  }
  if (large > 0) {
    size_t bcount = b->count;
    size_t ccount = bcount + large;
    kk_bigint_t* c = bigint_alloc_reuse_(b, ccount, ctx);
    memmove(&c->digits[large], &b->digits[0], bcount * sizeof(kk_digit_t));
    memset(&c->digits[0], 0, large * sizeof(kk_digit_t));
    kk_assert_internal(c->count == ccount);
    if (b != c) drop_bigint(b, ctx);
    b = c;
  }
  return integer_bigint(b, ctx);
}


kk_integer_t kk_integer_cdiv_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx) {
  if (kk_integer_is_zero(kk_integer_dup(p),ctx)) {
    kk_integer_drop(p, ctx);
    return x;
  }
  if (kk_integer_is_zero(kk_integer_dup(x),ctx)) {
    kk_integer_drop(p, ctx); // x is small
    return kk_integer_zero;
  }
  if (!kk_is_smallint(p)) {
    // TODO: raise error
    return kk_integer_zero;
  }
  kk_intx_t i = kk_smallint_from_integer(p);

  // negative?
  if (i < 0) {
    return kk_integer_mul_pow10(x, kk_integer_from_small(-i), ctx);
  }

  // small divide?
  if (kk_is_smallint(x) && i < LOG_BASE) {
    return kk_integer_cdiv(x, kk_integer_from_int(powers_of_10[i], ctx), ctx);
  }

  // divide a bigint
  size_t large = (size_t)i / LOG_BASE;  // number of zero digits to shift out
  size_t ismall = (size_t)i % LOG_BASE;  // small divide the left over
  kk_bigint_t* b = kk_integer_to_bigint(x, ctx);
  size_t bcount = b->count;
  if (large > 0) {
    if (large >= bcount) {
      drop_bigint(b, ctx);
      return kk_integer_zero;
    }
    size_t ccount = bcount - large;
    if (bigint_is_unique_(b)) {
      memmove(&b->digits[0], &b->digits[large], ccount * sizeof(kk_digit_t));
      b = kk_bigint_trim_to(b, ccount, true, ctx);
    }
    else {
      kk_bigint_t* c = bigint_alloc(ccount, b->is_neg, ctx);
      memcpy(&c->digits[0], &b->digits[large], ccount * sizeof(kk_digit_t));
      drop_bigint(b, ctx);
      b = c;
    }
  }
  if (ismall > 0) {
    b = kk_bigint_cdiv_cmod_small(b, powers_of_10[ismall], NULL, ctx);
  }
  return integer_bigint(b, ctx);
}

kk_integer_t kk_integer_div_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx) {
  bool xneg = kk_integer_is_neg(kk_integer_dup(x), ctx);
  kk_integer_t d = kk_integer_cdiv_pow10(x, p, ctx);
  if (xneg) {
    d = kk_integer_dec(d, ctx);
  }
  return d;
}

int32_t kk_integer_clamp32_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  int32_t i = 0;
#if (BASE < INT32_MAX)
  if (bx->count > 1) {
    i = (int32_t)(bx->digits[1]*BASE);
  }
#endif
  i += (int32_t)bx->digits[0];
  if (bx->is_neg) i = -i;
  drop_bigint(bx,ctx);
  return i;
}

int64_t kk_integer_clamp64_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  int64_t i = 0;
#if (BASE < (INT64_MAX/BASE))
  if (bx->count > 2) i += ((int64_t)bx->digits[2])*BASE*BASE;
#endif
#if (BASE < INT64_MAX)
  if (bx->count > 1) i += ((int64_t)bx->digits[1])*BASE;
#endif
  i += bx->digits[0];
  if (bx->is_neg) i = -i;
  drop_bigint(bx, ctx);
  return i;
}

size_t kk_integer_clamp_size_t_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  size_t i = 0;
  if (bx->is_neg) goto done;
#if (BASE < (SIZE_MAX/BASE))
  if (bx->count > 3) {
    i = SIZE_MAX; goto done; // overflow
  }
  else if (bx->count == 3) {
    kk_digit_t d = bx->digits[2];
    if (d > (SIZE_MAX/(BASE*(size_t)BASE))) {
      i = SIZE_MAX; goto done; // overflow;
    }
    i = (size_t)d*BASE*BASE;
  }
  if (bx->count >= 2) {
    i += (size_t)bx->digits[1]*BASE;
  }
#elif (BASE < SIZE_MAX)
  if (bx->count > 2) {
    i = SIZE_MAX; goto done; // overflow
  }
  else if (bx->count >= 2) {
    kk_digit_t d = bx->digits[1];
    if (d > (SIZE_MAX/BASE)) {
      i = SIZE_MAX; goto done; // overflow;
    }
    i = (size_t)d*BASE;
  }
#endif
  i += (size_t)bx->digits[0];
done:
  drop_bigint(bx, ctx);
  return i;
}

double kk_integer_as_double_generic(kk_integer_t x, kk_context_t* ctx) {
  kk_bigint_t* bx = kk_integer_to_bigint(x, ctx);
  if (bx->count > ((310/LOG_BASE) + 1)) return (bx->is_neg ? -HUGE_VAL : HUGE_VAL);
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

kk_integer_t kk_integer_from_double(double d, kk_context_t* ctx) {
  char buf[32];
  d = double_round_even(d);
  if (!isnormal(d)) {
    return kk_integer_zero;
  }
  else {
    snprintf(buf, 32, "%.20e", d);
    kk_integer_t i;
    bool ok = kk_integer_parse(buf, &i, ctx);
    return (ok ? i : kk_integer_zero);
  }
}
