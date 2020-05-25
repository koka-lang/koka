#pragma once
#ifndef __BOX_H__
#define __BOX_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------
  Boxing

On 64-bit the top 16-bits are the sign extension of the bottom 48 bits
and thus always 0x0000 or 0xFFFF (denoted as `ssss`).
Using `x` for bytes, and `b` for bits, with `z` the least significant byte, we have:

    ssss xxxx xxxx xxxz   z = bbbb bb00  : 48-bit sign extended pointer (always aligned to 4 bytes!)
    ssss xxxx xxxx xxxz   z = bbbb bb01  : 46-bit sign extended integer
    ssss xxxx xxxx xxxz   z = bbbb bb10  : 46-bit unsigned enumeration
    ssss xxxx xxxx xxxz   z = bbbb bb11  : 48-bit sign extended raw pointer (always aligned to 4 bytes)

So, an integer `n` is encoded boxed as `boxed(n) == n*4 + 1`.

On 64-bit we can now encode doubles such that the top 16-bits are
between 0x0001 and 0xFFFE. Normally, the range of negative quiet NaN's
is at the end of the double range between:
    FFF8 0000 0000 0000 and 0xFFFF FFFF FFFF FFFF.
If we add
    0001 0000 0000 0000  == 1 << 48
to a double, we set the range of all _other_ doubles between:
    0001 0000 0000 0000 and 0xFFF8 FFFF FFFF FFFF
which nicely fits in our boxed encoding.

(This also allows encoding of quiet NaN's up to 0xFFFD FFFF FFFF FFFF. If
we see a quiet NaN in that range we encode it as is; otherwise we decrease
the quite NaN by 0002 0000 0000 0000 so it falls into the acceptable range)

By reserving 2 of the lower bits we can make large integer arithmetic
more efficient where a large integer is either a pointer to a bigint
or a sign extended small integer of at most 30 bits; We can do addition
then directly on the boxed values as:

    boxed integer_add(boxed x, boxed y) {
      int_t z;
      if (unlikely(smallint_add_ovf32(x,y,&z) || (z&0x02)==0)) return integer_add_generic(x,y);
      return (boxed)(z - 1);  // or: z ^ 0x03
    }

We can do a direct 32-bit addition and test for 32-bit overflow and
only afterwards test if we actually added 2 integers and not two pointers.
If we add, the last 2 bits are:

     x + y = z
    00  00  00    ptr + ptr
    00  01  01    ptr + int
    01  00  01    int + ptr
    01  01  10    int + int

so the test `(z&0x02) == 0` checks if we added 2 integers.
Finally, we subtract 1 to normalize the integer again. With gcc on x86-64 we get:

integer_add(long,long)
        mov     edx, edi   // move bottom 32-bits of `x` to edx
        add     edx, esi   // add bottom 32-bits of `y`
        movsx   rax, edx   // sign extend to 64-bits result in rax
        jo      .L7        // on (32-bit) overflow goto slow
        and     edx, 2     // check bit 1 of the result
        je      .L7        // if it is zero, goto slow
        sub     rax, 1     // normalize back to an integer
        ret
.L7:
        jmp     integer_add_generic(long, long)


On 32-bit we use the same encoding of the bottom bits but no restriction
on the top 16 bits as we will box doubles as heap allocated values.

  xxxx xxxz   z = bbbb bb00  : 32-bit pointer (always aligned to 4 bytes)
  xxxx xxxz   z = bbbb bb01  : 30-bit integer
  xxxx xxxz   z = bbbb bb10  : 30-bit enumeration
  xxxx xxxz   z = bbbb bb11  : reserved

We still have fast addition of large integers but use 14-bit small
integers where we can do a 16-bit efficient overflow check.
----------------------------------------------------------------*/

#define box_null   (datatype_null)    // enum 0

// the _fast versions can apply if you are sure it is not a double
static inline bool is_ptr_fast(box_t b) {
  return ((b & 0x03)==0);
}
static inline bool is_int_fast(box_t b) {
  return ((b & 0x03)==1);
}
static inline bool is_enum_fast(box_t b) {
  return ((b & 0x03)==2);
}
static inline bool is_cptr_fast(box_t b) {
  return ((b & 0x03)==3);
}

#define MAX_BOXED_INT  ((int_t)INTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_INT  (- MAX_BOXED_INT - 1)

#define MAX_BOXED_ENUM ((uint_t)UINTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_ENUM (0)

#if INTPTR_SIZE==8

#define BOXED_INT_BITS      (46)

// checking does not have to be optimal as we do not generally use this.
static inline bool is_double(box_t b) {
  return ((uint16_t)((shr(b,48) + 1)) > 1);  // test of top 16 bits are not 0x0000 or 0xFFFF
}
static inline bool is_ptr_and_not_double(box_t b) {
  // faster test if a `ptr` is guaranteed to not have 0xFFFF as the top 16 bits. (which is usually the case, unless you are kernel programming)
  // this is used when doing `boxed_incref` for example.
  assert_internal(is_ptr_fast(b));
  return ((uint16_t)shr(b, 48) == 0);  
}
static inline bool is_ptr(box_t b) {
  return (is_ptr_fast(b) && likely(is_ptr_and_not_double(b)));
}
static inline bool is_int(box_t b) {
  return (is_int_fast(b) && likely(!is_double(b)));
}
static inline bool is_enum(box_t b) {
  return (is_enum_fast(b) && likely(!is_double(b)));
}
static inline bool is_cptr(box_t b) {
  return (is_cptr_fast(b) && likely(!is_double(b)));
}

static inline double unbox_double(box_t v, context_t* ctx) {
  UNUSED(ctx);
  assert_internal(is_double(v));
  union { uint64_t _v; double d; } u;
  u._v = ((uint64_t)v - ((uint64_t)1 << 48));  // unsigned to avoid UB
  return u.d;
}

static inline box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  union { double _d; uint64_t v; } u;
  u._d = d;
  if (unlikely(u.v >= ((uint64_t)0xFFFE << 48))) {
    // high quiet NaN, subtract to bring it in range
    u.v = u.v - ((uint64_t)0x0002 << 48);
    d = u._d;  // for the assert_internal
  }
  box_t v = (box_t)(u.v + ((uint64_t)1 << 48));
  assert_internal(is_double(v));
  assert_internal(unbox_double(v,ctx) == d); // (well, not for high qNaN)
  return v;
}

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  UNUSED(ctx);
  int_t i = unbox_int(v);
  assert_internal(i >= INT32_MIN && i <= INT32_MAX);
  return (int32_t)(i);
}

static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  UNUSED(ctx);
  return box_int(i);
}



#elif INTPTR_SIZE==4

#define BOXED_INT_BITS      (30)

static inline bool is_ptr(box_t b) {
  return is_ptr_fast(b);
}
static inline bool is_int(box_t b) {
  return is_int_fast(b);
}
static inline bool is_enum(box_t b) {
  return is_enum_fast(b);
}
static inline bool is_cptr(box_t b) {
  return is_cptr_fast(b);
}
static inline bool is_double(box_t v) {
  return (is_ptr(v) && ptr_tag(unbox_ptr(v)) == TAG_DOUBLE);
}

static inline double unbox_double(box_t b, context_t* ctx) {
  assert_internal(is_double(b));
  ptr_t p = unbox_ptr(b);
  double d = *(ptr_data_as(double, p));
  ptr_drop(p,ctx);
  return d;
}

static inline box_t box_double(double d, context_t* ctx) {
  double* data = ptr_alloc_data_as(double, 0, TAG_DOUBLE, ctx);
  *data = d;
  return box_ptr(ptr_from_data(data));
}

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  if (likely(is_int(v))) {
    return unbox_int(v);
  }
  else {
    assert_internal(is_ptr(v) && ptr_tag(unbox_ptr(v)) == TAG_INT32);
    ptr_t p = unbox_ptr(v);
    int32_t i = *(ptr_data_as(int32_t,p));
    ptr_drop(p,ctx);
    return i;
  }
}
static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  if (i >= MIN_BOXED_INT && i <= MAX_BOXED_INT) {
    return box_int(i);
  }
  else {
    int32_t* data = ptr_alloc_data_as(int32_t, 0, TAG_INT32, ctx);
    *data = i;
    return box_ptr(ptr_from_data(data));
  }
}

#else
# error "platform must be 32 or 64 bits."
#endif

static inline ptr_t unbox_ptr(box_t v) {
  assert_internal(is_ptr(v));
  return (ptr_t)v;
}

static inline box_t box_ptr(ptr_t p) {
  assert_internal(((uint_t)p & 0x03) == 0); // check alignment
  return (box_t)p;
}

static inline void* unbox_cptr(box_t b) {
  assert_internal(is_cptr(b));
  return (void*)(b & ~0x03);
}

static inline box_t box_cptr(void* p) {
  assert_internal(((uint_t)p & 0x03) == 0); // check alignment
  box_t b = (box_t)p | 0x03;
  assert_internal(is_cptr(b));
  return b;
}

static inline uint_t unbox_enum(box_t b) {
  assert_internal(is_enum(b));
  return shr(b, 2);
}

static inline box_t box_enum(uint_t u) {
  assert_internal(u <= MAX_BOXED_ENUM);
  box_t b = (u << 2) | 0x02;
  assert_internal(is_enum(b));
  return b;
}

static inline int_t unbox_int(box_t v) {
  assert_internal(is_int(v));
  return (sar(v, 2));
}

static inline box_t box_int(int_t i) {
  assert_internal(i >= MIN_BOXED_INT && i <= MAX_BOXED_INT);
  box_t v = (i << 2) | 0x01;
  assert_internal(is_int(v));
  return v;
}

static inline int16_t unbox_int16(box_t v) {
  int_t i = unbox_int(v);
  assert_internal(i >= INT16_MIN && i <= INT16_MAX);
  return (int16_t)i;
}

static inline box_t box_int16(int16_t i) {
  return box_int(i);
}

static inline bool unbox_bool(box_t v) {
  return (unbox_enum(v) != 0);
}

static inline box_t box_bool(bool b) {
  return box_enum(b ? 1 : 0);
}

static inline block_t* unbox_block_t(box_t v, tag_t expected_tag ) {
  UNUSED_RELEASE(expected_tag);
  block_t* b = ptr_as_block(unbox_ptr(v));
  assert_internal(block_tag(b) == expected_tag);
  return b;
}

static inline box_t box_block_t(block_t* b) {
  return box_ptr(block_as_ptr(b));
}


#define unbox_valuetype(tp,box,ctx) (*(ptr_data_as_assert(tp,unbox_ptr(box),TAG_BOX)))

#define box_valuetype(tp,x,val,scan_fsize,ctx)  \
  do{ \
     ptr_t p = ptr_alloc(sizeof(tp),scan_fsize,TAG_BOX,ctx); \
     *(ptr_data_as(tp,p)) = val; \
     x = box_ptr(p); \
  }while(0);

static inline datatype_t unbox_datatype(box_t v) {
  assert_internal(is_ptr(v) || is_enum(v));
  return v;
}

static inline datatype_t unbox_datatype_assert(box_t v, tag_t tag) {
  UNUSED_RELEASE(tag);
  datatype_t d = unbox_datatype(v);
  assert_internal(datatype_tag(d) == tag);
  return d;
}

static inline box_t box_datatype(datatype_t d) {
  assert_internal(is_ptr(d) || is_enum(d));
  return d;
}

#endif // include guard