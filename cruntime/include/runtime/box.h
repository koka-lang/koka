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
  xxxx xxxz   z = bbbb bb11  : 30-bit raw C pointer (aligned to 4 bytes)

We still have fast addition of large integers but use 14-bit small
integers where we can do a 16-bit efficient overflow check.
----------------------------------------------------------------*/

static inline box_t box_from_uintptr(uintptr_t u) {
  box_t b = { u };
  return b;
}
static inline box_t box_from_intptr(intptr_t i) {
  return box_from_uintptr((uintptr_t)i);
}
static inline uintptr_t box_as_uintptr(box_t b) {
  return b.box;
}
static inline intptr_t box_as_intptr(box_t b) {
  return (intptr_t)box_as_uintptr(b);
}
static inline bool box_eq(box_t b1, box_t b2) {
  return (b1.box == b2.box);
}

#define box_null   (box_from_uintptr(3))    // box_cptr(NULL)


// the _fast versions can apply if you are sure it is not a double
static inline bool is_ptr_fast(box_t b) {
  return ((b.box & 0x03)==0);
}
static inline bool is_int_fast(box_t b) {
  return ((b.box & 0x03)==1);
}
static inline bool is_enum_fast(box_t b) {
  return ((b.box & 0x03)==2);
}
static inline bool is_cptr_fast(box_t b) {
  return ((b.box & 0x03)==3);
}

#define MAX_BOXED_INT  ((intptr_t)INTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_INT  (- MAX_BOXED_INT - 1)

#define MAX_BOXED_ENUM ((uintptr_t)UINTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_ENUM (0)

#if INTPTR_SIZE==8

#define BOXED_INT_BITS      (46)

// checking does not have to be optimal as we do not generally use this.
static inline bool is_double(box_t b) {
  return ((uint16_t)((shr(b.box,48) + 1)) > 1);  // test of top 16 bits are not 0x0000 or 0xFFFF
}
static inline bool is_ptr_and_not_double(box_t b) {
  // faster test if a `ptr` is guaranteed to not have 0xFFFF as the top 16 bits. (which is usually the case, unless you are kernel programming)
  // this is used when doing `boxed_incref` for example.
  assert_internal(is_ptr_fast(b));
  return ((uint16_t)shr(b.box, 48) == 0);
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
  u._v = ((uint64_t)(v.box) - ((uint64_t)1 << 48));  // unsigned to avoid UB
  return u.d;
}

static inline box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  union { double _d; uint64_t v; } u;
  u._d = d;
  if (unlikely(u.v >= (U64(0xFFFE) << 48))) {
    // high quiet NaN, subtract to bring it in range
    u.v = u.v - (U64(0x0002) << 48);
    d = u._d;  // for the assert_internal
  }
  box_t v = { u.v + (U64(1) << 48) };
  assert_internal(is_double(v));
  assert_internal(unbox_double(v,ctx) == d); // (well, not for high qNaN)
  return v;
}

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  UNUSED(ctx);
  intx_t i = unbox_int(v);
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
  return (is_ptr(v) && block_tag(unbox_ptr(v)) == TAG_DOUBLE);
}

typedef struct boxed_double_s {
  block_t _block;
  double  value;
} *boxed_double_t;

static inline double unbox_double(box_t b, context_t* ctx) {
  assert_internal(is_double(b));
  boxed_double_t dt = block_as(boxed_double_t,unbox_ptr(b),TAG_DOUBLE);
  double d = dt->value;
  datatype_drop(dt,ctx);
  return d;
}

static inline box_t box_double(double d, context_t* ctx) {
  boxed_double_t dt = block_alloc_as(struct boxed_double_s, 0, TAG_DOUBLE, ctx);
  dt->value = d;
  return box_ptr(&dt->_block);
}

typedef struct boxed_int32_s {
  block_t  _block;
  int32_t  value;
} *boxed_int32_t;

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  if (likely(is_int(v))) {
    intx_t i = unbox_int(v);
    assert_internal(i >= INT32_MIN && i <= INT32_MAX);
    return (int32_t)i;
  }
  else {
    assert_internal(is_ptr(v) && block_tag(unbox_ptr(v)) == TAG_INT32);
    boxed_int32_t bi = block_as(boxed_int32_t, unbox_ptr(v), TAG_INT32);
    int32_t i = bi->value;
    block_drop(&bi->_block,ctx);
    return i;
  }
}

static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  if (i >= MIN_BOXED_INT && i <= MAX_BOXED_INT) {
    return box_int(i);
  }
  else {
    boxed_int32_t bi = block_alloc_as(struct boxed_int32_s, 0, TAG_INT32, ctx);
    bi->value = i;
    return box_ptr(&bi->_block);
  }
}

#else
# error "platform must be 32 or 64 bits."
#endif

static inline bool is_non_null_ptr(box_t v) {
  assert_internal(!is_ptr(v) || v.box != 0);   // NULL pointers are never allowed as boxed values
  return (is_ptr(v));  //  && unbox_ptr(v) != NULL
}

static inline ptr_t unbox_ptr(box_t v) {
  assert_internal(is_ptr(v));
  assert_internal(v.box != 0); // no NULL pointers allowed
  return (block_t*)(v.box);
}

static inline box_t box_ptr(const block_t* p) {
  assert_internal(((uintptr_t)p & 0x03) == 0); // check alignment
  assert_internal(p != NULL);                  // block should never be NULL
  box_t b = { (uintptr_t)(p) };
  return b;
}

static inline uintx_t unbox_enum(box_t b) {
  assert_internal(is_enum(b));
  return shr(b.box, 2);
}

static inline box_t box_enum(uintx_t u) {
  assert_internal(u <= MAX_BOXED_ENUM);
  box_t b = { ((uintptr_t)u << 2) | 0x02 };
  assert_internal(is_enum(b));
  return b;
}

static inline intx_t unbox_int(box_t v) {
  assert_internal(is_int(v));
  return (sar(v.box, 2));
}

static inline box_t box_int(intx_t i) {
  assert_internal(i >= MIN_BOXED_INT && i <= MAX_BOXED_INT);
  box_t v = { (uintptr_t)(i << 2) | 0x01 };
  assert_internal(is_int(v));
  return v;
}

static inline int16_t unbox_int16(box_t v) {
  intx_t i = unbox_int(v);
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
  block_t* b = unbox_ptr(v);
  assert_internal(block_tag(b) == expected_tag);
  return b;
}

static inline box_t box_block_t(block_t* b) {
  return box_ptr(b);
}

static inline box_t box_ptr_assert(block_t* b, tag_t tag) {
  UNUSED_RELEASE(tag);
  assert_internal(block_tag(b) == tag);
  return box_ptr(b);
}

#define unbox_datatype_as(tp,b,tag)      (block_as(tp,unbox_ptr(b),tag))
#define box_datatype_as(tp,b,tag)        (box_ptr_assert(&(b)->_block,tag))

#define unbox_constructor_as(tp,b,tag)   (unbox_datatype_as(tp,&(b)->_inherit,tag))
#define box_constructor_as(tp,b,tag)     (box_datatype_as(tp,&(b)->_inherit,tag))

/* Generic boxing of value types */

typedef struct boxed_value_s {
  block_t _block;
  char    data[INTPTR_SIZE]; 
} *boxed_value_t;

#define unbox_valuetype(tp,x,box,ctx) \
  do { \
    block_t* p = unbox_block_as(boxed_value_t,box,TAG_BOX); \
    x = *((tp*)(&p->data[0])); \
    block_drop(p,ctx); \
  }while(0);

#define box_valuetype(tp,x,val,scan_fsize,ctx)  \
  do{ \
     block_t* p = block_alloc(sizeof(block_t) + sizeof(tp), scan_fsize, TAG_BOX, ctx); \
     *((tp*)(&p->data[0])) = val;  \
     x = box_ptr(p); \
  }while(0);




static inline box_t box_cptr_raw(free_fun_t* freefun, void* p, context_t* ctx) {
  cptr_raw_t raw = block_alloc_as(struct cptr_raw_s, 0, TAG_CPTR_RAW, ctx);
  raw->free = freefun;
  raw->cptr = p;
  return box_ptr(&raw->_block);
}

static inline void* unbox_cptr_raw(box_t b) {
  cptr_raw_t raw = unbox_datatype_as( cptr_raw_t, b, TAG_CPTR_RAW);
  return raw->cptr;
}

static inline box_t box_cptr(void* p, context_t* ctx) {
  if (((uintptr_t)p & 0x03) == 0) {
    // aligned, box in-place
    return box_from_uintptr((uintptr_t)p | 0x03);
  }
  else {
    // allocate 
    return box_cptr_raw(&free_fun_null, p, ctx);
  }
}

static inline void* unbox_cptr(box_t b) {
  if (is_cptr_fast(b)) {
    return (void*)(box_as_uintptr(b) & ~UP(0x03));
  }
  else {
    return unbox_cptr_raw(b);
  }
}



#endif // include guard
