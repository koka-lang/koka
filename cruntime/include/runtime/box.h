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

On 64-bit we like to box doubles without heap allocation, as well as allowing
for 52-bit (sign extended) pointers to allow for a large virtual addressing space
(like on ARMv8-A). 

For pointers and integers, the top 12-bits are the sign extension of the bottom 52 bits
and thus always 0x000 or 0xFFF (denoted as `sss`).
Using `x` for bytes, and `b` for bits, with `z` the least significant byte, we have:

    sssx xxxx xxxx xxxz   z = bbbb bb00  : 52-bit sign extended pointer (always aligned to 4 bytes!)
    sssx xxxx xxxx xxxz   z = bbbb bb01  : 50-bit sign extended integer
    sssx xxxx xxxx xxxz   z = bbbb bb10  : 50-bit unsigned enumeration
    sssx xxxx xxxx xxxz   z = bbbb bb11  : 50-bit special double (see below)

So, an integer `n` is encoded boxed as `boxed(n) == n*4 + 1`.

---------------------
On 64-bit we can now encode most doubles such that the top 12-bits are
between 0x001 and 0xFFE. The ranges of IEEE double values are:
  positive doubles        : 0000 0000 0000 0000 - 7FEF FFFF FFFF FFFF
  positive infinity       : 7FF0 0000 0000 0000
  positive NaN            : 7FF0 0000 0000 0001 - 7FFF FFFF FFFF FFFF
  negative doubles        : 8000 0000 0000 0000 - FFEF FFFF FFFF FFFF
  negative infinity       : FFF0 0000 0000 0000
  negative NaN            : FFF0 0000 0000 0001 - FFFF FFFF FFFF FFFF

Now, if a double is:
- positive: we add 0010 0000 0000 000, such that the range of positive doubles is boxed between
            0010 0000 0000 0000 and 7FFF FFFF FFFF FFFF
- negative: leave it as is, so the negative doubles are boxed between
            8000 0000 0000 0000 and FFEF FFFF FFFF FFFF
- special : either infinity or NaN. We save the sign, NaN signal bit, and bottom 48 bits as
            a 50-bit number and encode as a boxed special double ending in 0x03.
            (this means we may lose the top 3 bits (bits 49 to 51) of a potential NaN payload)


-------------------------
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

----------------
On 32-bit we use the same encoding of the bottom bits but no restriction
on the top 16 bits as we will box doubles as heap allocated values.

  xxxx xxxz   z = bbbb bb00  : 32-bit pointer (always aligned to 4 bytes)
  xxxx xxxz   z = bbbb bb01  : 30-bit integer
  xxxx xxxz   z = bbbb bb10  : 30-bit enumeration
  xxxx xxxz   z = bbbb bb11  : reserved (unused special double)

We still have fast addition of large integers but use 14-bit small
integers where we can do a 16-bit efficient overflow check.
----------------------------------------------------------------*/

// Forward declarations
static inline bool      is_ptr(box_t b);
static inline bool      is_ptr_fast(box_t b);   // if it is either a pointer, int, or enum, but not a double
static inline bool      is_enum_fast(box_t b);  // if it is either a pointer, int, or enum, but not a double
static inline block_t*  unbox_ptr(box_t b);
static inline box_t     box_ptr(const block_t* p);
static inline intx_t    unbox_int(box_t v);
static inline box_t     box_int(intx_t i);

// Use a boxed representation as an intptr
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

// Are two boxed representations equal?
static inline bool box_eq(box_t b1, box_t b2) {
  return (b1.box == b2.box);
}

// We cannot store NULL in boxed values; use `box_null` instead
#define box_null   (box_from_uintptr(7))    // = NaN with payload 1

// `box_any` is used to return when yielding (and should be accepted by any unbox operation)
#define box_any    (box_from_uintptr(11))   // = NaN with payload 2

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
static inline bool is_double_special_fast(box_t b) {
  return ((b.box & 0x03)==3);
}
static inline bool is_box_null(box_t b) {
  return (b.box == box_null.box);
}
static inline bool is_box_any(box_t b) {
  return (b.box == box_any.box);
}

#define MAX_BOXED_INT  ((intptr_t)INTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_INT  (- MAX_BOXED_INT - 1)

#define MAX_BOXED_ENUM ((uintptr_t)UINTPTR_MAX >> (INTPTR_BITS - BOXED_INT_BITS))
#define MIN_BOXED_ENUM (0)

// 64-bit
#if INTPTR_SIZE==8

#define BOXED_INT_BITS      (50)

// checking does not have to be optimal as we do not generally use this.
static inline bool is_double_normal(box_t b) {
  // test if top 16 bits are not 0xFFFx or 0x000x; adding 0x10 brings that in the range 0x0000 to 0x001F
  uint16_t top = (uint16_t)shr(b.box, 48);
  return ((uint16_t)(top + 0x10) > 0x1F);
}
static inline bool _ptr_is_not_double_normal(box_t b) {
  // faster test if a `ptr` is guaranteed to not have 0xFFF as the top 12 bits. 
  // (which is usually the case, unless you are kernel programming)
  // this is used when doing `boxed_incref` for example.
  assert_internal(is_ptr_fast(b));
  return ((uint16_t)shr(b.box, 52) == 0);
}
static inline bool is_ptr(box_t b) {
  return (is_ptr_fast(b) && likely(_ptr_is_not_double_normal(b)));
}
static inline bool is_int(box_t b) {
  return (is_int_fast(b) && likely(!is_double_normal(b)));
}
static inline bool is_enum(box_t b) {
  return (is_enum_fast(b) && likely(!is_double_normal(b)));
}
static inline bool is_double_special(box_t b) {
  return (is_double_special_fast(b) && likely(!is_double_normal(b)));
}
static inline bool is_double(box_t b) {
  return (is_double_normal(b) || is_double_special_fast(b));  // first test for double_normal!
}

static inline double unbox_double(box_t v, context_t* ctx) {
  UNUSED(ctx);
  assert_internal(is_double(v) || is_box_any(v));
  double d;
  uint64_t u = v.box;
  if (likely(is_double_normal(v))) {
    // regular double
    if ((int64_t)u >= 0) { u -= (U64(1) << 52); } // subtract 0x0010 0000 0000 0000 to positive doubles    
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    assert_internal(isfinite(d));
  }
  else {
    // NaN or infinity
    assert_internal(is_double_special(v) || is_box_any(v));
    uint64_t bot = ((u << 14) >> 16);  // clear top 14 bits and bottom 2 bits (= bottom 48 bits of the double)
    uint64_t signal = ((u >> 50) & 1) << 51;
    uint64_t top = ((int64_t)u >= 0 ? U64(0x7FF) : U64(0xFFF)) << 52;
    u = (top | signal | bot);
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing
    assert_internal(!isfinite(d));
  }  
  return d;
}

static inline box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  uint64_t u;
  box_t v;
  memcpy(&u, &d, sizeof(u));  // safe for C aliasing
  if (likely(isfinite(d))) {
    // regular double
    if (!signbit(d)) { u += (U64(1) << 52); }  // add 0x0010 0000 0000 0000 to positive doubles (use signbit to encode -0.0 properly)
    v.box = u;
    assert_internal(is_double_normal(v));
    assert_internal(unbox_double(v, ctx) == d);
  }
  else {
    // NaN or infinity
    uint16_t utop = (u >> 48);
    int16_t   top = (((int16_t)utop >> 12) & 0xFFF8) | ((utop>>1)&0x0004);  // maintain the sign and save NaN signal bit
    uint64_t bot  = ((u << 16) >> 16);                  // clear top 16 bits
    if ((utop & 0x0007) != 0 && bot == 0) { bot = 1; }  // ensure we maintain a non-zero NaN payload if just the top 3 bits were set (or we may unbox a NaN as infinity!)
    u = ((uint64_t)top << 48) | (bot << 2) | 0x03;      // and merge and set as special double
    v.box = u;
    assert_internal(!is_double_normal(v) && is_double_special_fast(v));
#if (DEBUG>=3)
    double dx = unbox_double(v, ctx);
    uint64_t ux;
    memcpy(&ux, &dx, sizeof(double));
    assert_internal(u == ux);  // (may fail due to top 3 bits of a NaN payload)
#endif
  }
  assert_internal(is_double(v));
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

// 32 bit
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
static inline bool is_double_special(box_t b) {
  bool is_special = is_double_special_fast(b);
  assert_internal(!is_special);
  return is_special;
}
static inline bool is_double_normal(box_t v) {
  return (is_ptr(v) && block_tag(unbox_ptr(v)) == TAG_DOUBLE);
}
static inline bool is_double(box_t v) {
  return is_double_normal(v);
}

typedef struct boxed_double_s {
  block_t _block;
  double  value;
} *boxed_double_t;

static inline double unbox_double(box_t b, context_t* ctx) {
  assert_internal(is_double(b));
  boxed_double_t dt = block_as_assert(boxed_double_t,unbox_ptr(b),TAG_DOUBLE);
  double d = dt->value;
  drop_datatype(dt,ctx);
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
    boxed_int32_t bi = block_as_assert(boxed_int32_t, unbox_ptr(v), TAG_INT32);
    int32_t i = bi->value;
    drop_block(&bi->_block,ctx);
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
  assert_internal(is_ptr(v) || is_box_any(v));
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
  assert_internal(is_enum(b) || is_box_any(b));
  return shr(b.box, 2);
}

static inline box_t box_enum(uintx_t u) {
  assert_internal(u <= MAX_BOXED_ENUM);
  box_t b = { ((uintptr_t)u << 2) | 0x02 };
  assert_internal(is_enum(b));
  return b;
}

static inline intx_t unbox_int(box_t v) {
  assert_internal(is_int(v) || is_box_any(v));
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
  return box_enum(b ? UX(1) : UX(0));
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

#define unbox_datatype_as_assert(tp,b,tag)  (block_as_assert(tp,unbox_ptr(b),tag))
#define unbox_datatype_as(tp,b)             ((tp)unbox_ptr(b))
#define box_datatype(b)                     (box_ptr(&(b)->_block))

#define unbox_constructor_as(tp,b,tag)   (unbox_datatype_as_assert(tp,b,tag))
#define box_constructor(b)               (box_datatype(&(b)->_type))

/* Generic boxing of value types */

typedef struct boxed_value_s {
  block_t _block;
  char    data[INTPTR_SIZE]; 
} *boxed_value_t;

#define unbox_valuetype(tp,x,box,ctx) \
  do { \
    boxed_value_t p = unbox_datatype_as_assert(boxed_value_t,box,TAG_BOX); \
    x = *((tp*)(&p->data[0])); \
    drop_datatype(p,ctx); \
  }while(0);

#define box_valuetype(tp,x,val,scan_fsize,ctx)  \
  do{ \
     boxed_value_t p = block_as_assert(boxed_value_t, block_alloc(sizeof(block_t) + sizeof(tp), scan_fsize, TAG_BOX, ctx), TAG_BOX); \
     *((tp*)(&p->data[0])) = val;  \
     x = box_datatype(p); \
  }while(0);


// C pointers

static inline box_t box_cptr_raw(free_fun_t* freefun, void* p, context_t* ctx) {
  cptr_raw_t raw = block_alloc_as(struct cptr_raw_s, 0, TAG_CPTR_RAW, ctx);
  raw->free = freefun;
  raw->cptr = p;
  return box_ptr(&raw->_block);
}

static inline void* unbox_cptr_raw(box_t b) {
  cptr_raw_t raw = unbox_datatype_as_assert( cptr_raw_t, b, TAG_CPTR_RAW );
  return raw->cptr;
}

static inline box_t box_cptr(void* p, context_t* ctx) {
  intx_t i = (intptr_t)p;
  if (i >= MIN_BOXED_INT && i <= MAX_BOXED_INT) {
    // box as int
    return box_int(i);
  }
  else {
    // allocate 
    return box_cptr_raw(&free_fun_null, p, ctx);
  }
}

static inline void* unbox_cptr(box_t b) {
  if (is_int_fast(b)) {
    assert_internal(is_int(b));
    return (void*)(unbox_int(b));
  }
  else {
    return unbox_cptr_raw(b);
  }
}

typedef void (*fun_ptr_t)(void);
#define box_fun_ptr(f,ctx)  _box_fun_ptr((fun_ptr_t)f, ctx)

static inline box_t _box_fun_ptr(fun_ptr_t f, context_t* ctx) {
  return box_cptr((void*)f, ctx);
}

static inline void* unbox_fun_ptr(box_t b) {
  return unbox_cptr(b);
}

static inline box_t dup_box_t(box_t b) {
  if (is_ptr(b)) dup_block(unbox_ptr(b));
  return b;
}

static inline void drop_box_t(box_t b, context_t* ctx) {
  if (is_ptr(b)) drop_block(unbox_ptr(b), ctx);
}




#endif // include guard
