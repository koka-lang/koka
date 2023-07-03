#pragma once
#ifndef KK_MAYBE_H
#define KK_MAYBE_H
/*---------------------------------------------------------------------------
  Copyright 2020-2022, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Optimized support for maybe<a> like datatypes.
  We try to avoid allocating for maybe-like types. First we define maybe<a> as a value
  type (in std/core/types) and thus a Just is usually passed in 2 registers for the tag
  and payload. This does not help though if it becomes boxed, say, a list of maybe
  values. In that case we can still avoid allocation through the special TAG_NOTHING
  and TAG_JUST tags. If the Just value is neither of those, we just use it directly
  without allocation. This way, only nested maybe types (`Just(Just(x))` or `Just(Nothing)`)
  are allocated, and sometimes value types like `int32` if these happen to be equal
  to `kk_box_Nothing`.
--------------------------------------------------------------------------------------*/

static inline kk_box_t kk_box_Nothing(void) {
  return kk_datatype_box(kk_datatype_from_tag(KK_TAG_NOTHING));
}

static inline bool kk_box_is_Nothing(kk_box_t b) {
  return (b.box == kk_datatype_from_tag(KK_TAG_NOTHING).dbox);
}

static inline bool kk_box_is_Just(kk_box_t b, kk_context_t* ctx) {
  return (kk_box_is_ptr(b) && kk_block_has_tag(kk_ptr_unbox(b, ctx), KK_TAG_JUST));
}

static inline bool kk_box_is_maybe(kk_box_t b, kk_context_t* ctx) {
  return (kk_box_is_Just(b, ctx) || kk_box_is_Nothing(b));
}

typedef struct kk_just_s {
  struct kk_block_s _block;
  kk_box_t          value;
} kk_just_t;

kk_decl_export kk_box_t kk_unbox_Just_block(kk_block_t* b, kk_borrow_t borrow, kk_context_t* ctx);

static inline kk_box_t kk_unbox_Just(kk_box_t b, kk_borrow_t borrow, kk_context_t* ctx) {
  if (kk_box_is_ptr(b)) {
    kk_block_t* bl = kk_ptr_unbox(b, ctx);
    if kk_unlikely(kk_block_has_tag(bl, KK_TAG_JUST)) {
      return kk_unbox_Just_block(bl, borrow, ctx);
    }
  }
  // if borrowing we should not change refcounts, 
  // and if not borrowing, we consume the b
  return b;
}

static inline kk_box_t kk_box_Just(kk_box_t b, kk_context_t* ctx) {
  if kk_likely(!kk_box_is_maybe(b, ctx)) {
    return b;
  }
  else {
    kk_just_t* just = kk_block_alloc_as(kk_just_t, 1, KK_TAG_JUST, ctx);
    just->value = b;
    return kk_ptr_box(&just->_block, ctx);
  }
}

static inline kk_datatype_t kk_datatype_as_Just(kk_box_t b) {
  kk_assert_internal(!kk_box_is_maybe(b, kk_get_context()));
  return kk_datatype_unbox(b);
}

static inline kk_box_t kk_datatype_unJust(kk_datatype_t d, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_assert_internal(!kk_datatype_has_singleton_tag(d, KK_TAG_NOTHING));
  if (kk_datatype_is_ptr(d)) {
    kk_block_t* b = kk_datatype_as_ptr(d, ctx);
    if (kk_block_has_tag(b, KK_TAG_JUST)) {
      return kk_block_field(b, 0);
    }
  }
  return kk_datatype_box(d);
}

#endif // KK_MAYBE_H
