





/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
static inline kk_field_addr_t kk_field_addr_create(void* p, kk_context_t* ctx) {
  return kk_internal_ptr_box((kk_box_t*)p, ctx);
}

static inline kk_box_t* kk_field_addr_ptr(kk_field_addr_t faddr, kk_context_t* ctx) {
  return kk_internal_ptr_unbox(faddr, ctx);
}

static inline kk_field_addr_t kk_field_addr_null(void) {
  return kk_box_null();
}

static inline kk_box_t kk_cctx_hole(void) {
  return kk_box_null();  // for now, this must be a value; see `kklib/src/refcount.c:kk_cctx_copy_apply`
}

static inline kk_std_core_types__cctx kk_cctx_empty(kk_context_t* ctx) {
  return kk_std_core_types__new_Cctx( kk_cctx_hole(), kk_field_addr_null(), ctx);
}

static inline kk_std_core_types__cctx kk_cctx_create( kk_box_t res, kk_field_addr_t field, kk_context_t* ctx) {
  return kk_std_core_types__new_Cctx( res, field, ctx);
}

static inline bool kk_cctx_is_empty( kk_std_core_types__cctx cctx ) {
  return kk_box_is_value(cctx.res);
}

static inline bool kk_cctx_is_valid( kk_std_core_types__cctx cctx ) {
  return kk_box_is_ptr(cctx.res);
}

static inline kk_box_t kk_cctx_apply_linear( kk_std_core_types__cctx acc, kk_box_t child, kk_context_t* ctx ) {
  #if 1
  if (kk_likely(kk_cctx_is_valid(acc))) {
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,kk_get_context())));
    *kk_field_addr_ptr(acc.holeptr,ctx) = child;
    return acc.res;
  }
  else {
    return child;
  }
  #else
  // this form entices conditional moves (but seems slower in general)
  if (kk_cctx_is_valid(acc)) { *kk_field_addr_ptr(acc.holeptr,ctx) = child; }
  return (kk_cctx_is_valid(acc) ? acc.res : child);
  #endif
}

static inline kk_box_t kk_cctx_apply_nonlinear( kk_std_core_types__cctx acc, kk_box_t child, kk_context_t* ctx ) {
  // note: written like this for best codegen; be careful when rewriting.
  if (kk_cctx_is_valid(acc) && kk_block_is_unique(kk_ptr_unbox(acc.res,ctx))) { // no kk_likely seem slightly better
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    *kk_field_addr_ptr(acc.holeptr,ctx) = child;   // in-place update the hole with the child
    return acc.res;
  }
  else if (kk_likely(kk_cctx_is_empty(acc))) {
    return child;
  }
  else {
    kk_assert_internal(!kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    return kk_cctx_copy_apply(acc.res,kk_field_addr_ptr(acc.holeptr,ctx),child,ctx);  // copy the context path to the hole and compose with the child
  }
}

// apply a context to a child value
// is_linear is always a constant and set to `true` if the effect is guaranteed linear
static inline kk_box_t kk_cctx_apply( kk_std_core_types__cctx acc, kk_box_t child, bool is_linear, kk_context_t* ctx ) {
  #if defined(KK_CCTX_NO_CONTEXT_PATH)
  return kk_cctx_apply_linear(acc,child);  // compiler generates the right code for the non-linear case
  #else
  if (is_linear) return kk_cctx_apply_linear(acc,child,ctx);
            else return kk_cctx_apply_nonlinear(acc,child,ctx);
  #endif
}

// extend a context with a non-empty context
static inline kk_std_core_types__cctx kk_cctx_extend( kk_std_core_types__cctx acc, kk_box_t child, kk_field_addr_t field, bool is_linear, kk_context_t* ctx  ) {
  return kk_std_core_types__new_Cctx( kk_cctx_apply(acc,child,is_linear,ctx), field, ctx );
}

// compose a context
static inline kk_std_core_types__cctx kk_cctx_compose( kk_std_core_types__cctx acc1, kk_std_core_types__cctx acc2, bool is_linear, kk_context_t* ctx  ) {
  if (kk_cctx_is_empty(acc2)) return acc1;
  if kk_likely(kk_block_is_unique(kk_ptr_unbox(acc2.res,ctx))) {
    return kk_cctx_extend(acc1,acc2.res,acc2.holeptr,is_linear,ctx);
  }
  else {
    kk_box_t* holeptr = NULL;
    kk_box_t res = kk_cctx_copy(acc2.res,kk_field_addr_ptr(acc2.holeptr,ctx),&holeptr,ctx);
    return kk_cctx_extend(acc1,res,kk_field_addr_create(holeptr,ctx),is_linear,ctx);
  }
}

