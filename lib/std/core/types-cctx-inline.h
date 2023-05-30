





/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_box_t kk_cctx_hole(void) {
  return kk_intf_box(0);  // for now, this must be a value; see `kklib/src/refcount.c:kk_cctx_copy_apply`
}

static inline kk_std_core_types__cctx kk_cctx_empty(kk_context_t* ctx) {
  return kk_std_core_types__new_Cctx( kk_cctx_hole(), NULL, ctx);
}

static inline kk_std_core_types__cctx kk_cctx_create( kk_box_t res, kk_box_t* field, kk_context_t* ctx) {
  return kk_std_core_types__new_Cctx( res, field, ctx);
}


static inline kk_box_t kk_cctx_apply_linear( kk_std_core_types__cctx acc, kk_box_t child ) {
  #if 1
  if (kk_likely(acc.holeptr != NULL)) {
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,kk_get_context())));
    *(acc.holeptr) = child;
    return acc.res;
  }
  else {
    return child;
  }
  #else
  // this form entices conditional moves (but seems slower in general)
  if (acc.holeptr != NULL) { *acc.holeptr = child; }
  return (acc.holeptr != NULL ? acc.res : child);
  #endif
}

static inline kk_box_t kk_cctx_apply_nonlinear( kk_std_core_types__cctx acc, kk_box_t child, kk_context_t* ctx ) {
  // note: written like this for best codegen; be careful when rewriting.
  if (acc.holeptr != NULL && kk_block_is_unique(kk_ptr_unbox(acc.res,ctx))) { // no kk_likely seem slightly better
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    *(acc.holeptr) = child;   // in-place update the hole with the child
    return acc.res;      
  }
  else if (kk_likely(acc.holeptr == NULL)) {
    return child;
  }
  else {
    kk_assert_internal(!kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    return kk_cctx_copy_apply(acc.res,acc.holeptr,child,ctx);  // copy the context path to the hole and compose with the child
  }
}

// apply a context to a child value
// is_linear is always a constant and set to `true` if the effect is guaranteed linear
static inline kk_box_t kk_cctx_apply( kk_std_core_types__cctx acc, kk_box_t child, bool is_linear, kk_context_t* ctx ) {
  #if defined(KK_CCTX_NO_CONTEXT_PATH)
  return kk_cctx_apply_linear(acc,child);  // compiler generates the right code for the non-linear case
  #else
  if (is_linear) return kk_cctx_apply_linear(acc,child);
            else return kk_cctx_apply_nonlinear(acc,child,ctx);
  #endif
}

// extend a context with a non-empty context
static inline kk_std_core_types__cctx kk_cctx_extend( kk_std_core_types__cctx acc, kk_box_t child, kk_box_t* field, bool is_linear, kk_context_t* ctx  ) {
  return kk_std_core_types__new_Cctx( kk_cctx_apply(acc,child,is_linear,ctx), field, ctx );  
}

// compose a context
static inline kk_std_core_types__cctx kk_cctx_compose( kk_std_core_types__cctx acc1, kk_std_core_types__cctx acc2, bool is_linear, kk_context_t* ctx  ) {
  if (acc2.holeptr == NULL) return acc1;
  return kk_cctx_extend(acc1,acc2.res,acc2.holeptr,is_linear,ctx);
}

