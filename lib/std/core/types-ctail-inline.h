





/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_box_t kk_ctail_hole(void) {
  return kk_intf_box(0);
}

static inline kk_std_core_types__ctail kk_ctail_unit(kk_context_t* ctx) {
  return kk_std_core_types__new_CTail( kk_ctail_hole(), NULL, ctx);
}


static inline kk_box_t kk_ctail_apply_linear( kk_std_core_types__ctail acc, kk_box_t child ) {
  #if 1
  if (kk_likely(acc.hole != NULL)) {
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,kk_get_context())));
    *(acc.hole) = child;
    return acc.res;
  }
  else {
    return child;
  }
  #else
  // this form entices conditional moves (but seems slower in general)
  if (acc.hole != NULL) { *acc.hole = child; }
  return (acc.hole != NULL ? acc.res : child);
  #endif
}

static inline kk_box_t kk_ctail_apply_nonlinear( kk_std_core_types__ctail acc, kk_box_t child, kk_context_t* ctx ) {
  // note: written like this for best codegen; be careful when rewriting.
  if (acc.hole != NULL && kk_block_is_unique(kk_ptr_unbox(acc.res,ctx))) { // no kk_likely seem slightly better
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    *(acc.hole) = child;   // in-place update the hole with the child
    return acc.res;      
  }
  else if (kk_likely(acc.hole == NULL)) {
    return child;
  }
  else {
    kk_assert_internal(!kk_block_is_unique(kk_ptr_unbox(acc.res,ctx)));
    return kk_ctail_context_copy_compose(acc.res,child,ctx);  // copy the context path to the hole and compose with the child
  }
}

// apply a context to a child value
// is_linear is always a constant and set to `true` if the effect is guaranteed linear
static inline kk_box_t kk_ctail_apply( kk_std_core_types__ctail acc, kk_box_t child, bool is_linear, kk_context_t* ctx ) {
  #if defined(KK_CTAIL_NO_CONTEXT_PATH)
  return kk_ctail_apply_linear(acc,child);  // compiler generates the right code for the non-linear case
  #else
  if (is_linear) return kk_ctail_apply_linear(acc,child);
            else return kk_ctail_apply_nonlinear(acc,child,ctx);
  #endif
}

// compose a context to a new one
static inline kk_std_core_types__ctail kk_ctail_compose( kk_std_core_types__ctail acc, kk_box_t child, kk_box_t* field, bool is_linear, kk_context_t* ctx  ) {
  return kk_std_core_types__new_CTail( kk_ctail_apply(acc,child,is_linear,ctx), field, ctx );  
}


