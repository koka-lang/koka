





/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_box_t kk_ctail_hole(void) {
  return kk_intf_box(0);
}

static inline kk_std_core_types__ctail kk_ctail_nil(kk_context_t* ctx) {
  return kk_std_core_types__new_CTail( kk_ctail_hole(), NULL, ctx);
}

// apply a context to a child value
static inline kk_box_t kk_ctail_resolve( kk_std_core_types__ctail acc, kk_box_t child, bool is_linear, kk_context_t* ctx ) {
  #if !defined(KK_CTAIL_NO_CONTEXT_PATH)
  // note: written like this for best codegen; be careful when rewriting.
  if (kk_likely(acc.hole != NULL && (is_linear || kk_block_is_unique(kk_ptr_unbox(acc.res))))) {
    *(acc.hole) = child;   // in-place update the hole with the child
    return acc.res;      
  }
  else if (acc.hole == NULL) {
    return child;
  }
  else {
    return kk_ctail_context_compose(acc.res,child,ctx);  // copy the context path to the hole and compose with the child
  }
  #else
  if (kk_likely(acc.hole != NULL)) {
    kk_assert_internal(kk_block_is_unique(kk_ptr_unbox(acc.res)));
    *(acc.hole) = child;
    return acc.res;
  }
  else {
    return child;
  }
  #endif
}

// compose a context to a new one
static inline kk_std_core_types__ctail kk_ctail_link( kk_std_core_types__ctail acc, kk_box_t child, kk_box_t* field, bool is_linear, kk_context_t* ctx  ) {
  return kk_std_core_types__new_CTail( kk_ctail_resolve(acc,child,is_linear,ctx), field, ctx );
}


