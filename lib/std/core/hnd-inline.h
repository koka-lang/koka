





/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
struct kk_std_core_hnd__ev_s;
typedef kk_vector_t kk_evv_t;         // either a vector, or a single evidence

static inline bool kk_evv_is_vector(kk_evv_t evv) {
  return (kk_datatype_is_singleton(evv) || kk_datatype_has_tag(evv,KK_TAG_VECTOR));
}

static inline kk_evv_t kk_evv_dup(kk_evv_t evv) {
  return kk_datatype_dup(evv);  
}

static inline void kk_evv_drop(kk_evv_t evv, kk_context_t* ctx) {
  kk_datatype_drop(evv,ctx);
}

static inline struct kk_std_core_hnd__ev_s* kk_evv_at( size_t i, kk_context_t* ctx ) {
  // todo: make this faster by 1) use a value type for `ev`, and 2) inline the evv at the end of the context?
  const kk_evv_t evv = ctx->evv;
  if (kk_evv_is_vector(evv)) {
    return kk_basetype_unbox_as(struct kk_std_core_hnd__ev_s*,kk_vector_at(evv,i));
  }
  else {    
    kk_assert_internal(i==0);
    kk_assert_internal(kk_datatype_is_ptr(evv));
    kk_evv_dup(evv);
    return kk_datatype_as(struct kk_std_core_hnd__ev_s*, evv); // single evidence    
  }
}

static inline kk_evv_t kk_evv_get(kk_context_t* ctx) {
  return kk_evv_dup(ctx->evv);
}

static inline kk_unit_t kk_evv_set(kk_evv_t evv, kk_context_t* ctx) {
  kk_evv_drop(ctx->evv, ctx);
  ctx->evv = evv;
  return kk_Unit;
}

static inline kk_evv_t kk_evv_swap(kk_evv_t evv, kk_context_t* ctx) {
  kk_vector_t evv0 = ctx->evv;
  ctx->evv = evv;
  return evv0;
}

static inline bool kk_evv_eq(kk_evv_t evv1, kk_evv_t evv2, kk_context_t* ctx) {  // TODO:make borrowing
  bool eq = kk_datatype_eq(evv1,evv2);  
  kk_evv_drop(evv1,ctx);
  kk_evv_drop(evv2,ctx);
  return eq;
}

static inline kk_evv_t kk_evv_total(kk_context_t* ctx) {
  return kk_vector_empty();
}

static inline kk_evv_t kk_evv_swap_create0(kk_context_t* ctx) {
  return kk_evv_swap(kk_evv_total(ctx),ctx);
}

static inline kk_evv_t kk_evv_swap_create1(size_t i, kk_context_t* ctx) {
  kk_evv_t evv0 = ctx->evv;  
  if (kk_evv_is_vector(evv0)) {
    ctx->evv = kk_datatype_from_ptr(kk_ptr_unbox(kk_vector_at(evv0,i))); // set single evidence
    return evv0;
  }
  else {      
    kk_assert_internal(i==0);
    return kk_evv_dup(evv0);  // already a single evidence
  }
}


struct kk_std_core_hnd_Htag;
struct kk_std_core_hnd_Marker;
struct kk_std_core_hnd_yld_s;


struct kk_std_core_hnd__ev_s* kk_ev_none(kk_context_t* cxt);
struct kk_std_core_hnd__ev_s* kk_evv_lookup( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx );
size_t          kk_evv_index( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx );
kk_evv_t        kk_evv_create(kk_evv_t evv, kk_vector_t indices, kk_context_t* ctx);
kk_evv_t        kk_evv_insert(kk_evv_t evv, struct kk_std_core_hnd__ev_s* ev, kk_context_t* ctx);
kk_evv_t        kk_evv_delete(kk_evv_t evv, size_t index, bool behind, kk_context_t* ctx);
kk_string_t     kk_evv_show(kk_evv_t evv, kk_context_t* ctx);
kk_unit_t       kk_evv_guard(kk_evv_t evv, kk_context_t* ctx);
kk_evv_t        kk_evv_swap_create( kk_vector_t indices, kk_context_t* ctx );
kk_box_t        kk_fatal_resume_final(kk_context_t* ctx);
kk_box_t        kk_yield_cont( kk_function_t next, kk_context_t* ctx );
kk_box_t        kk_yield_extend( kk_function_t next, kk_context_t* ctx );
kk_box_t        kk_yield_final( struct kk_std_core_hnd_Marker m, kk_function_t clause, kk_context_t* ctx );
kk_function_t   kk_yield_to( struct kk_std_core_hnd_Marker m, kk_function_t clause, kk_context_t* ctx );
struct kk_std_core_hnd_yld_s  kk_yield_prompt( struct kk_std_core_hnd_Marker m, kk_context_t* ctx );

kk_datatype_t   kk_yield_capture(kk_context_t* ctx);
kk_box_t        kk_yield_reyield(kk_datatype_t yld, kk_context_t* ctx);

static inline kk_evv_t kk_evv_swap_delete(size_t i, bool behind, kk_context_t* ctx) {
  kk_evv_t evv0 = ctx->evv;  
  ctx->evv = kk_evv_delete(kk_evv_dup(evv0), i, behind, ctx);
  return evv0;
}
