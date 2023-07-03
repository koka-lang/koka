





/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
typedef kk_datatype_ptr_t kk_std_core_hnd__ev_t;
static inline kk_std_core_hnd__ev_t kk_std_core_hnd__ev_dup(kk_std_core_hnd__ev_t _x, kk_context_t* ctx);

typedef struct kk_evv_vector_s {  
  struct kk_block_s     _block;
  kk_integer_t          cfc;       // control flow context (0-3) as a small int
  kk_std_core_hnd__ev_t vec[1];
} *kk_evv_vector_t;


typedef kk_datatype_ptr_t kk_evv_t;   // either a kk_evv_vector_t, or a single evidence

static inline kk_evv_t kk_evv_dup(kk_evv_t evv, kk_context_t* ctx) {
  return kk_datatype_ptr_dup(evv,ctx);  
}

static inline void kk_evv_drop(kk_evv_t evv, kk_context_t* ctx) {
  kk_datatype_ptr_drop(evv,ctx);
}

static inline kk_evv_t kk_evv_empty(kk_context_t* ctx) {
  return kk_evv_empty_singleton(ctx);
}

static inline bool kk_evv_is_empty(kk_evv_t evv, kk_context_t* ctx) {  // todo: optimize
  kk_evv_t empty = kk_evv_empty(ctx);    
  bool eq = kk_datatype_eq(evv,empty);
  kk_datatype_ptr_drop(empty,ctx);
  return eq;
}

static inline bool kk_evv_is_vector(kk_evv_t evv, kk_context_t* ctx) {
  return kk_datatype_ptr_has_tag(evv,KK_TAG_EVV_VECTOR,ctx);
}

static inline kk_std_core_hnd__ev_t kk_evv_as_ev( kk_evv_t evv, kk_context_t* ctx ) {
  kk_unused_internal(ctx);
  kk_assert_internal(!kk_evv_is_vector(evv,ctx));
  return evv;
}

static inline kk_evv_t kk_ev_as_evv( kk_std_core_hnd__ev_t ev, kk_context_t* ctx ) {
  kk_unused(ctx);
  return ev;
}

static inline kk_evv_vector_t kk_evv_as_vector( kk_evv_t evv, kk_context_t* ctx ) {
  kk_assert_internal(kk_evv_is_vector(evv,ctx));
  return kk_datatype_as_assert(kk_evv_vector_t,evv,KK_TAG_EVV_VECTOR,ctx);
}

static inline kk_std_core_hnd__ev_t kk_evv_at( kk_ssize_t i, kk_context_t* ctx ) {
  kk_evv_t evv = ctx->evv;
  if (!kk_evv_is_vector(evv,ctx)) {  // evv is a single evidence
    kk_assert_internal(i==0);
    return kk_evv_as_ev(kk_evv_dup(evv,ctx),ctx);
  }
  else {  // evv as a vector
    kk_assert_internal(i >= 0 && i < (kk_block_scan_fsize(kk_datatype_as_ptr(evv,ctx)) - 1));
    kk_evv_vector_t vec = kk_evv_as_vector(evv,ctx);
    return kk_std_core_hnd__ev_dup(vec->vec[i],ctx); 
  }
}

static inline kk_evv_t kk_evv_get(kk_context_t* ctx) {
  return kk_evv_dup(ctx->evv,ctx);
}

static inline kk_unit_t kk_evv_set(kk_evv_t evv, kk_context_t* ctx) {
  kk_evv_drop(ctx->evv, ctx);
  ctx->evv = evv;
  return kk_Unit;
}

static inline kk_evv_t kk_evv_swap(kk_evv_t evv, kk_context_t* ctx) {
  kk_evv_t evv0 = ctx->evv;
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
  return kk_evv_empty(ctx);
}

static inline kk_evv_t kk_evv_swap_create0(kk_context_t* ctx) {
  return kk_evv_swap(kk_evv_total(ctx),ctx);
}

static inline kk_evv_t kk_evv_swap_create1(kk_ssize_t i, kk_context_t* ctx) {
  kk_evv_t evv0 = ctx->evv;  
  if (kk_evv_is_vector(evv0,ctx)) {
    ctx->evv = kk_evv_at(i, ctx); // cast as ev struct is not defined yet 
    return evv0;
  }
  else {      
    kk_assert_internal(i==0);
    return kk_evv_dup(evv0,ctx);  // already a single evidence
  }
}

struct kk_std_core_hnd_Htag;
struct kk_std_core_hnd_Marker;
struct kk_std_core_hnd_yld_s;


kk_std_core_hnd__ev_t kk_ev_none(kk_context_t* cxt);
kk_std_core_hnd__ev_t kk_evv_lookup( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx );
int32_t         kk_evv_cfc(kk_context_t* ctx);
kk_ssize_t      kk_evv_index( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx );
kk_evv_t        kk_evv_create(kk_evv_t evv, kk_vector_t indices, kk_context_t* ctx);
kk_evv_t        kk_evv_insert(kk_evv_t evv, kk_std_core_hnd__ev_t ev, kk_context_t* ctx);
kk_evv_t        kk_evv_delete(kk_evv_t evv, kk_ssize_t index, bool behind, kk_context_t* ctx);
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

static inline kk_evv_t kk_evv_swap_delete(kk_ssize_t i, bool behind, kk_context_t* ctx) {
  kk_evv_t evv0 = ctx->evv;  
  ctx->evv = kk_evv_delete(kk_evv_dup(evv0,ctx), i, behind, ctx);
  return evv0;
}
