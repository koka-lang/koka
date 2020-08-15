





/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
struct __std_core_hnd__ev_s;
typedef vector_t evv_t;         // either a vector, or a single evidence

static inline bool evv_is_vector(evv_t evv) {
  return (datatype_is_singleton(evv) || datatype_has_tag(evv,TAG_VECTOR));
}

static inline evv_t dup_evv_t(evv_t evv) {
  return dup_datatype(evv);  
}

static inline void drop_evv_t(evv_t evv, context_t* ctx) {
  drop_datatype(evv,ctx);
}

static inline struct __std_core_hnd__ev_s* evv_at( int32_t i, context_t* ctx ) {
  // todo: make this faster by 1) use a value type for `ev`, and 2) inline the evv at the end of the context?
  const evv_t evv = ctx->evv;
  if (evv_is_vector(evv)) {
    return unbox_basetype_as(struct __std_core_hnd__ev_s*,vector_at(evv,(size_t)i));
  }
  else {    
    assert_internal(i==0);
    assert_internal(datatype_is_ptr(evv));
    dup_datatype(evv);
    return datatype_as(struct __std_core_hnd__ev_s*, evv); // single evidence    
  }
}

static inline evv_t evv_get(context_t* ctx) {
  return dup_evv_t(ctx->evv);
}

static inline unit_t evv_set(evv_t evv, context_t* ctx) {
  drop_evv_t(ctx->evv, ctx);
  ctx->evv = evv;
  return Unit;
}

static inline evv_t evv_swap(evv_t evv, context_t* ctx) {
  vector_t evv0 = ctx->evv;
  ctx->evv = evv;
  return evv0;
}

static inline evv_t evv_total(context_t* ctx) {
  return vector_empty();
}

static inline evv_t evv_swap_create0(context_t* ctx) {
  return evv_swap(evv_total(ctx),ctx);
}

static inline evv_t evv_swap_create1(int32_t i, context_t* ctx) {
  evv_t evv0 = ctx->evv;  
  if (evv_is_vector(evv0)) {
    ctx->evv = datatype_from_ptr(unbox_ptr(vector_at(evv0,(size_t)i))); // set single evidence
    return evv0;
  }
  else {      
    assert_internal(i==0);
    return dup_datatype(evv0);  // already a single evidence
  }
}


struct __std_core_hnd_Htag;
struct __std_core_hnd_Marker;
struct __std_core_hnd_yld_s;


struct __std_core_hnd__ev_s* ev_none(context_t* cxt);
struct __std_core_hnd__ev_s* evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx );
int32_t      evv_index( struct __std_core_hnd_Htag htag, context_t* ctx );
evv_t        evv_create(evv_t evv, vector_t indices, context_t* ctx);
evv_t        evv_insert(evv_t evv, struct __std_core_hnd__ev_s* ev, context_t* ctx);
evv_t        evv_delete(evv_t evv, int32_t index, bool behind, context_t* ctx);
string_t     evv_show(evv_t evv, context_t* ctx);
unit_t       evv_guard(evv_t evv, context_t* ctx);
evv_t        evv_swap_create( vector_t indices, context_t* ctx );
box_t        fatal_resume_final(context_t* ctx);
box_t        yield_cont( function_t next, context_t* ctx );
box_t        yield_extend( function_t next, context_t* ctx );
box_t        yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
function_t   yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx );

datatype_t   yield_capture(context_t* ctx);
box_t        yield_reyield(datatype_t yld, context_t* ctx);

static inline evv_t evv_swap_delete(int32_t i, bool behind, context_t* ctx) {
  evv_t evv0 = ctx->evv;  
  ctx->evv = evv_delete(dup_evv_t(evv0), i, behind, ctx);
  return evv0;
}
