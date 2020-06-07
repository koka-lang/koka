/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
struct __std_core_hnd__ev_s;

static inline struct __std_core_hnd__ev_s* evv_at( int32_t i, context_t* ctx ) {
  // todo: make this faster by 1) use a value type for `ev`, and 2) inline the evv at the end of the context?
  const vector_t evv = ctx->evv;
  if (datatype_tag(evv) >= TAG_VECTOR_SMALL) {
    return unbox_datatype_as(struct __std_core_hnd__ev_s*,vector_at(evv,i));
  }
  else {    
    assert_internal(i==0);
    return dup_datatype_as(struct __std_core_hnd__ev_s*, evv); // single evidence
  }
}

static inline vector_t evv_get(context_t* ctx) {
  return dup_datatype_as(vector_t, ctx->evv);
}

static inline unit_t evv_set(vector_t evv, context_t* ctx) {
  drop_datatype(ctx->evv, ctx);
  ctx->evv = evv;
  return Unit;
}

static inline vector_t evv_swap(vector_t evv, context_t* ctx) {
  vector_t evv0 = ctx->evv;
  ctx->evv = evv;
  return evv0;
}

static inline vector_t evv_total(context_t* ctx) {
  return dup_vector(vector_empty);
}

static inline vector_t evv_swap_create0(context_t* ctx) {
  return evv_swap(evv_total(ctx),ctx);
}

static inline vector_t evv_swap_create1(int32_t i, context_t* ctx) {
  vector_t evv0 = ctx->evv;  
  if (datatype_tag(evv0)==TAG_VECTOR) {
    ctx->evv = (vector_t)unbox_ptr(vector_at(evv0,i)); // set single evidence
    return evv0;
  }
  else {      
    assert_internal(i==0);
    return dup_datatype_as(vector_t,evv0);  // already a single evidence
  }
}

struct __std_core_hnd_Htag;
struct __std_core_hnd_Marker;

int32_t      evv_index( struct __std_core_hnd_Htag htag, context_t* ctx );
struct __std_core_hnd__ev_s* evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx );
vector_t     evv_create(vector_t evv, vector_t indices, context_t* ctx);
vector_t     evv_insert(vector_t evv, struct __std_core_hnd__ev_s* ev, context_t* ctx);
vector_t     evv_delete(vector_t evv, int32_t index, bool behind, context_t* ctx);
string_t     evv_show(vector_t evv, context_t* ctx);
unit_t       evv_guard(vector_t evv, context_t* ctx);
vector_t     evv_swap_create( vector_t indices, context_t* ctx );
box_t        fatal_resume_final(context_t* ctx);
box_t        yield_cont( function_t next, context_t* ctx );
box_t        yield_extend( function_t next, context_t* ctx );
box_t        yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
box_t        yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx );
