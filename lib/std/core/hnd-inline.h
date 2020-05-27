/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static inline datatype_t evv_get(context_t* ctx) {
  return datatype_dup(ctx->evv);
}

static inline datatype_t evv_at( int32_t i, context_t* ctx ) {
  // todo: make this faster by 1) use a value type for `ev`, and 2) inline the evv at the end of the context?
  const vector_t evv = ctx->evv;
  if (i==0 && (datatype_tag(evv) != TAG_VECTOR)) {
    return datatype_dup(evv);
  }
  else {
    return datatype_dup(vector_at(evv,i));
  }
}

static inline datatype_t evv_get(context_t* ctx) {
  return datatype_dup(ctx->evv);
}

static inline unit_t evv_set(datatype_t evv, context_t* ctx) {
  datatype_drop(ctx->evv, ctx);
  ctx->evv = evv;
  return Unit;
}

static inline datatype_t evv_swap(datatype_t evv, context_t* ctx) {
  datatype_t evv0 = ctx->evv;
  ctx->evv = evv;
  return evv0;
}

static inline datatype_t evv_total(context_t* ctx) {
  return vector_dup(vector_empty);
}

static inline datatype_t evv_swap_create0(context_t* ctx) {
  return evv_swap(evv_total(ctx),ctx);
}

static inline datatype_t evv_swap_create1(int32_t i, context_t* ctx) {
  datatype_t evv0 = ctx->evv;  
  if (i==0 && (datatype_tag(evv0)!=TAG_VECTOR)) {
    return datatype_dup(evv0);
  }
  else {      
    ctx->evv = vector_at(evv0,i);
    return evv0;
  }
}

int32_t      evv_index( struct __std_core_hnd_Htag htag, context_t* ctx );
datatype_t   evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx );
datatype_t   evv_create(datatype_t evv, datatype_t indices, context_t* ctx);
datatype_t   evv_insert(datatype_t evv, datatype_t ev, context_t* ctx);
datatype_t   evv_delete(datatype_t evv, int32_t index, bool behind, context_t* ctx);
string_t     evv_show(datatype_t evv, context_t* ctx);
unit_t       evv_guard(datatype_t evv, context_t* ctx);
datatype     evv_swap_create( vector_t indices );
box_t        fatal_resume_final(context_t* ctx);
box_t        yield_cont( function_t next, context_t* ctx );
box_t        yield_extend( function_t next, context_t* ctx );
box_t        yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
box_t        yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx );
