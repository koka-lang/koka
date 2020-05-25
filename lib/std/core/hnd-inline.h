/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

typedef struct evv_s {
  box_t       len;     // total length of `vec`
  box_t       ofs;     // start offset in the evidence vector (to avoid re-allocations)
  datatype_t  vec[1];  // evidence vector
} evv_t;

static inline evv_t* context_evv(context_t* ctx) {
  return datatype_data_as(evv_t,ctx->evv);
}

static inline datatype_t evv_get(context_t* ctx) {
  return datatype_dup(ctx->evv);
}

static inline datatype_t evv_at( int32_t i, context_t* ctx ) {
  // todo: make this faster by 1) use a value type for `ev`, and 2) inline the evv at the end of the context?
  evv_t* evv = context_evv(ctx);
  return datatype_dup( evv->vec[unbox_int(evv->ofs) + i] );
}

static inline __std_core_types__unit_ evv_set(datatype_t evv, context_t* ctx) {
  datatype_drop(ctx->evv, ctx);
  ctx->evv = evv;
  return __std_core_types__Unit_;
}

static inline int32_t evv_set_ofs(int32_t ofs, context_t* ctx) {
  assert_internal(unbox_int(context_evv(ctx)->len) > ofs);   
  int_t old = unbox_int(context_evv(ctx)->ofs); 
  context_evv(ctx)->ofs = box_int(ofs);            
  return (int32_t)old;  
}

int32_t      evv_index( struct __std_core_hnd_Htag htag, context_t* ctx );
int32_t      evv_count( context_t* ctx );
datatype_t   evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx );
datatype_t   evv_total(context_t* ctx);
datatype_t   evv_create(datatype_t evv, datatype_t indices, context_t* ctx);
datatype_t   evv_insert(datatype_t evv, datatype_t ev, context_t* ctx);
datatype_t   evv_delete(datatype_t evv, int32_t index, bool behind, context_t* ctx);
string_t     evv_show(datatype_t evv, context_t* ctx);
unit_t       evv_expect(struct __std_core_hnd_Marker m, datatype_t evv, context_t* ctx);
unit_t       evv_guard(datatype_t evv, context_t* ctx);
box_t        resume_final(context_t* ctx);
bool         yielding_non_final(context_t* ctx);
box_t        yield_cont( function_t next, context_t* ctx );
box_t        yield_extend( function_t next, context_t* ctx );
box_t        yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
box_t        yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx );
struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx );
