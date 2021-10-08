#pragma once
#ifndef kk_std_core_hnd_H
#define kk_std_core_hnd_H
// Koka generated module: "std/core/hnd", koka version: 2.3.2, platform: 64-bit
#include <kklib.h>
#include "std_core_types.h"
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
struct kk_std_core_hnd__ev_s;
static inline struct kk_std_core_hnd__ev_s* kk_std_core_hnd__ev_dup(struct kk_std_core_hnd__ev_s* _x);

typedef struct kk_evv_vector_s {  
  struct kk_block_s             _block;
  kk_integer_t                  cfc;       // control flow context (0-3) as a small int
  struct kk_std_core_hnd__ev_s* vec[1];
} *kk_evv_vector_t;


typedef kk_ptr_t kk_evv_t;   // either a kk_evv_vector_t, or a single evidence

static inline kk_evv_t kk_evv_dup(kk_evv_t evv) {
  return kk_block_dup(evv);  
}

static inline void kk_evv_drop(kk_evv_t evv, kk_context_t* ctx) {
  kk_block_drop(evv,ctx);
}

static inline kk_evv_t kk_evv_empty(kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_evv_dup(kk_evv_empty_singleton);
}

static inline bool kk_evv_is_empty(kk_evv_t evv) {
  return (evv == kk_evv_empty_singleton);
}

static inline bool kk_evv_is_vector(kk_evv_t evv) {
  return kk_block_has_tag(evv,KK_TAG_EVV_VECTOR);
}

static inline struct kk_std_core_hnd__ev_s* kk_evv_as_ev( kk_evv_t evv ) {
  kk_assert_internal(!kk_evv_is_vector(evv));
  return (struct kk_std_core_hnd__ev_s*)evv;
}

static inline kk_evv_vector_t kk_evv_as_vector( kk_evv_t evv ) {
  kk_assert_internal(kk_evv_is_vector(evv));
  return (kk_evv_vector_t)evv;
}

static inline struct kk_std_core_hnd__ev_s* kk_evv_at( kk_ssize_t i, kk_context_t* ctx ) {
  kk_evv_t evv = ctx->evv;
  if (!kk_evv_is_vector(evv)) {  // evv is a single evidence
    kk_assert_internal(i==0);
    return kk_evv_as_ev(kk_evv_dup(evv));
  }
  else {  // evv as a vector
    kk_assert_internal(i >= 0 && i < (kk_block_scan_fsize(evv) - 1));
    kk_evv_vector_t vec = kk_evv_as_vector(evv);
    return kk_std_core_hnd__ev_dup(vec->vec[i]); 
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
  kk_ptr_t evv0 = ctx->evv;
  ctx->evv = evv;
  return evv0;
}

static inline bool kk_evv_eq(kk_evv_t evv1, kk_evv_t evv2, kk_context_t* ctx) {  // TODO:make borrowing
  bool eq = (evv1 == evv2);
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
  if (kk_evv_is_vector(evv0)) {
    ctx->evv = (kk_block_t*)kk_evv_at(i, ctx); // cast as ev struct is not defined yet 
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
int32_t         kk_evv_cfc(kk_context_t* ctx);
kk_ssize_t      kk_evv_index( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx );
kk_evv_t        kk_evv_create(kk_evv_t evv, kk_vector_t indices, kk_context_t* ctx);
kk_evv_t        kk_evv_insert(kk_evv_t evv, struct kk_std_core_hnd__ev_s* ev, kk_context_t* ctx);
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
  ctx->evv = kk_evv_delete(kk_evv_dup(evv0), i, behind, ctx);
  return evv0;
}


// type declarations

// type std/core/hnd/evv
struct kk_std_core_hnd__evv_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_hnd__evv;
static inline kk_std_core_hnd__evv kk_std_core_hnd__evv_dup(kk_std_core_hnd__evv _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_hnd__evv_drop(kk_std_core_hnd__evv _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_hnd__evv_is_unique(kk_std_core_hnd__evv _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_hnd__evv_free(kk_std_core_hnd__evv _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_hnd__evv_decref(kk_std_core_hnd__evv _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__evv_dropn_reuse(kk_std_core_hnd__evv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_hnd__evv_dropn(kk_std_core_hnd__evv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__evv_reuse(kk_std_core_hnd__evv _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_hnd__evv kk_std_core_hnd__evv_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_hnd__evv_box(kk_std_core_hnd__evv _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_hnd__evv kk_std_core_hnd__evv_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/hnd/htag
struct kk_std_core_hnd_Htag {
  kk_string_t tagname;
};
typedef struct kk_std_core_hnd_Htag kk_std_core_hnd__htag;
static inline kk_std_core_hnd__htag kk_std_core_hnd__new_Htag(kk_string_t tagname, kk_context_t* _ctx){
  kk_std_core_hnd__htag _con = { tagname };
  return _con;
}
static inline bool kk_std_core_hnd__is_Htag(kk_std_core_hnd__htag x) {
  return (true);
}
static inline kk_std_core_hnd__htag kk_std_core_hnd__htag_dup(kk_std_core_hnd__htag _x) {
  kk_string_dup(_x.tagname);
  return _x;
}
static inline void kk_std_core_hnd__htag_drop(kk_std_core_hnd__htag _x, kk_context_t* _ctx) {
  kk_string_drop(_x.tagname, _ctx);
}
static inline kk_box_t kk_std_core_hnd__htag_box(kk_std_core_hnd__htag _x, kk_context_t* _ctx) {
  return kk_string_box(_x.tagname);
}
static inline kk_std_core_hnd__htag kk_std_core_hnd__htag_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Htag(kk_string_unbox(_x), _ctx);
}

// value type std/core/hnd/marker
struct kk_std_core_hnd_Marker {
  int32_t m;
};
typedef struct kk_std_core_hnd_Marker kk_std_core_hnd__marker;
static inline kk_std_core_hnd__marker kk_std_core_hnd__new_Marker(int32_t m, kk_context_t* _ctx){
  kk_std_core_hnd__marker _con = { m };
  return _con;
}
static inline bool kk_std_core_hnd__is_Marker(kk_std_core_hnd__marker x) {
  return (true);
}
static inline kk_std_core_hnd__marker kk_std_core_hnd__marker_dup(kk_std_core_hnd__marker _x) {
  return _x;
}
static inline void kk_std_core_hnd__marker_drop(kk_std_core_hnd__marker _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_hnd__marker_box(kk_std_core_hnd__marker _x, kk_context_t* _ctx) {
  return kk_int32_box(_x.m, _ctx);
}
static inline kk_std_core_hnd__marker kk_std_core_hnd__marker_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Marker(kk_int32_unbox(_x, _ctx), _ctx);
}

// type std/core/hnd/ev
struct kk_std_core_hnd__ev_s {
  kk_block_t _block;
};
typedef struct kk_std_core_hnd__ev_s* kk_std_core_hnd__ev;
struct kk_std_core_hnd_Ev {
  struct kk_std_core_hnd__ev_s _base;
  kk_std_core_hnd__htag htag;
  kk_box_t hnd;
  kk_evv_t hevv;
  kk_std_core_hnd__marker marker;
  int32_t cfc;
};
static inline kk_std_core_hnd__ev kk_std_core_hnd__base_Ev(struct kk_std_core_hnd_Ev* _x){
  return &_x->_base;
}
static inline kk_std_core_hnd__ev kk_std_core_hnd__new_Ev(kk_reuse_t _at, kk_std_core_hnd__htag htag, kk_std_core_hnd__marker marker, kk_box_t hnd, int32_t cfc, kk_evv_t hevv, kk_context_t* _ctx){
  struct kk_std_core_hnd_Ev* _con = kk_block_alloc_at_as(struct kk_std_core_hnd_Ev, _at, 3 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->htag = htag;
  _con->hnd = hnd;
  _con->hevv = hevv;
  _con->marker = marker;
  _con->cfc = cfc;
  return kk_std_core_hnd__base_Ev(_con);
}
static inline struct kk_std_core_hnd_Ev* kk_std_core_hnd__as_Ev(kk_std_core_hnd__ev x) {
  return kk_basetype_as_assert(struct kk_std_core_hnd_Ev*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_core_hnd__is_Ev(kk_std_core_hnd__ev x) {
  return (true);
}
static inline kk_std_core_hnd__ev kk_std_core_hnd__ev_dup(kk_std_core_hnd__ev _x) {
  return kk_basetype_dup_as(kk_std_core_hnd__ev, _x);
}
static inline void kk_std_core_hnd__ev_drop(kk_std_core_hnd__ev _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_core_hnd__ev_is_unique(kk_std_core_hnd__ev _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_core_hnd__ev_free(kk_std_core_hnd__ev _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_core_hnd__ev_decref(kk_std_core_hnd__ev _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__ev_dropn_reuse(kk_std_core_hnd__ev _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_hnd__ev_dropn(kk_std_core_hnd__ev _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__ev_reuse(kk_std_core_hnd__ev _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_core_hnd__ev kk_std_core_hnd__ev_hole() {
  return (kk_std_core_hnd__ev)(1);
}
static inline kk_box_t kk_std_core_hnd__ev_box(kk_std_core_hnd__ev _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_core_hnd__ev kk_std_core_hnd__ev_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_core_hnd__ev, _x);
}

// value type std/core/hnd/clause0
struct kk_std_core_hnd_Clause0 {
  kk_function_t clause;
};
typedef struct kk_std_core_hnd_Clause0 kk_std_core_hnd__clause0;
static inline kk_std_core_hnd__clause0 kk_std_core_hnd__new_Clause0(kk_function_t clause, kk_context_t* _ctx){
  kk_std_core_hnd__clause0 _con = { clause };
  return _con;
}
static inline bool kk_std_core_hnd__is_Clause0(kk_std_core_hnd__clause0 x) {
  return (true);
}
static inline kk_std_core_hnd__clause0 kk_std_core_hnd__clause0_dup(kk_std_core_hnd__clause0 _x) {
  kk_function_dup(_x.clause);
  return _x;
}
static inline void kk_std_core_hnd__clause0_drop(kk_std_core_hnd__clause0 _x, kk_context_t* _ctx) {
  kk_function_drop(_x.clause, _ctx);
}
static inline kk_box_t kk_std_core_hnd__clause0_box(kk_std_core_hnd__clause0 _x, kk_context_t* _ctx) {
  return kk_function_box(_x.clause);
}
static inline kk_std_core_hnd__clause0 kk_std_core_hnd__clause0_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Clause0(kk_function_unbox(_x), _ctx);
}

// value type std/core/hnd/clause1
struct kk_std_core_hnd_Clause1 {
  kk_function_t clause;
};
typedef struct kk_std_core_hnd_Clause1 kk_std_core_hnd__clause1;
static inline kk_std_core_hnd__clause1 kk_std_core_hnd__new_Clause1(kk_function_t clause, kk_context_t* _ctx){
  kk_std_core_hnd__clause1 _con = { clause };
  return _con;
}
static inline bool kk_std_core_hnd__is_Clause1(kk_std_core_hnd__clause1 x) {
  return (true);
}
static inline kk_std_core_hnd__clause1 kk_std_core_hnd__clause1_dup(kk_std_core_hnd__clause1 _x) {
  kk_function_dup(_x.clause);
  return _x;
}
static inline void kk_std_core_hnd__clause1_drop(kk_std_core_hnd__clause1 _x, kk_context_t* _ctx) {
  kk_function_drop(_x.clause, _ctx);
}
static inline kk_box_t kk_std_core_hnd__clause1_box(kk_std_core_hnd__clause1 _x, kk_context_t* _ctx) {
  return kk_function_box(_x.clause);
}
static inline kk_std_core_hnd__clause1 kk_std_core_hnd__clause1_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Clause1(kk_function_unbox(_x), _ctx);
}

// value type std/core/hnd/clause2
struct kk_std_core_hnd_Clause2 {
  kk_function_t clause;
};
typedef struct kk_std_core_hnd_Clause2 kk_std_core_hnd__clause2;
static inline kk_std_core_hnd__clause2 kk_std_core_hnd__new_Clause2(kk_function_t clause, kk_context_t* _ctx){
  kk_std_core_hnd__clause2 _con = { clause };
  return _con;
}
static inline bool kk_std_core_hnd__is_Clause2(kk_std_core_hnd__clause2 x) {
  return (true);
}
static inline kk_std_core_hnd__clause2 kk_std_core_hnd__clause2_dup(kk_std_core_hnd__clause2 _x) {
  kk_function_dup(_x.clause);
  return _x;
}
static inline void kk_std_core_hnd__clause2_drop(kk_std_core_hnd__clause2 _x, kk_context_t* _ctx) {
  kk_function_drop(_x.clause, _ctx);
}
static inline kk_box_t kk_std_core_hnd__clause2_box(kk_std_core_hnd__clause2 _x, kk_context_t* _ctx) {
  return kk_function_box(_x.clause);
}
static inline kk_std_core_hnd__clause2 kk_std_core_hnd__clause2_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Clause2(kk_function_unbox(_x), _ctx);
}

// value type std/core/hnd/resume-result
struct kk_std_core_hnd_Deep {
  kk_box_t result;
};
struct kk_std_core_hnd_Shallow {
  kk_box_t result;
};
struct kk_std_core_hnd_Finalize {
  kk_box_t result;
};
struct kk_std_core_hnd_resume_result_s {
  kk_value_tag_t _tag;
  union {
    struct kk_std_core_hnd_Deep Deep;
    struct kk_std_core_hnd_Shallow Shallow;
    struct kk_std_core_hnd_Finalize Finalize;
  } _cons;
};
typedef struct kk_std_core_hnd_resume_result_s kk_std_core_hnd__resume_result;
static inline kk_std_core_hnd__resume_result kk_std_core_hnd__new_Deep(kk_box_t result, kk_context_t* _ctx){
  kk_std_core_hnd__resume_result _con;
  _con._tag = kk_value_tag(1);
  _con._cons.Deep.result = result;
  return _con;
}
static inline kk_std_core_hnd__resume_result kk_std_core_hnd__new_Shallow(kk_box_t result, kk_context_t* _ctx){
  kk_std_core_hnd__resume_result _con;
  _con._tag = kk_value_tag(2);
  _con._cons.Shallow.result = result;
  return _con;
}
static inline kk_std_core_hnd__resume_result kk_std_core_hnd__new_Finalize(kk_box_t result, kk_context_t* _ctx){
  kk_std_core_hnd__resume_result _con;
  _con._tag = kk_value_tag(3);
  _con._cons.Finalize.result = result;
  return _con;
}
static inline bool kk_std_core_hnd__is_Deep(kk_std_core_hnd__resume_result x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(1)));
}
static inline bool kk_std_core_hnd__is_Shallow(kk_std_core_hnd__resume_result x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(2)));
}
static inline bool kk_std_core_hnd__is_Finalize(kk_std_core_hnd__resume_result x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(3)));
}
static inline kk_ssize_t kk_std_core_hnd__resume_result_scan_count(kk_std_core_hnd__resume_result _x) {
  if (kk_std_core_hnd__is_Deep(_x)) return 2;
  else if (kk_std_core_hnd__is_Shallow(_x)) return 2;
  else return 2;
}
static inline kk_std_core_hnd__resume_result kk_std_core_hnd__resume_result_dup(kk_std_core_hnd__resume_result _x) {
  if (kk_std_core_hnd__is_Deep(_x)) {
    kk_box_dup(_x._cons.Deep.result);
  }
  else if (kk_std_core_hnd__is_Shallow(_x)) {
    kk_box_dup(_x._cons.Shallow.result);
  }
  else {
    kk_box_dup(_x._cons.Finalize.result);
  }
  return _x;
}
static inline void kk_std_core_hnd__resume_result_drop(kk_std_core_hnd__resume_result _x, kk_context_t* _ctx) {
  if (kk_std_core_hnd__is_Deep(_x)) {
    kk_box_drop(_x._cons.Deep.result, _ctx);
  }
  else if (kk_std_core_hnd__is_Shallow(_x)) {
    kk_box_drop(_x._cons.Shallow.result, _ctx);
  }
  else {
    kk_box_drop(_x._cons.Finalize.result, _ctx);
  }
}
static inline kk_box_t kk_std_core_hnd__resume_result_box(kk_std_core_hnd__resume_result _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_hnd__resume_result, _box, _x, kk_std_core_hnd__resume_result_scan_count(_x), _ctx);
  return _box;
}
static inline kk_std_core_hnd__resume_result kk_std_core_hnd__resume_result_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_hnd__resume_result _unbox;
  kk_valuetype_unbox_(kk_std_core_hnd__resume_result, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_hnd__resume_result_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value type std/core/hnd/resume-context
struct kk_std_core_hnd_Resume_context {
  kk_function_t k;
};
typedef struct kk_std_core_hnd_Resume_context kk_std_core_hnd__resume_context;
static inline kk_std_core_hnd__resume_context kk_std_core_hnd__new_Resume_context(kk_function_t k, kk_context_t* _ctx){
  kk_std_core_hnd__resume_context _con = { k };
  return _con;
}
static inline bool kk_std_core_hnd__is_Resume_context(kk_std_core_hnd__resume_context x) {
  return (true);
}
static inline kk_std_core_hnd__resume_context kk_std_core_hnd__resume_context_dup(kk_std_core_hnd__resume_context _x) {
  kk_function_dup(_x.k);
  return _x;
}
static inline void kk_std_core_hnd__resume_context_drop(kk_std_core_hnd__resume_context _x, kk_context_t* _ctx) {
  kk_function_drop(_x.k, _ctx);
}
static inline kk_box_t kk_std_core_hnd__resume_context_box(kk_std_core_hnd__resume_context _x, kk_context_t* _ctx) {
  return kk_function_box(_x.k);
}
static inline kk_std_core_hnd__resume_context kk_std_core_hnd__resume_context_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_hnd__new_Resume_context(kk_function_unbox(_x), _ctx);
}

// type std/core/hnd/yield-info
struct kk_std_core_hnd__yield_info_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_hnd__yield_info;
static inline kk_std_core_hnd__yield_info kk_std_core_hnd__yield_info_dup(kk_std_core_hnd__yield_info _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_hnd__yield_info_drop(kk_std_core_hnd__yield_info _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_hnd__yield_info_is_unique(kk_std_core_hnd__yield_info _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_hnd__yield_info_free(kk_std_core_hnd__yield_info _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_hnd__yield_info_decref(kk_std_core_hnd__yield_info _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__yield_info_dropn_reuse(kk_std_core_hnd__yield_info _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_hnd__yield_info_dropn(kk_std_core_hnd__yield_info _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_hnd__yield_info_reuse(kk_std_core_hnd__yield_info _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_hnd__yield_info kk_std_core_hnd__yield_info_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_hnd__yield_info_box(kk_std_core_hnd__yield_info _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_hnd__yield_info kk_std_core_hnd__yield_info_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/hnd/yld
struct kk_std_core_hnd_Pure {
  kk_box_t _unused;
};
struct kk_std_core_hnd_YieldingFinal {
  kk_box_t _unused;
};
struct kk_std_core_hnd_Yielding {
  kk_box_t _unused;
};
struct kk_std_core_hnd_Yield {
  kk_function_t clause;
  kk_function_t cont;
};
struct kk_std_core_hnd_yld_s {
  kk_value_tag_t _tag;
  union {
    struct kk_std_core_hnd_Pure Pure;
    struct kk_std_core_hnd_YieldingFinal YieldingFinal;
    struct kk_std_core_hnd_Yielding Yielding;
    struct kk_std_core_hnd_Yield Yield;
    kk_box_t _fields[2];
  } _cons;
};
typedef struct kk_std_core_hnd_yld_s kk_std_core_hnd__yld;
static inline kk_std_core_hnd__yld kk_std_core_hnd__new_Pure(kk_context_t* _ctx){
  kk_std_core_hnd__yld _con;
  _con._tag = kk_value_tag(1);
  _con._cons._fields[0] = kk_box_null;
  _con._cons._fields[1] = kk_box_null;
  return _con;
}
static inline kk_std_core_hnd__yld kk_std_core_hnd__new_YieldingFinal(kk_context_t* _ctx){
  kk_std_core_hnd__yld _con;
  _con._tag = kk_value_tag(2);
  _con._cons._fields[0] = kk_box_null;
  _con._cons._fields[1] = kk_box_null;
  return _con;
}
static inline kk_std_core_hnd__yld kk_std_core_hnd__new_Yielding(kk_context_t* _ctx){
  kk_std_core_hnd__yld _con;
  _con._tag = kk_value_tag(3);
  _con._cons._fields[0] = kk_box_null;
  _con._cons._fields[1] = kk_box_null;
  return _con;
}
static inline kk_std_core_hnd__yld kk_std_core_hnd__new_Yield(kk_function_t clause, kk_function_t cont, kk_context_t* _ctx){
  kk_std_core_hnd__yld _con;
  _con._tag = kk_value_tag(4);
  _con._cons.Yield.clause = clause;
  _con._cons.Yield.cont = cont;
  return _con;
}
static inline bool kk_std_core_hnd__is_Pure(kk_std_core_hnd__yld x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(1)));
}
static inline bool kk_std_core_hnd__is_YieldingFinal(kk_std_core_hnd__yld x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(2)));
}
static inline bool kk_std_core_hnd__is_Yielding(kk_std_core_hnd__yld x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(3)));
}
static inline bool kk_std_core_hnd__is_Yield(kk_std_core_hnd__yld x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(4)));
}
static inline kk_ssize_t kk_std_core_hnd__yld_scan_count(kk_std_core_hnd__yld _x) {
  if (kk_std_core_hnd__is_Pure(_x)) return 1;
  else if (kk_std_core_hnd__is_YieldingFinal(_x)) return 1;
  else if (kk_std_core_hnd__is_Yielding(_x)) return 1;
  else return 3;
}
static inline kk_std_core_hnd__yld kk_std_core_hnd__yld_dup(kk_std_core_hnd__yld _x) {
  if (kk_std_core_hnd__is_Pure(_x)) { }
  else if (kk_std_core_hnd__is_YieldingFinal(_x)) { }
  else if (kk_std_core_hnd__is_Yielding(_x)) { }
  else {
    kk_function_dup(_x._cons.Yield.clause);
    kk_function_dup(_x._cons.Yield.cont);
  }
  return _x;
}
static inline void kk_std_core_hnd__yld_drop(kk_std_core_hnd__yld _x, kk_context_t* _ctx) {
  if (kk_std_core_hnd__is_Pure(_x)) { }
  else if (kk_std_core_hnd__is_YieldingFinal(_x)) { }
  else if (kk_std_core_hnd__is_Yielding(_x)) { }
  else {
    kk_function_drop(_x._cons.Yield.clause, _ctx);
    kk_function_drop(_x._cons.Yield.cont, _ctx);
  }
}
static inline kk_box_t kk_std_core_hnd__yld_box(kk_std_core_hnd__yld _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_hnd__yld, _box, _x, kk_std_core_hnd__yld_scan_count(_x), _ctx);
  return _box;
}
static inline kk_std_core_hnd__yld kk_std_core_hnd__yld_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_hnd__yld _unbox;
  kk_valuetype_unbox_(kk_std_core_hnd__yld, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_hnd__yld_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value declarations
 
// Automatically generated. Retrieves the `tagname` constructor field of the `:htag` type.

static inline kk_string_t kk_std_core_hnd_tagname(kk_std_core_hnd__htag htag0, kk_context_t* _ctx) { /* forall<a> (htag : htag<a>) -> string */ 
  {
    kk_string_t _x = htag0.tagname;
    return _x;
  }
}

static inline kk_std_core_hnd__htag kk_std_core_hnd__copy(kk_std_core_hnd__htag _this, kk_std_core_types__optional tagname0, kk_context_t* _ctx) { /* forall<a> (htag<a>, tagname : optional<string>) -> htag<a> */ 
  kk_string_t _x10579;
  if (kk_std_core_types__is_Optional(tagname0)) {
    kk_box_t _box_x10323 = tagname0._cons.Optional.value;
    kk_string_t _tagname_1940 = kk_string_unbox(_box_x10323);
    kk_string_dup(_tagname_1940);
    kk_std_core_types__optional_drop(tagname0, _ctx);
    kk_std_core_hnd__htag_drop(_this, _ctx);
    _x10579 = _tagname_1940; /*string*/
    goto _match10580;
  }
  {
    kk_string_t _x = _this.tagname;
    _x10579 = _x; /*string*/
  }
  _match10580: ;
  return kk_std_core_hnd__new_Htag(_x10579, _ctx);
}
 
// Automatically generated. Retrieves the `m` constructor field of the `:marker` type.

static inline int32_t kk_std_core_hnd_m(kk_std_core_hnd__marker marker, kk_context_t* _ctx) { /* forall<e,a> (marker : marker<e,a>) -> int32 */ 
  {
    int32_t _x = marker.m;
    return _x;
  }
}

static inline kk_std_core_hnd__marker kk_std_core_hnd__copy_1(kk_std_core_hnd__marker _this, kk_std_core_types__optional m0, kk_context_t* _ctx) { /* forall<e,a> (marker<e,a>, m : optional<int32>) -> marker<e,a> */ 
  int32_t _x10582;
  if (kk_std_core_types__is_Optional(m0)) {
    kk_box_t _box_x10324 = m0._cons.Optional.value;
    int32_t _m_1976 = kk_int32_unbox(_box_x10324, NULL);
    kk_std_core_types__optional_drop(m0, _ctx);
    _x10582 = _m_1976; /*int32*/
    goto _match10583;
  }
  {
    int32_t _x = _this.m;
    _x10582 = _x; /*int32*/
  }
  _match10583: ;
  return kk_std_core_hnd__new_Marker(_x10582, _ctx);
}
 
// Automatically generated. Retrieves the `htag` constructor field of the `:ev` type.

static inline kk_std_core_hnd__htag kk_std_core_hnd_htag(kk_std_core_hnd__ev ev, kk_context_t* _ctx) { /* forall<a> (ev : ev<a>) -> htag<a> */ 
  {
    struct kk_std_core_hnd_Ev* _con10585 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__htag _x = _con10585->htag;
    kk_box_t _pat1 = _con10585->hnd;
    kk_evv_t _pat3 = _con10585->hevv;
    if (kk_likely(kk_std_core_hnd__ev_is_unique(ev))) {
      kk_evv_drop(_pat3, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_std_core_hnd__ev_free(ev);
    }
    else {
      kk_std_core_hnd__htag_dup(_x);
      kk_std_core_hnd__ev_decref(ev, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `cfc` constructor field of the `:ev` type.

static inline int32_t kk_std_core_hnd_cfc(kk_std_core_hnd__ev ev, kk_context_t* _ctx) { /* forall<a> (ev : ev<a>) -> cfc */ 
  {
    struct kk_std_core_hnd_Ev* _con10586 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__htag _pat0 = _con10586->htag;
    kk_box_t _pat2 = _con10586->hnd;
    int32_t _x = _con10586->cfc;
    kk_evv_t _pat3 = _con10586->hevv;
    if (kk_likely(kk_std_core_hnd__ev_is_unique(ev))) {
      kk_evv_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_std_core_hnd__htag_drop(_pat0, _ctx);
      kk_std_core_hnd__ev_free(ev);
    }
    else {
      kk_std_core_hnd__ev_decref(ev, _ctx);
    }
    return _x;
  }
}

kk_std_core_hnd__ev kk_std_core_hnd__copy_2(kk_std_core_hnd__ev _this, kk_std_core_types__optional htag0, kk_std_core_hnd__marker marker, kk_box_t hnd, kk_std_core_types__optional cfc0, kk_evv_t hevv, kk_context_t* _ctx); /* forall<a,e,b> (ev<a>, htag : optional<htag<a>>, marker : marker<e,b>, hnd : a<e,b>, cfc : optional<cfc>, hevv : evv<e>) -> ev<a> */ 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause0` type.

static inline kk_function_t kk_std_core_hnd_clause(kk_std_core_hnd__clause0 clause0, kk_context_t* _ctx) { /* forall<a,b,e,c> (clause0 : clause0<a,b,e,c>) -> ((marker<e,c>, ev<b>) -> e a) */ 
  {
    kk_function_t _x = clause0.clause;
    return _x;
  }
}

kk_std_core_hnd__clause0 kk_std_core_hnd__copy_3(kk_std_core_hnd__clause0 _this, kk_std_core_types__optional clause0, kk_context_t* _ctx); /* forall<a,b,e,c> (clause0<a,b,e,c>, clause : optional<(marker<e,c>, ev<b>) -> e a>) -> clause0<a,b,e,c> */ 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause1` type.

static inline kk_function_t kk_std_core_hnd_clause_1(kk_std_core_hnd__clause1 clause1, kk_context_t* _ctx) { /* forall<a,b,c,e,d> (clause1 : clause1<a,b,c,e,d>) -> ((marker<e,d>, ev<c>, a) -> e b) */ 
  {
    kk_function_t _x = clause1.clause;
    return _x;
  }
}

kk_std_core_hnd__clause1 kk_std_core_hnd__copy_4(kk_std_core_hnd__clause1 _this, kk_std_core_types__optional clause0, kk_context_t* _ctx); /* forall<a,b,c,e,d> (clause1<a,b,c,e,d>, clause : optional<(marker<e,d>, ev<c>, a) -> e b>) -> clause1<a,b,c,e,d> */ 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause2` type.

static inline kk_function_t kk_std_core_hnd_clause_2(kk_std_core_hnd__clause2 clause2, kk_context_t* _ctx) { /* forall<a,b,c,d,e,a1> (clause2 : clause2<a,b,c,d,e,a1>) -> ((marker<e,a1>, ev<d>, a, b) -> e c) */ 
  {
    kk_function_t _x = clause2.clause;
    return _x;
  }
}

kk_std_core_hnd__clause2 kk_std_core_hnd__copy_5(kk_std_core_hnd__clause2 _this, kk_std_core_types__optional clause0, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1> (clause2<a,b,c,d,e,a1>, clause : optional<(marker<e,a1>, ev<d>, a, b) -> e c>) -> clause2<a,b,c,d,e,a1> */ 
 
// Automatically generated. Tests for the `Deep` constructor of the `:resume-result` type.

static inline bool kk_std_core_hnd_is_deep(kk_std_core_hnd__resume_result resume_result, kk_context_t* _ctx) { /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Deep(resume_result)) {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return true;
  }
  {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Shallow` constructor of the `:resume-result` type.

static inline bool kk_std_core_hnd_is_shallow(kk_std_core_hnd__resume_result resume_result, kk_context_t* _ctx) { /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Shallow(resume_result)) {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return true;
  }
  {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Finalize` constructor of the `:resume-result` type.

static inline bool kk_std_core_hnd_is_finalize(kk_std_core_hnd__resume_result resume_result, kk_context_t* _ctx) { /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Finalize(resume_result)) {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return true;
  }
  {
    kk_std_core_hnd__resume_result_drop(resume_result, _ctx);
    return false;
  }
}
 
// Automatically generated. Retrieves the `k` constructor field of the `:resume-context` type.

static inline kk_function_t kk_std_core_hnd_k(kk_std_core_hnd__resume_context _this, kk_context_t* _ctx) { /* forall<a,e,e1,b> (resume-context<a,e,e1,b>) -> ((resume-result<a,b>) -> e b) */ 
  {
    kk_function_t _x = _this.k;
    return _x;
  }
}

kk_std_core_hnd__resume_context kk_std_core_hnd__copy_6(kk_std_core_hnd__resume_context _this, kk_std_core_types__optional k0, kk_context_t* _ctx); /* forall<a,e,e1,b> (resume-context<a,e,e1,b>, k : optional<(resume-result<a,b>) -> e b>) -> resume-context<a,e,e1,b> */ 
 
// Automatically generated. Tests for the `Pure` constructor of the `:yld` type.

static inline bool kk_std_core_hnd_is_pure(kk_std_core_hnd__yld yld, kk_context_t* _ctx) { /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Pure(yld)) {
    return true;
  }
  {
    kk_std_core_hnd__yld_drop(yld, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `YieldingFinal` constructor of the `:yld` type.

static inline bool kk_std_core_hnd_is_yieldingFinal(kk_std_core_hnd__yld yld, kk_context_t* _ctx) { /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */ 
  if (kk_std_core_hnd__is_YieldingFinal(yld)) {
    return true;
  }
  {
    kk_std_core_hnd__yld_drop(yld, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Yielding` constructor of the `:yld` type.

static inline bool kk_std_core_hnd_is_yielding(kk_std_core_hnd__yld yld, kk_context_t* _ctx) { /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Yielding(yld)) {
    return true;
  }
  {
    kk_std_core_hnd__yld_drop(yld, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Yield` constructor of the `:yld` type.

static inline bool kk_std_core_hnd_is_yield(kk_std_core_hnd__yld yld, kk_context_t* _ctx) { /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */ 
  if (kk_std_core_hnd__is_Yield(yld)) {
    kk_std_core_hnd__yld_drop(yld, _ctx);
    return true;
  }
  {
    kk_std_core_hnd__yld_drop(yld, _ctx);
    return false;
  }
}

kk_ssize_t kk_std_core_hnd__evv_index(kk_std_core_hnd__htag htag0, kk_context_t* _ctx); /* forall<e,a> (htag : htag<a>) -> e ev-index */ 

bool kk_std_core_hnd__evv_is_affine(kk_context_t* _ctx); /* () -> bool */ 

kk_std_core_hnd__ev kk_std_core_hnd__evv_lookup(kk_std_core_hnd__htag htag0, kk_context_t* _ctx); /* forall<a> (htag : htag<a>) -> ev<a> */ 
 
// mask for builtin effects without a handler or evidence

static inline kk_box_t kk_std_core_hnd__mask_builtin(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,e1> (action : () -> e a) -> e1 a */ 
  return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}

static inline kk_std_core_hnd__htag kk_std_core_hnd__new_htag(kk_string_t tag, kk_context_t* _ctx) { /* forall<a> (tag : string) -> htag<a> */ 
  return kk_std_core_hnd__new_Htag(tag, _ctx);
}

static inline kk_box_t kk_std_core_hnd__open_none0(kk_function_t f, kk_context_t* _ctx) { /* forall<a,e,e1> (f : () -> e a) -> e1 a */ 
  kk_evv_t w = kk_evv_swap_create0(kk_context()); /*std/core/hnd/evv<3216>*/;
  kk_box_t x = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), f, (f, _ctx)); /*3214*/;
  kk_unit_t keep = kk_Unit;
  kk_evv_set(w,kk_context());
  return x;
}

static inline kk_box_t kk_std_core_hnd__open_none1(kk_function_t f, kk_box_t x1, kk_context_t* _ctx) { /* forall<a,b,e,e1> (f : (a) -> e b, x1 : a) -> e1 b */ 
  kk_evv_t w = kk_evv_swap_create0(kk_context()); /*std/core/hnd/evv<3293>*/;
  kk_box_t x = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x1, _ctx)); /*3291*/;
  kk_unit_t keep = kk_Unit;
  kk_evv_set(w,kk_context());
  return x;
}

static inline kk_box_t kk_std_core_hnd__open_none2(kk_function_t f, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx) { /* forall<a,b,c,e,e1> (f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */ 
  kk_evv_t w = kk_evv_swap_create0(kk_context()); /*std/core/hnd/evv<3385>*/;
  kk_box_t x = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), f, (f, x1, x2, _ctx)); /*3383*/;
  kk_unit_t keep = kk_Unit;
  kk_evv_set(w,kk_context());
  return x;
}

static inline kk_box_t kk_std_core_hnd__open_none3(kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_context_t* _ctx) { /* forall<a,b,c,d,e,e1> (f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */ 
  kk_evv_t w = kk_evv_swap_create0(kk_context()); /*std/core/hnd/evv<3429>*/;
  kk_box_t x = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_box_t, kk_context_t*), f, (f, x1, x2, x3, _ctx)); /*3427*/;
  kk_unit_t keep = kk_Unit;
  kk_evv_set(w,kk_context());
  return x;
}

static inline kk_box_t kk_std_core_hnd__open_none4(kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_box_t x4, kk_context_t* _ctx) { /* forall<a,b,c,d,a1,e,e1> (f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */ 
  kk_evv_t w = kk_evv_swap_create0(kk_context()); /*std/core/hnd/evv<3479>*/;
  kk_box_t x = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_box_t, kk_box_t, kk_context_t*), f, (f, x1, x2, x3, x4, _ctx)); /*3477*/;
  kk_unit_t keep = kk_Unit;
  kk_evv_set(w,kk_context());
  return x;
}

static inline kk_box_t kk_std_core_hnd__perform0(kk_std_core_hnd__ev ev, kk_function_t op, kk_context_t* _ctx) { /* forall<a,e,b> (ev : ev<b>, op : forall<e1,c> (b<e1,c>) -> clause0<a,b,e1,c>) -> e a */ 
  {
    struct kk_std_core_hnd_Ev* _con10607 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__marker m0 = _con10607->marker;
    kk_box_t h = _con10607->hnd;
    kk_box_dup(h);
    kk_std_core_hnd__clause0 _match_10578 = kk_function_call(kk_std_core_hnd__clause0, (kk_function_t, kk_box_t, kk_context_t*), op, (op, h, _ctx)); /*std/core/hnd/clause0<3566,3568,547,548>*/;
    {
      kk_function_t f = _match_10578.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), f, (f, m0, ev, _ctx));
    }
  }
}

static inline kk_box_t kk_std_core_hnd__perform1(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x, kk_context_t* _ctx) { /* forall<a,b,e,c> (ev : ev<c>, op : forall<e1,d> (c<e1,d>) -> clause1<a,b,c,e1,d>, x : a) -> e b */ 
  {
    struct kk_std_core_hnd_Ev* _con10608 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__marker m0 = _con10608->marker;
    kk_box_t h = _con10608->hnd;
    kk_box_dup(h);
    kk_std_core_hnd__clause1 _match_10577 = kk_function_call(kk_std_core_hnd__clause1, (kk_function_t, kk_box_t, kk_context_t*), op, (op, h, _ctx)); /*std/core/hnd/clause1<3670,3671,3673,569,570>*/;
    {
      kk_function_t f = _match_10577.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), f, (f, m0, ev, x, _ctx));
    }
  }
}

static inline kk_box_t kk_std_core_hnd__perform2(kk_std_core_hnd__ev evx, kk_function_t op, kk_box_t x, kk_box_t y, kk_context_t* _ctx) { /* forall<a,b,c,e,d> (evx : ev<d>, op : forall<e1,a1> (d<e1,a1>) -> clause2<a,b,c,d,e1,a1>, x : a, y : b) -> e c */ 
  {
    struct kk_std_core_hnd_Ev* _con10609 = kk_std_core_hnd__as_Ev(evx);
    kk_std_core_hnd__marker m0 = _con10609->marker;
    kk_box_t h = _con10609->hnd;
    kk_box_dup(h);
    kk_std_core_hnd__clause2 _match_10576 = kk_function_call(kk_std_core_hnd__clause2, (kk_function_t, kk_box_t, kk_context_t*), op, (op, h, _ctx)); /*std/core/hnd/clause2<3790,3791,3792,3794,594,595>*/;
    {
      kk_function_t f = _match_10576.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_box_t, kk_context_t*), f, (f, m0, evx, x, y, _ctx));
    }
  }
}

kk_evv_t kk_std_core_hnd_evv_get(kk_context_t* _ctx); /* forall<e> () -> e evv<e> */ 

kk_evv_t kk_std_core_hnd_evv_insert(kk_evv_t evv, kk_std_core_hnd__ev ev, kk_context_t* _ctx); /* forall<e,e1,a> (evv : evv<e>, ev : ev<a>) -> e evv<e1> */ 

int32_t kk_std_core_hnd_fresh_marker_int(kk_context_t* _ctx); /* () -> int32 */ 

bool kk_std_core_hnd_evv_eq(kk_evv_t evv0, kk_evv_t evv1, kk_context_t* _ctx); /* forall<e> (evv0 : evv<e>, evv1 : evv<e>) -> bool */ 

kk_unit_t kk_std_core_hnd_guard(kk_evv_t w, kk_context_t* _ctx); /* forall<e> (w : evv<e>) -> e () */ 

kk_box_t kk_std_core_hnd_yield_extend(kk_function_t next, kk_context_t* _ctx); /* forall<a,b,e> (next : (a) -> e b) -> e b */ 

kk_box_t kk_std_core_hnd_yield_cont(kk_function_t f, kk_context_t* _ctx); /* forall<a,e,b> (f : forall<c> ((c) -> e a, c) -> e b) -> e b */ 

kk_std_core_hnd__yld kk_std_core_hnd_yield_prompt(kk_std_core_hnd__marker m0, kk_context_t* _ctx); /* forall<a,e,b> (m : marker<e,b>) -> yld<e,a,b> */ 

kk_box_t kk_std_core_hnd_yield_to_final(kk_std_core_hnd__marker m0, kk_function_t clause0, kk_context_t* _ctx); /* forall<a,e,e1,b> (m : marker<e1,b>, clause : ((resume-result<a,b>) -> e1 b) -> e1 b) -> e a */ 

kk_evv_t kk_std_core_hnd_evv_swap_delete(kk_ssize_t i, bool behind, kk_context_t* _ctx); /* forall<e,e1> (i : ev-index, behind : bool) -> e1 evv<e> */ 

int32_t kk_std_core_hnd_fresh_marker_named_int(kk_context_t* _ctx); /* () -> int32 */ 

kk_evv_t kk_std_core_hnd_evv_swap_create(kk_vector_t indices, kk_context_t* _ctx); /* forall<e> (indices : vector<ev-index>) -> e evv<e> */ 
 
// For interal use

static inline kk_box_t kk_std_core_hnd_xperform1(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x, kk_context_t* _ctx) { /* forall<a,b,e,c> (ev : ev<c>, op : forall<e1,d> (c<e1,d>) -> clause1<a,b,c,e1,d>, x : a) -> e b */ 
  {
    struct kk_std_core_hnd_Ev* _con10610 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__marker m0 = _con10610->marker;
    kk_box_t h = _con10610->hnd;
    kk_box_dup(h);
    kk_std_core_hnd__clause1 _match_10575 = kk_function_call(kk_std_core_hnd__clause1, (kk_function_t, kk_box_t, kk_context_t*), op, (op, h, _ctx)); /*std/core/hnd/clause1<4055,4056,4058,698,699>*/;
    {
      kk_function_t f = _match_10575.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), f, (f, m0, ev, x, _ctx));
    }
  }
}

kk_function_t kk_std_core_hnd_yield_to_prim(kk_std_core_hnd__marker m0, kk_function_t clause0, kk_context_t* _ctx); /* forall<a,e,e1,b> (m : marker<e1,b>, clause : ((resume-result<a,b>) -> e1 b) -> e1 b) -> e (() -> a) */ 


// lift anonymous function
struct kk_std_core_hnd_clause_tail_noyield0_fun10611__t {
  struct kk_function_s _base;
  kk_function_t op;
};
extern kk_box_t kk_std_core_hnd_clause_tail_noyield0_fun10611(kk_function_t _fself, kk_std_core_hnd__marker ___wildcard__637__14, kk_std_core_hnd__ev ___wildcard__637__17, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_tail_noyield0_fun10611(kk_function_t op, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_tail_noyield0_fun10611__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_tail_noyield0_fun10611__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_tail_noyield0_fun10611, kk_context());
  _self->op = op;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause0 kk_std_core_hnd_clause_tail_noyield0(kk_function_t op, kk_context_t* _ctx) { /* forall<e,a,b,c> (op : () -> e b) -> clause0<b,c,e,a> */ 
  return kk_std_core_hnd__new_Clause0(kk_std_core_hnd_new_clause_tail_noyield0_fun10611(op, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_hnd_clause_tail_noyield1_fun10612__t {
  struct kk_function_s _base;
  kk_function_t op;
};
extern kk_box_t kk_std_core_hnd_clause_tail_noyield1_fun10612(kk_function_t _fself, kk_std_core_hnd__marker ___wildcard__581__14, kk_std_core_hnd__ev ___wildcard__581__17, kk_box_t x, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_tail_noyield1_fun10612(kk_function_t op, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_tail_noyield1_fun10612__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_tail_noyield1_fun10612__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_tail_noyield1_fun10612, kk_context());
  _self->op = op;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause1 kk_std_core_hnd_clause_tail_noyield1(kk_function_t op, kk_context_t* _ctx) { /* forall<e,a,b,c,d> (op : (b) -> e c) -> clause1<b,c,d,e,a> */ 
  return kk_std_core_hnd__new_Clause1(kk_std_core_hnd_new_clause_tail_noyield1_fun10612(op, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_hnd_clause_tail_noyield2_fun10613__t {
  struct kk_function_s _base;
  kk_function_t op;
};
extern kk_box_t kk_std_core_hnd_clause_tail_noyield2_fun10613(kk_function_t _fself, kk_std_core_hnd__marker ___wildcard__689__14, kk_std_core_hnd__ev ___wildcard__689__17, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_tail_noyield2_fun10613(kk_function_t op, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_tail_noyield2_fun10613__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_tail_noyield2_fun10613__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_tail_noyield2_fun10613, kk_context());
  _self->op = op;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause2 kk_std_core_hnd_clause_tail_noyield2(kk_function_t op, kk_context_t* _ctx) { /* forall<e,a,b,c,d,a1> (op : (b, c) -> e d) -> clause2<b,c,d,a1,e,a> */ 
  return kk_std_core_hnd__new_Clause2(kk_std_core_hnd_new_clause_tail_noyield2_fun10613(op, _ctx), _ctx);
}

kk_evv_t kk_std_core_hnd_evv_swap_with(kk_std_core_hnd__ev ev, kk_context_t* _ctx); /* forall<a,e> (ev : ev<a>) -> evv<e> */ 


// lift anonymous function
struct kk_std_core_hnd_clause_value_fun10616__t {
  struct kk_function_s _base;
  kk_box_t v;
};
extern kk_box_t kk_std_core_hnd_clause_value_fun10616(kk_function_t _fself, kk_std_core_hnd__marker ___wildcard__641__14, kk_std_core_hnd__ev ___wildcard__641__17, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_value_fun10616(kk_box_t v, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_value_fun10616__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_value_fun10616__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_value_fun10616, kk_context());
  _self->v = v;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause0 kk_std_core_hnd_clause_value(kk_box_t v, kk_context_t* _ctx) { /* forall<a,e,b,c> (v : a) -> clause0<a,b,e,c> */ 
  return kk_std_core_hnd__new_Clause0(kk_std_core_hnd_new_clause_value_fun10616(v, _ctx), _ctx);
}

kk_string_t kk_std_core_hnd_evv_show(kk_evv_t evv, kk_context_t* _ctx); /* forall<e> (evv : evv<e>) -> string */ 

kk_box_t kk_std_core_hnd_unsafe_reyield(kk_std_core_hnd__yield_info yld, kk_context_t* _ctx); /* forall<a,e> (yld : yield-info) -> e a */ 

kk_std_core_hnd__yield_info kk_std_core_hnd_yield_capture(kk_context_t* _ctx); /* forall<e> () -> e yield-info */ 

static inline kk_box_t kk_std_core_hnd_get(kk_ref_t ref, kk_context_t* _ctx) { /* forall<a,h> (ref : ref<h,a>) -> <read<h>,div> a */ 
  return kk_ref_get(ref,kk_context());
}

static inline kk_std_core_hnd__htag kk_std_core_hnd_hidden_htag(kk_string_t tag, kk_context_t* _ctx) { /* forall<a> (tag : string) -> htag<a> */ 
  return kk_std_core_hnd__new_Htag(tag, _ctx);
}

static inline kk_box_t kk_std_core_hnd_resume(kk_std_core_hnd__resume_context r, kk_box_t x, kk_context_t* _ctx) { /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : a) -> e b */ 
  kk_function_t _x10618 = kk_std_core_hnd_k(r, _ctx); /*(std/core/hnd/resume-result<2857,2860>) -> 2858 2860*/
  kk_std_core_hnd__resume_result _x10617 = kk_std_core_hnd__new_Deep(x, _ctx); /*std/core/hnd/resume-result<80,81>*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _x10618, (_x10618, _x10617, _ctx));
}

kk_box_t kk_std_core_hnd_resume_final(kk_context_t* _ctx); /* forall<a> () -> a */ 

static inline kk_box_t kk_std_core_hnd_resume_shallow(kk_std_core_hnd__resume_context r, kk_box_t x, kk_context_t* _ctx) { /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : a) -> e1 b */ 
  kk_function_t _x10620 = kk_std_core_hnd_k(r, _ctx); /*(std/core/hnd/resume-result<2857,2860>) -> 2858 2860*/
  kk_std_core_hnd__resume_result _x10619 = kk_std_core_hnd__new_Shallow(x, _ctx); /*std/core/hnd/resume-result<80,81>*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _x10620, (_x10620, _x10619, _ctx));
}

static inline kk_std_core_hnd__marker kk_std_core_hnd_fresh_marker(kk_context_t* _ctx) { /* forall<e,a> () -> marker<e,a> */ 
  int32_t _x10621 = kk_std_core_hnd_fresh_marker_int(_ctx); /*int32*/
  return kk_std_core_hnd__new_Marker(_x10621, _ctx);
}

static inline kk_box_t kk_std_core_hnd_yield_bind(kk_box_t x, kk_function_t next, kk_context_t* _ctx) { /* forall<a,b,e> (x : a, next : (a) -> e b) -> e b */ 
  if (kk_yielding(kk_context())) {
    kk_box_drop(x, _ctx);
    return kk_std_core_hnd_yield_extend(next, _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), next, (next, x, _ctx));
  }
}

kk_box_t kk_std_core_hnd_prompt(kk_evv_t w0, kk_evv_t w1, kk_std_core_hnd__ev ev, kk_std_core_hnd__marker m0, kk_function_t ret, kk_box_t result, kk_context_t* _ctx); /* forall<a,e,b,c> (w0 : evv<e>, w1 : evv<e>, ev : ev<b>, m : marker<e,c>, ret : (a) -> e c, result : a) -> e c */ 

kk_box_t kk_std_core_hnd__hhandle(kk_std_core_hnd__htag tag, int32_t cfc0, kk_box_t h, kk_function_t ret, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,e1,b,c> (tag : htag<b>, cfc : cfc, h : b<e,c>, ret : (a) -> e c, action : () -> e1 a) -> e c */ 

kk_box_t kk_std_core_hnd_mask_at1(kk_ssize_t i, bool behind, kk_function_t action, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,e1> (i : ev-index, behind : bool, action : (a) -> e b, x : a) -> e1 b */ 

kk_box_t kk_std_core_hnd__mask_at(kk_ssize_t i, bool behind, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,e1> (i : ev-index, behind : bool, action : () -> e a) -> e1 a */ 

static inline kk_std_core_hnd__marker kk_std_core_hnd_fresh_marker_named(kk_context_t* _ctx) { /* forall<e,a> () -> marker<e,a> */ 
  int32_t _x10653 = kk_std_core_hnd_fresh_marker_named_int(_ctx); /*int32*/
  return kk_std_core_hnd__new_Marker(_x10653, _ctx);
}

kk_box_t kk_std_core_hnd__named_handle(kk_std_core_hnd__htag tag, int32_t cfc0, kk_box_t h, kk_function_t ret, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,e1,b,c> (tag : htag<b>, cfc : cfc, h : b<e,c>, ret : (a) -> e c, action : (ev<b>) -> e1 a) -> e c */ 

kk_box_t kk_std_core_hnd_open_at1(kk_ssize_t i, kk_function_t f, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,e1> (i : ev-index, f : (a) -> e b, x : a) -> e1 b */ 

kk_box_t kk_std_core_hnd__open_at0(kk_ssize_t i, kk_function_t f, kk_context_t* _ctx); /* forall<a,e,e1> (i : ev-index, f : () -> e a) -> e1 a */ 

kk_box_t kk_std_core_hnd__open_at1(kk_ssize_t i, kk_function_t f, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,e1> (i : ev-index, f : (a) -> e b, x : a) -> e1 b */ 

kk_box_t kk_std_core_hnd__open_at2(kk_ssize_t i, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx); /* forall<a,b,c,e,e1> (i : ev-index, f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */ 

kk_box_t kk_std_core_hnd__open_at3(kk_ssize_t i, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_context_t* _ctx); /* forall<a,b,c,d,e,e1> (i : ev-index, f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */ 

kk_box_t kk_std_core_hnd__open_at4(kk_ssize_t i, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_box_t x4, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,e1> (i : ev-index, f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */ 

kk_box_t kk_std_core_hnd_open1(kk_vector_t indices, kk_function_t f, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,e1> (indices : vector<ev-index>, f : (a) -> e b, x : a) -> e1 b */ 

kk_box_t kk_std_core_hnd__open0(kk_vector_t indices, kk_function_t f, kk_context_t* _ctx); /* forall<a,e,e1> (indices : vector<ev-index>, f : () -> e a) -> e1 a */ 

kk_box_t kk_std_core_hnd__open1(kk_vector_t indices, kk_function_t f, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,e1> (indices : vector<ev-index>, f : (a) -> e b, x : a) -> e1 b */ 

kk_box_t kk_std_core_hnd__open2(kk_vector_t indices, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx); /* forall<a,b,c,e,e1> (indices : vector<ev-index>, f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */ 

kk_box_t kk_std_core_hnd__open3(kk_vector_t indices, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_context_t* _ctx); /* forall<a,b,c,d,e,e1> (indices : vector<ev-index>, f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */ 

kk_box_t kk_std_core_hnd__open4(kk_vector_t indices, kk_function_t f, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_box_t x4, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,e1> (indices : vector<ev-index>, f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */ 

static inline kk_box_t kk_std_core_hnd__perform3(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_context_t* _ctx) { /* forall<a,b,c,d,e,a1> (ev : ev<a1>, op : forall<e1,b1> (a1<e1,b1>) -> clause1<(a, b, c),d,a1,e1,b1>, x1 : a, x2 : b, x3 : c) -> e d */ 
  {
    struct kk_std_core_hnd_Ev* _con10677 = kk_std_core_hnd__as_Ev(ev);
    kk_std_core_hnd__marker m0 = _con10677->marker;
    kk_box_t h = _con10677->hnd;
    kk_box_dup(h);
    kk_std_core_hnd__clause1 _match_10554 = kk_function_call(kk_std_core_hnd__clause1, (kk_function_t, kk_box_t, kk_context_t*), op, (op, h, _ctx)); /*std/core/hnd/clause1<(6336, 6337, 6338),6339,6341,1105,1106>*/;
    {
      kk_function_t _fun_unbox_x10382 = _match_10554.clause;
      kk_box_t _x10678;
      kk_std_core_types__tuple3_ _x10679 = kk_std_core_types__new_dash__lp__comma__comma__rp_(x1, x2, x3, _ctx); /*(13, 14, 15)*/
      _x10678 = kk_std_core_types__tuple3__box(_x10679, _ctx); /*51*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x10382, (_fun_unbox_x10382, m0, ev, _x10678, _ctx));
    }
  }
}

kk_box_t kk_std_core_hnd__perform4(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_box_t x4, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,b1> (ev : ev<b1>, op : forall<e1,c1> (b1<e1,c1>) -> clause1<(a, b, c, d),a1,b1,e1,c1>, x1 : a, x2 : b, x3 : c, x4 : d) -> e a1 */ 

kk_box_t kk_std_core_hnd_yield_to(kk_std_core_hnd__marker m0, kk_function_t clause0, kk_context_t* _ctx); /* forall<a,e,b> (m : marker<e,b>, clause : ((resume-result<a,b>) -> e b) -> e b) -> e a */ 

kk_std_core_hnd__clause0 kk_std_core_hnd_clause_control_raw0(kk_function_t op, kk_context_t* _ctx); /* forall<a,e,e1,b,c> (op : (resume-context<a,e,e1,c>) -> e c) -> clause0<a,b,e,c> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_control_raw1(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,e,e1,c,d> (op : (x : a, r : resume-context<b,e,e1,d>) -> e d) -> clause1<a,b,c,e,d> */ 

kk_std_core_hnd__clause2 kk_std_core_hnd_clause_control_raw2(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,e,e1,d,a1> (op : (x1 : a, x2 : b, r : resume-context<c,e,e1,a1>) -> e a1) -> clause2<a,b,c,d,e,a1> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_control_raw3(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,e,e1,a1,b1> (op : (x1 : a, x2 : b, x3 : c, r : resume-context<d,e,e1,b1>) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */ 

kk_box_t kk_std_core_hnd_finalize(kk_function_t cont, kk_box_t res, kk_context_t* _ctx); /* forall<a,b,e,c> (cont : (() -> b) -> e c, res : a) -> e a */ 

static inline kk_box_t kk_std_core_hnd_finalize_1(kk_std_core_hnd__resume_context r, kk_box_t x, kk_context_t* _ctx) { /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : b) -> e b */ 
  kk_function_t _x10717 = kk_std_core_hnd_k(r, _ctx); /*(std/core/hnd/resume-result<2857,2860>) -> 2858 2860*/
  kk_std_core_hnd__resume_result _x10716 = kk_std_core_hnd__new_Finalize(x, _ctx); /*std/core/hnd/resume-result<80,81>*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _x10717, (_x10717, _x10716, _ctx));
}

kk_box_t kk_std_core_hnd_protect_check(kk_ref_t resumed, kk_function_t k0, kk_box_t res, kk_context_t* _ctx); /* forall<a,e,b> (resumed : ref<global,bool>, k : (resume-result<a,b>) -> e b, res : b) -> e b */ 

kk_box_t kk_std_core_hnd_protect(kk_box_t x, kk_function_t clause0, kk_function_t k0, kk_context_t* _ctx); /* forall<a,b,e,c> (x : a, clause : (x : a, k : (b) -> e c) -> e c, k : (resume-result<b,c>) -> e c) -> e c */ 

kk_box_t kk_std_core_hnd_protect_1(kk_box_t x1, kk_box_t x2, kk_function_t clause0, kk_function_t k0, kk_context_t* _ctx); /* forall<a,b,c,e,d> (x1 : a, x2 : b, clause : (x : a, x : b, k : (c) -> e d) -> e d, k : (resume-result<c,d>) -> e d) -> e d */ 

kk_std_core_hnd__clause0 kk_std_core_hnd_clause_control0(kk_function_t op, kk_context_t* _ctx); /* forall<a,e,b,c> (op : ((a) -> e c) -> e c) -> clause0<a,b,e,c> */ 


// lift anonymous function
struct kk_std_core_hnd_clause_control1_fun10731__t {
  struct kk_function_s _base;
  kk_function_t clause0;
};
extern kk_box_t kk_std_core_hnd_clause_control1_fun10731(kk_function_t _fself, kk_std_core_hnd__marker m0, kk_std_core_hnd__ev ___wildcard__573__16, kk_box_t x, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_control1_fun10731(kk_function_t clause0, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_control1_fun10731__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_control1_fun10731__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_control1_fun10731, kk_context());
  _self->clause0 = clause0;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_hnd_clause_control1_fun10732__t {
  struct kk_function_s _base;
  kk_function_t clause0;
  kk_box_t x;
};
extern kk_box_t kk_std_core_hnd_clause_control1_fun10732(kk_function_t _fself, kk_function_t k0, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_control1_fun10732(kk_function_t clause0, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_control1_fun10732__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_control1_fun10732__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_control1_fun10732, kk_context());
  _self->clause0 = clause0;
  _self->x = x;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause1 kk_std_core_hnd_clause_control1(kk_function_t clause0, kk_context_t* _ctx) { /* forall<a,b,e,c,d> (clause : (x : a, k : (b) -> e d) -> e d) -> clause1<a,b,c,e,d> */ 
  return kk_std_core_hnd__new_Clause1(kk_std_core_hnd_new_clause_control1_fun10731(clause0, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_hnd_clause_control2_fun10733__t {
  struct kk_function_s _base;
  kk_function_t clause0;
};
extern kk_box_t kk_std_core_hnd_clause_control2_fun10733(kk_function_t _fself, kk_std_core_hnd__marker m0, kk_std_core_hnd__ev ___wildcard__676__16, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_control2_fun10733(kk_function_t clause0, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_control2_fun10733__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_control2_fun10733__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_control2_fun10733, kk_context());
  _self->clause0 = clause0;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_hnd_clause_control2_fun10734__t {
  struct kk_function_s _base;
  kk_function_t clause0;
  kk_box_t x1;
  kk_box_t x2;
};
extern kk_box_t kk_std_core_hnd_clause_control2_fun10734(kk_function_t _fself, kk_function_t k0, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_control2_fun10734(kk_function_t clause0, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_control2_fun10734__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_control2_fun10734__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_control2_fun10734, kk_context());
  _self->clause0 = clause0;
  _self->x1 = x1;
  _self->x2 = x2;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause2 kk_std_core_hnd_clause_control2(kk_function_t clause0, kk_context_t* _ctx) { /* forall<a,b,c,e,d,a1> (clause : (x1 : a, x2 : b, k : (c) -> e a1) -> e a1) -> clause2<a,b,c,d,e,a1> */ 
  return kk_std_core_hnd__new_Clause2(kk_std_core_hnd_new_clause_control2_fun10733(clause0, _ctx), _ctx);
}

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_control3(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1,b1> (op : (x1 : a, x2 : b, x3 : c, k : (d) -> e b1) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_control4(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,b1,c1> (op : (x1 : a, x2 : b, x3 : c, x4 : d, k : (a1) -> e c1) -> e c1) -> clause1<(a, b, c, d),a1,b1,e,c1> */ 

kk_std_core_hnd__clause0 kk_std_core_hnd_clause_never0(kk_function_t op, kk_context_t* _ctx); /* forall<a,e,b,c> (op : () -> e c) -> clause0<a,b,e,c> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_never1(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,e,c,d> (op : (a) -> e d) -> clause1<a,b,c,e,d> */ 

kk_std_core_hnd__clause2 kk_std_core_hnd_clause_never2(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,e,d,a1> (op : (a, b) -> e a1) -> clause2<a,b,c,d,e,a1> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_never3(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1,b1> (op : (a, b, c) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_tail_noyield3(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1,b1> (op : (a, b, c) -> e d) -> clause1<(a, b, c),d,a1,e,b1> */ 

kk_box_t kk_std_core_hnd_under1x(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x, kk_context_t* _ctx); /* forall<a,b,e,c> (ev : ev<c>, op : (a) -> e b, x : a) -> e b */ 


// lift anonymous function
struct kk_std_core_hnd_under1_fun10784__t {
  struct kk_function_s _base;
  kk_std_core_hnd__ev ev;
};
extern kk_box_t kk_std_core_hnd_under1_fun10784(kk_function_t _fself, kk_function_t cont, kk_box_t res, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_under1_fun10784(kk_std_core_hnd__ev ev, kk_context_t* _ctx) {
  struct kk_std_core_hnd_under1_fun10784__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_under1_fun10784__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_under1_fun10784, kk_context());
  _self->ev = ev;
  return &_self->_base;
}


static inline kk_box_t kk_std_core_hnd_under1(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x, kk_context_t* _ctx) { /* forall<a,b,e,c> (ev : ev<c>, op : (a) -> e b, x : a) -> e b */ 
  kk_evv_t w0;
  kk_std_core_hnd__ev _x10783 = kk_std_core_hnd__ev_dup(ev); /*std/core/hnd/ev<8753>*/
  w0 = kk_std_core_hnd_evv_swap_with(_x10783, _ctx); /*std/core/hnd/evv<_8698>*/
  kk_box_t y = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), op, (op, x, _ctx)); /*8751*/;
  if (kk_yielding(kk_context())) {
    kk_evv_drop(w0, _ctx);
    kk_box_drop(y, _ctx);
    return kk_std_core_hnd_yield_cont(kk_std_core_hnd_new_under1_fun10784(ev, _ctx), _ctx);
  }
  {
    kk_std_core_hnd__ev_dropn(ev, ((int32_t)KI32(3)), _ctx);
    kk_unit_t __0 = kk_Unit;
    kk_evv_set(w0,kk_context());
    return y;
  }
}


// lift anonymous function
struct kk_std_core_hnd_under0_fun10786__t {
  struct kk_function_s _base;
  kk_std_core_hnd__ev ev;
};
extern kk_box_t kk_std_core_hnd_under0_fun10786(kk_function_t _fself, kk_function_t cont, kk_box_t res, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_under0_fun10786(kk_std_core_hnd__ev ev, kk_context_t* _ctx) {
  struct kk_std_core_hnd_under0_fun10786__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_under0_fun10786__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_under0_fun10786, kk_context());
  _self->ev = ev;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_hnd_under0_fun10788__t {
  struct kk_function_s _base;
  kk_std_core_hnd__ev ev;
};
extern kk_box_t kk_std_core_hnd_under0_fun10788(kk_function_t _fself, kk_function_t cont0, kk_box_t res0, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_under0_fun10788(kk_std_core_hnd__ev ev, kk_context_t* _ctx) {
  struct kk_std_core_hnd_under0_fun10788__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_under0_fun10788__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_under0_fun10788, kk_context());
  _self->ev = ev;
  return &_self->_base;
}


static inline kk_box_t kk_std_core_hnd_under0(kk_std_core_hnd__ev ev, kk_function_t op, kk_context_t* _ctx) { /* forall<a,e,b> (ev : ev<b>, op : () -> e a) -> e a */ 
  kk_evv_t w0;
  kk_std_core_hnd__ev _x10785 = kk_std_core_hnd__ev_dup(ev); /*std/core/hnd/ev<8806>*/
  w0 = kk_std_core_hnd_evv_swap_with(_x10785, _ctx); /*std/core/hnd/evv<_8758>*/
  kk_box_t y = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), op, (op, _ctx)); /*8804*/;
  kk_unit_t __ = kk_Unit;
  kk_evv_set(w0,kk_context());
  if (kk_yielding(kk_context())) {
    kk_box_drop(y, _ctx);
    return kk_std_core_hnd_yield_cont(kk_std_core_hnd_new_under0_fun10786(ev, _ctx), _ctx);
  }
  {
    kk_std_core_hnd__ev_dropn(ev, ((int32_t)KI32(3)), _ctx);
    return y;
  }
}

kk_std_core_hnd__clause0 kk_std_core_hnd_clause_tail0(kk_function_t op, kk_context_t* _ctx); /* forall<e,a,b,c> (op : () -> e b) -> clause0<b,c,e,a> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_tail1(kk_function_t op, kk_context_t* _ctx); /* forall<e,a,b,c,d> (op : (b) -> e c) -> clause1<b,c,d,e,a> */ 

kk_box_t kk_std_core_hnd_under2(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx); /* forall<a,b,c,e,d> (ev : ev<d>, op : (a, b) -> e c, x1 : a, x2 : b) -> e c */ 


// lift anonymous function
struct kk_std_core_hnd_clause_tail2_fun10801__t {
  struct kk_function_s _base;
  kk_function_t op;
};
extern kk_box_t kk_std_core_hnd_clause_tail2_fun10801(kk_function_t _fself, kk_std_core_hnd__marker m0, kk_std_core_hnd__ev ev, kk_box_t x1, kk_box_t x2, kk_context_t* _ctx);
static inline kk_function_t kk_std_core_hnd_new_clause_tail2_fun10801(kk_function_t op, kk_context_t* _ctx) {
  struct kk_std_core_hnd_clause_tail2_fun10801__t* _self = kk_function_alloc_as(struct kk_std_core_hnd_clause_tail2_fun10801__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_hnd_clause_tail2_fun10801, kk_context());
  _self->op = op;
  return &_self->_base;
}


static inline kk_std_core_hnd__clause2 kk_std_core_hnd_clause_tail2(kk_function_t op, kk_context_t* _ctx) { /* forall<e,a,b,c,d,a1> (op : (b, c) -> e d) -> clause2<b,c,d,a1,e,a> */ 
  return kk_std_core_hnd__new_Clause2(kk_std_core_hnd_new_clause_tail2_fun10801(op, _ctx), _ctx);
}

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_tail3(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1,b1> (op : (a, b, c) -> e d) -> clause1<(a, b, c),d,a1,e,b1> */ 

kk_std_core_hnd__clause1 kk_std_core_hnd_clause_tail4(kk_function_t op, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,b1,c1> (op : (a, b, c, d) -> e a1) -> clause1<(a, b, c, d),a1,b1,e,c1> */ 

kk_box_t kk_std_core_hnd_finally_prompt(kk_function_t fin, kk_box_t res, kk_context_t* _ctx); /* forall<a,e> (fin : () -> e (), res : a) -> e a */ 

static inline kk_box_t kk_std_core_hnd_finally(kk_function_t fin, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (fin : () -> e (), action : () -> e a) -> e a */ 
  kk_box_t _x10822 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx)); /*9629*/
  return kk_std_core_hnd_finally_prompt(fin, _x10822, _ctx);
}

kk_box_t kk_std_core_hnd_initially_prompt(kk_function_t init, kk_box_t res, kk_context_t* _ctx); /* forall<a,e> (init : (int) -> e (), res : a) -> e a */ 

kk_box_t kk_std_core_hnd_initially(kk_function_t init, kk_function_t action, kk_context_t* _ctx); /* forall<a,e> (init : (int) -> e (), action : () -> e a) -> e a */ 

kk_box_t kk_std_core_hnd_prompt_local_var(kk_ref_t loc, kk_box_t res, kk_context_t* _ctx); /* forall<a,b,h> (loc : local-var<h,a>, res : b) -> <div,local<h>> b */ 

static inline kk_box_t kk_std_core_hnd_local_var(kk_box_t init, kk_function_t action, kk_context_t* _ctx) { /* forall<a,b,e,h> (init : a, action : (l : local-var<h,a>) -> <local<h>|e> b) -> <local<h>|e> b */ 
  kk_ref_t loc = kk_ref_alloc(init,kk_context()); /*local-var<9998,9995>*/;
  kk_box_t res;
  kk_ref_t _x10841 = kk_ref_dup(loc); /*local-var<9998,9995>*/
  res = kk_function_call(kk_box_t, (kk_function_t, kk_ref_t, kk_context_t*), action, (action, _x10841, _ctx)); /*9996*/
  return kk_std_core_hnd_prompt_local_var(loc, res, _ctx);
}

kk_std_core_types__either kk_std_core_hnd_try_finalize_prompt(kk_box_t res, kk_context_t* _ctx); /* forall<a,e> (res : a) -> e either<yield-info,a> */ 

kk_box_t kk_std_core_hnd_under3(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_context_t* _ctx); /* forall<a,b,c,d,e,a1> (ev : ev<a1>, op : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e d */ 

kk_box_t kk_std_core_hnd_under4(kk_std_core_hnd__ev ev, kk_function_t op, kk_box_t x1, kk_box_t x2, kk_box_t x3, kk_box_t x4, kk_context_t* _ctx); /* forall<a,b,c,d,a1,e,b1> (ev : ev<b1>, op : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e a1 */ 

static inline kk_std_core_types__either kk_std_core_hnd_unsafe_try_finalize(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> e a) -> e either<yield-info,a> */ 
  kk_box_t _x10854 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx)); /*10226*/
  return kk_std_core_hnd_try_finalize_prompt(_x10854, _ctx);
}

static inline kk_box_t kk_std_core_hnd_yield_bind2(kk_box_t x, kk_function_t extend, kk_function_t next, kk_context_t* _ctx) { /* forall<a,b,e> (x : a, extend : (a) -> e b, next : (a) -> e b) -> e b */ 
  if (kk_yielding(kk_context())) {
    kk_function_drop(next, _ctx);
    kk_box_drop(x, _ctx);
    return kk_std_core_hnd_yield_extend(extend, _ctx);
  }
  {
    kk_function_drop(extend, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), next, (next, x, _ctx));
  }
}

void kk_std_core_hnd__init(kk_context_t* _ctx);


void kk_std_core_hnd__done(kk_context_t* _ctx);

#endif // header
