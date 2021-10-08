#pragma once
#ifndef kk_test_float_bench1_H
#define kk_test_float_bench1_H
// Koka generated module: "test/float/bench1", koka version: 2.3.2, platform: 64-bit
#include <kklib.h>
#include "std_core_types.h"
#include "std_core_hnd.h"
#include "std_core.h"
#include "std_text_parse.h"
#include "std_os_path.h"
#include "std_os_env.h"

// type declarations

// type test/float/bench1/.hnd-bra
struct kk_test_float_bench1__hnd_bra_s {
  kk_block_t _block;
};
typedef struct kk_test_float_bench1__hnd_bra_s* kk_test_float_bench1__hnd_bra;
struct kk_test_float_bench1__Hnd_bra {
  struct kk_test_float_bench1__hnd_bra_s _base;
  kk_std_core_hnd__clause0 fun_brara;
};
static inline kk_test_float_bench1__hnd_bra kk_test_float_bench1__base_Hnd_bra(struct kk_test_float_bench1__Hnd_bra* _x){
  return &_x->_base;
}
static inline kk_test_float_bench1__hnd_bra kk_test_float_bench1__new_Hnd_bra(kk_reuse_t _at, kk_std_core_hnd__clause0 fun_brara, kk_context_t* _ctx){
  struct kk_test_float_bench1__Hnd_bra* _con = kk_block_alloc_at_as(struct kk_test_float_bench1__Hnd_bra, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->fun_brara = fun_brara;
  return kk_test_float_bench1__base_Hnd_bra(_con);
}
static inline struct kk_test_float_bench1__Hnd_bra* kk_test_float_bench1__as_Hnd_bra(kk_test_float_bench1__hnd_bra x) {
  return kk_basetype_as_assert(struct kk_test_float_bench1__Hnd_bra*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_test_float_bench1__is_Hnd_bra(kk_test_float_bench1__hnd_bra x) {
  return (true);
}
static inline kk_test_float_bench1__hnd_bra kk_test_float_bench1__hnd_bra_dup(kk_test_float_bench1__hnd_bra _x) {
  return kk_basetype_dup_as(kk_test_float_bench1__hnd_bra, _x);
}
static inline void kk_test_float_bench1__hnd_bra_drop(kk_test_float_bench1__hnd_bra _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_test_float_bench1__hnd_bra_is_unique(kk_test_float_bench1__hnd_bra _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_test_float_bench1__hnd_bra_free(kk_test_float_bench1__hnd_bra _x) {
  kk_basetype_free(_x);
}
static inline void kk_test_float_bench1__hnd_bra_decref(kk_test_float_bench1__hnd_bra _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__hnd_bra_dropn_reuse(kk_test_float_bench1__hnd_bra _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_test_float_bench1__hnd_bra_dropn(kk_test_float_bench1__hnd_bra _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__hnd_bra_reuse(kk_test_float_bench1__hnd_bra _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_test_float_bench1__hnd_bra kk_test_float_bench1__hnd_bra_hole() {
  return (kk_test_float_bench1__hnd_bra)(1);
}
static inline kk_box_t kk_test_float_bench1__hnd_bra_box(kk_test_float_bench1__hnd_bra _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_test_float_bench1__hnd_bra kk_test_float_bench1__hnd_bra_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_test_float_bench1__hnd_bra, _x);
}

// type test/float/bench1/.hnd-count
struct kk_test_float_bench1__hnd_count_s {
  kk_block_t _block;
};
typedef struct kk_test_float_bench1__hnd_count_s* kk_test_float_bench1__hnd_count;
struct kk_test_float_bench1__Hnd_count {
  struct kk_test_float_bench1__hnd_count_s _base;
  kk_std_core_hnd__clause1 fun_one;
};
static inline kk_test_float_bench1__hnd_count kk_test_float_bench1__base_Hnd_count(struct kk_test_float_bench1__Hnd_count* _x){
  return &_x->_base;
}
static inline kk_test_float_bench1__hnd_count kk_test_float_bench1__new_Hnd_count(kk_reuse_t _at, kk_std_core_hnd__clause1 fun_one, kk_context_t* _ctx){
  struct kk_test_float_bench1__Hnd_count* _con = kk_block_alloc_at_as(struct kk_test_float_bench1__Hnd_count, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->fun_one = fun_one;
  return kk_test_float_bench1__base_Hnd_count(_con);
}
static inline struct kk_test_float_bench1__Hnd_count* kk_test_float_bench1__as_Hnd_count(kk_test_float_bench1__hnd_count x) {
  return kk_basetype_as_assert(struct kk_test_float_bench1__Hnd_count*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_test_float_bench1__is_Hnd_count(kk_test_float_bench1__hnd_count x) {
  return (true);
}
static inline kk_test_float_bench1__hnd_count kk_test_float_bench1__hnd_count_dup(kk_test_float_bench1__hnd_count _x) {
  return kk_basetype_dup_as(kk_test_float_bench1__hnd_count, _x);
}
static inline void kk_test_float_bench1__hnd_count_drop(kk_test_float_bench1__hnd_count _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_test_float_bench1__hnd_count_is_unique(kk_test_float_bench1__hnd_count _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_test_float_bench1__hnd_count_free(kk_test_float_bench1__hnd_count _x) {
  kk_basetype_free(_x);
}
static inline void kk_test_float_bench1__hnd_count_decref(kk_test_float_bench1__hnd_count _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__hnd_count_dropn_reuse(kk_test_float_bench1__hnd_count _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_test_float_bench1__hnd_count_dropn(kk_test_float_bench1__hnd_count _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__hnd_count_reuse(kk_test_float_bench1__hnd_count _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_test_float_bench1__hnd_count kk_test_float_bench1__hnd_count_hole() {
  return (kk_test_float_bench1__hnd_count)(1);
}
static inline kk_box_t kk_test_float_bench1__hnd_count_box(kk_test_float_bench1__hnd_count _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_test_float_bench1__hnd_count kk_test_float_bench1__hnd_count_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_test_float_bench1__hnd_count, _x);
}

// type test/float/bench1/bra
struct kk_test_float_bench1__bra_s {
  kk_block_t _block;
};
typedef struct kk_test_float_bench1__bra_s* kk_test_float_bench1__bra;
struct kk_test_float_bench1_Bra {
  struct kk_test_float_bench1__bra_s _base;
  kk_test_float_bench1__hnd_bra _field1;
};
static inline kk_test_float_bench1__bra kk_test_float_bench1__base_Bra(struct kk_test_float_bench1_Bra* _x){
  return &_x->_base;
}
static inline kk_test_float_bench1__bra kk_test_float_bench1__new_Bra(kk_reuse_t _at, kk_test_float_bench1__hnd_bra _field1, kk_context_t* _ctx){
  struct kk_test_float_bench1_Bra* _con = kk_block_alloc_at_as(struct kk_test_float_bench1_Bra, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->_field1 = _field1;
  return kk_test_float_bench1__base_Bra(_con);
}
static inline struct kk_test_float_bench1_Bra* kk_test_float_bench1__as_Bra(kk_test_float_bench1__bra x) {
  return kk_basetype_as_assert(struct kk_test_float_bench1_Bra*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_test_float_bench1__is_Bra(kk_test_float_bench1__bra x) {
  return (true);
}
static inline kk_test_float_bench1__bra kk_test_float_bench1__bra_dup(kk_test_float_bench1__bra _x) {
  return kk_basetype_dup_as(kk_test_float_bench1__bra, _x);
}
static inline void kk_test_float_bench1__bra_drop(kk_test_float_bench1__bra _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_test_float_bench1__bra_is_unique(kk_test_float_bench1__bra _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_test_float_bench1__bra_free(kk_test_float_bench1__bra _x) {
  kk_basetype_free(_x);
}
static inline void kk_test_float_bench1__bra_decref(kk_test_float_bench1__bra _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__bra_dropn_reuse(kk_test_float_bench1__bra _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_test_float_bench1__bra_dropn(kk_test_float_bench1__bra _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__bra_reuse(kk_test_float_bench1__bra _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_test_float_bench1__bra kk_test_float_bench1__bra_hole() {
  return (kk_test_float_bench1__bra)(1);
}
static inline kk_box_t kk_test_float_bench1__bra_box(kk_test_float_bench1__bra _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_test_float_bench1__bra kk_test_float_bench1__bra_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_test_float_bench1__bra, _x);
}

// type test/float/bench1/count
struct kk_test_float_bench1__count_s {
  kk_block_t _block;
};
typedef struct kk_test_float_bench1__count_s* kk_test_float_bench1__count;
struct kk_test_float_bench1_Count {
  struct kk_test_float_bench1__count_s _base;
  kk_test_float_bench1__hnd_count _field1;
};
static inline kk_test_float_bench1__count kk_test_float_bench1__base_Count(struct kk_test_float_bench1_Count* _x){
  return &_x->_base;
}
static inline kk_test_float_bench1__count kk_test_float_bench1__new_Count(kk_reuse_t _at, kk_test_float_bench1__hnd_count _field1, kk_context_t* _ctx){
  struct kk_test_float_bench1_Count* _con = kk_block_alloc_at_as(struct kk_test_float_bench1_Count, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->_field1 = _field1;
  return kk_test_float_bench1__base_Count(_con);
}
static inline struct kk_test_float_bench1_Count* kk_test_float_bench1__as_Count(kk_test_float_bench1__count x) {
  return kk_basetype_as_assert(struct kk_test_float_bench1_Count*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_test_float_bench1__is_Count(kk_test_float_bench1__count x) {
  return (true);
}
static inline kk_test_float_bench1__count kk_test_float_bench1__count_dup(kk_test_float_bench1__count _x) {
  return kk_basetype_dup_as(kk_test_float_bench1__count, _x);
}
static inline void kk_test_float_bench1__count_drop(kk_test_float_bench1__count _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_test_float_bench1__count_is_unique(kk_test_float_bench1__count _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_test_float_bench1__count_free(kk_test_float_bench1__count _x) {
  kk_basetype_free(_x);
}
static inline void kk_test_float_bench1__count_decref(kk_test_float_bench1__count _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__count_dropn_reuse(kk_test_float_bench1__count _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_test_float_bench1__count_dropn(kk_test_float_bench1__count _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_test_float_bench1__count_reuse(kk_test_float_bench1__count _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_test_float_bench1__count kk_test_float_bench1__count_hole() {
  return (kk_test_float_bench1__count)(1);
}
static inline kk_box_t kk_test_float_bench1__count_box(kk_test_float_bench1__count _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_test_float_bench1__count kk_test_float_bench1__count_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_test_float_bench1__count, _x);
}

// value declarations

extern kk_std_core_hnd__htag kk_test_float_bench1__tag_bra;

kk_box_t kk_test_float_bench1__handle_bra(int32_t cfc, kk_test_float_bench1__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 

extern kk_std_core_hnd__htag kk_test_float_bench1__tag_count;

kk_box_t kk_test_float_bench1__handle_count(int32_t cfc, kk_test_float_bench1__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
 
// select `brara` operation out of the `:bra` effect handler

static inline kk_std_core_hnd__clause0 kk_test_float_bench1__select_brara(kk_test_float_bench1__hnd_bra hnd, kk_context_t* _ctx) { /* forall<e,a> (hnd : .hnd-bra<e,a>) -> std/core/hnd/clause0<(),.hnd-bra,e,a> */ 
  {
    struct kk_test_float_bench1__Hnd_bra* _con1014 = kk_test_float_bench1__as_Hnd_bra(hnd);
    kk_std_core_hnd__clause0 fun_brara = _con1014->fun_brara;
    if (kk_likely(kk_test_float_bench1__hnd_bra_is_unique(hnd))) {
      kk_test_float_bench1__hnd_bra_free(hnd);
    }
    else {
      kk_std_core_hnd__clause0_dup(fun_brara);
      kk_test_float_bench1__hnd_bra_decref(hnd, _ctx);
    }
    return fun_brara;
  }
}
 
// select `one` operation out of the `:count` effect handler

static inline kk_std_core_hnd__clause1 kk_test_float_bench1__select_one(kk_test_float_bench1__hnd_count hnd, kk_context_t* _ctx) { /* forall<e,a> (hnd : .hnd-count<e,a>) -> std/core/hnd/clause1<int,int,.hnd-count,e,a> */ 
  {
    struct kk_test_float_bench1__Hnd_count* _con1015 = kk_test_float_bench1__as_Hnd_count(hnd);
    kk_std_core_hnd__clause1 fun_one = _con1015->fun_one;
    if (kk_likely(kk_test_float_bench1__hnd_count_is_unique(hnd))) {
      kk_test_float_bench1__hnd_count_free(hnd);
    }
    else {
      kk_std_core_hnd__clause1_dup(fun_one);
      kk_test_float_bench1__hnd_count_decref(hnd, _ctx);
    }
    return fun_one;
  }
}
 
// call `brara` operation of the `:bra` effect

static inline kk_unit_t kk_test_float_bench1_brara(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_732;
  kk_ssize_t _x1016 = ((kk_ssize_t)0); /*ssize_t*/
  ev_732 = kk_evv_at(_x1016,kk_context()); /*std/core/hnd/ev<test/float/bench1/.hnd-bra>*/
  kk_box_t _x1017;
  {
    struct kk_std_core_hnd_Ev* _con1018 = kk_std_core_hnd__as_Ev(ev_732);
    kk_std_core_hnd__marker m0 = _con1018->marker;
    kk_box_t _box_x805 = _con1018->hnd;
    kk_test_float_bench1__hnd_bra h = kk_test_float_bench1__hnd_bra_unbox(_box_x805, NULL);
    kk_test_float_bench1__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1007 = kk_test_float_bench1__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench1/.hnd-bra,161,162>*/;
    {
      kk_function_t _fun_unbox_x808 = _match_1007.clause;
      _x1017 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x808, (_fun_unbox_x808, m0, ev_732, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1017); return kk_Unit;
}

kk_unit_t kk_test_float_bench1_k(kk_context_t* _ctx); /* () -> bra () */ 
 
// call `one` operation of the `:count` effect

static inline kk_integer_t kk_test_float_bench1_one(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_std_core_hnd__ev ev_736;
  kk_ssize_t _x1024 = ((kk_ssize_t)0); /*ssize_t*/
  ev_736 = kk_evv_at(_x1024,kk_context()); /*std/core/hnd/ev<test/float/bench1/.hnd-count>*/
  kk_box_t _x1025;
  {
    struct kk_std_core_hnd_Ev* _con1026 = kk_std_core_hnd__as_Ev(ev_736);
    kk_std_core_hnd__marker m0 = _con1026->marker;
    kk_box_t _box_x817 = _con1026->hnd;
    kk_test_float_bench1__hnd_count h = kk_test_float_bench1__hnd_count_unbox(_box_x817, NULL);
    kk_test_float_bench1__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1005 = kk_test_float_bench1__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench1/.hnd-count,178,179>*/;
    {
      kk_function_t _fun_unbox_x821 = _match_1005.clause;
      _x1025 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x821, (_fun_unbox_x821, m0, ev_736, kk_integer_box(a), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1025);
}

kk_integer_t kk_test_float_bench1_one__(kk_integer_t a, kk_context_t* _ctx); /* (a : int) -> count int */ 
 
// monadic lift

static inline kk_integer_t kk_test_float_bench1__mlift719_f(kk_ref_t i, kk_unit_t wild__0, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_0 : ()) -> <local<h>,bra,div> int */ 
  kk_box_t _x1036 = (kk_ref_get(i,kk_context())); /*1000*/
  return kk_integer_unbox(_x1036);
}

kk_integer_t kk_test_float_bench1__mlift720_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_696, kk_context_t* _ctx); /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 

kk_integer_t kk_test_float_bench1__mlift721_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx); /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
 
// monadic lift

static inline bool kk_test_float_bench1__mlift722_f(kk_integer_t n, kk_integer_t _y_700, kk_context_t* _ctx) { /* forall<h> (n : int, int) -> <local<h>,div,bra,count> bool */ 
  kk_integer_t _x1045 = kk_integer_mul(n,(kk_integer_from_small(4)),kk_context()); /*int*/
  return kk_integer_lt(_y_700,_x1045,kk_context());
}
 
// monadic lift

static inline kk_unit_t kk_test_float_bench1__mlift723_f(kk_integer_t wild__2, kk_context_t* _ctx) { /* (wild_2 : int) -> count () */ 
  kk_integer_drop(wild__2, _ctx);
  kk_Unit; return kk_Unit;
}

kk_unit_t kk_test_float_bench1__mlift724_f(kk_integer_t a3, kk_context_t* _ctx); /* (a3 : int) -> count () */ 

kk_unit_t kk_test_float_bench1__mlift725_f(kk_integer_t a2, kk_context_t* _ctx); /* (a2 : int) -> count () */ 

kk_unit_t kk_test_float_bench1__mlift726_f(kk_integer_t a1, kk_context_t* _ctx); /* (a1 : int) -> count () */ 

kk_unit_t kk_test_float_bench1__mlift727_f(kk_integer_t a0, kk_context_t* _ctx); /* forall<h> (a0 : int) -> <local<h>,count,bra,div> () */ 

kk_unit_t kk_test_float_bench1__mlift728_f(kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx); /* forall<h> (i : local-var<h,int>, wild_1 : ()) -> <bra,count,div,local<h>> () */ 

kk_integer_t kk_test_float_bench1_f(kk_integer_t n, kk_context_t* _ctx); /* (n : int) -> div int */ 
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift729_main_fun1126__t {
  struct kk_function_s _base;
  kk_integer_t n;
};
extern kk_box_t kk_test_float_bench1__mlift729_main_fun1126(kk_function_t _fself, kk_context_t* _ctx);
static inline kk_function_t kk_test_float_bench1__new_mlift729_main_fun1126(kk_integer_t n, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift729_main_fun1126__t* _self = kk_function_alloc_as(struct kk_test_float_bench1__mlift729_main_fun1126__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1__mlift729_main_fun1126, kk_context());
  _self->n = n;
  return &_self->_base;
}


static inline kk_unit_t kk_test_float_bench1__mlift729_main(kk_integer_t n, kk_context_t* _ctx) { /* (n : int) -> exn () */ 
  kk_box_t _x1125 = kk_std_core_hnd__open_none0(kk_test_float_bench1__new_mlift729_main_fun1126(n, _ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1125); return kk_Unit;
}

kk_unit_t kk_test_float_bench1_main(kk_context_t* _ctx); /* () -> <pure,ndet> () */ 

kk_unit_t kk_test_float_bench1__hmain(kk_context_t* _ctx); /* () -> <console,div,ndet> () */ 

void kk_test_float_bench1__init(kk_context_t* _ctx);


void kk_test_float_bench1__done(kk_context_t* _ctx);

#endif // header
