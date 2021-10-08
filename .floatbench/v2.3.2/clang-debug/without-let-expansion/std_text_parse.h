#pragma once
#ifndef kk_std_text_parse_H
#define kk_std_text_parse_H
// Koka generated module: "std/text/parse", koka version: 2.3.2, platform: 64-bit
#include <kklib.h>
#include "std_core_types.h"
#include "std_core_hnd.h"
#include "std_core.h"

// type declarations

// type std/text/parse/.hnd-parse
struct kk_std_text_parse__hnd_parse_s {
  kk_block_t _block;
};
typedef struct kk_std_text_parse__hnd_parse_s* kk_std_text_parse__hnd_parse;
struct kk_std_text_parse__Hnd_parse {
  struct kk_std_text_parse__hnd_parse_s _base;
  kk_std_core_hnd__clause0 fun_current_input;
  kk_std_core_hnd__clause1 ctl_fail;
  kk_std_core_hnd__clause0 ctl_pick;
  kk_std_core_hnd__clause1 fun_satisfy;
};
static inline kk_std_text_parse__hnd_parse kk_std_text_parse__base_Hnd_parse(struct kk_std_text_parse__Hnd_parse* _x){
  return &_x->_base;
}
static inline kk_std_text_parse__hnd_parse kk_std_text_parse__new_Hnd_parse(kk_reuse_t _at, kk_std_core_hnd__clause0 fun_current_input, kk_std_core_hnd__clause1 ctl_fail, kk_std_core_hnd__clause0 ctl_pick, kk_std_core_hnd__clause1 fun_satisfy, kk_context_t* _ctx){
  struct kk_std_text_parse__Hnd_parse* _con = kk_block_alloc_at_as(struct kk_std_text_parse__Hnd_parse, _at, 4 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->fun_current_input = fun_current_input;
  _con->ctl_fail = ctl_fail;
  _con->ctl_pick = ctl_pick;
  _con->fun_satisfy = fun_satisfy;
  return kk_std_text_parse__base_Hnd_parse(_con);
}
static inline struct kk_std_text_parse__Hnd_parse* kk_std_text_parse__as_Hnd_parse(kk_std_text_parse__hnd_parse x) {
  return kk_basetype_as_assert(struct kk_std_text_parse__Hnd_parse*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_text_parse__is_Hnd_parse(kk_std_text_parse__hnd_parse x) {
  return (true);
}
static inline kk_std_text_parse__hnd_parse kk_std_text_parse__hnd_parse_dup(kk_std_text_parse__hnd_parse _x) {
  return kk_basetype_dup_as(kk_std_text_parse__hnd_parse, _x);
}
static inline void kk_std_text_parse__hnd_parse_drop(kk_std_text_parse__hnd_parse _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_text_parse__hnd_parse_is_unique(kk_std_text_parse__hnd_parse _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_text_parse__hnd_parse_free(kk_std_text_parse__hnd_parse _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_text_parse__hnd_parse_decref(kk_std_text_parse__hnd_parse _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__hnd_parse_dropn_reuse(kk_std_text_parse__hnd_parse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_text_parse__hnd_parse_dropn(kk_std_text_parse__hnd_parse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__hnd_parse_reuse(kk_std_text_parse__hnd_parse _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_text_parse__hnd_parse kk_std_text_parse__hnd_parse_hole() {
  return (kk_std_text_parse__hnd_parse)(1);
}
static inline kk_box_t kk_std_text_parse__hnd_parse_box(kk_std_text_parse__hnd_parse _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_text_parse__hnd_parse kk_std_text_parse__hnd_parse_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_text_parse__hnd_parse, _x);
}

// type std/text/parse/parse
struct kk_std_text_parse__parse_s {
  kk_block_t _block;
};
typedef struct kk_std_text_parse__parse_s* kk_std_text_parse__parse;
struct kk_std_text_parse_Parse {
  struct kk_std_text_parse__parse_s _base;
  kk_std_text_parse__hnd_parse _field1;
};
static inline kk_std_text_parse__parse kk_std_text_parse__base_Parse(struct kk_std_text_parse_Parse* _x){
  return &_x->_base;
}
static inline kk_std_text_parse__parse kk_std_text_parse__new_Parse(kk_reuse_t _at, kk_std_text_parse__hnd_parse _field1, kk_context_t* _ctx){
  struct kk_std_text_parse_Parse* _con = kk_block_alloc_at_as(struct kk_std_text_parse_Parse, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->_field1 = _field1;
  return kk_std_text_parse__base_Parse(_con);
}
static inline struct kk_std_text_parse_Parse* kk_std_text_parse__as_Parse(kk_std_text_parse__parse x) {
  return kk_basetype_as_assert(struct kk_std_text_parse_Parse*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_text_parse__is_Parse(kk_std_text_parse__parse x) {
  return (true);
}
static inline kk_std_text_parse__parse kk_std_text_parse__parse_dup(kk_std_text_parse__parse _x) {
  return kk_basetype_dup_as(kk_std_text_parse__parse, _x);
}
static inline void kk_std_text_parse__parse_drop(kk_std_text_parse__parse _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_text_parse__parse_is_unique(kk_std_text_parse__parse _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_text_parse__parse_free(kk_std_text_parse__parse _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_text_parse__parse_decref(kk_std_text_parse__parse _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__parse_dropn_reuse(kk_std_text_parse__parse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_text_parse__parse_dropn(kk_std_text_parse__parse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__parse_reuse(kk_std_text_parse__parse _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_text_parse__parse kk_std_text_parse__parse_hole() {
  return (kk_std_text_parse__parse)(1);
}
static inline kk_box_t kk_std_text_parse__parse_box(kk_std_text_parse__parse _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_text_parse__parse kk_std_text_parse__parse_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_text_parse__parse, _x);
}

// type std/text/parse/parse-error
struct kk_std_text_parse__parse_error_s {
  kk_block_t _block;
};
typedef struct kk_std_text_parse__parse_error_s* kk_std_text_parse__parse_error;
struct kk_std_text_parse_ParseOk {
  struct kk_std_text_parse__parse_error_s _base;
  kk_box_t result;
  kk_std_core__sslice rest;
};
struct kk_std_text_parse_ParseError {
  struct kk_std_text_parse__parse_error_s _base;
  kk_string_t msg;
  kk_std_core__sslice rest;
};
static inline kk_std_text_parse__parse_error kk_std_text_parse__base_ParseOk(struct kk_std_text_parse_ParseOk* _x){
  return &_x->_base;
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__new_ParseOk(kk_reuse_t _at, kk_box_t result, kk_std_core__sslice rest, kk_context_t* _ctx){
  struct kk_std_text_parse_ParseOk* _con = kk_block_alloc_at_as(struct kk_std_text_parse_ParseOk, _at, 2 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->result = result;
  _con->rest = rest;
  return kk_std_text_parse__base_ParseOk(_con);
}
static inline struct kk_std_text_parse_ParseOk* kk_std_text_parse__as_ParseOk(kk_std_text_parse__parse_error x) {
  return kk_basetype_as_assert(struct kk_std_text_parse_ParseOk*, x, (kk_tag_t)(1) /* _tag */);
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__base_ParseError(struct kk_std_text_parse_ParseError* _x){
  return &_x->_base;
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__new_ParseError(kk_reuse_t _at, kk_string_t msg, kk_std_core__sslice rest, kk_context_t* _ctx){
  struct kk_std_text_parse_ParseError* _con = kk_block_alloc_at_as(struct kk_std_text_parse_ParseError, _at, 2 /* scan count */, (kk_tag_t)(2), _ctx);
  _con->msg = msg;
  _con->rest = rest;
  return kk_std_text_parse__base_ParseError(_con);
}
static inline struct kk_std_text_parse_ParseError* kk_std_text_parse__as_ParseError(kk_std_text_parse__parse_error x) {
  return kk_basetype_as_assert(struct kk_std_text_parse_ParseError*, x, (kk_tag_t)(2) /* _tag */);
}
static inline bool kk_std_text_parse__is_ParseOk(kk_std_text_parse__parse_error x) {
  return (kk_basetype_has_tag(x, (kk_tag_t)(1)));
}
static inline bool kk_std_text_parse__is_ParseError(kk_std_text_parse__parse_error x) {
  return (kk_basetype_has_tag(x, (kk_tag_t)(2)));
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__parse_error_dup(kk_std_text_parse__parse_error _x) {
  return kk_basetype_dup_as(kk_std_text_parse__parse_error, _x);
}
static inline void kk_std_text_parse__parse_error_drop(kk_std_text_parse__parse_error _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_text_parse__parse_error_is_unique(kk_std_text_parse__parse_error _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_text_parse__parse_error_free(kk_std_text_parse__parse_error _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_text_parse__parse_error_decref(kk_std_text_parse__parse_error _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__parse_error_dropn_reuse(kk_std_text_parse__parse_error _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_text_parse__parse_error_dropn(kk_std_text_parse__parse_error _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_text_parse__parse_error_reuse(kk_std_text_parse__parse_error _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__parse_error_hole() {
  return (kk_std_text_parse__parse_error)(1);
}
static inline kk_box_t kk_std_text_parse__parse_error_box(kk_std_text_parse__parse_error _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_text_parse__parse_error kk_std_text_parse__parse_error_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_text_parse__parse_error, _x);
}

// value declarations
 
// Automatically generated. Retrieves the `rest` constructor field of the `:parse-error` type.

static inline kk_std_core__sslice kk_std_text_parse_rest(kk_std_text_parse__parse_error _this, kk_context_t* _ctx) { /* forall<a> (parse-error<a>) -> sslice */ 
  if (kk_std_text_parse__is_ParseOk(_this)) {
    struct kk_std_text_parse_ParseOk* _con3391 = kk_std_text_parse__as_ParseOk(_this);
    kk_box_t _pat0 = _con3391->result;
    kk_std_core__sslice _x = _con3391->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_this))) {
      kk_box_drop(_pat0, _ctx);
      kk_std_text_parse__parse_error_free(_this);
    }
    else {
      kk_std_core__sslice_dup(_x);
      kk_std_text_parse__parse_error_decref(_this, _ctx);
    }
    return _x;
  }
  {
    struct kk_std_text_parse_ParseError* _con3392 = kk_std_text_parse__as_ParseError(_this);
    kk_string_t _pat5 = _con3392->msg;
    kk_std_core__sslice _x0 = _con3392->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_this))) {
      kk_string_drop(_pat5, _ctx);
      kk_std_text_parse__parse_error_free(_this);
    }
    else {
      kk_std_core__sslice_dup(_x0);
      kk_std_text_parse__parse_error_decref(_this, _ctx);
    }
    return _x0;
  }
}
 
// Automatically generated. Tests for the `ParseOk` constructor of the `:parse-error` type.

static inline bool kk_std_text_parse_is_parseOk(kk_std_text_parse__parse_error parse_error, kk_context_t* _ctx) { /* forall<a> (parse-error : parse-error<a>) -> bool */ 
  if (kk_std_text_parse__is_ParseOk(parse_error)) {
    struct kk_std_text_parse_ParseOk* _con3393 = kk_std_text_parse__as_ParseOk(parse_error);
    kk_std_core__sslice _pat1 = _con3393->rest;
    kk_std_text_parse__parse_error_dropn(parse_error, ((int32_t)KI32(2)), _ctx);
    return true;
  }
  {
    kk_std_text_parse__parse_error_drop(parse_error, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `ParseError` constructor of the `:parse-error` type.

static inline bool kk_std_text_parse_is_parseError(kk_std_text_parse__parse_error parse_error, kk_context_t* _ctx) { /* forall<a> (parse-error : parse-error<a>) -> bool */ 
  if (kk_std_text_parse__is_ParseError(parse_error)) {
    struct kk_std_text_parse_ParseError* _con3394 = kk_std_text_parse__as_ParseError(parse_error);
    kk_std_core__sslice _pat1 = _con3394->rest;
    kk_std_text_parse__parse_error_dropn(parse_error, ((int32_t)KI32(2)), _ctx);
    return true;
  }
  {
    kk_std_text_parse__parse_error_drop(parse_error, _ctx);
    return false;
  }
}

extern kk_std_core_hnd__htag kk_std_text_parse__tag_parse;

kk_box_t kk_std_text_parse__handle_parse(int32_t cfc, kk_std_text_parse__hnd_parse hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx); /* forall<a,e,b> (cfc : int32, hnd : .hnd-parse<e,b>, ret : (res : a) -> e b, action : () -> <parse|e> a) -> e b */ 
 
// select `current-input` operation out of the `:parse` effect handler

static inline kk_std_core_hnd__clause0 kk_std_text_parse__select_current_input(kk_std_text_parse__hnd_parse hnd, kk_context_t* _ctx) { /* forall<e,a> (hnd : .hnd-parse<e,a>) -> std/core/hnd/clause0<sslice,.hnd-parse,e,a> */ 
  {
    struct kk_std_text_parse__Hnd_parse* _con3398 = kk_std_text_parse__as_Hnd_parse(hnd);
    kk_std_core_hnd__clause0 fun_current_input = _con3398->fun_current_input;
    kk_std_core_hnd__clause1 _pat0 = _con3398->ctl_fail;
    kk_std_core_hnd__clause0 _pat1 = _con3398->ctl_pick;
    kk_std_core_hnd__clause1 _pat2 = _con3398->fun_satisfy;
    if (kk_likely(kk_std_text_parse__hnd_parse_is_unique(hnd))) {
      kk_std_core_hnd__clause1_drop(_pat2, _ctx);
      kk_std_core_hnd__clause0_drop(_pat1, _ctx);
      kk_std_core_hnd__clause1_drop(_pat0, _ctx);
      kk_std_text_parse__hnd_parse_free(hnd);
    }
    else {
      kk_std_core_hnd__clause0_dup(fun_current_input);
      kk_std_text_parse__hnd_parse_decref(hnd, _ctx);
    }
    return fun_current_input;
  }
}
 
// select `fail` operation out of the `:parse` effect handler

static inline kk_std_core_hnd__clause1 kk_std_text_parse__select_fail(kk_std_text_parse__hnd_parse hnd, kk_context_t* _ctx) { /* forall<a,e,b> (hnd : .hnd-parse<e,b>) -> std/core/hnd/clause1<string,a,.hnd-parse,e,b> */ 
  {
    struct kk_std_text_parse__Hnd_parse* _con3399 = kk_std_text_parse__as_Hnd_parse(hnd);
    kk_std_core_hnd__clause0 _pat0 = _con3399->fun_current_input;
    kk_std_core_hnd__clause1 ctl_fail = _con3399->ctl_fail;
    kk_std_core_hnd__clause0 _pat1 = _con3399->ctl_pick;
    kk_std_core_hnd__clause1 _pat2 = _con3399->fun_satisfy;
    if (kk_likely(kk_std_text_parse__hnd_parse_is_unique(hnd))) {
      kk_std_core_hnd__clause1_drop(_pat2, _ctx);
      kk_std_core_hnd__clause0_drop(_pat1, _ctx);
      kk_std_core_hnd__clause0_drop(_pat0, _ctx);
      kk_std_text_parse__hnd_parse_free(hnd);
    }
    else {
      kk_std_core_hnd__clause1_dup(ctl_fail);
      kk_std_text_parse__hnd_parse_decref(hnd, _ctx);
    }
    return ctl_fail;
  }
}
 
// select `pick` operation out of the `:parse` effect handler

static inline kk_std_core_hnd__clause0 kk_std_text_parse__select_pick(kk_std_text_parse__hnd_parse hnd, kk_context_t* _ctx) { /* forall<e,a> (hnd : .hnd-parse<e,a>) -> std/core/hnd/clause0<bool,.hnd-parse,e,a> */ 
  {
    struct kk_std_text_parse__Hnd_parse* _con3400 = kk_std_text_parse__as_Hnd_parse(hnd);
    kk_std_core_hnd__clause0 _pat0 = _con3400->fun_current_input;
    kk_std_core_hnd__clause1 _pat1 = _con3400->ctl_fail;
    kk_std_core_hnd__clause0 ctl_pick = _con3400->ctl_pick;
    kk_std_core_hnd__clause1 _pat2 = _con3400->fun_satisfy;
    if (kk_likely(kk_std_text_parse__hnd_parse_is_unique(hnd))) {
      kk_std_core_hnd__clause1_drop(_pat2, _ctx);
      kk_std_core_hnd__clause1_drop(_pat1, _ctx);
      kk_std_core_hnd__clause0_drop(_pat0, _ctx);
      kk_std_text_parse__hnd_parse_free(hnd);
    }
    else {
      kk_std_core_hnd__clause0_dup(ctl_pick);
      kk_std_text_parse__hnd_parse_decref(hnd, _ctx);
    }
    return ctl_pick;
  }
}
 
// select `satisfy` operation out of the `:parse` effect handler

static inline kk_std_core_hnd__clause1 kk_std_text_parse__select_satisfy(kk_std_text_parse__hnd_parse hnd, kk_context_t* _ctx) { /* forall<a,e,b> (hnd : .hnd-parse<e,b>) -> std/core/hnd/clause1<(sslice) -> total maybe<(a, sslice)>,maybe<a>,.hnd-parse,e,b> */ 
  {
    struct kk_std_text_parse__Hnd_parse* _con3401 = kk_std_text_parse__as_Hnd_parse(hnd);
    kk_std_core_hnd__clause0 _pat0 = _con3401->fun_current_input;
    kk_std_core_hnd__clause1 _pat1 = _con3401->ctl_fail;
    kk_std_core_hnd__clause0 _pat2 = _con3401->ctl_pick;
    kk_std_core_hnd__clause1 fun_satisfy = _con3401->fun_satisfy;
    if (kk_likely(kk_std_text_parse__hnd_parse_is_unique(hnd))) {
      kk_std_core_hnd__clause0_drop(_pat2, _ctx);
      kk_std_core_hnd__clause1_drop(_pat1, _ctx);
      kk_std_core_hnd__clause0_drop(_pat0, _ctx);
      kk_std_text_parse__hnd_parse_free(hnd);
    }
    else {
      kk_std_core_hnd__clause1_dup(fun_satisfy);
      kk_std_text_parse__hnd_parse_decref(hnd, _ctx);
    }
    return fun_satisfy;
  }
}

kk_std_core_types__either kk_std_text_parse_either(kk_std_text_parse__parse_error perr, kk_context_t* _ctx); /* forall<a> (perr : parse-error<a>) -> either<string,a> */ 
 
// call `fail` operation of the `:parse` effect

static inline kk_box_t kk_std_text_parse_fail(kk_string_t msg, kk_context_t* _ctx) { /* forall<a> (msg : string) -> parse a */ 
  kk_std_core_hnd__ev ev_2439;
  kk_ssize_t _x3404 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2439 = kk_evv_at(_x3404,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  {
    struct kk_std_core_hnd_Ev* _con3405 = kk_std_core_hnd__as_Ev(ev_2439);
    kk_std_core_hnd__marker m0 = _con3405->marker;
    kk_box_t _box_x2616 = _con3405->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2616, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause1 _match_3390 = kk_std_text_parse__select_fail(h, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
    {
      kk_function_t _fun_unbox_x2620 = _match_3390.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2620, (_fun_unbox_x2620, m0, ev_2439, kk_string_box(msg), _ctx));
    }
  }
}
 
// call `satisfy` operation of the `:parse` effect


// lift anonymous function
struct kk_std_text_parse_satisfy_fun3411__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
extern kk_box_t kk_std_text_parse_satisfy_fun3411(kk_function_t _fself, kk_box_t _b_2637, kk_context_t* _ctx);
static inline kk_function_t kk_std_text_parse_new_satisfy_fun3411(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fun3411__t* _self = kk_function_alloc_as(struct kk_std_text_parse_satisfy_fun3411__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_satisfy_fun3411, kk_context());
  _self->pred = pred;
  return &_self->_base;
}


static inline kk_std_core_types__maybe kk_std_text_parse_satisfy(kk_function_t pred, kk_context_t* _ctx) { /* forall<a> (pred : (sslice) -> total maybe<(a, sslice)>) -> parse maybe<a> */ 
  kk_std_core_hnd__ev ev_2442;
  kk_ssize_t _x3407 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2442 = kk_evv_at(_x3407,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_box_t _x3408;
  {
    struct kk_std_core_hnd_Ev* _con3409 = kk_std_core_hnd__as_Ev(ev_2442);
    kk_std_core_hnd__marker m0 = _con3409->marker;
    kk_box_t _box_x2624 = _con3409->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2624, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause1 _match_3389 = kk_std_text_parse__select_satisfy(h, _ctx); /*std/core/hnd/clause1<(sslice) -> total maybe<(438, sslice)>,maybe<438>,std/text/parse/.hnd-parse,439,440>*/;
    {
      kk_function_t _fun_unbox_x2631 = _match_3389.clause;
      _x3408 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2631, (_fun_unbox_x2631, m0, ev_2442, kk_function_box(kk_std_text_parse_new_satisfy_fun3411(pred, _ctx)), _ctx)); /*1011*/
    }
  }
  return kk_std_core_types__maybe_unbox(_x3408, _ctx);
}

kk_box_t kk_std_text_parse__mlift2415_satisfy_fail(kk_string_t msg, kk_std_core_types__maybe _y_2328, kk_context_t* _ctx); /* forall<a> (msg : string, maybe<a>) -> parse a */ 

kk_box_t kk_std_text_parse_satisfy_fail(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx); /* forall<a> (msg : string, pred : (sslice) -> maybe<(a, sslice)>) -> parse a */ 

kk_char_t kk_std_text_parse_char_is(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx); /* (msg : string, pred : (char) -> bool) -> parse char */ 

kk_char_t kk_std_text_parse_alpha(kk_context_t* _ctx); /* () -> parse char */ 

kk_char_t kk_std_text_parse_alpha_num(kk_context_t* _ctx); /* () -> parse char */ 

kk_char_t kk_std_text_parse_char(kk_char_t c, kk_context_t* _ctx); /* (c : char) -> parse char */ 

kk_std_core_types__tuple2_ kk_std_text_parse_next_while0(kk_std_core__sslice slice, kk_function_t pred, kk_std_core__list acc, kk_context_t* _ctx); /* (slice : sslice, pred : (char) -> bool, acc : list<char>) -> (list<char>, sslice) */ 

kk_std_core__list kk_std_text_parse_chars_are(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx); /* (msg : string, pred : (char) -> bool) -> parse list<char> */ 
 
// call `pick` operation of the `:parse` effect

static inline bool kk_std_text_parse_pick(kk_context_t* _ctx) { /* () -> parse bool */ 
  kk_std_core_hnd__ev ev_2457;
  kk_ssize_t _x3482 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2457 = kk_evv_at(_x3482,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_box_t _x3483;
  {
    struct kk_std_core_hnd_Ev* _con3484 = kk_std_core_hnd__as_Ev(ev_2457);
    kk_std_core_hnd__marker m0 = _con3484->marker;
    kk_box_t _box_x2749 = _con3484->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2749, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause0 _match_3378 = kk_std_text_parse__select_pick(h, _ctx); /*std/core/hnd/clause0<bool,std/text/parse/.hnd-parse,417,418>*/;
    {
      kk_function_t _fun_unbox_x2752 = _match_3378.clause;
      _x3483 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x2752, (_fun_unbox_x2752, m0, ev_2457, _ctx)); /*1006*/
    }
  }
  return kk_bool_unbox(_x3483);
}

kk_box_t kk_std_text_parse__mlift2416_choose(kk_function_t p0, kk_std_core__list pp, bool _y_2341, kk_context_t* _ctx); /* forall<a,e> (p0 : parser<e,a>, pp : list<parser<e,a>>, bool) -> <parse|e> a */ 

kk_box_t kk_std_text_parse_choose(kk_std_core__list ps, kk_context_t* _ctx); /* forall<a,e> (ps : list<parser<e,a>>) -> <parse|e> a */ 

kk_std_core__list kk_std_text_parse__mlift2417_count_acc(kk_std_core__list acc, kk_integer_t n, kk_function_t p, kk_box_t x, kk_context_t* _ctx); /* forall<a,e> (acc : list<a>, n : int, p : parser<e,a>, x : a) -> <parse|e> list<a> */ 

kk_std_core__list kk_std_text_parse_count_acc(kk_integer_t n0, kk_std_core__list acc0, kk_function_t p0, kk_context_t* _ctx); /* forall<a,e> (n : int, acc : list<a>, p : parser<e,a>) -> <parse|e> list<a> */ 

static inline kk_std_core__list kk_std_text_parse_count(kk_integer_t n, kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (n : int, p : parser<e,a>) -> <parse|e> list<a> */ 
  return kk_std_text_parse_count_acc(n, kk_std_core__new_Nil(_ctx), p, _ctx);
}
 
// call `current-input` operation of the `:parse` effect

static inline kk_std_core__sslice kk_std_text_parse_current_input(kk_context_t* _ctx) { /* () -> parse sslice */ 
  kk_std_core_hnd__ev ev_2471;
  kk_ssize_t _x3517 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2471 = kk_evv_at(_x3517,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_box_t _x3518;
  {
    struct kk_std_core_hnd_Ev* _con3519 = kk_std_core_hnd__as_Ev(ev_2471);
    kk_std_core_hnd__marker m0 = _con3519->marker;
    kk_box_t _box_x2787 = _con3519->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2787, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause0 _match_3372 = kk_std_text_parse__select_current_input(h, _ctx); /*std/core/hnd/clause0<sslice,std/text/parse/.hnd-parse,378,379>*/;
    {
      kk_function_t _fun_unbox_x2790 = _match_3372.clause;
      _x3518 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x2790, (_fun_unbox_x2790, m0, ev_2471, _ctx)); /*1006*/
    }
  }
  return kk_std_core__sslice_unbox(_x3518, _ctx);
}
 
// monadic lift

static inline kk_integer_t kk_std_text_parse__mlift2418_digit(kk_char_t c00, kk_context_t* _ctx) { /* (c00 : char) -> parse int */ 
  kk_char_t _x3521;
  kk_integer_t _x3522;
  kk_integer_t _x3523 = kk_integer_from_int(c00,kk_context()); /*int*/
  kk_integer_t _x3524 = kk_integer_from_int(('0'),kk_context()); /*int*/
  _x3522 = kk_integer_sub(_x3523,_x3524,kk_context()); /*int*/
  _x3521 = kk_integer_clamp32(_x3522,kk_context()); /*char*/
  return kk_integer_from_int(_x3521,kk_context());
}

kk_integer_t kk_std_text_parse_digit(kk_context_t* _ctx); /* () -> parse int */ 

kk_string_t kk_std_text_parse_digits(kk_context_t* _ctx); /* () -> parse string */ 
 
// monadic lift

static inline kk_box_t kk_std_text_parse__mlift2419_op(kk_function_t p1, kk_function_t p2, bool _y_2354, kk_context_t* _ctx) { /* forall<a,e> (p1 : parser<e,a>, p2 : parser<e,a>, bool) -> <parse|e> a */ 
  if (_y_2354) {
    kk_function_drop(p2, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p1, (p1, _ctx));
  }
  {
    kk_function_drop(p1, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p2, (p2, _ctx));
  }
}

kk_box_t kk_std_text_parse__lp__bar__bar__rp_(kk_function_t p1, kk_function_t p2, kk_context_t* _ctx); /* forall<a,e> (p1 : parser<e,a>, p2 : parser<e,a>) -> <parse|e> a */ 


// lift anonymous function
struct kk_std_text_parse_optional_fun3568__t {
  struct kk_function_s _base;
  kk_box_t kkloc_default;
};
extern kk_box_t kk_std_text_parse_optional_fun3568(kk_function_t _fself, kk_context_t* _ctx);
static inline kk_function_t kk_std_text_parse_new_optional_fun3568(kk_box_t kkloc_default, kk_context_t* _ctx) {
  struct kk_std_text_parse_optional_fun3568__t* _self = kk_function_alloc_as(struct kk_std_text_parse_optional_fun3568__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_optional_fun3568, kk_context());
  _self->kkloc_default = kkloc_default;
  return &_self->_base;
}


static inline kk_box_t kk_std_text_parse_optional(kk_box_t kkloc_default, kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (default : a, p : parser<e,a>) -> <parse|e> a */ 
  return kk_std_text_parse__lp__bar__bar__rp_(p, kk_std_text_parse_new_optional_fun3568(kkloc_default, _ctx), _ctx);
}

kk_string_t kk_std_text_parse_digits0(kk_context_t* _ctx); /* () -> parse string */ 

kk_unit_t kk_std_text_parse__mlift2420_eof(kk_std_core_types__maybe _y_2361, kk_context_t* _ctx); /* (maybe<()>) -> parse () */ 

kk_unit_t kk_std_text_parse_eof(kk_context_t* _ctx); /* () -> parse () */ 

kk_string_t kk_std_text_parse_hex_digits(kk_context_t* _ctx); /* () -> parse string */ 

kk_std_core__list kk_std_text_parse__mlift2421_many_acc(kk_std_core__list acc, kk_function_t p, kk_box_t x, kk_context_t* _ctx); /* forall<a,e> (acc : list<a>, p : parser<e,a>, x : a) -> <parse|e> list<a> */ 

kk_std_core__list kk_std_text_parse_many_acc(kk_function_t p0, kk_std_core__list acc0, kk_context_t* _ctx); /* forall<a,e> (p : parser<e,a>, acc : list<a>) -> <parse|e> list<a> */ 

static inline kk_std_core__list kk_std_text_parse_many(kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (p : parser<e,a>) -> <parse|e> list<a> */ 
  return kk_std_text_parse_many_acc(p, kk_std_core__new_Nil(_ctx), _ctx);
}
 
// monadic lift

static inline kk_std_core__list kk_std_text_parse__mlift2422_many1(kk_box_t _y_2369, kk_std_core__list _y_2370, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> <parse|e> list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_2369, _y_2370, _ctx);
}

kk_std_core__list kk_std_text_parse__mlift2423_many1(kk_function_t p, kk_box_t _y_2369, kk_context_t* _ctx); /* forall<a,e> (p : parser<e,a>, a) -> <parse|e> list<a> */ 

kk_std_core__list kk_std_text_parse_many1(kk_function_t p, kk_context_t* _ctx); /* forall<a,e> (p : parser<e,a>) -> <parse|e> list<a> */ 

kk_std_core_types__maybe kk_std_text_parse_maybe(kk_std_text_parse__parse_error perr, kk_context_t* _ctx); /* forall<a> (perr : parse-error<a>) -> maybe<a> */ 

kk_std_core_types__maybe kk_std_text_parse_next_match(kk_std_core__sslice slice, kk_std_core__list cs, kk_context_t* _ctx); /* (slice : sslice, cs : list<char>) -> maybe<sslice> */ 

kk_char_t kk_std_text_parse_no_digit(kk_context_t* _ctx); /* () -> parse char */ 

kk_char_t kk_std_text_parse_none_of(kk_string_t chars, kk_context_t* _ctx); /* (chars : string) -> parse char */ 

kk_string_t kk_std_text_parse_none_of_many1(kk_string_t chars, kk_context_t* _ctx); /* (chars : string) -> parse string */ 

kk_char_t kk_std_text_parse_one_of(kk_string_t chars, kk_context_t* _ctx); /* (chars : string) -> parse char */ 


// lift anonymous function
struct kk_std_text_parse_one_of_or_fun3719__t {
  struct kk_function_s _base;
  kk_string_t chars;
};
extern kk_box_t kk_std_text_parse_one_of_or_fun3719(kk_function_t _fself, kk_context_t* _ctx);
static inline kk_function_t kk_std_text_parse_new_one_of_or_fun3719(kk_string_t chars, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_or_fun3719__t* _self = kk_function_alloc_as(struct kk_std_text_parse_one_of_or_fun3719__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_one_of_or_fun3719, kk_context());
  _self->chars = chars;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_one_of_or_fun3721__t {
  struct kk_function_s _base;
  kk_char_t kkloc_default;
};
extern kk_box_t kk_std_text_parse_one_of_or_fun3721(kk_function_t _fself, kk_context_t* _ctx);
static inline kk_function_t kk_std_text_parse_new_one_of_or_fun3721(kk_char_t kkloc_default, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_or_fun3721__t* _self = kk_function_alloc_as(struct kk_std_text_parse_one_of_or_fun3721__t, 1, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_one_of_or_fun3721, kk_context());
  _self->kkloc_default = kkloc_default;
  return &_self->_base;
}


static inline kk_char_t kk_std_text_parse_one_of_or(kk_string_t chars, kk_char_t kkloc_default, kk_context_t* _ctx) { /* (chars : string, default : char) -> parse char */ 
  kk_box_t _x3718 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_text_parse_new_one_of_or_fun3719(chars, _ctx), kk_std_text_parse_new_one_of_or_fun3721(kkloc_default, _ctx), _ctx); /*948*/
  return kk_char_unbox(_x3718, _ctx);
}
 
// monadic lift

static inline kk_std_text_parse__parse_error kk_std_text_parse__mlift2424_parse(kk_string_t msg, kk_std_core__sslice _y_2378, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (msg : string, sslice) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  return kk_std_text_parse__new_ParseError(kk_reuse_null, msg, _y_2378, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse__mlift2425_parse(kk_std_text_parse__parse_error err1, kk_std_text_parse__parse_error _y_2382, kk_context_t* _ctx); /* forall<_h,h1,a,e> (err1 : parse-error<a>, parse-error<a>) -> <local<h1>,local<_h>|e> parse-error<a> */ 

kk_std_text_parse__parse_error kk_std_text_parse__mlift2426_parse(kk_std_text_parse__parse_error err1, kk_function_t resume, kk_unit_t wild__, kk_context_t* _ctx); /* forall<_h,h1,a,e> (err1 : parse-error<a>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, wild_ : ()) -> <local<h1>,local<_h>|e> parse-error<a> */ 

kk_std_text_parse__parse_error kk_std_text_parse__mlift2427_parse(kk_ref_t input, kk_function_t resume, kk_std_core__sslice save, kk_std_text_parse__parse_error _y_2380, kk_context_t* _ctx); /* forall<_h,h1,a,e> (input : local-var<h1,sslice>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, save : sslice, parse-error<a>) -> <local<h1>,local<_h>|e> parse-error<a> */ 

kk_std_text_parse__parse_error kk_std_text_parse__mlift2428_parse(kk_ref_t input, kk_function_t resume, kk_std_core__sslice save, kk_context_t* _ctx); /* forall<_h,h1,a,e> (input : local-var<h1,sslice>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, save : sslice) -> <local<h1>,local<_h>|e> parse-error<a> */ 
 
// monadic lift

static inline kk_std_core_types__maybe kk_std_text_parse__mlift2429_parse(kk_box_t x, kk_unit_t wild__0, kk_context_t* _ctx) { /* forall<_h,a,h1,e> (x : a, wild_0 : ()) -> <local<h1>,local<_h>|e> maybe<a> */ 
  return kk_std_core_types__new_Just(x, _ctx);
}

kk_std_core_types__maybe kk_std_text_parse__mlift2430_parse(kk_ref_t input, kk_function_t pred, kk_std_core__sslice inp, kk_context_t* _ctx); /* forall<_h,a,h1,e> (input : local-var<h1,sslice>, pred : (sslice) -> total maybe<(a, sslice)>, inp : sslice) -> <local<h1>,local<_h>|e> maybe<a> */ 
 
// monadic lift

static inline kk_std_text_parse__parse_error kk_std_text_parse__mlift2431_parse(kk_box_t x0, kk_std_core__sslice _y_2388, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (x0 : a, sslice) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  return kk_std_text_parse__new_ParseOk(kk_reuse_null, x0, _y_2388, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse_parse(kk_std_core__sslice input0, kk_function_t p, kk_context_t* _ctx); /* forall<a,e> (input0 : sslice, p : () -> <parse|e> a) -> e parse-error<a> */ 
 
// monadic lift

static inline kk_box_t kk_std_text_parse__mlift2432_parse_eof(kk_box_t x, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<a> (x : a, wild_ : ()) -> parse a */ 
  return x;
}

kk_box_t kk_std_text_parse__mlift2433_parse_eof(kk_box_t x, kk_context_t* _ctx); /* forall<a,e> (x : a) -> <parse|e> a */ 

kk_std_text_parse__parse_error kk_std_text_parse_parse_eof(kk_std_core__sslice input, kk_function_t p, kk_context_t* _ctx); /* forall<a,e> (input : sslice, p : () -> <parse|e> a) -> e parse-error<a> */ 
 
// monadic lift

static inline kk_integer_t kk_std_text_parse__mlift2434_pnat(kk_std_core__list _y_2398, kk_context_t* _ctx) { /* (list<char>) -> parse int */ 
  kk_string_t _x3808 = kk_std_core_string_2(_y_2398, _ctx); /*string*/
  kk_std_core_types__optional _x3809 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(0)), _ctx); /*optional<1035>*/
  return kk_std_core_parse_int_default(_x3808, _x3809, kk_std_core_types__new_None(_ctx), _ctx);
}

kk_integer_t kk_std_text_parse_pnat(kk_context_t* _ctx); /* () -> parse int */ 
 
// monadic lift

static inline bool kk_std_text_parse__mlift2435_sign(kk_char_t c0, kk_context_t* _ctx) { /* (c0 : char) -> parse bool */ 
  return (c0 == ('-'));
}

bool kk_std_text_parse_sign(kk_context_t* _ctx); /* () -> parse bool */ 
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2436_pint_fun3847__t {
  struct kk_function_s _base;
  kk_integer_t i;
  bool neg;
};
extern kk_box_t kk_std_text_parse__mlift2436_pint_fun3847(kk_function_t _fself, kk_context_t* _ctx);
static inline kk_function_t kk_std_text_parse__new_mlift2436_pint_fun3847(kk_integer_t i, bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2436_pint_fun3847__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2436_pint_fun3847__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2436_pint_fun3847, kk_context());
  _self->i = i;
  _self->neg = neg;
  return &_self->_base;
}


static inline kk_integer_t kk_std_text_parse__mlift2436_pint(bool neg, kk_integer_t i, kk_context_t* _ctx) { /* (neg : bool, i : int) -> parse int */ 
  kk_box_t _x3846 = kk_std_core_hnd__open_none0(kk_std_text_parse__new_mlift2436_pint_fun3847(i, neg, _ctx), _ctx); /*1001*/
  return kk_integer_unbox(_x3846);
}

kk_integer_t kk_std_text_parse__mlift2437_pint(kk_char_t c0, kk_context_t* _ctx); /* (c0 : char) -> parse int */ 

kk_integer_t kk_std_text_parse_pint(kk_context_t* _ctx); /* () -> parse int */ 

kk_string_t kk_std_text_parse_pstring(kk_string_t s, kk_context_t* _ctx); /* (s : string) -> parse string */ 

kk_std_core_types__maybe kk_std_text_parse_starts_with(kk_string_t s, kk_function_t p, kk_context_t* _ctx); /* forall<a> (s : string, p : () -> parse a) -> maybe<(a, sslice)> */ 

kk_char_t kk_std_text_parse_white(kk_context_t* _ctx); /* () -> parse char */ 

kk_string_t kk_std_text_parse_whitespace(kk_context_t* _ctx); /* () -> parse string */ 

kk_string_t kk_std_text_parse_whitespace0(kk_context_t* _ctx); /* () -> parse string */ 

void kk_std_text_parse__init(kk_context_t* _ctx);


void kk_std_text_parse__done(kk_context_t* _ctx);

#endif // header
