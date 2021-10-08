#pragma once
#ifndef kk_std_core_types_H
#define kk_std_core_types_H
// Koka generated module: "std/core/types", koka version: 2.3.2, platform: 64-bit
#include <kklib.h>

// type declarations

// value type std/core/types/()
enum kk_std_core_types__unit__e {
  kk_std_core_types__Unit_
};
typedef uint8_t kk_std_core_types__unit_;

static inline kk_std_core_types__unit_ kk_std_core_types__new_dash__lp__rp_(kk_context_t* _ctx){
  return kk_std_core_types__Unit_;
}
static inline bool kk_std_core_types__is_dash__lp__rp_(kk_std_core_types__unit_ x) {
  return (x == kk_std_core_types__Unit_);
}
static inline kk_std_core_types__unit_ kk_std_core_types__unit__dup(kk_std_core_types__unit_ _x) {
  return _x;
}
static inline void kk_std_core_types__unit__drop(kk_std_core_types__unit_ _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__unit__box(kk_std_core_types__unit_ _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__unit_ kk_std_core_types__unit__unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__unit_)kk_enum_unbox(_x);
}

// value type std/core/types/(,)
struct kk_std_core_types__Tuple2_ {
  kk_box_t fst;
  kk_box_t snd;
};
typedef struct kk_std_core_types__Tuple2_ kk_std_core_types__tuple2_;
static inline kk_std_core_types__tuple2_ kk_std_core_types__new_dash__lp__comma__rp_(kk_box_t fst, kk_box_t snd, kk_context_t* _ctx){
  kk_std_core_types__tuple2_ _con;
  _con.fst = fst;
  _con.snd = snd;
  return _con;
}
static inline bool kk_std_core_types__is_dash__lp__comma__rp_(kk_std_core_types__tuple2_ x) {
  return (true);
}
static inline kk_std_core_types__tuple2_ kk_std_core_types__tuple2__dup(kk_std_core_types__tuple2_ _x) {
  kk_box_dup(_x.fst);
  kk_box_dup(_x.snd);
  return _x;
}
static inline void kk_std_core_types__tuple2__drop(kk_std_core_types__tuple2_ _x, kk_context_t* _ctx) {
  kk_box_drop(_x.fst, _ctx);
  kk_box_drop(_x.snd, _ctx);
}
static inline kk_box_t kk_std_core_types__tuple2__box(kk_std_core_types__tuple2_ _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__tuple2_, _box, _x, 2 /* scan count */, _ctx);
  return _box;
}
static inline kk_std_core_types__tuple2_ kk_std_core_types__tuple2__unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__tuple2_ _unbox;
  kk_valuetype_unbox_(kk_std_core_types__tuple2_, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__tuple2__dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value type std/core/types/(,,)
struct kk_std_core_types__Tuple3_ {
  kk_box_t fst;
  kk_box_t snd;
  kk_box_t thd;
};
typedef struct kk_std_core_types__Tuple3_ kk_std_core_types__tuple3_;
static inline kk_std_core_types__tuple3_ kk_std_core_types__new_dash__lp__comma__comma__rp_(kk_box_t fst, kk_box_t snd, kk_box_t thd, kk_context_t* _ctx){
  kk_std_core_types__tuple3_ _con;
  _con.fst = fst;
  _con.snd = snd;
  _con.thd = thd;
  return _con;
}
static inline bool kk_std_core_types__is_dash__lp__comma__comma__rp_(kk_std_core_types__tuple3_ x) {
  return (true);
}
static inline kk_std_core_types__tuple3_ kk_std_core_types__tuple3__dup(kk_std_core_types__tuple3_ _x) {
  kk_box_dup(_x.fst);
  kk_box_dup(_x.snd);
  kk_box_dup(_x.thd);
  return _x;
}
static inline void kk_std_core_types__tuple3__drop(kk_std_core_types__tuple3_ _x, kk_context_t* _ctx) {
  kk_box_drop(_x.fst, _ctx);
  kk_box_drop(_x.snd, _ctx);
  kk_box_drop(_x.thd, _ctx);
}
static inline kk_box_t kk_std_core_types__tuple3__box(kk_std_core_types__tuple3_ _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__tuple3_, _box, _x, 3 /* scan count */, _ctx);
  return _box;
}
static inline kk_std_core_types__tuple3_ kk_std_core_types__tuple3__unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__tuple3_ _unbox;
  kk_valuetype_unbox_(kk_std_core_types__tuple3_, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__tuple3__dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// type std/core/types/(,,,)
struct kk_std_core_types__tuple4__s {
  kk_block_t _block;
};
typedef struct kk_std_core_types__tuple4__s* kk_std_core_types__tuple4_;
struct kk_std_core_types__Tuple4_ {
  struct kk_std_core_types__tuple4__s _base;
  kk_box_t fst;
  kk_box_t snd;
  kk_box_t thd;
  kk_box_t field4;
};
static inline kk_std_core_types__tuple4_ kk_std_core_types__base_dash__lp__comma__comma__comma__rp_(struct kk_std_core_types__Tuple4_* _x){
  return &_x->_base;
}
static inline kk_std_core_types__tuple4_ kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_t _at, kk_box_t fst, kk_box_t snd, kk_box_t thd, kk_box_t field4, kk_context_t* _ctx){
  struct kk_std_core_types__Tuple4_* _con = kk_block_alloc_at_as(struct kk_std_core_types__Tuple4_, _at, 4 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->fst = fst;
  _con->snd = snd;
  _con->thd = thd;
  _con->field4 = field4;
  return kk_std_core_types__base_dash__lp__comma__comma__comma__rp_(_con);
}
static inline struct kk_std_core_types__Tuple4_* kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(kk_std_core_types__tuple4_ x) {
  return kk_basetype_as_assert(struct kk_std_core_types__Tuple4_*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_core_types__is_dash__lp__comma__comma__comma__rp_(kk_std_core_types__tuple4_ x) {
  return (true);
}
static inline kk_std_core_types__tuple4_ kk_std_core_types__tuple4__dup(kk_std_core_types__tuple4_ _x) {
  return kk_basetype_dup_as(kk_std_core_types__tuple4_, _x);
}
static inline void kk_std_core_types__tuple4__drop(kk_std_core_types__tuple4_ _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__tuple4__is_unique(kk_std_core_types__tuple4_ _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_core_types__tuple4__free(kk_std_core_types__tuple4_ _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_core_types__tuple4__decref(kk_std_core_types__tuple4_ _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__tuple4__dropn_reuse(kk_std_core_types__tuple4_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__tuple4__dropn(kk_std_core_types__tuple4_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__tuple4__reuse(kk_std_core_types__tuple4_ _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_core_types__tuple4_ kk_std_core_types__tuple4__hole() {
  return (kk_std_core_types__tuple4_)(1);
}
static inline kk_box_t kk_std_core_types__tuple4__box(kk_std_core_types__tuple4_ _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_core_types__tuple4_ kk_std_core_types__tuple4__unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_core_types__tuple4_, _x);
}

// type std/core/types/(,,,,)
struct kk_std_core_types___lp__comma__comma__comma__comma__rp__s {
  kk_block_t _block;
};
typedef struct kk_std_core_types___lp__comma__comma__comma__comma__rp__s* kk_std_core_types___lp__comma__comma__comma__comma__rp_;
struct kk_std_core_types__lp__comma__comma__comma__comma__rp_ {
  struct kk_std_core_types___lp__comma__comma__comma__comma__rp__s _base;
  kk_box_t fst;
  kk_box_t snd;
  kk_box_t thd;
  kk_box_t field4;
  kk_box_t field5;
};
static inline kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types__base_dash__lp__comma__comma__comma__comma__rp_(struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _x){
  return &_x->_base;
}
static inline kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types__new_dash__lp__comma__comma__comma__comma__rp_(kk_reuse_t _at, kk_box_t fst, kk_box_t snd, kk_box_t thd, kk_box_t field4, kk_box_t field5, kk_context_t* _ctx){
  struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con = kk_block_alloc_at_as(struct kk_std_core_types__lp__comma__comma__comma__comma__rp_, _at, 5 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->fst = fst;
  _con->snd = snd;
  _con->thd = thd;
  _con->field4 = field4;
  _con->field5 = field5;
  return kk_std_core_types__base_dash__lp__comma__comma__comma__comma__rp_(_con);
}
static inline struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(kk_std_core_types___lp__comma__comma__comma__comma__rp_ x) {
  return kk_basetype_as_assert(struct kk_std_core_types__lp__comma__comma__comma__comma__rp_*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_core_types__is_dash__lp__comma__comma__comma__comma__rp_(kk_std_core_types___lp__comma__comma__comma__comma__rp_ x) {
  return (true);
}
static inline kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types___lp__comma__comma__comma__comma__rp__dup(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x) {
  return kk_basetype_dup_as(kk_std_core_types___lp__comma__comma__comma__comma__rp_, _x);
}
static inline void kk_std_core_types___lp__comma__comma__comma__comma__rp__drop(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_core_types___lp__comma__comma__comma__comma__rp__free(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types___lp__comma__comma__comma__comma__rp__dropn_reuse(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types___lp__comma__comma__comma__comma__rp__dropn(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types___lp__comma__comma__comma__comma__rp__reuse(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types___lp__comma__comma__comma__comma__rp__hole() {
  return (kk_std_core_types___lp__comma__comma__comma__comma__rp_)(1);
}
static inline kk_box_t kk_std_core_types___lp__comma__comma__comma__comma__rp__box(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types___lp__comma__comma__comma__comma__rp__unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_core_types___lp__comma__comma__comma__comma__rp_, _x);
}

// type std/core/types/alloc
struct kk_std_core_types__alloc_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__alloc;
static inline kk_std_core_types__alloc kk_std_core_types__alloc_dup(kk_std_core_types__alloc _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__alloc_drop(kk_std_core_types__alloc _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__alloc_is_unique(kk_std_core_types__alloc _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__alloc_free(kk_std_core_types__alloc _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__alloc_decref(kk_std_core_types__alloc _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__alloc_dropn_reuse(kk_std_core_types__alloc _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__alloc_dropn(kk_std_core_types__alloc _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__alloc_reuse(kk_std_core_types__alloc _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__alloc kk_std_core_types__alloc_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__alloc_box(kk_std_core_types__alloc _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__alloc kk_std_core_types__alloc_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/any
struct kk_std_core_types__any_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__any;
static inline kk_std_core_types__any kk_std_core_types__any_dup(kk_std_core_types__any _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__any_drop(kk_std_core_types__any _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__any_is_unique(kk_std_core_types__any _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__any_free(kk_std_core_types__any _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__any_decref(kk_std_core_types__any _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__any_dropn_reuse(kk_std_core_types__any _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__any_dropn(kk_std_core_types__any _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__any_reuse(kk_std_core_types__any _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__any kk_std_core_types__any_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__any_box(kk_std_core_types__any _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__any kk_std_core_types__any_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/bool
enum kk_std_core_types__bool_e {
  kk_std_core_types_False,
  kk_std_core_types_True
};
typedef uint8_t kk_std_core_types__bool;

static inline kk_std_core_types__bool kk_std_core_types__new_False(kk_context_t* _ctx){
  return kk_std_core_types_False;
}
static inline kk_std_core_types__bool kk_std_core_types__new_True(kk_context_t* _ctx){
  return kk_std_core_types_True;
}
static inline bool kk_std_core_types__is_False(kk_std_core_types__bool x) {
  return (x == kk_std_core_types_False);
}
static inline bool kk_std_core_types__is_True(kk_std_core_types__bool x) {
  return (x == kk_std_core_types_True);
}
static inline kk_std_core_types__bool kk_std_core_types__bool_dup(kk_std_core_types__bool _x) {
  return _x;
}
static inline void kk_std_core_types__bool_drop(kk_std_core_types__bool _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__bool_box(kk_std_core_types__bool _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__bool kk_std_core_types__bool_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__bool)kk_enum_unbox(_x);
}

// value type std/core/types/box
struct kk_std_core_types_Box {
  kk_box_t unbox;
};
typedef struct kk_std_core_types_Box kk_std_core_types__box;
static inline kk_std_core_types__box kk_std_core_types__new_Box(kk_box_t unbox, kk_context_t* _ctx){
  kk_std_core_types__box _con = { unbox };
  return _con;
}
static inline bool kk_std_core_types__is_Box(kk_std_core_types__box x) {
  return (true);
}
static inline kk_std_core_types__box kk_std_core_types__box_dup(kk_std_core_types__box _x) {
  kk_box_dup(_x.unbox);
  return _x;
}
static inline void kk_std_core_types__box_drop(kk_std_core_types__box _x, kk_context_t* _ctx) {
  kk_box_drop(_x.unbox, _ctx);
}
static inline kk_box_t kk_std_core_types__box_box(kk_std_core_types__box _x, kk_context_t* _ctx) {
  return kk_box_box(_x.unbox, _ctx);
}
static inline kk_std_core_types__box kk_std_core_types__box_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_std_core_types__new_Box(kk_box_unbox(_x, _ctx), _ctx);
}

// value type std/core/types/byte
enum kk_std_core_types__byte_e {
  kk_std_core_types_byte_empty
};
typedef uint8_t kk_std_core_types__byte;

static inline kk_std_core_types__byte kk_std_core_types__byte_dup(kk_std_core_types__byte _x) {
  return _x;
}
static inline void kk_std_core_types__byte_drop(kk_std_core_types__byte _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__byte_box(kk_std_core_types__byte _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__byte kk_std_core_types__byte_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__byte)kk_enum_unbox(_x);
}

// value type std/core/types/cfield
enum kk_std_core_types__cfield_e {
  kk_std_core_types_cfield_empty
};
typedef uint32_t kk_std_core_types__cfield;

static inline kk_std_core_types__cfield kk_std_core_types__cfield_dup(kk_std_core_types__cfield _x) {
  return _x;
}
static inline void kk_std_core_types__cfield_drop(kk_std_core_types__cfield _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__cfield_box(kk_std_core_types__cfield _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__cfield kk_std_core_types__cfield_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__cfield)kk_enum_unbox(_x);
}

// value type std/core/types/char
enum kk_std_core_types__char_e {
  kk_std_core_types_char_empty
};
typedef uint32_t kk_std_core_types__char;

static inline kk_std_core_types__char kk_std_core_types__char_dup(kk_std_core_types__char _x) {
  return _x;
}
static inline void kk_std_core_types__char_drop(kk_std_core_types__char _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__char_box(kk_std_core_types__char _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__char kk_std_core_types__char_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__char)kk_enum_unbox(_x);
}

// value type std/core/types/ctail
struct kk_std_core_types__CTail {
  kk_box_t res;
  kk_box_t* hole;
};
typedef struct kk_std_core_types__CTail kk_std_core_types__ctail;
static inline kk_std_core_types__ctail kk_std_core_types__new_CTail(kk_box_t res, kk_box_t* hole, kk_context_t* _ctx){
  kk_std_core_types__ctail _con;
  _con.res = res;
  _con.hole = hole;
  return _con;
}
static inline bool kk_std_core_types__is_CTail(kk_std_core_types__ctail x) {
  return (true);
}
static inline kk_std_core_types__ctail kk_std_core_types__ctail_dup(kk_std_core_types__ctail _x) {
  kk_box_dup(_x.res);
  return _x;
}
static inline void kk_std_core_types__ctail_drop(kk_std_core_types__ctail _x, kk_context_t* _ctx) {
  kk_box_drop(_x.res, _ctx);
}
static inline kk_box_t kk_std_core_types__ctail_box(kk_std_core_types__ctail _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__ctail, _box, _x, 1 /* scan count */, _ctx);
  return _box;
}
static inline kk_std_core_types__ctail kk_std_core_types__ctail_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__ctail _unbox;
  kk_valuetype_unbox_(kk_std_core_types__ctail, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__ctail_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// type std/core/types/div
struct kk_std_core_types__div_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__div;
static inline kk_std_core_types__div kk_std_core_types__div_dup(kk_std_core_types__div _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__div_drop(kk_std_core_types__div _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__div_is_unique(kk_std_core_types__div _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__div_free(kk_std_core_types__div _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__div_decref(kk_std_core_types__div _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__div_dropn_reuse(kk_std_core_types__div _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__div_dropn(kk_std_core_types__div _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__div_reuse(kk_std_core_types__div _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__div kk_std_core_types__div_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__div_box(kk_std_core_types__div _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__div kk_std_core_types__div_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/double
enum kk_std_core_types__double_e {
  kk_std_core_types_double_empty
};
typedef uint32_t kk_std_core_types__double;

static inline kk_std_core_types__double kk_std_core_types__double_dup(kk_std_core_types__double _x) {
  return _x;
}
static inline void kk_std_core_types__double_drop(kk_std_core_types__double _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__double_box(kk_std_core_types__double _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__double kk_std_core_types__double_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__double)kk_enum_unbox(_x);
}

// type std/core/types/ediv
struct kk_std_core_types__ediv_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__ediv;
static inline kk_std_core_types__ediv kk_std_core_types__ediv_dup(kk_std_core_types__ediv _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__ediv_drop(kk_std_core_types__ediv _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__ediv_is_unique(kk_std_core_types__ediv _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__ediv_free(kk_std_core_types__ediv _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__ediv_decref(kk_std_core_types__ediv _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ediv_dropn_reuse(kk_std_core_types__ediv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__ediv_dropn(kk_std_core_types__ediv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ediv_reuse(kk_std_core_types__ediv _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__ediv kk_std_core_types__ediv_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__ediv_box(kk_std_core_types__ediv _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__ediv kk_std_core_types__ediv_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/either
struct kk_std_core_types_Left {
  kk_box_t left;
};
struct kk_std_core_types_Right {
  kk_box_t right;
};
struct kk_std_core_types_either_s {
  kk_value_tag_t _tag;
  union {
    struct kk_std_core_types_Left Left;
    struct kk_std_core_types_Right Right;
  } _cons;
};
typedef struct kk_std_core_types_either_s kk_std_core_types__either;
static inline kk_std_core_types__either kk_std_core_types__new_Left(kk_box_t left, kk_context_t* _ctx){
  kk_std_core_types__either _con;
  _con._tag = kk_value_tag(1);
  _con._cons.Left.left = left;
  return _con;
}
static inline kk_std_core_types__either kk_std_core_types__new_Right(kk_box_t right, kk_context_t* _ctx){
  kk_std_core_types__either _con;
  _con._tag = kk_value_tag(2);
  _con._cons.Right.right = right;
  return _con;
}
static inline bool kk_std_core_types__is_Left(kk_std_core_types__either x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(1)));
}
static inline bool kk_std_core_types__is_Right(kk_std_core_types__either x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(2)));
}
static inline kk_ssize_t kk_std_core_types__either_scan_count(kk_std_core_types__either _x) {
  if (kk_std_core_types__is_Left(_x)) return 2;
  else return 2;
}
static inline kk_std_core_types__either kk_std_core_types__either_dup(kk_std_core_types__either _x) {
  if (kk_std_core_types__is_Left(_x)) {
    kk_box_dup(_x._cons.Left.left);
  }
  else {
    kk_box_dup(_x._cons.Right.right);
  }
  return _x;
}
static inline void kk_std_core_types__either_drop(kk_std_core_types__either _x, kk_context_t* _ctx) {
  if (kk_std_core_types__is_Left(_x)) {
    kk_box_drop(_x._cons.Left.left, _ctx);
  }
  else {
    kk_box_drop(_x._cons.Right.right, _ctx);
  }
}
static inline kk_box_t kk_std_core_types__either_box(kk_std_core_types__either _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__either, _box, _x, kk_std_core_types__either_scan_count(_x), _ctx);
  return _box;
}
static inline kk_std_core_types__either kk_std_core_types__either_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__either _unbox;
  kk_valuetype_unbox_(kk_std_core_types__either, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__either_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value type std/core/types/float32
enum kk_std_core_types__float32_e {
  kk_std_core_types_float32_empty
};
typedef uint32_t kk_std_core_types__float32;

static inline kk_std_core_types__float32 kk_std_core_types__float32_dup(kk_std_core_types__float32 _x) {
  return _x;
}
static inline void kk_std_core_types__float32_drop(kk_std_core_types__float32 _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__float32_box(kk_std_core_types__float32 _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__float32 kk_std_core_types__float32_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__float32)kk_enum_unbox(_x);
}

// type std/core/types/global
struct kk_std_core_types__global_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__global;
static inline kk_std_core_types__global kk_std_core_types__global_dup(kk_std_core_types__global _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__global_drop(kk_std_core_types__global _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__global_is_unique(kk_std_core_types__global _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__global_free(kk_std_core_types__global _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__global_decref(kk_std_core_types__global _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__global_dropn_reuse(kk_std_core_types__global _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__global_dropn(kk_std_core_types__global _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__global_reuse(kk_std_core_types__global _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__global kk_std_core_types__global_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__global_box(kk_std_core_types__global _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__global kk_std_core_types__global_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/handled
struct kk_std_core_types__handled_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__handled;
static inline kk_std_core_types__handled kk_std_core_types__handled_dup(kk_std_core_types__handled _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__handled_drop(kk_std_core_types__handled _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__handled_is_unique(kk_std_core_types__handled _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__handled_free(kk_std_core_types__handled _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__handled_decref(kk_std_core_types__handled _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__handled_dropn_reuse(kk_std_core_types__handled _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__handled_dropn(kk_std_core_types__handled _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__handled_reuse(kk_std_core_types__handled _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__handled kk_std_core_types__handled_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__handled_box(kk_std_core_types__handled _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__handled kk_std_core_types__handled_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/handled1
struct kk_std_core_types__handled1_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__handled1;
static inline kk_std_core_types__handled1 kk_std_core_types__handled1_dup(kk_std_core_types__handled1 _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__handled1_drop(kk_std_core_types__handled1 _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__handled1_is_unique(kk_std_core_types__handled1 _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__handled1_free(kk_std_core_types__handled1 _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__handled1_decref(kk_std_core_types__handled1 _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__handled1_dropn_reuse(kk_std_core_types__handled1 _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__handled1_dropn(kk_std_core_types__handled1 _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__handled1_reuse(kk_std_core_types__handled1 _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__handled1 kk_std_core_types__handled1_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__handled1_box(kk_std_core_types__handled1 _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__handled1 kk_std_core_types__handled1_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/hbox
struct kk_std_core_types__hbox_s {
  kk_block_t _block;
};
typedef struct kk_std_core_types__hbox_s* kk_std_core_types__hbox;
struct kk_std_core_types_Hbox {
  struct kk_std_core_types__hbox_s _base;
  kk_box_t unhbox;
};
static inline kk_std_core_types__hbox kk_std_core_types__base_Hbox(struct kk_std_core_types_Hbox* _x){
  return &_x->_base;
}
static inline kk_std_core_types__hbox kk_std_core_types__new_Hbox(kk_reuse_t _at, kk_box_t unhbox, kk_context_t* _ctx){
  struct kk_std_core_types_Hbox* _con = kk_block_alloc_at_as(struct kk_std_core_types_Hbox, _at, 1 /* scan count */, (kk_tag_t)(1), _ctx);
  _con->unhbox = unhbox;
  return kk_std_core_types__base_Hbox(_con);
}
static inline struct kk_std_core_types_Hbox* kk_std_core_types__as_Hbox(kk_std_core_types__hbox x) {
  return kk_basetype_as_assert(struct kk_std_core_types_Hbox*, x, (kk_tag_t)(1) /* _tag */);
}
static inline bool kk_std_core_types__is_Hbox(kk_std_core_types__hbox x) {
  return (true);
}
static inline kk_std_core_types__hbox kk_std_core_types__hbox_dup(kk_std_core_types__hbox _x) {
  return kk_basetype_dup_as(kk_std_core_types__hbox, _x);
}
static inline void kk_std_core_types__hbox_drop(kk_std_core_types__hbox _x, kk_context_t* _ctx) {
  kk_basetype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__hbox_is_unique(kk_std_core_types__hbox _x) {
  return kk_basetype_is_unique(_x);
}
static inline void kk_std_core_types__hbox_free(kk_std_core_types__hbox _x) {
  kk_basetype_free(_x);
}
static inline void kk_std_core_types__hbox_decref(kk_std_core_types__hbox _x, kk_context_t* _ctx) {
  kk_basetype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__hbox_dropn_reuse(kk_std_core_types__hbox _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_basetype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__hbox_dropn(kk_std_core_types__hbox _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_basetype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__hbox_reuse(kk_std_core_types__hbox _x) {
  return kk_basetype_reuse(_x);
}
static inline kk_std_core_types__hbox kk_std_core_types__hbox_hole() {
  return (kk_std_core_types__hbox)(1);
}
static inline kk_box_t kk_std_core_types__hbox_box(kk_std_core_types__hbox _x, kk_context_t* _ctx) {
  return kk_basetype_box(_x);
}
static inline kk_std_core_types__hbox kk_std_core_types__hbox_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_basetype_unbox_as(kk_std_core_types__hbox, _x);
}

// type std/core/types/hdiv
struct kk_std_core_types__hdiv_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__hdiv;
static inline kk_std_core_types__hdiv kk_std_core_types__hdiv_dup(kk_std_core_types__hdiv _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__hdiv_drop(kk_std_core_types__hdiv _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__hdiv_is_unique(kk_std_core_types__hdiv _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__hdiv_free(kk_std_core_types__hdiv _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__hdiv_decref(kk_std_core_types__hdiv _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__hdiv_dropn_reuse(kk_std_core_types__hdiv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__hdiv_dropn(kk_std_core_types__hdiv _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__hdiv_reuse(kk_std_core_types__hdiv _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__hdiv kk_std_core_types__hdiv_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__hdiv_box(kk_std_core_types__hdiv _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__hdiv kk_std_core_types__hdiv_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/int
struct kk_std_core_types__int_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__int;
static inline kk_std_core_types__int kk_std_core_types__int_dup(kk_std_core_types__int _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__int_drop(kk_std_core_types__int _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__int_is_unique(kk_std_core_types__int _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__int_free(kk_std_core_types__int _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__int_decref(kk_std_core_types__int _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__int_dropn_reuse(kk_std_core_types__int _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__int_dropn(kk_std_core_types__int _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__int_reuse(kk_std_core_types__int _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__int kk_std_core_types__int_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__int_box(kk_std_core_types__int _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__int kk_std_core_types__int_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/int32
enum kk_std_core_types__int32_e {
  kk_std_core_types_int32_empty
};
typedef uint32_t kk_std_core_types__int32;

static inline kk_std_core_types__int32 kk_std_core_types__int32_dup(kk_std_core_types__int32 _x) {
  return _x;
}
static inline void kk_std_core_types__int32_drop(kk_std_core_types__int32 _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__int32_box(kk_std_core_types__int32 _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__int32 kk_std_core_types__int32_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__int32)kk_enum_unbox(_x);
}

// value type std/core/types/int64
enum kk_std_core_types__int64_e {
  kk_std_core_types_int64_empty
};
typedef uint32_t kk_std_core_types__int64;

static inline kk_std_core_types__int64 kk_std_core_types__int64_dup(kk_std_core_types__int64 _x) {
  return _x;
}
static inline void kk_std_core_types__int64_drop(kk_std_core_types__int64 _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__int64_box(kk_std_core_types__int64 _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__int64 kk_std_core_types__int64_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__int64)kk_enum_unbox(_x);
}

// value type std/core/types/intptr_t
enum kk_std_core_types__intptr__t_e {
  kk_std_core_types_intptr__t_empty
};
typedef uint32_t kk_std_core_types__intptr__t;

static inline kk_std_core_types__intptr__t kk_std_core_types__intptr__t_dup(kk_std_core_types__intptr__t _x) {
  return _x;
}
static inline void kk_std_core_types__intptr__t_drop(kk_std_core_types__intptr__t _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__intptr__t_box(kk_std_core_types__intptr__t _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__intptr__t kk_std_core_types__intptr__t_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__intptr__t)kk_enum_unbox(_x);
}

// type std/core/types/local
struct kk_std_core_types__local_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__local;
static inline kk_std_core_types__local kk_std_core_types__local_dup(kk_std_core_types__local _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__local_drop(kk_std_core_types__local _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__local_is_unique(kk_std_core_types__local _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__local_free(kk_std_core_types__local _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__local_decref(kk_std_core_types__local _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__local_dropn_reuse(kk_std_core_types__local _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__local_dropn(kk_std_core_types__local _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__local_reuse(kk_std_core_types__local _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__local kk_std_core_types__local_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__local_box(kk_std_core_types__local _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__local kk_std_core_types__local_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/local-var
struct kk_std_core_types__local_var_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__local_var;
static inline kk_std_core_types__local_var kk_std_core_types__local_var_dup(kk_std_core_types__local_var _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__local_var_drop(kk_std_core_types__local_var _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__local_var_is_unique(kk_std_core_types__local_var _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__local_var_free(kk_std_core_types__local_var _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__local_var_decref(kk_std_core_types__local_var _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__local_var_dropn_reuse(kk_std_core_types__local_var _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__local_var_dropn(kk_std_core_types__local_var _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__local_var_reuse(kk_std_core_types__local_var _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__local_var kk_std_core_types__local_var_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__local_var_box(kk_std_core_types__local_var _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__local_var kk_std_core_types__local_var_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/maybe
struct kk_std_core_types_Nothing {
  kk_box_t _unused;
};
struct kk_std_core_types_Just {
  kk_box_t value;
};
struct kk_std_core_types_maybe_s {
  kk_value_tag_t _tag;
  union {
    struct kk_std_core_types_Nothing Nothing;
    struct kk_std_core_types_Just Just;
    kk_box_t _fields[1];
  } _cons;
};
typedef struct kk_std_core_types_maybe_s kk_std_core_types__maybe;
static inline kk_std_core_types__maybe kk_std_core_types__new_Nothing(kk_context_t* _ctx){
  kk_std_core_types__maybe _con;
  _con._tag = kk_value_tag(1);
  _con._cons._fields[0] = kk_box_null;
  return _con;
}
static inline kk_std_core_types__maybe kk_std_core_types__new_Just(kk_box_t value, kk_context_t* _ctx){
  kk_std_core_types__maybe _con;
  _con._tag = kk_value_tag(2);
  _con._cons.Just.value = value;
  return _con;
}
static inline bool kk_std_core_types__is_Nothing(kk_std_core_types__maybe x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(1)));
}
static inline bool kk_std_core_types__is_Just(kk_std_core_types__maybe x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(2)));
}
static inline kk_ssize_t kk_std_core_types__maybe_scan_count(kk_std_core_types__maybe _x) {
  if (kk_std_core_types__is_Nothing(_x)) return 1;
  else return 2;
}
static inline kk_std_core_types__maybe kk_std_core_types__maybe_dup(kk_std_core_types__maybe _x) {
  if (kk_std_core_types__is_Nothing(_x)) { }
  else {
    kk_box_dup(_x._cons.Just.value);
  }
  return _x;
}
static inline void kk_std_core_types__maybe_drop(kk_std_core_types__maybe _x, kk_context_t* _ctx) {
  if (kk_std_core_types__is_Nothing(_x)) { }
  else {
    kk_box_drop(_x._cons.Just.value, _ctx);
  }
}
static inline kk_box_t kk_std_core_types__maybe_box(kk_std_core_types__maybe _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__maybe, _box, _x, kk_std_core_types__maybe_scan_count(_x), _ctx);
  return _box;
}
static inline kk_std_core_types__maybe kk_std_core_types__maybe_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__maybe _unbox;
  kk_valuetype_unbox_(kk_std_core_types__maybe, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__maybe_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// type std/core/types/ndet
struct kk_std_core_types__ndet_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__ndet;
static inline kk_std_core_types__ndet kk_std_core_types__ndet_dup(kk_std_core_types__ndet _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__ndet_drop(kk_std_core_types__ndet _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__ndet_is_unique(kk_std_core_types__ndet _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__ndet_free(kk_std_core_types__ndet _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__ndet_decref(kk_std_core_types__ndet _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ndet_dropn_reuse(kk_std_core_types__ndet _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__ndet_dropn(kk_std_core_types__ndet _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ndet_reuse(kk_std_core_types__ndet _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__ndet kk_std_core_types__ndet_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__ndet_box(kk_std_core_types__ndet _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__ndet kk_std_core_types__ndet_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/optional
struct kk_std_core_types_Optional {
  kk_box_t value;
};
struct kk_std_core_types_None {
  kk_box_t _unused;
};
struct kk_std_core_types_optional_s {
  kk_value_tag_t _tag;
  union {
    struct kk_std_core_types_Optional Optional;
    struct kk_std_core_types_None None;
    kk_box_t _fields[1];
  } _cons;
};
typedef struct kk_std_core_types_optional_s kk_std_core_types__optional;
static inline kk_std_core_types__optional kk_std_core_types__new_Optional(kk_box_t value, kk_context_t* _ctx){
  kk_std_core_types__optional _con;
  _con._tag = kk_value_tag(1);
  _con._cons.Optional.value = value;
  return _con;
}
static inline kk_std_core_types__optional kk_std_core_types__new_None(kk_context_t* _ctx){
  kk_std_core_types__optional _con;
  _con._tag = kk_value_tag(2);
  _con._cons._fields[0] = kk_box_null;
  return _con;
}
static inline bool kk_std_core_types__is_Optional(kk_std_core_types__optional x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(1)));
}
static inline bool kk_std_core_types__is_None(kk_std_core_types__optional x) {
  return (kk_integer_small_eq(x._tag, kk_value_tag(2)));
}
static inline kk_ssize_t kk_std_core_types__optional_scan_count(kk_std_core_types__optional _x) {
  if (kk_std_core_types__is_Optional(_x)) return 2;
  else return 1;
}
static inline kk_std_core_types__optional kk_std_core_types__optional_dup(kk_std_core_types__optional _x) {
  if (kk_std_core_types__is_Optional(_x)) {
    kk_box_dup(_x._cons.Optional.value);
  }
  return _x;
}
static inline void kk_std_core_types__optional_drop(kk_std_core_types__optional _x, kk_context_t* _ctx) {
  if (kk_std_core_types__is_Optional(_x)) {
    kk_box_drop(_x._cons.Optional.value, _ctx);
  }
}
static inline kk_box_t kk_std_core_types__optional_box(kk_std_core_types__optional _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_core_types__optional, _box, _x, kk_std_core_types__optional_scan_count(_x), _ctx);
  return _box;
}
static inline kk_std_core_types__optional kk_std_core_types__optional_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_core_types__optional _unbox;
  kk_valuetype_unbox_(kk_std_core_types__optional, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_core_types__optional_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value type std/core/types/order
enum kk_std_core_types__order_e {
  kk_std_core_types_Lt,
  kk_std_core_types_Eq,
  kk_std_core_types_Gt
};
typedef uint8_t kk_std_core_types__order;

static inline kk_std_core_types__order kk_std_core_types__new_Lt(kk_context_t* _ctx){
  return kk_std_core_types_Lt;
}
static inline kk_std_core_types__order kk_std_core_types__new_Eq(kk_context_t* _ctx){
  return kk_std_core_types_Eq;
}
static inline kk_std_core_types__order kk_std_core_types__new_Gt(kk_context_t* _ctx){
  return kk_std_core_types_Gt;
}
static inline bool kk_std_core_types__is_Lt(kk_std_core_types__order x) {
  return (x == kk_std_core_types_Lt);
}
static inline bool kk_std_core_types__is_Eq(kk_std_core_types__order x) {
  return (x == kk_std_core_types_Eq);
}
static inline bool kk_std_core_types__is_Gt(kk_std_core_types__order x) {
  return (x == kk_std_core_types_Gt);
}
static inline kk_std_core_types__order kk_std_core_types__order_dup(kk_std_core_types__order _x) {
  return _x;
}
static inline void kk_std_core_types__order_drop(kk_std_core_types__order _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__order_box(kk_std_core_types__order _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__order kk_std_core_types__order_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__order)kk_enum_unbox(_x);
}

// type std/core/types/read
struct kk_std_core_types__read_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__read;
static inline kk_std_core_types__read kk_std_core_types__read_dup(kk_std_core_types__read _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__read_drop(kk_std_core_types__read _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__read_is_unique(kk_std_core_types__read _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__read_free(kk_std_core_types__read _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__read_decref(kk_std_core_types__read _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__read_dropn_reuse(kk_std_core_types__read _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__read_dropn(kk_std_core_types__read _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__read_reuse(kk_std_core_types__read _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__read kk_std_core_types__read_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__read_box(kk_std_core_types__read _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__read kk_std_core_types__read_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/ref
struct kk_std_core_types__ref_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__ref;
static inline kk_std_core_types__ref kk_std_core_types__ref_dup(kk_std_core_types__ref _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__ref_drop(kk_std_core_types__ref _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__ref_is_unique(kk_std_core_types__ref _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__ref_free(kk_std_core_types__ref _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__ref_decref(kk_std_core_types__ref _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ref_dropn_reuse(kk_std_core_types__ref _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__ref_dropn(kk_std_core_types__ref _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__ref_reuse(kk_std_core_types__ref _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__ref kk_std_core_types__ref_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__ref_box(kk_std_core_types__ref _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__ref kk_std_core_types__ref_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/reuse
struct kk_std_core_types__reuse_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__reuse;
static inline kk_std_core_types__reuse kk_std_core_types__reuse_dup(kk_std_core_types__reuse _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__reuse_drop(kk_std_core_types__reuse _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__reuse_is_unique(kk_std_core_types__reuse _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__reuse_free(kk_std_core_types__reuse _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__reuse_decref(kk_std_core_types__reuse _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__reuse_dropn_reuse(kk_std_core_types__reuse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__reuse_dropn(kk_std_core_types__reuse _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__reuse_reuse(kk_std_core_types__reuse _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__reuse kk_std_core_types__reuse_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__reuse_box(kk_std_core_types__reuse _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__reuse kk_std_core_types__reuse_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value type std/core/types/ssize_t
enum kk_std_core_types__ssize__t_e {
  kk_std_core_types_ssize__t_empty
};
typedef uint32_t kk_std_core_types__ssize__t;

static inline kk_std_core_types__ssize__t kk_std_core_types__ssize__t_dup(kk_std_core_types__ssize__t _x) {
  return _x;
}
static inline void kk_std_core_types__ssize__t_drop(kk_std_core_types__ssize__t _x, kk_context_t* _ctx) {
  
}
static inline kk_box_t kk_std_core_types__ssize__t_box(kk_std_core_types__ssize__t _x, kk_context_t* _ctx) {
  return kk_enum_box(_x);
}
static inline kk_std_core_types__ssize__t kk_std_core_types__ssize__t_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return (kk_std_core_types__ssize__t)kk_enum_unbox(_x);
}

// type std/core/types/write
struct kk_std_core_types__write_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__write;
static inline kk_std_core_types__write kk_std_core_types__write_dup(kk_std_core_types__write _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__write_drop(kk_std_core_types__write _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__write_is_unique(kk_std_core_types__write _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__write_free(kk_std_core_types__write _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__write_decref(kk_std_core_types__write _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__write_dropn_reuse(kk_std_core_types__write _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__write_dropn(kk_std_core_types__write _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__write_reuse(kk_std_core_types__write _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__write kk_std_core_types__write_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__write_box(kk_std_core_types__write _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__write kk_std_core_types__write_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/(<>)
struct kk_std_core_types__total__s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__total_;
static inline kk_std_core_types__total_ kk_std_core_types__total__dup(kk_std_core_types__total_ _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__total__drop(kk_std_core_types__total_ _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__total__is_unique(kk_std_core_types__total_ _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__total__free(kk_std_core_types__total_ _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__total__decref(kk_std_core_types__total_ _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__total__dropn_reuse(kk_std_core_types__total_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__total__dropn(kk_std_core_types__total_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__total__reuse(kk_std_core_types__total_ _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__total_ kk_std_core_types__total__hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__total__box(kk_std_core_types__total_ _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__total_ kk_std_core_types__total__unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/(<|>)
struct kk_std_core_types__extend__s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__extend_;
static inline kk_std_core_types__extend_ kk_std_core_types__extend__dup(kk_std_core_types__extend_ _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__extend__drop(kk_std_core_types__extend_ _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__extend__is_unique(kk_std_core_types__extend_ _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__extend__free(kk_std_core_types__extend_ _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__extend__decref(kk_std_core_types__extend_ _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__extend__dropn_reuse(kk_std_core_types__extend_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__extend__dropn(kk_std_core_types__extend_ _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__extend__reuse(kk_std_core_types__extend_ _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__extend_ kk_std_core_types__extend__hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__extend__box(kk_std_core_types__extend_ _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__extend_ kk_std_core_types__extend__unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/string
struct kk_std_core_types__string_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__string;
static inline kk_std_core_types__string kk_std_core_types__string_dup(kk_std_core_types__string _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__string_drop(kk_std_core_types__string _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__string_is_unique(kk_std_core_types__string _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__string_free(kk_std_core_types__string _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__string_decref(kk_std_core_types__string _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__string_dropn_reuse(kk_std_core_types__string _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__string_dropn(kk_std_core_types__string _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__string_reuse(kk_std_core_types__string _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__string kk_std_core_types__string_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__string_box(kk_std_core_types__string _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__string kk_std_core_types__string_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/vector
struct kk_std_core_types__vector_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__vector;
static inline kk_std_core_types__vector kk_std_core_types__vector_dup(kk_std_core_types__vector _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__vector_drop(kk_std_core_types__vector _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__vector_is_unique(kk_std_core_types__vector _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__vector_free(kk_std_core_types__vector _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__vector_decref(kk_std_core_types__vector _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__vector_dropn_reuse(kk_std_core_types__vector _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__vector_dropn(kk_std_core_types__vector _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__vector_reuse(kk_std_core_types__vector _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__vector kk_std_core_types__vector_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__vector_box(kk_std_core_types__vector _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__vector kk_std_core_types__vector_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// type std/core/types/void
struct kk_std_core_types__void_s {
  kk_block_t _block;
};
typedef kk_datatype_t kk_std_core_types__void;
static inline kk_std_core_types__void kk_std_core_types__void_dup(kk_std_core_types__void _x) {
  return kk_datatype_dup(_x);
}
static inline void kk_std_core_types__void_drop(kk_std_core_types__void _x, kk_context_t* _ctx) {
  kk_datatype_drop(_x, _ctx);
}
static inline bool kk_std_core_types__void_is_unique(kk_std_core_types__void _x) {
  return kk_datatype_is_unique(_x);
}
static inline void kk_std_core_types__void_free(kk_std_core_types__void _x) {
  kk_datatype_free(_x);
}
static inline void kk_std_core_types__void_decref(kk_std_core_types__void _x, kk_context_t* _ctx) {
  kk_datatype_decref(_x, _ctx);
}
static inline kk_reuse_t kk_std_core_types__void_dropn_reuse(kk_std_core_types__void _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  return kk_datatype_dropn_reuse(_x, _scan_fsize, _ctx);
}
static inline void kk_std_core_types__void_dropn(kk_std_core_types__void _x, kk_ssize_t _scan_fsize, kk_context_t* _ctx) {
  kk_datatype_dropn(_x, _scan_fsize, _ctx);
}
static inline kk_reuse_t kk_std_core_types__void_reuse(kk_std_core_types__void _x) {
  return kk_datatype_reuse(_x);
}
static inline kk_std_core_types__void kk_std_core_types__void_hole() {
  return kk_datatype_from_tag((kk_tag_t)0);
}
static inline kk_box_t kk_std_core_types__void_box(kk_std_core_types__void _x, kk_context_t* _ctx) {
  return kk_datatype_box(_x);
}
static inline kk_std_core_types__void kk_std_core_types__void_unbox(kk_box_t _x, kk_context_t* _ctx) {
  return kk_datatype_unbox(_x);
}

// value declarations

static inline kk_unit_t kk_std_core_types__copy(kk_unit_t _this, kk_context_t* _ctx) { /* (()) -> () */ 
  kk_Unit; return kk_Unit;
}
 
// Automatically generated. Retrieves the `fst` constructor field of the `:(,)` type.

static inline kk_box_t kk_std_core_types_fst(kk_std_core_types__tuple2_ _this, kk_context_t* _ctx) { /* forall<a,b> ((a, b)) -> a */ 
  {
    kk_box_t _x = _this.fst;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(_this, _ctx);
    return _x;
  }
}
 
// Automatically generated. Retrieves the `snd` constructor field of the `:(,)` type.

static inline kk_box_t kk_std_core_types_snd(kk_std_core_types__tuple2_ _this, kk_context_t* _ctx) { /* forall<a,b> ((a, b)) -> b */ 
  {
    kk_box_t _x = _this.snd;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(_this, _ctx);
    return _x;
  }
}

kk_std_core_types__tuple2_ kk_std_core_types__copy_1(kk_std_core_types__tuple2_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_context_t* _ctx); /* forall<a,b> ((a, b), fst : optional<a>, snd : optional<b>) -> (a, b) */ 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:(,,)` type.

static inline kk_box_t kk_std_core_types_fst_1(kk_std_core_types__tuple3_ _this, kk_context_t* _ctx) { /* forall<a,b,c> ((a, b, c)) -> a */ 
  {
    kk_box_t _x = _this.fst;
    kk_box_dup(_x);
    kk_std_core_types__tuple3__drop(_this, _ctx);
    return _x;
  }
}
 
// Automatically generated. Retrieves the `snd` constructor field of the `:(,,)` type.

static inline kk_box_t kk_std_core_types_snd_1(kk_std_core_types__tuple3_ _this, kk_context_t* _ctx) { /* forall<a,b,c> ((a, b, c)) -> b */ 
  {
    kk_box_t _x = _this.snd;
    kk_box_dup(_x);
    kk_std_core_types__tuple3__drop(_this, _ctx);
    return _x;
  }
}
 
// Automatically generated. Retrieves the `thd` constructor field of the `:(,,)` type.

static inline kk_box_t kk_std_core_types_thd(kk_std_core_types__tuple3_ _this, kk_context_t* _ctx) { /* forall<a,b,c> ((a, b, c)) -> c */ 
  {
    kk_box_t _x = _this.thd;
    kk_box_dup(_x);
    kk_std_core_types__tuple3__drop(_this, _ctx);
    return _x;
  }
}

kk_std_core_types__tuple3_ kk_std_core_types__copy_2(kk_std_core_types__tuple3_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_context_t* _ctx); /* forall<a,b,c> ((a, b, c), fst : optional<a>, snd : optional<b>, thd : optional<c>) -> (a, b, c) */ 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:(,,,)` type.

static inline kk_box_t kk_std_core_types_fst_2(kk_std_core_types__tuple4_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d> ((a, b, c, d)) -> a */ 
  {
    struct kk_std_core_types__Tuple4_* _con2449 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _x = _con2449->fst;
    kk_box_t _pat0 = _con2449->snd;
    kk_box_t _pat1 = _con2449->thd;
    kk_box_t _pat2 = _con2449->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(_this))) {
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types__tuple4__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__tuple4__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `snd` constructor field of the `:(,,,)` type.

static inline kk_box_t kk_std_core_types_snd_2(kk_std_core_types__tuple4_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d> ((a, b, c, d)) -> b */ 
  {
    struct kk_std_core_types__Tuple4_* _con2450 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2450->fst;
    kk_box_t _x = _con2450->snd;
    kk_box_t _pat1 = _con2450->thd;
    kk_box_t _pat2 = _con2450->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(_this))) {
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types__tuple4__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__tuple4__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `thd` constructor field of the `:(,,,)` type.

static inline kk_box_t kk_std_core_types_thd_1(kk_std_core_types__tuple4_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d> ((a, b, c, d)) -> c */ 
  {
    struct kk_std_core_types__Tuple4_* _con2451 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2451->fst;
    kk_box_t _pat1 = _con2451->snd;
    kk_box_t _x = _con2451->thd;
    kk_box_t _pat2 = _con2451->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(_this))) {
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types__tuple4__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__tuple4__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `field4` constructor field of the `:(,,,)` type.

static inline kk_box_t kk_std_core_types_field4(kk_std_core_types__tuple4_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d> ((a, b, c, d)) -> d */ 
  {
    struct kk_std_core_types__Tuple4_* _con2452 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2452->fst;
    kk_box_t _pat1 = _con2452->snd;
    kk_box_t _pat2 = _con2452->thd;
    kk_box_t _x = _con2452->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(_this))) {
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types__tuple4__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__tuple4__decref(_this, _ctx);
    }
    return _x;
  }
}

kk_std_core_types__tuple4_ kk_std_core_types__copy_3(kk_std_core_types__tuple4_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_std_core_types__optional field40, kk_context_t* _ctx); /* forall<a,b,c,d> ((a, b, c, d), fst : optional<a>, snd : optional<b>, thd : optional<c>, field4 : optional<d>) -> (a, b, c, d) */ 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:(,,,,)` type.

static inline kk_box_t kk_std_core_types_fst_3(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1)) -> a */ 
  {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2461 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _x = _con2461->fst;
    kk_box_t _pat0 = _con2461->snd;
    kk_box_t _pat1 = _con2461->thd;
    kk_box_t _pat2 = _con2461->field4;
    kk_box_t _pat3 = _con2461->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `snd` constructor field of the `:(,,,,)` type.

static inline kk_box_t kk_std_core_types_snd_3(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1)) -> b */ 
  {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2462 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2462->fst;
    kk_box_t _x = _con2462->snd;
    kk_box_t _pat1 = _con2462->thd;
    kk_box_t _pat2 = _con2462->field4;
    kk_box_t _pat3 = _con2462->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `thd` constructor field of the `:(,,,,)` type.

static inline kk_box_t kk_std_core_types_thd_2(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1)) -> c */ 
  {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2463 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2463->fst;
    kk_box_t _pat1 = _con2463->snd;
    kk_box_t _x = _con2463->thd;
    kk_box_t _pat2 = _con2463->field4;
    kk_box_t _pat3 = _con2463->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `field4` constructor field of the `:(,,,,)` type.

static inline kk_box_t kk_std_core_types_field4_1(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1)) -> d */ 
  {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2464 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2464->fst;
    kk_box_t _pat1 = _con2464->snd;
    kk_box_t _pat2 = _con2464->thd;
    kk_box_t _x = _con2464->field4;
    kk_box_t _pat3 = _con2464->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    return _x;
  }
}
 
// Automatically generated. Retrieves the `field5` constructor field of the `:(,,,,)` type.

static inline kk_box_t kk_std_core_types_field5(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1)) -> a1 */ 
  {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2465 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _pat0 = _con2465->fst;
    kk_box_t _pat1 = _con2465->snd;
    kk_box_t _pat2 = _con2465->thd;
    kk_box_t _pat3 = _con2465->field4;
    kk_box_t _x = _con2465->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat3, _ctx);
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat1, _ctx);
      kk_box_drop(_pat0, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    return _x;
  }
}

kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types__copy_4(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_std_core_types__optional field40, kk_std_core_types__optional field50, kk_context_t* _ctx); /* forall<a,b,c,d,a1> ((a, b, c, d, a1), fst : optional<a>, snd : optional<b>, thd : optional<c>, field4 : optional<d>, field5 : optional<a1>) -> (a, b, c, d, a1) */ 
 
// Automatically generated. Tests for the `False` constructor of the `:bool` type.

static inline bool kk_std_core_types_is_false(bool kkloc_bool, kk_context_t* _ctx) { /* (bool : bool) -> bool */ 
  if (!(kkloc_bool)) {
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Tests for the `True` constructor of the `:bool` type.

static inline bool kk_std_core_types_is_true(bool kkloc_bool, kk_context_t* _ctx) { /* (bool : bool) -> bool */ 
  if (kkloc_bool) {
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Retrieves the `unbox` constructor field of the `:box` type.

static inline kk_box_t kk_std_core_types_unbox(kk_std_core_types__box box, kk_context_t* _ctx) { /* forall<a> (box : box<a>) -> a */ 
  {
    kk_box_t _x = box.unbox;
    return _x;
  }
}

static inline kk_std_core_types__box kk_std_core_types__copy_5(kk_std_core_types__box _this, kk_std_core_types__optional unbox0, kk_context_t* _ctx) { /* forall<a> (box<a>, unbox : optional<a>) -> box<a> */ 
  kk_box_t _x2476;
  if (kk_std_core_types__is_Optional(unbox0)) {
    kk_box_t _unbox_1967 = unbox0._cons.Optional.value;
    kk_std_core_types__box_drop(_this, _ctx);
    _x2476 = _unbox_1967; /*1976*/
  }
  else {
    kk_box_t _x = _this.unbox;
    _x2476 = _x; /*1976*/
  }
  return kk_std_core_types__new_Box(_x2476, _ctx);
}
 
// Automatically generated. Retrieves the `res` constructor field of the `:ctail` type.

static inline kk_box_t kk_std_core_types_res(kk_std_core_types__ctail ctail, kk_context_t* _ctx) { /* forall<a> (ctail : ctail<a>) -> a */ 
  {
    kk_box_t _x = ctail.res;
    kk_box_dup(_x);
    kk_std_core_types__ctail_drop(ctail, _ctx);
    return _x;
  }
}
 
// Automatically generated. Retrieves the `hole` constructor field of the `:ctail` type.

static inline kk_box_t* kk_std_core_types_hole(kk_std_core_types__ctail ctail, kk_context_t* _ctx) { /* forall<a> (ctail : ctail<a>) -> cfield<a> */ 
  {
    kk_box_t* _x = ctail.hole;
    kk_std_core_types__ctail_drop(ctail, _ctx);
    return _x;
  }
}
 
// Automatically generated. Tests for the `Left` constructor of the `:either` type.

static inline bool kk_std_core_types_is_left(kk_std_core_types__either either, kk_context_t* _ctx) { /* forall<a,b> (either : either<a,b>) -> bool */ 
  if (kk_std_core_types__is_Left(either)) {
    kk_std_core_types__either_drop(either, _ctx);
    return true;
  }
  {
    kk_std_core_types__either_drop(either, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Right` constructor of the `:either` type.

static inline bool kk_std_core_types_is_right(kk_std_core_types__either either, kk_context_t* _ctx) { /* forall<a,b> (either : either<a,b>) -> bool */ 
  if (kk_std_core_types__is_Right(either)) {
    kk_std_core_types__either_drop(either, _ctx);
    return true;
  }
  {
    kk_std_core_types__either_drop(either, _ctx);
    return false;
  }
}
 
// Automatically generated. Retrieves the `unhbox` constructor field of the `:hbox` type.

static inline kk_box_t kk_std_core_types_unhbox(kk_std_core_types__hbox hbox0, kk_context_t* _ctx) { /* forall<a> (hbox : hbox<a>) -> a */ 
  {
    struct kk_std_core_types_Hbox* _con2477 = kk_std_core_types__as_Hbox(hbox0);
    kk_box_t _x = _con2477->unhbox;
    if (kk_likely(kk_std_core_types__hbox_is_unique(hbox0))) {
      kk_std_core_types__hbox_free(hbox0);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__hbox_decref(hbox0, _ctx);
    }
    return _x;
  }
}

kk_std_core_types__hbox kk_std_core_types__copy_6(kk_std_core_types__hbox _this, kk_std_core_types__optional unhbox0, kk_context_t* _ctx); /* forall<a> (hbox<a>, unhbox : optional<a>) -> hbox<a> */ 
 
// Automatically generated. Tests for the `Nothing` constructor of the `:maybe` type.

static inline bool kk_std_core_types_is_nothing(kk_std_core_types__maybe maybe, kk_context_t* _ctx) { /* forall<a> (maybe : maybe<a>) -> bool */ 
  if (kk_std_core_types__is_Nothing(maybe)) {
    return true;
  }
  {
    kk_std_core_types__maybe_drop(maybe, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Just` constructor of the `:maybe` type.

static inline bool kk_std_core_types_is_just(kk_std_core_types__maybe maybe, kk_context_t* _ctx) { /* forall<a> (maybe : maybe<a>) -> bool */ 
  if (kk_std_core_types__is_Just(maybe)) {
    kk_std_core_types__maybe_drop(maybe, _ctx);
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Tests for the `Optional` constructor of the `:optional` type.

static inline bool kk_std_core_types_is_optional(kk_std_core_types__optional optional, kk_context_t* _ctx) { /* forall<a> (optional : optional<a>) -> bool */ 
  if (kk_std_core_types__is_Optional(optional)) {
    kk_std_core_types__optional_drop(optional, _ctx);
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Tests for the `None` constructor of the `:optional` type.

static inline bool kk_std_core_types_is_none(kk_std_core_types__optional optional, kk_context_t* _ctx) { /* forall<a> (optional : optional<a>) -> bool */ 
  if (kk_std_core_types__is_None(optional)) {
    return true;
  }
  {
    kk_std_core_types__optional_drop(optional, _ctx);
    return false;
  }
}
 
// Automatically generated. Tests for the `Lt` constructor of the `:order` type.

static inline bool kk_std_core_types_is_lt(kk_std_core_types__order order, kk_context_t* _ctx) { /* (order : order) -> bool */ 
  if (kk_std_core_types__is_Lt(order)) {
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Tests for the `Eq` constructor of the `:order` type.

static inline bool kk_std_core_types_is_eq(kk_std_core_types__order order, kk_context_t* _ctx) { /* (order : order) -> bool */ 
  if (kk_std_core_types__is_Eq(order)) {
    return true;
  }
  {
    return false;
  }
}
 
// Automatically generated. Tests for the `Gt` constructor of the `:order` type.

static inline bool kk_std_core_types_is_gt(kk_std_core_types__order order, kk_context_t* _ctx) { /* (order : order) -> bool */ 
  if (kk_std_core_types__is_Gt(order)) {
    return true;
  }
  {
    return false;
  }
}

kk_box_t kk_std_core_types__open(kk_box_t x, kk_context_t* _ctx); /* forall<e,e1,a,b> (x : a) -> e1 b */ 

static inline bool kk_std_core_types__lp__excl__1_rp_(bool b, kk_context_t* _ctx) { /* (b : bool) -> bool */ 
  if (b) {
    return false;
  }
  {
    return true;
  }
}
 
// Logical conjuction

static inline bool kk_std_core_types__lp__amp__amp__rp_(bool x, bool y, kk_context_t* _ctx) { /* (x : bool, y : bool) -> bool */ 
  if (x) {
    return y;
  }
  {
    return false;
  }
}

static inline kk_box_t kk_std_core_types_id(kk_box_t x, kk_context_t* _ctx) { /* forall<a> (x : a) -> a */ 
  return x;
}

kk_box_t kk_std_core_types_keep(kk_box_t x, kk_context_t* _ctx); /* forall<a> (x : a) -> a */ 
 
// If local mutation is unobservable, the `:local` effect can be erased by using the `local-scope` function.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.

static inline kk_box_t kk_std_core_types_local_scope(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : forall<h> () -> <local<h>|e> a) -> e a */ 
  return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}

kk_reuse_t kk_std_core_types_no_reuse(kk_context_t* _ctx); /* () -> reuse */ 

static inline bool kk_std_core_types_not(bool b, kk_context_t* _ctx) { /* (b : bool) -> bool */ 
  if (b) {
    return false;
  }
  {
    return true;
  }
}

kk_box_t kk_std_core_types_run(kk_function_t action, kk_context_t* _ctx); /* forall<e,a> (action : forall<h> () -> <alloc<h>,read<h>,write<h>|e> a) -> e a */ 
 
// _Unsafe_. This function pretends the give action is terminating

static inline kk_box_t kk_std_core_types_unsafe_no_div(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <div|e> a) -> e a */ 
  return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}
 
// _Unsafe_. This function pretends the give action was deterministic

static inline kk_box_t kk_std_core_types_unsafe_no_ndet(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <ndet|e> a) -> e a */ 
  return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}
 
// _Unsafe_. This function calls a function and pretends it did not have any effect at all.

static inline kk_box_t kk_std_core_types_unsafe_total(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> e a) -> a */ 
  return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}
 
// Logical disjunction

static inline bool kk_std_core_types__lp__bar__bar__rp_(bool x, bool y, kk_context_t* _ctx) { /* (x : bool, y : bool) -> bool */ 
  if (x) {
    return true;
  }
  {
    return y;
  }
}

static inline kk_std_core_types__hbox kk_std_core_types_hbox(kk_box_t x, kk_context_t* _ctx) { /* forall<a> (x : a) -> hbox<a> */ 
  return kk_std_core_types__new_Hbox(kk_reuse_null, x, _ctx);
}

void kk_std_core_types__init(kk_context_t* _ctx);


void kk_std_core_types__done(kk_context_t* _ctx);

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_box_t kk_ctail_hole(void) {
  return kk_int_box(0);
}

static inline kk_std_core_types__ctail kk_ctail_nil(void) {
  return kk_std_core_types__new_CTail( kk_ctail_hole(), NULL, NULL );
}

static inline kk_std_core_types__ctail kk_ctail_link( kk_std_core_types__ctail acc, kk_box_t res, kk_box_t* field ) {
  return kk_std_core_types__new_CTail( (kk_likely(acc.hole != NULL) ? (*(acc.hole) = res, acc.res) : res ), field, NULL );
}

static inline kk_box_t kk_ctail_resolve( kk_std_core_types__ctail acc, kk_box_t res ) {
  return (kk_likely(acc.hole != NULL) ? (*(acc.hole) = res, acc.res) : res );
}


#endif // header
