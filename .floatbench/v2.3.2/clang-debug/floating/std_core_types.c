// Koka generated module: "std/core/types", koka version: 2.3.2, platform: 64-bit
#include "std_core_types.h"

kk_std_core_types__tuple2_ kk_std_core_types__copy_1(kk_std_core_types__tuple2_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_context_t* _ctx) { /* forall<a,b> ((a, b), fst : optional<a>, snd : optional<b>) -> (a, b) */ 
  kk_box_t _x2444;
  if (kk_std_core_types__is_Optional(fst0)) {
    kk_box_t _fst_382 = fst0._cons.Optional.value;
    _x2444 = _fst_382; /*403*/
  }
  else {
    kk_box_t _x = _this.fst;
    kk_box_dup(_x);
    _x2444 = _x; /*403*/
  }
  kk_box_t _x2445;
  if (kk_std_core_types__is_Optional(snd0)) {
    kk_box_t _snd_390 = snd0._cons.Optional.value;
    kk_std_core_types__tuple2__drop(_this, _ctx);
    _x2445 = _snd_390; /*404*/
  }
  else {
    kk_box_t _x0 = _this.snd;
    kk_box_dup(_x0);
    kk_std_core_types__tuple2__drop(_this, _ctx);
    _x2445 = _x0; /*404*/
  }
  return kk_std_core_types__new_dash__lp__comma__rp_(_x2444, _x2445, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core_types__copy_2(kk_std_core_types__tuple3_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_context_t* _ctx) { /* forall<a,b,c> ((a, b, c), fst : optional<a>, snd : optional<b>, thd : optional<c>) -> (a, b, c) */ 
  kk_box_t _x2446;
  if (kk_std_core_types__is_Optional(fst0)) {
    kk_box_t _fst_564 = fst0._cons.Optional.value;
    _x2446 = _fst_564; /*627*/
  }
  else {
    kk_box_t _x = _this.fst;
    kk_box_dup(_x);
    _x2446 = _x; /*627*/
  }
  kk_box_t _x2447;
  if (kk_std_core_types__is_Optional(snd0)) {
    kk_box_t _snd_601 = snd0._cons.Optional.value;
    _x2447 = _snd_601; /*628*/
  }
  else {
    kk_box_t _x0 = _this.snd;
    kk_box_dup(_x0);
    _x2447 = _x0; /*628*/
  }
  kk_box_t _x2448;
  if (kk_std_core_types__is_Optional(thd0)) {
    kk_box_t _thd_610 = thd0._cons.Optional.value;
    kk_std_core_types__tuple3__drop(_this, _ctx);
    _x2448 = _thd_610; /*629*/
  }
  else {
    kk_box_t _x1 = _this.thd;
    kk_box_dup(_x1);
    kk_std_core_types__tuple3__drop(_this, _ctx);
    _x2448 = _x1; /*629*/
  }
  return kk_std_core_types__new_dash__lp__comma__comma__rp_(_x2446, _x2447, _x2448, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core_types__copy_3(kk_std_core_types__tuple4_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_std_core_types__optional field40, kk_context_t* _ctx) { /* forall<a,b,c,d> ((a, b, c, d), fst : optional<a>, snd : optional<b>, thd : optional<c>, field4 : optional<d>) -> (a, b, c, d) */ 
  kk_box_t _x2453;
  if (kk_std_core_types__is_Optional(fst0)) {
    kk_box_t _fst_937 = fst0._cons.Optional.value;
    _x2453 = _fst_937; /*1073*/
  }
  else {
    struct kk_std_core_types__Tuple4_* _con2454 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _x = _con2454->fst;
    kk_box_dup(_x);
    _x2453 = _x; /*1073*/
  }
  kk_box_t _x2455;
  if (kk_std_core_types__is_Optional(snd0)) {
    kk_box_t _snd_995 = snd0._cons.Optional.value;
    _x2455 = _snd_995; /*1074*/
  }
  else {
    struct kk_std_core_types__Tuple4_* _con2456 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _x0 = _con2456->snd;
    kk_box_dup(_x0);
    _x2455 = _x0; /*1074*/
  }
  kk_box_t _x2457;
  if (kk_std_core_types__is_Optional(thd0)) {
    kk_box_t _thd_1042 = thd0._cons.Optional.value;
    _x2457 = _thd_1042; /*1075*/
  }
  else {
    struct kk_std_core_types__Tuple4_* _con2458 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _x1 = _con2458->thd;
    kk_box_dup(_x1);
    _x2457 = _x1; /*1075*/
  }
  kk_box_t _x2459;
  if (kk_std_core_types__is_Optional(field40)) {
    kk_box_t _field4_1052 = field40._cons.Optional.value;
    kk_std_core_types__tuple4__dropn(_this, ((int32_t)KI32(4)), _ctx);
    _x2459 = _field4_1052; /*1076*/
  }
  else {
    struct kk_std_core_types__Tuple4_* _con2460 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(_this);
    kk_box_t _pat03 = _con2460->fst;
    kk_box_t _pat14 = _con2460->snd;
    kk_box_t _pat23 = _con2460->thd;
    kk_box_t _x2 = _con2460->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(_this))) {
      kk_box_drop(_pat23, _ctx);
      kk_box_drop(_pat14, _ctx);
      kk_box_drop(_pat03, _ctx);
      kk_std_core_types__tuple4__free(_this);
    }
    else {
      kk_box_dup(_x2);
      kk_std_core_types__tuple4__decref(_this, _ctx);
    }
    _x2459 = _x2; /*1076*/
  }
  return kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, _x2453, _x2455, _x2457, _x2459, _ctx);
}

kk_std_core_types___lp__comma__comma__comma__comma__rp_ kk_std_core_types__copy_4(kk_std_core_types___lp__comma__comma__comma__comma__rp_ _this, kk_std_core_types__optional fst0, kk_std_core_types__optional snd0, kk_std_core_types__optional thd0, kk_std_core_types__optional field40, kk_std_core_types__optional field50, kk_context_t* _ctx) { /* forall<a,b,c,d,a1> ((a, b, c, d, a1), fst : optional<a>, snd : optional<b>, thd : optional<c>, field4 : optional<d>, field5 : optional<a1>) -> (a, b, c, d, a1) */ 
  kk_box_t _x2466;
  if (kk_std_core_types__is_Optional(fst0)) {
    kk_box_t _fst_1596 = fst0._cons.Optional.value;
    _x2466 = _fst_1596; /*1845*/
  }
  else {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2467 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _x = _con2467->fst;
    kk_box_dup(_x);
    _x2466 = _x; /*1845*/
  }
  kk_box_t _x2468;
  if (kk_std_core_types__is_Optional(snd0)) {
    kk_box_t _snd_1680 = snd0._cons.Optional.value;
    _x2468 = _snd_1680; /*1846*/
  }
  else {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2469 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _x0 = _con2469->snd;
    kk_box_dup(_x0);
    _x2468 = _x0; /*1846*/
  }
  kk_box_t _x2470;
  if (kk_std_core_types__is_Optional(thd0)) {
    kk_box_t _thd_1752 = thd0._cons.Optional.value;
    _x2470 = _thd_1752; /*1847*/
  }
  else {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2471 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _x1 = _con2471->thd;
    kk_box_dup(_x1);
    _x2470 = _x1; /*1847*/
  }
  kk_box_t _x2472;
  if (kk_std_core_types__is_Optional(field40)) {
    kk_box_t _field4_1809 = field40._cons.Optional.value;
    _x2472 = _field4_1809; /*1848*/
  }
  else {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2473 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _x2 = _con2473->field4;
    kk_box_dup(_x2);
    _x2472 = _x2; /*1848*/
  }
  kk_box_t _x2474;
  if (kk_std_core_types__is_Optional(field50)) {
    kk_box_t _field5_1820 = field50._cons.Optional.value;
    kk_std_core_types___lp__comma__comma__comma__comma__rp__dropn(_this, ((int32_t)KI32(5)), _ctx);
    _x2474 = _field5_1820; /*1849*/
  }
  else {
    struct kk_std_core_types__lp__comma__comma__comma__comma__rp_* _con2475 = kk_std_core_types__as_dash__lp__comma__comma__comma__comma__rp_(_this);
    kk_box_t _pat04 = _con2475->fst;
    kk_box_t _pat15 = _con2475->snd;
    kk_box_t _pat24 = _con2475->thd;
    kk_box_t _pat34 = _con2475->field4;
    kk_box_t _x3 = _con2475->field5;
    if (kk_likely(kk_std_core_types___lp__comma__comma__comma__comma__rp__is_unique(_this))) {
      kk_box_drop(_pat34, _ctx);
      kk_box_drop(_pat24, _ctx);
      kk_box_drop(_pat15, _ctx);
      kk_box_drop(_pat04, _ctx);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__free(_this);
    }
    else {
      kk_box_dup(_x3);
      kk_std_core_types___lp__comma__comma__comma__comma__rp__decref(_this, _ctx);
    }
    _x2474 = _x3; /*1849*/
  }
  return kk_std_core_types__new_dash__lp__comma__comma__comma__comma__rp_(kk_reuse_null, _x2466, _x2468, _x2470, _x2472, _x2474, _ctx);
}

kk_std_core_types__hbox kk_std_core_types__copy_6(kk_std_core_types__hbox _this, kk_std_core_types__optional unhbox0, kk_context_t* _ctx) { /* forall<a> (hbox<a>, unhbox : optional<a>) -> hbox<a> */ 
  kk_box_t _x2478;
  if (kk_std_core_types__is_Optional(unhbox0)) {
    kk_box_t _unhbox_2093 = unhbox0._cons.Optional.value;
    kk_std_core_types__hbox_dropn(_this, ((int32_t)KI32(1)), _ctx);
    _x2478 = _unhbox_2093; /*2102*/
  }
  else {
    struct kk_std_core_types_Hbox* _con2479 = kk_std_core_types__as_Hbox(_this);
    kk_box_t _x = _con2479->unhbox;
    if (kk_likely(kk_std_core_types__hbox_is_unique(_this))) {
      kk_std_core_types__hbox_free(_this);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__hbox_decref(_this, _ctx);
    }
    _x2478 = _x; /*2102*/
  }
  return kk_std_core_types__new_Hbox(kk_reuse_null, _x2478, _ctx);
}
 
// Generated by type inference and later refined into one of the `open` variants in `std/core/hnd`.

kk_box_t kk_std_core_types__open(kk_box_t x, kk_context_t* _ctx) { /* forall<e,e1,a,b> (x : a) -> e1 b */ 
  return (x);
}

kk_box_t kk_std_core_types_keep(kk_box_t x, kk_context_t* _ctx) { /* forall<a> (x : a) -> a */ 
  return x;
}

kk_reuse_t kk_std_core_types_no_reuse(kk_context_t* _ctx) { /* () -> reuse */ 
  return NULL;
}
 
// If a heap effect is unobservable, the heap effect can be erased by using the `run` fun.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.

kk_box_t kk_std_core_types_run(kk_function_t action, kk_context_t* _ctx) { /* forall<e,a> (action : forall<h> () -> <alloc<h>,read<h>,write<h>|e> a) -> e a */ 
  return (kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),action,(action,kk_context())));
}

// initialization
void kk_std_core_types__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
}

// termination
void kk_std_core_types__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
}
