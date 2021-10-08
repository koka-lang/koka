// Koka generated module: "test/float/bench2", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench2.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench2__handle_bra(int32_t cfc, kk_test_float_bench2__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1108 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_bra); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1108, cfc, kk_test_float_bench2__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench2__handle_count(int32_t cfc, kk_test_float_bench2__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1111 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_count); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1111, cfc, kk_test_float_bench2__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench2_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_788;
  kk_ssize_t _x1119 = ((kk_ssize_t)0); /*ssize_t*/
  ev_788 = kk_evv_at(_x1119,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1120;
  {
    struct kk_std_core_hnd_Ev* _con1121 = kk_std_core_hnd__as_Ev(ev_788);
    kk_std_core_hnd__marker m0 = _con1121->marker;
    kk_box_t _box_x875 = _con1121->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x875, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1104 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x878 = _match_1104.clause;
      _x1120 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x878, (_fun_unbox_x878, m0, ev_788, _ctx)); /*37*/
    }
  }
  kk_unit_unbox(_x1120); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1128__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_one___fun1128(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1128(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1128__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_one___fun1128__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_one___fun1128, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_one___fun1128(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1128__t* _self = kk_function_as(struct kk_test_float_bench2_one___fun1128__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1129;
  bool b_16563;
  kk_integer_t _x1130 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1130,kk_context()); /*bool*/
  if (b_16563) {
    _x1129 = a; /*int*/
  }
  else {
    _x1129 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1129);
}

kk_integer_t kk_test_float_bench2_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_735;
  kk_box_t _x1127 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_one___fun1128(a, _ctx), _ctx); /*3214*/
  a0_735 = kk_integer_unbox(_x1127); /*int*/
  kk_std_core_hnd__ev ev_793;
  kk_ssize_t _x1131 = ((kk_ssize_t)0); /*ssize_t*/
  ev_793 = kk_evv_at(_x1131,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1132;
  {
    struct kk_std_core_hnd_Ev* _con1133 = kk_std_core_hnd__as_Ev(ev_793);
    kk_std_core_hnd__marker m0 = _con1133->marker;
    kk_box_t _box_x891 = _con1133->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x891, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1102 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
    {
      kk_function_t _fun_unbox_x895 = _match_1102.clause;
      _x1132 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x895, (_fun_unbox_x895, m0, ev_793, kk_integer_box(a0_735), _ctx)); /*52*/
    }
  }
  return kk_integer_unbox(_x1132);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1140__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_two___fun1140(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1140(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1140__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_two___fun1140__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_two___fun1140, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_two___fun1140(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1140__t* _self = kk_function_as(struct kk_test_float_bench2_two___fun1140__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1141;
  bool b_16563;
  kk_integer_t _x1142 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1142,kk_context()); /*bool*/
  if (b_16563) {
    _x1141 = a; /*int*/
  }
  else {
    _x1141 = kk_integer_add(a,(kk_integer_from_small(2)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1141);
}

kk_integer_t kk_test_float_bench2_two__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_736;
  kk_box_t _x1139 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_two___fun1140(a, _ctx), _ctx); /*3214*/
  a0_736 = kk_integer_unbox(_x1139); /*int*/
  kk_std_core_hnd__ev ev_799;
  kk_ssize_t _x1143 = ((kk_ssize_t)0); /*ssize_t*/
  ev_799 = kk_evv_at(_x1143,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1144;
  {
    struct kk_std_core_hnd_Ev* _con1145 = kk_std_core_hnd__as_Ev(ev_799);
    kk_std_core_hnd__marker m0 = _con1145->marker;
    kk_box_t _box_x909 = _con1145->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x909, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1100 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
    {
      kk_function_t _fun_unbox_x913 = _match_1100.clause;
      _x1144 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x913, (_fun_unbox_x913, m0, ev_799, kk_integer_box(a0_736), _ctx)); /*52*/
    }
  }
  return kk_integer_unbox(_x1144);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift773_f_fun1149__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift773_f_fun1149(kk_function_t _fself, kk_box_t _b_925, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift773_f_fun1149(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift773_f_fun1149__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift773_f_fun1149__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift773_f_fun1149, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift773_f_fun1149(kk_function_t _fself, kk_box_t _b_925, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift773_f_fun1149__t* _self = kk_function_as(struct kk_test_float_bench2__mlift773_f_fun1149__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_925, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift773_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_749, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_922_920 = kk_integer_add(_y_749,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_802 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_922_920)),kk_context()));
  kk_box_t _x1148;
  if (kk_yielding(kk_context())) {
    _x1148 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift773_f_fun1149(i, _ctx), _ctx); /*3860*/
  }
  else {
    _x1148 = (kk_ref_get(i,kk_context())); /*3860*/
  }
  return kk_integer_unbox(_x1148);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift774_f_fun1153__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift774_f_fun1153(kk_function_t _fself, kk_box_t _b_934, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift774_f_fun1153(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift774_f_fun1153__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift774_f_fun1153__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift774_f_fun1153, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift774_f_fun1153(kk_function_t _fself, kk_box_t _b_934, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift774_f_fun1153__t* _self = kk_function_as(struct kk_test_float_bench2__mlift774_f_fun1153__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1154;
  kk_integer_t _x1155 = kk_integer_unbox(_b_934); /*int*/
  _x1154 = kk_test_float_bench2__mlift773_f(c, i, _x1155, _ctx); /*int*/
  return kk_integer_box(_x1154);
}

kk_integer_t kk_test_float_bench2__mlift774_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_806;
  kk_box_t _x1150;
  kk_ref_t _x1151 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1150 = (kk_ref_get(_x1151,kk_context())); /*233*/
  x_806 = kk_integer_unbox(_x1150); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_806, _ctx);
    kk_box_t _x1152 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift774_f_fun1153(c, i, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1152);
  }
  {
    return kk_test_float_bench2__mlift773_f(c, i, x_806, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift776_f_fun1158__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift776_f_fun1158(kk_function_t _fself, kk_box_t _b_945, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift776_f_fun1158(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift776_f_fun1158__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift776_f_fun1158__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift776_f_fun1158, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift776_f_fun1158(kk_function_t _fself, kk_box_t _b_945, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift776_f_fun1158__t* _self = kk_function_as(struct kk_test_float_bench2__mlift776_f_fun1158__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_945, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift776_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_753, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_942_940 = kk_integer_add(_y_753,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_808 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_942_940)),kk_context()));
  kk_box_t _x1157;
  if (kk_yielding(kk_context())) {
    _x1157 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift776_f_fun1158(i, _ctx), _ctx); /*3860*/
  }
  else {
    _x1157 = (kk_ref_get(i,kk_context())); /*3860*/
  }
  return kk_integer_unbox(_x1157);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift777_f_fun1162__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift777_f_fun1162(kk_function_t _fself, kk_box_t _b_954, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift777_f_fun1162(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift777_f_fun1162__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift777_f_fun1162__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift777_f_fun1162, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift777_f_fun1162(kk_function_t _fself, kk_box_t _b_954, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift777_f_fun1162__t* _self = kk_function_as(struct kk_test_float_bench2__mlift777_f_fun1162__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1163;
  kk_integer_t _x1164 = kk_integer_unbox(_b_954); /*int*/
  _x1163 = kk_test_float_bench2__mlift776_f(c, i, _x1164, _ctx); /*int*/
  return kk_integer_box(_x1163);
}

kk_integer_t kk_test_float_bench2__mlift777_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_1 : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_812;
  kk_box_t _x1159;
  kk_ref_t _x1160 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1159 = (kk_ref_get(_x1160,kk_context())); /*233*/
  x_812 = kk_integer_unbox(_x1159); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_812, _ctx);
    kk_box_t _x1161 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift777_f_fun1162(c, i, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1161);
  }
  {
    return kk_test_float_bench2__mlift776_f(c, i, x_812, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift780_f_fun1167__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift780_f_fun1167(kk_function_t _fself, kk_box_t _b_958, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift780_f_fun1167(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift780_f_fun1167, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift780_f_fun1167(kk_function_t _fself, kk_box_t _b_958, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1168 = kk_Unit;
  kk_integer_t _x1169 = kk_integer_unbox(_b_958); /*int*/
  kk_test_float_bench2__mlift779_f(_x1169, _ctx);
  return kk_unit_box(_x1168);
}

kk_unit_t kk_test_float_bench2__mlift780_f(kk_integer_t a2, kk_context_t* _ctx) { /* (a2 : int) -> count () */ 
  kk_integer_t x_814 = kk_test_float_bench2_one__(a2, _ctx); /*int*/;
  kk_integer_drop(x_814, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1166 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift780_f_fun1167(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1166); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift781_f_fun1171__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift781_f_fun1171(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift781_f_fun1171(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift781_f_fun1171, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift781_f_fun1171(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1172 = kk_Unit;
  kk_integer_t _x1173 = kk_integer_unbox(_b_961); /*int*/
  kk_test_float_bench2__mlift780_f(_x1173, _ctx);
  return kk_unit_box(_x1172);
}

kk_unit_t kk_test_float_bench2__mlift781_f(kk_integer_t a1, kk_context_t* _ctx) { /* (a1 : int) -> count () */ 
  kk_integer_t x_816 = kk_test_float_bench2_two__(a1, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_816, _ctx);
    kk_box_t _x1170 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift781_f_fun1171(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1170); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift780_f(x_816, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift782_f_fun1175__t {
  struct kk_function_s _base;
  kk_integer_t a00;
};
static kk_box_t kk_test_float_bench2__mlift782_f_fun1175(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift782_f_fun1175(kk_integer_t a00, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift782_f_fun1175__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift782_f_fun1175__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift782_f_fun1175, kk_context());
  _self->a00 = a00;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2__mlift782_f_fun1178__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift782_f_fun1178(kk_function_t _fself, kk_box_t _b_964, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift782_f_fun1178(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift782_f_fun1178, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift782_f_fun1178(kk_function_t _fself, kk_box_t _b_964, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1179 = kk_Unit;
  kk_integer_t _x1180 = kk_integer_unbox(_b_964); /*int*/
  kk_test_float_bench2__mlift781_f(_x1180, _ctx);
  return kk_unit_box(_x1179);
}
static kk_box_t kk_test_float_bench2__mlift782_f_fun1175(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift782_f_fun1175__t* _self = kk_function_as(struct kk_test_float_bench2__mlift782_f_fun1175__t*, _fself);
  kk_integer_t a00 = _self->a00; /* int */
  kk_drop_match(_self, {kk_integer_dup(a00);}, {}, _ctx)
  kk_unit_t _x1176 = kk_Unit;
  kk_integer_t x_818 = kk_test_float_bench2_one__(a00, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_818, _ctx);
    kk_box_t _x1177 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift782_f_fun1178(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1177);
  }
  else {
    kk_test_float_bench2__mlift781_f(x_818, _ctx);
  }
  return kk_unit_box(_x1176);
}

kk_unit_t kk_test_float_bench2__mlift782_f(kk_integer_t a00, kk_context_t* _ctx) { /* forall<h> (a00 : int) -> <local<h>,count,bra,div> () */ 
  kk_ssize_t _b_967_965 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_box_t _x1174 = kk_std_core_hnd__open_at0(_b_967_965, kk_test_float_bench2__new_mlift782_f_fun1175(a00, _ctx), _ctx); /*5322*/
  kk_unit_unbox(_x1174); return kk_Unit;
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift783_f_fun1183__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift783_f_fun1183(kk_function_t _fself, kk_box_t _b_973, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift783_f_fun1183(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift783_f_fun1183, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift783_f_fun1183(kk_function_t _fself, kk_box_t _b_973, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1184 = kk_Unit;
  kk_integer_t _x1185 = kk_integer_unbox(_b_973); /*int*/
  kk_test_float_bench2__mlift782_f(_x1185, _ctx);
  return kk_unit_box(_x1184);
}

kk_unit_t kk_test_float_bench2__mlift783_f(kk_ref_t i, kk_unit_t wild__3, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_3 : ()) -> <bra,count,div,local<h>> () */ 
  kk_integer_t x_820;
  kk_box_t _x1181 = (kk_ref_get(i,kk_context())); /*233*/
  x_820 = kk_integer_unbox(_x1181); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_820, _ctx);
    kk_box_t _x1182 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift783_f_fun1183(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1182); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift782_f(x_820, _ctx); return kk_Unit;
  }
}
 
// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// }


// lift anonymous function
struct kk_test_float_bench2_f_fun1190__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench2_f_fun1190(kk_function_t _fself, kk_std_core_hnd__marker _b_987, kk_std_core_hnd__ev _b_988, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1190(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1190__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1190__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1190, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1190(kk_function_t _fself, kk_std_core_hnd__marker _b_987, kk_std_core_hnd__ev _b_988, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1190__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1190__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_988, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1191 = kk_Unit;
  kk_integer_t _b_1039_985;
  kk_integer_t _x1192;
  kk_box_t _x1193 = (kk_ref_get(loc,kk_context())); /*233*/
  _x1192 = kk_integer_unbox(_x1193); /*int*/
  kk_integer_t _x1194;
  kk_box_t _x1195 = (kk_ref_get(loc0,kk_context())); /*233*/
  _x1194 = kk_integer_unbox(_x1195); /*int*/
  _b_1039_985 = kk_integer_add(_x1192,_x1194,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_1039_985)),kk_context()));
  return kk_unit_box(_x1191);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1196__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1196(kk_function_t _fself, kk_box_t _b_1032, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1196(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1196, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1196(kk_function_t _fself, kk_box_t _b_1032, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_1032;
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1198__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1198(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1198(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1198__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1198__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1198, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1202__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1202(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1202(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1202__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1202__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1202, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1206__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1206(kk_function_t _fself, kk_box_t _b_996, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1206(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1206__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1206__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1206, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1211__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1211(kk_function_t _fself, kk_box_t _b_994, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1211(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1211__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1211__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1211, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1211(kk_function_t _fself, kk_box_t _b_994, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1211__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1211__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1212;
  kk_unit_t _x1213 = kk_Unit;
  kk_unit_unbox(_b_994);
  _x1212 = kk_test_float_bench2__mlift774_f(loc0, loc, _x1213, _ctx); /*int*/
  return kk_integer_box(_x1212);
}
static kk_box_t kk_test_float_bench2_f_fun1206(kk_function_t _fself, kk_box_t _b_996, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1206__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1206__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1207;
  kk_integer_t _b_992_990;
  kk_integer_t _x1208 = kk_integer_unbox(_b_996); /*int*/
  _b_992_990 = kk_integer_add(_x1208,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_829 = kk_Unit;
  kk_ref_t _x1209 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1209,(kk_integer_box(_b_992_990)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1210 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1211(loc, loc0, _ctx), _ctx); /*3860*/
    _x1207 = kk_integer_unbox(_x1210); /*int*/
  }
  else {
    _x1207 = kk_test_float_bench2__mlift774_f(loc0, loc, x_829, _ctx); /*int*/
  }
  return kk_integer_box(_x1207);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1215__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_box_t _b_1004, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1215(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1215__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1215__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1215, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1220__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_box_t _b_1002, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1220(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1220__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1220__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1220, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_box_t _b_1002, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1220__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1220__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1221;
  kk_unit_t _x1222 = kk_Unit;
  kk_unit_unbox(_b_1002);
  _x1221 = kk_test_float_bench2__mlift777_f(loc0, loc, _x1222, _ctx); /*int*/
  return kk_integer_box(_x1221);
}
static kk_box_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_box_t _b_1004, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1215__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1215__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1216;
  kk_integer_t _b_1000_998;
  kk_integer_t _x1217 = kk_integer_unbox(_b_1004); /*int*/
  _b_1000_998 = kk_integer_add(_x1217,(kk_integer_from_small(2)),kk_context()); /*int*/
  kk_unit_t x0_831 = kk_Unit;
  kk_ref_t _x1218 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1218,(kk_integer_box(_b_1000_998)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1219 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1220(loc, loc0, _ctx), _ctx); /*3860*/
    _x1216 = kk_integer_unbox(_x1219); /*int*/
  }
  else {
    _x1216 = kk_test_float_bench2__mlift777_f(loc0, loc, x0_831, _ctx); /*int*/
  }
  return kk_integer_box(_x1216);
}
static kk_box_t kk_test_float_bench2_f_fun1202(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1202__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1202__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_test_float_bench2__hnd_count _x1203;
  kk_std_core_hnd__clause1 _x1204;
  kk_function_t _x1205;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1205 = kk_test_float_bench2_new_f_fun1206(loc, loc0, _ctx); /*(8873) -> 8871 8874*/
  _x1204 = kk_std_core_hnd_clause_tail1(_x1205, _ctx); /*std/core/hnd/clause1<8873,8874,8875,8871,8872>*/
  kk_std_core_hnd__clause1 _x1214 = kk_std_core_hnd_clause_tail1(kk_test_float_bench2_new_f_fun1215(loc, loc0, _ctx), _ctx); /*std/core/hnd/clause1<8873,8874,8875,8871,8872>*/
  _x1203 = kk_test_float_bench2__new_Hnd_count(kk_reuse_null, _x1204, _x1214, _ctx); /*test/float/bench2/.hnd-count<20,21>*/
  return kk_test_float_bench2__hnd_count_box(_x1203, _ctx);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1224__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1224(kk_function_t _fself, kk_box_t _b_1027, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1224(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1224__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1224__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1224, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1224(kk_function_t _fself, kk_box_t _b_1027, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1224__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1224__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_1027, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1225__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1225(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1225(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1225__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1225__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1225, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1228__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static bool kk_test_float_bench2_f_fun1228(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1228(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1228__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1228__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1228, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1231__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1231(kk_function_t _fself, kk_box_t _b_1010, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1231(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1231, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1231(kk_function_t _fself, kk_box_t _b_1010, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1232;
  kk_integer_t _x1233 = kk_integer_unbox(_b_1010); /*int*/
  kk_integer_t _x1234 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1232 = kk_integer_lt(_x1233,_x1234,kk_context()); /*bool*/
  return kk_bool_box(_x1232);
}
static bool kk_test_float_bench2_f_fun1228(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1228__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1228__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x1_833;
  kk_box_t _x1229 = (kk_ref_get(loc,kk_context())); /*233*/
  x1_833 = kk_integer_unbox(_x1229); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x1_833, _ctx);
    kk_box_t _x1230 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1231(_ctx), _ctx); /*3860*/
    return kk_bool_unbox(_x1230);
  }
  {
    kk_integer_t _x1235 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x1_833,_x1235,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1236__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1236(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1236(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1236__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1236__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1236, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1238__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1238(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1238(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1238, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1238(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1239 = kk_Unit;
  kk_std_core_hnd__ev ev_838;
  kk_ssize_t _x1240 = ((kk_ssize_t)0); /*ssize_t*/
  ev_838 = kk_evv_at(_x1240,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1241;
  {
    struct kk_std_core_hnd_Ev* _con1242 = kk_std_core_hnd__as_Ev(ev_838);
    kk_std_core_hnd__marker m0 = _con1242->marker;
    kk_box_t _box_x1011 = _con1242->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x1011, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1088 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x1014 = _match_1088.clause;
      _x1241 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x1014, (_fun_unbox_x1014, m0, ev_838, _ctx)); /*37*/
    }
  }
  kk_unit_unbox(_x1241);
  return kk_unit_box(_x1239);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1245__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1245(kk_function_t _fself, kk_box_t _b_1022, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1245(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1245__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1245__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1245, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1245(kk_function_t _fself, kk_box_t _b_1022, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1245__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1245__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1246 = kk_Unit;
  kk_unit_t _x1247 = kk_Unit;
  kk_unit_unbox(_b_1022);
  kk_test_float_bench2__mlift783_f(loc, _x1247, _ctx);
  return kk_unit_box(_x1246);
}
static kk_unit_t kk_test_float_bench2_f_fun1236(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1236__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1236__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_1019_1017 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x2_836 = kk_Unit;
  kk_box_t _x1237 = kk_std_core_hnd__open_at0(_b_1019_1017, kk_test_float_bench2_new_f_fun1238(_ctx), _ctx); /*5322*/
  kk_unit_unbox(_x1237);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1244 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1245(loc, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x1244);
  }
  {
    return kk_test_float_bench2__mlift783_f(loc, x2_836, _ctx);
  }
}
static kk_box_t kk_test_float_bench2_f_fun1225(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1225__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1225__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1226 = kk_Unit;
  kk_function_t _x1227;
  kk_ref_dup(loc);
  _x1227 = kk_test_float_bench2_new_f_fun1228(loc, _ctx); /*() -> <div,local<722>,test/float/bench2/bra,test/float/bench2/count> bool*/
  kk_std_core_while(_x1227, kk_test_float_bench2_new_f_fun1236(loc, _ctx), _ctx);
  return kk_unit_box(_x1226);
}
static kk_box_t kk_test_float_bench2_f_fun1198(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1198__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1198__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1199;
  int32_t _b_1042_1023 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench2__hnd_count _b_1043_1024;
  kk_box_t _x1200;
  kk_function_t _x1201;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1201 = kk_test_float_bench2_new_f_fun1202(loc, loc0, _ctx); /*() -> 3215 3214*/
  _x1200 = kk_std_core_hnd__open_none0(_x1201, _ctx); /*3214*/
  _b_1043_1024 = kk_test_float_bench2__hnd_count_unbox(_x1200, _ctx); /*test/float/bench2/.hnd-count<<local<722>,test/float/bench2/bra,div>,int>*/
  kk_box_t _x1223 = kk_test_float_bench2__handle_count(_b_1042_1023, _b_1043_1024, kk_test_float_bench2_new_f_fun1224(loc0, _ctx), kk_test_float_bench2_new_f_fun1225(loc, _ctx), _ctx); /*171*/
  _x1199 = kk_integer_unbox(_x1223); /*int*/
  return kk_integer_box(_x1199);
}

kk_integer_t kk_test_float_bench2_f(kk_context_t* _ctx) { /* () -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  int32_t _b_1033_1028 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1186;
  kk_test_float_bench2__hnd_bra _x1187;
  kk_std_core_hnd__clause0 _x1188;
  kk_function_t _x1189;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1189 = kk_test_float_bench2_new_f_fun1190(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<39,40>, std/core/hnd/ev<38>) -> 39 37*/
  _x1188 = kk_std_core_hnd__new_Clause0(_x1189, _ctx); /*std/core/hnd/clause0<37,38,39,40>*/
  _x1187 = kk_test_float_bench2__new_Hnd_bra(kk_reuse_null, _x1188, _ctx); /*test/float/bench2/.hnd-bra<6,7>*/
  kk_function_t _x1197;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1197 = kk_test_float_bench2_new_f_fun1198(loc, loc0, _ctx); /*() -> <test/float/bench2/bra|141> 140*/
  _x1186 = kk_test_float_bench2__handle_bra(_b_1033_1028, _x1187, kk_test_float_bench2_new_f_fun1196(_ctx), _x1197, _ctx); /*142*/
  res1 = kk_integer_unbox(_x1186); /*int*/
  kk_integer_t res0;
  kk_box_t _x1248 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*9897*/
  res0 = kk_integer_unbox(_x1248); /*int*/
  kk_integer_t res;
  kk_box_t _x1249 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*9897*/
  res = kk_integer_unbox(_x1249); /*int*/
  kk_box_t _x1250 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*9897*/
  return kk_integer_unbox(_x1250);
}

// main exit
static void _kk_main_exit(void) {
  kk_context_t* _ctx = kk_get_context();
  kk_test_float_bench2__done(_ctx);
}

// main entry
int main(int argc, char** argv) {
  kk_assert(sizeof(size_t)==8 && sizeof(void*)==8);
  kk_context_t* _ctx = kk_main_start(argc, argv);
  kk_test_float_bench2__init(_ctx);
  atexit(&_kk_main_exit);
  kk_test_float_bench2_main(_ctx);
  kk_test_float_bench2__done(_ctx);
  kk_main_end(_ctx);
  return 0;
}

// initialization
void kk_test_float_bench2__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  kk_std_core_types__init(_ctx);
  kk_std_core_hnd__init(_ctx);
  kk_std_core__init(_ctx);
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
  {
    kk_string_t _x1106;
    kk_define_string_literal(, _s1107, 10, "bra.bench2")
    _x1106 = kk_string_dup(_s1107); /*string*/
    kk_test_float_bench2__tag_bra = kk_std_core_hnd__new_Htag(_x1106, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  }
  {
    kk_string_t _x1109;
    kk_define_string_literal(, _s1110, 12, "count.bench2")
    _x1109 = kk_string_dup(_s1110); /*string*/
    kk_test_float_bench2__tag_count = kk_std_core_hnd__new_Htag(_x1109, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  }
}

// termination
void kk_test_float_bench2__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_core_hnd__htag_drop(kk_test_float_bench2__tag_count, _ctx);
  kk_std_core_hnd__htag_drop(kk_test_float_bench2__tag_bra, _ctx);
  kk_std_core__done(_ctx);
  kk_std_core_hnd__done(_ctx);
  kk_std_core_types__done(_ctx);
}
