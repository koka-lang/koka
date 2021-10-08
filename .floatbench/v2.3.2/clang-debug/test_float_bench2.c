// Koka generated module: "test/float/bench2", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench2.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench2__handle_bra(int32_t cfc, kk_test_float_bench2__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1126 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_bra); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1126, cfc, kk_test_float_bench2__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench2__handle_count(int32_t cfc, kk_test_float_bench2__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1129 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_count); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1129, cfc, kk_test_float_bench2__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench2_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_792;
  kk_ssize_t _x1137 = ((kk_ssize_t)0); /*ssize_t*/
  ev_792 = kk_evv_at(_x1137,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1138;
  {
    struct kk_std_core_hnd_Ev* _con1139 = kk_std_core_hnd__as_Ev(ev_792);
    kk_std_core_hnd__marker m0 = _con1139->marker;
    kk_box_t _box_x882 = _con1139->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x882, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1122 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x885 = _match_1122.clause;
      _x1138 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x885, (_fun_unbox_x885, m0, ev_792, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1138); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1146__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_one___fun1146(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1146(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1146__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_one___fun1146__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_one___fun1146, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_one___fun1146(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1146__t* _self = kk_function_as(struct kk_test_float_bench2_one___fun1146__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1147;
  bool b_16563;
  kk_integer_t _x1148 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1148,kk_context()); /*bool*/
  if (b_16563) {
    _x1147 = a; /*int*/
  }
  else {
    _x1147 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1147);
}

kk_integer_t kk_test_float_bench2_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_735;
  kk_box_t _x1145 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_one___fun1146(a, _ctx), _ctx); /*1001*/
  a0_735 = kk_integer_unbox(_x1145); /*int*/
  kk_std_core_hnd__ev ev_797;
  kk_ssize_t _x1149 = ((kk_ssize_t)0); /*ssize_t*/
  ev_797 = kk_evv_at(_x1149,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1150;
  {
    struct kk_std_core_hnd_Ev* _con1151 = kk_std_core_hnd__as_Ev(ev_797);
    kk_std_core_hnd__marker m0 = _con1151->marker;
    kk_box_t _box_x898 = _con1151->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x898, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1120 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
    {
      kk_function_t _fun_unbox_x902 = _match_1120.clause;
      _x1150 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x902, (_fun_unbox_x902, m0, ev_797, kk_integer_box(a0_735), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1150);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1158__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_two___fun1158(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1158(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1158__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_two___fun1158__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_two___fun1158, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_two___fun1158(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1158__t* _self = kk_function_as(struct kk_test_float_bench2_two___fun1158__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1159;
  bool b_16563;
  kk_integer_t _x1160 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1160,kk_context()); /*bool*/
  if (b_16563) {
    _x1159 = a; /*int*/
  }
  else {
    _x1159 = kk_integer_add(a,(kk_integer_from_small(2)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1159);
}

kk_integer_t kk_test_float_bench2_two__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_736;
  kk_box_t _x1157 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_two___fun1158(a, _ctx), _ctx); /*1001*/
  a0_736 = kk_integer_unbox(_x1157); /*int*/
  kk_std_core_hnd__ev ev_803;
  kk_ssize_t _x1161 = ((kk_ssize_t)0); /*ssize_t*/
  ev_803 = kk_evv_at(_x1161,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1162;
  {
    struct kk_std_core_hnd_Ev* _con1163 = kk_std_core_hnd__as_Ev(ev_803);
    kk_std_core_hnd__marker m0 = _con1163->marker;
    kk_box_t _box_x916 = _con1163->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x916, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1118 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
    {
      kk_function_t _fun_unbox_x920 = _match_1118.clause;
      _x1162 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x920, (_fun_unbox_x920, m0, ev_803, kk_integer_box(a0_736), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1162);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift777_f_fun1167__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift777_f_fun1167(kk_function_t _fself, kk_box_t _b_932, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift777_f_fun1167(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift777_f_fun1167__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift777_f_fun1167__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift777_f_fun1167, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift777_f_fun1167(kk_function_t _fself, kk_box_t _b_932, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift777_f_fun1167__t* _self = kk_function_as(struct kk_test_float_bench2__mlift777_f_fun1167__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_932, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift777_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_749, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_929_927 = kk_integer_add(_y_749,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_806 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_929_927)),kk_context()));
  kk_box_t _x1166;
  if (kk_yielding(kk_context())) {
    _x1166 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift777_f_fun1167(i, _ctx), _ctx); /*1002*/
  }
  else {
    _x1166 = (kk_ref_get(i,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1166);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift778_f_fun1171__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift778_f_fun1171(kk_function_t _fself, kk_box_t _b_941, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift778_f_fun1171(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift778_f_fun1171__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift778_f_fun1171__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift778_f_fun1171, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift778_f_fun1171(kk_function_t _fself, kk_box_t _b_941, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift778_f_fun1171__t* _self = kk_function_as(struct kk_test_float_bench2__mlift778_f_fun1171__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1172;
  kk_integer_t _x1173 = kk_integer_unbox(_b_941); /*int*/
  _x1172 = kk_test_float_bench2__mlift777_f(c, i, _x1173, _ctx); /*int*/
  return kk_integer_box(_x1172);
}

kk_integer_t kk_test_float_bench2__mlift778_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_810;
  kk_box_t _x1168;
  kk_ref_t _x1169 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1168 = (kk_ref_get(_x1169,kk_context())); /*1000*/
  x_810 = kk_integer_unbox(_x1168); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_810, _ctx);
    kk_box_t _x1170 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift778_f_fun1171(c, i, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1170);
  }
  {
    return kk_test_float_bench2__mlift777_f(c, i, x_810, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift780_f_fun1176__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift780_f_fun1176(kk_function_t _fself, kk_box_t _b_952, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift780_f_fun1176(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift780_f_fun1176__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift780_f_fun1176__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift780_f_fun1176, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift780_f_fun1176(kk_function_t _fself, kk_box_t _b_952, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift780_f_fun1176__t* _self = kk_function_as(struct kk_test_float_bench2__mlift780_f_fun1176__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_952, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift780_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_753, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_949_947 = kk_integer_add(_y_753,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_812 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_949_947)),kk_context()));
  kk_box_t _x1175;
  if (kk_yielding(kk_context())) {
    _x1175 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift780_f_fun1176(i, _ctx), _ctx); /*1002*/
  }
  else {
    _x1175 = (kk_ref_get(i,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1175);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift781_f_fun1180__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift781_f_fun1180(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift781_f_fun1180(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift781_f_fun1180__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift781_f_fun1180__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift781_f_fun1180, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift781_f_fun1180(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift781_f_fun1180__t* _self = kk_function_as(struct kk_test_float_bench2__mlift781_f_fun1180__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1181;
  kk_integer_t _x1182 = kk_integer_unbox(_b_961); /*int*/
  _x1181 = kk_test_float_bench2__mlift780_f(c, i, _x1182, _ctx); /*int*/
  return kk_integer_box(_x1181);
}

kk_integer_t kk_test_float_bench2__mlift781_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_1 : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_816;
  kk_box_t _x1177;
  kk_ref_t _x1178 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1177 = (kk_ref_get(_x1178,kk_context())); /*1000*/
  x_816 = kk_integer_unbox(_x1177); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_816, _ctx);
    kk_box_t _x1179 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift781_f_fun1180(c, i, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1179);
  }
  {
    return kk_test_float_bench2__mlift780_f(c, i, x_816, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift784_f_fun1185__t {
  struct kk_function_s _base;
  kk_integer_t a2;
};
static kk_box_t kk_test_float_bench2__mlift784_f_fun1185(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift784_f_fun1185(kk_integer_t a2, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift784_f_fun1185__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift784_f_fun1185__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift784_f_fun1185, kk_context());
  _self->a2 = a2;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift784_f_fun1185(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift784_f_fun1185__t* _self = kk_function_as(struct kk_test_float_bench2__mlift784_f_fun1185__t*, _fself);
  kk_integer_t a2 = _self->a2; /* int */
  kk_drop_match(_self, {kk_integer_dup(a2);}, {}, _ctx)
  kk_integer_t _x1186 = kk_test_float_bench2_one__(a2, _ctx); /*int*/
  return kk_integer_box(_x1186);
}


// lift anonymous function
struct kk_test_float_bench2__mlift784_f_fun1188__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift784_f_fun1188(kk_function_t _fself, kk_box_t _b_969, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift784_f_fun1188(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift784_f_fun1188, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift784_f_fun1188(kk_function_t _fself, kk_box_t _b_969, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_969, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_test_float_bench2__mlift784_f(kk_integer_t a2, kk_context_t* _ctx) { /* forall<h> (a2 : int) -> <bra,count,div,local<h>> () */ 
  kk_ssize_t _b_966_964 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_818;
  kk_box_t _x1184 = kk_std_core_hnd__open_at0(_b_966_964, kk_test_float_bench2__new_mlift784_f_fun1185(a2, _ctx), _ctx); /*1001*/
  x_818 = kk_integer_unbox(_x1184); /*int*/
  kk_integer_drop(x_818, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1187 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift784_f_fun1188(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1187); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift785_f_fun1190__t {
  struct kk_function_s _base;
  kk_integer_t a1;
};
static kk_box_t kk_test_float_bench2__mlift785_f_fun1190(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift785_f_fun1190(kk_integer_t a1, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift785_f_fun1190__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift785_f_fun1190__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift785_f_fun1190, kk_context());
  _self->a1 = a1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift785_f_fun1190(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift785_f_fun1190__t* _self = kk_function_as(struct kk_test_float_bench2__mlift785_f_fun1190__t*, _fself);
  kk_integer_t a1 = _self->a1; /* int */
  kk_drop_match(_self, {kk_integer_dup(a1);}, {}, _ctx)
  kk_integer_t _x1191 = kk_test_float_bench2_two__(a1, _ctx); /*int*/
  return kk_integer_box(_x1191);
}


// lift anonymous function
struct kk_test_float_bench2__mlift785_f_fun1193__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift785_f_fun1193(kk_function_t _fself, kk_box_t _b_977, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift785_f_fun1193(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift785_f_fun1193, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift785_f_fun1193(kk_function_t _fself, kk_box_t _b_977, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1194 = kk_Unit;
  kk_integer_t _x1195 = kk_integer_unbox(_b_977); /*int*/
  kk_test_float_bench2__mlift784_f(_x1195, _ctx);
  return kk_unit_box(_x1194);
}

kk_unit_t kk_test_float_bench2__mlift785_f(kk_integer_t a1, kk_context_t* _ctx) { /* forall<h> (a1 : int) -> <bra,count,div,local<h>> () */ 
  kk_ssize_t _b_974_972 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_821;
  kk_box_t _x1189 = kk_std_core_hnd__open_at0(_b_974_972, kk_test_float_bench2__new_mlift785_f_fun1190(a1, _ctx), _ctx); /*1001*/
  x_821 = kk_integer_unbox(_x1189); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_821, _ctx);
    kk_box_t _x1192 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift785_f_fun1193(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1192); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift784_f(x_821, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift786_f_fun1197__t {
  struct kk_function_s _base;
  kk_integer_t a00;
};
static kk_box_t kk_test_float_bench2__mlift786_f_fun1197(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift786_f_fun1197(kk_integer_t a00, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift786_f_fun1197__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift786_f_fun1197__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift786_f_fun1197, kk_context());
  _self->a00 = a00;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift786_f_fun1197(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift786_f_fun1197__t* _self = kk_function_as(struct kk_test_float_bench2__mlift786_f_fun1197__t*, _fself);
  kk_integer_t a00 = _self->a00; /* int */
  kk_drop_match(_self, {kk_integer_dup(a00);}, {}, _ctx)
  kk_integer_t _x1198 = kk_test_float_bench2_one__(a00, _ctx); /*int*/
  return kk_integer_box(_x1198);
}


// lift anonymous function
struct kk_test_float_bench2__mlift786_f_fun1200__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift786_f_fun1200(kk_function_t _fself, kk_box_t _b_985, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift786_f_fun1200(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift786_f_fun1200, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift786_f_fun1200(kk_function_t _fself, kk_box_t _b_985, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1201 = kk_Unit;
  kk_integer_t _x1202 = kk_integer_unbox(_b_985); /*int*/
  kk_test_float_bench2__mlift785_f(_x1202, _ctx);
  return kk_unit_box(_x1201);
}

kk_unit_t kk_test_float_bench2__mlift786_f(kk_integer_t a00, kk_context_t* _ctx) { /* forall<h> (a00 : int) -> <local<h>,count,bra,div> () */ 
  kk_ssize_t _b_982_980 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_823;
  kk_box_t _x1196 = kk_std_core_hnd__open_at0(_b_982_980, kk_test_float_bench2__new_mlift786_f_fun1197(a00, _ctx), _ctx); /*1001*/
  x_823 = kk_integer_unbox(_x1196); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_823, _ctx);
    kk_box_t _x1199 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift786_f_fun1200(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1199); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift785_f(x_823, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift787_f_fun1205__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift787_f_fun1205(kk_function_t _fself, kk_box_t _b_991, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift787_f_fun1205(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift787_f_fun1205, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift787_f_fun1205(kk_function_t _fself, kk_box_t _b_991, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1206 = kk_Unit;
  kk_integer_t _x1207 = kk_integer_unbox(_b_991); /*int*/
  kk_test_float_bench2__mlift786_f(_x1207, _ctx);
  return kk_unit_box(_x1206);
}

kk_unit_t kk_test_float_bench2__mlift787_f(kk_ref_t i, kk_unit_t wild__3, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_3 : ()) -> <bra,count,div,local<h>> () */ 
  kk_integer_t x_825;
  kk_box_t _x1203 = (kk_ref_get(i,kk_context())); /*1000*/
  x_825 = kk_integer_unbox(_x1203); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_825, _ctx);
    kk_box_t _x1204 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift787_f_fun1205(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1204); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift786_f(x_825, _ctx); return kk_Unit;
  }
}
 
// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// }


// lift anonymous function
struct kk_test_float_bench2_f_fun1212__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench2_f_fun1212(kk_function_t _fself, kk_std_core_hnd__marker _b_1005, kk_std_core_hnd__ev _b_1006, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1212(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1212__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1212__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1212, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1212(kk_function_t _fself, kk_std_core_hnd__marker _b_1005, kk_std_core_hnd__ev _b_1006, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1212__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1212__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_1006, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1213 = kk_Unit;
  kk_integer_t _b_1057_1003;
  kk_integer_t _x1214;
  kk_box_t _x1215 = (kk_ref_get(loc,kk_context())); /*1000*/
  _x1214 = kk_integer_unbox(_x1215); /*int*/
  kk_integer_t _x1216;
  kk_box_t _x1217 = (kk_ref_get(loc0,kk_context())); /*1000*/
  _x1216 = kk_integer_unbox(_x1217); /*int*/
  _b_1057_1003 = kk_integer_add(_x1214,_x1216,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_1057_1003)),kk_context()));
  return kk_unit_box(_x1213);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1218__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1218(kk_function_t _fself, kk_box_t _b_1050, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1218(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1218, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1218(kk_function_t _fself, kk_box_t _b_1050, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_1050;
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1220__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1220(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1220__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1220__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1220, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1224__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1224(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1224(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1224__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1224__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1224, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1228__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1228(kk_function_t _fself, kk_box_t _b_1014, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1228(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1228__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1228__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1228, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1233__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1233(kk_function_t _fself, kk_box_t _b_1012, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1233(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1233__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1233__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1233, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1233(kk_function_t _fself, kk_box_t _b_1012, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1233__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1233__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1234;
  kk_unit_t _x1235 = kk_Unit;
  kk_unit_unbox(_b_1012);
  _x1234 = kk_test_float_bench2__mlift778_f(loc0, loc, _x1235, _ctx); /*int*/
  return kk_integer_box(_x1234);
}
static kk_box_t kk_test_float_bench2_f_fun1228(kk_function_t _fself, kk_box_t _b_1014, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1228__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1228__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1229;
  kk_integer_t _b_1010_1008;
  kk_integer_t _x1230 = kk_integer_unbox(_b_1014); /*int*/
  _b_1010_1008 = kk_integer_add(_x1230,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_834 = kk_Unit;
  kk_ref_t _x1231 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1231,(kk_integer_box(_b_1010_1008)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1232 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1233(loc, loc0, _ctx), _ctx); /*1002*/
    _x1229 = kk_integer_unbox(_x1232); /*int*/
  }
  else {
    _x1229 = kk_test_float_bench2__mlift778_f(loc0, loc, x_834, _ctx); /*int*/
  }
  return kk_integer_box(_x1229);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1237__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1237(kk_function_t _fself, kk_box_t _b_1022, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1237(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1237__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1237__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1237, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1242__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1242(kk_function_t _fself, kk_box_t _b_1020, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1242(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1242__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1242__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1242, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1242(kk_function_t _fself, kk_box_t _b_1020, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1242__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1242__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1243;
  kk_unit_t _x1244 = kk_Unit;
  kk_unit_unbox(_b_1020);
  _x1243 = kk_test_float_bench2__mlift781_f(loc0, loc, _x1244, _ctx); /*int*/
  return kk_integer_box(_x1243);
}
static kk_box_t kk_test_float_bench2_f_fun1237(kk_function_t _fself, kk_box_t _b_1022, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1237__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1237__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1238;
  kk_integer_t _b_1018_1016;
  kk_integer_t _x1239 = kk_integer_unbox(_b_1022); /*int*/
  _b_1018_1016 = kk_integer_add(_x1239,(kk_integer_from_small(2)),kk_context()); /*int*/
  kk_unit_t x0_836 = kk_Unit;
  kk_ref_t _x1240 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1240,(kk_integer_box(_b_1018_1016)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1241 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1242(loc, loc0, _ctx), _ctx); /*1002*/
    _x1238 = kk_integer_unbox(_x1241); /*int*/
  }
  else {
    _x1238 = kk_test_float_bench2__mlift781_f(loc0, loc, x0_836, _ctx); /*int*/
  }
  return kk_integer_box(_x1238);
}
static kk_box_t kk_test_float_bench2_f_fun1224(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1224__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1224__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_test_float_bench2__hnd_count _x1225;
  kk_std_core_hnd__clause1 _x1226;
  kk_function_t _x1227;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1227 = kk_test_float_bench2_new_f_fun1228(loc, loc0, _ctx); /*(1003) -> 1001 1004*/
  _x1226 = kk_std_core_hnd_clause_tail1(_x1227, _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  kk_std_core_hnd__clause1 _x1236 = kk_std_core_hnd_clause_tail1(kk_test_float_bench2_new_f_fun1237(loc, loc0, _ctx), _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  _x1225 = kk_test_float_bench2__new_Hnd_count(kk_reuse_null, _x1226, _x1236, _ctx); /*test/float/bench2/.hnd-count<20,21>*/
  return kk_test_float_bench2__hnd_count_box(_x1225, _ctx);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1246__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1246(kk_function_t _fself, kk_box_t _b_1045, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1246(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1246__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1246__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1246, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1246(kk_function_t _fself, kk_box_t _b_1045, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1246__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1246__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_1045, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1247__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1247(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1247(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1247__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1247__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1247, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1250__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static bool kk_test_float_bench2_f_fun1250(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1250(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1250__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1250__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1250, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1253__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1253(kk_function_t _fself, kk_box_t _b_1028, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1253(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1253, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1253(kk_function_t _fself, kk_box_t _b_1028, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1254;
  kk_integer_t _x1255 = kk_integer_unbox(_b_1028); /*int*/
  kk_integer_t _x1256 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1254 = kk_integer_lt(_x1255,_x1256,kk_context()); /*bool*/
  return kk_bool_box(_x1254);
}
static bool kk_test_float_bench2_f_fun1250(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1250__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1250__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x1_838;
  kk_box_t _x1251 = (kk_ref_get(loc,kk_context())); /*1000*/
  x1_838 = kk_integer_unbox(_x1251); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x1_838, _ctx);
    kk_box_t _x1252 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1253(_ctx), _ctx); /*1002*/
    return kk_bool_unbox(_x1252);
  }
  {
    kk_integer_t _x1257 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x1_838,_x1257,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1258__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1258(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1258(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1258__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1258__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1258, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1260__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1260(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1260(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1260, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1260(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1261 = kk_Unit;
  kk_std_core_hnd__ev ev_843;
  kk_ssize_t _x1262 = ((kk_ssize_t)0); /*ssize_t*/
  ev_843 = kk_evv_at(_x1262,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1263;
  {
    struct kk_std_core_hnd_Ev* _con1264 = kk_std_core_hnd__as_Ev(ev_843);
    kk_std_core_hnd__marker m0 = _con1264->marker;
    kk_box_t _box_x1029 = _con1264->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x1029, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1106 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x1032 = _match_1106.clause;
      _x1263 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x1032, (_fun_unbox_x1032, m0, ev_843, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1263);
  return kk_unit_box(_x1261);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1267__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1267(kk_function_t _fself, kk_box_t _b_1040, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1267(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1267__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1267__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1267, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1267(kk_function_t _fself, kk_box_t _b_1040, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1267__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1267__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1268 = kk_Unit;
  kk_unit_t _x1269 = kk_Unit;
  kk_unit_unbox(_b_1040);
  kk_test_float_bench2__mlift787_f(loc, _x1269, _ctx);
  return kk_unit_box(_x1268);
}
static kk_unit_t kk_test_float_bench2_f_fun1258(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1258__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1258__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_1037_1035 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x2_841 = kk_Unit;
  kk_box_t _x1259 = kk_std_core_hnd__open_at0(_b_1037_1035, kk_test_float_bench2_new_f_fun1260(_ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1259);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1266 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1267(loc, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1266);
  }
  {
    return kk_test_float_bench2__mlift787_f(loc, x2_841, _ctx);
  }
}
static kk_box_t kk_test_float_bench2_f_fun1247(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1247__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1247__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1248 = kk_Unit;
  kk_function_t _x1249;
  kk_ref_dup(loc);
  _x1249 = kk_test_float_bench2_new_f_fun1250(loc, _ctx); /*() -> <div,local<722>,test/float/bench2/bra,test/float/bench2/count> bool*/
  kk_std_core_while(_x1249, kk_test_float_bench2_new_f_fun1258(loc, _ctx), _ctx);
  return kk_unit_box(_x1248);
}
static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1220__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1220__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1221;
  int32_t _b_1060_1041 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench2__hnd_count _b_1061_1042;
  kk_box_t _x1222;
  kk_function_t _x1223;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1223 = kk_test_float_bench2_new_f_fun1224(loc, loc0, _ctx); /*() -> 1002 1001*/
  _x1222 = kk_std_core_hnd__open_none0(_x1223, _ctx); /*1001*/
  _b_1061_1042 = kk_test_float_bench2__hnd_count_unbox(_x1222, _ctx); /*test/float/bench2/.hnd-count<<local<722>,test/float/bench2/bra,div>,int>*/
  kk_box_t _x1245 = kk_test_float_bench2__handle_count(_b_1060_1041, _b_1061_1042, kk_test_float_bench2_new_f_fun1246(loc0, _ctx), kk_test_float_bench2_new_f_fun1247(loc, _ctx), _ctx); /*171*/
  _x1221 = kk_integer_unbox(_x1245); /*int*/
  return kk_integer_box(_x1221);
}

kk_integer_t kk_test_float_bench2_f(kk_context_t* _ctx) { /* () -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  int32_t _b_1051_1046 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1208;
  kk_test_float_bench2__hnd_bra _x1209;
  kk_std_core_hnd__clause0 _x1210;
  kk_function_t _x1211;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1211 = kk_test_float_bench2_new_f_fun1212(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1011*/
  _x1210 = kk_std_core_hnd__new_Clause0(_x1211, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  _x1209 = kk_test_float_bench2__new_Hnd_bra(kk_reuse_null, _x1210, _ctx); /*test/float/bench2/.hnd-bra<6,7>*/
  kk_function_t _x1219;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1219 = kk_test_float_bench2_new_f_fun1220(loc, loc0, _ctx); /*() -> <test/float/bench2/bra|141> 140*/
  _x1208 = kk_test_float_bench2__handle_bra(_b_1051_1046, _x1209, kk_test_float_bench2_new_f_fun1218(_ctx), _x1219, _ctx); /*142*/
  res1 = kk_integer_unbox(_x1208); /*int*/
  kk_integer_t res0;
  kk_box_t _x1270 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*1002*/
  res0 = kk_integer_unbox(_x1270); /*int*/
  kk_integer_t res;
  kk_box_t _x1271 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*1002*/
  res = kk_integer_unbox(_x1271); /*int*/
  kk_box_t _x1272 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*1002*/
  return kk_integer_unbox(_x1272);
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
    kk_string_t _x1124;
    kk_define_string_literal(, _s1125, 10, "bra.bench2")
    _x1124 = kk_string_dup(_s1125); /*string*/
    kk_test_float_bench2__tag_bra = kk_std_core_hnd__new_Htag(_x1124, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  }
  {
    kk_string_t _x1127;
    kk_define_string_literal(, _s1128, 12, "count.bench2")
    _x1127 = kk_string_dup(_s1128); /*string*/
    kk_test_float_bench2__tag_count = kk_std_core_hnd__new_Htag(_x1127, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
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
