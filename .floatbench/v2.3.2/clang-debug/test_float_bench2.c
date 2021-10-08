// Koka generated module: "test/float/bench2", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench2.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench2__handle_bra(int32_t cfc, kk_test_float_bench2__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1127 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_bra); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1127, cfc, kk_test_float_bench2__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench2__handle_count(int32_t cfc, kk_test_float_bench2__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1130 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_count); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1130, cfc, kk_test_float_bench2__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench2_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_807;
  kk_ssize_t _x1138 = ((kk_ssize_t)0); /*ssize_t*/
  ev_807 = kk_evv_at(_x1138,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1139;
  {
    struct kk_std_core_hnd_Ev* _con1140 = kk_std_core_hnd__as_Ev(ev_807);
    kk_std_core_hnd__marker m0 = _con1140->marker;
    kk_box_t _box_x894 = _con1140->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x894, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1123 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x897 = _match_1123.clause;
      _x1139 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x897, (_fun_unbox_x897, m0, ev_807, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1139); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1147__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_one___fun1147(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1147(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1147__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_one___fun1147__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_one___fun1147, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_one___fun1147(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1147__t* _self = kk_function_as(struct kk_test_float_bench2_one___fun1147__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1148;
  bool b_16563;
  kk_integer_t _x1149 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1149,kk_context()); /*bool*/
  if (b_16563) {
    _x1148 = a; /*int*/
  }
  else {
    _x1148 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1148);
}

kk_integer_t kk_test_float_bench2_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_735;
  kk_box_t _x1146 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_one___fun1147(a, _ctx), _ctx); /*1001*/
  a0_735 = kk_integer_unbox(_x1146); /*int*/
  kk_std_core_hnd__ev ev_812;
  kk_ssize_t _x1150 = ((kk_ssize_t)0); /*ssize_t*/
  ev_812 = kk_evv_at(_x1150,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1151;
  {
    struct kk_std_core_hnd_Ev* _con1152 = kk_std_core_hnd__as_Ev(ev_812);
    kk_std_core_hnd__marker m0 = _con1152->marker;
    kk_box_t _box_x910 = _con1152->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x910, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1121 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
    {
      kk_function_t _fun_unbox_x914 = _match_1121.clause;
      _x1151 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x914, (_fun_unbox_x914, m0, ev_812, kk_integer_box(a0_735), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1151);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1159__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_two___fun1159(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1159(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1159__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_two___fun1159__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_two___fun1159, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_two___fun1159(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1159__t* _self = kk_function_as(struct kk_test_float_bench2_two___fun1159__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1160;
  bool b_16563;
  kk_integer_t _x1161 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1161,kk_context()); /*bool*/
  if (b_16563) {
    _x1160 = a; /*int*/
  }
  else {
    _x1160 = kk_integer_add(a,(kk_integer_from_small(2)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1160);
}

kk_integer_t kk_test_float_bench2_two__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_736;
  kk_box_t _x1158 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_two___fun1159(a, _ctx), _ctx); /*1001*/
  a0_736 = kk_integer_unbox(_x1158); /*int*/
  kk_std_core_hnd__ev ev_818;
  kk_ssize_t _x1162 = ((kk_ssize_t)0); /*ssize_t*/
  ev_818 = kk_evv_at(_x1162,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1163;
  {
    struct kk_std_core_hnd_Ev* _con1164 = kk_std_core_hnd__as_Ev(ev_818);
    kk_std_core_hnd__marker m0 = _con1164->marker;
    kk_box_t _box_x928 = _con1164->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x928, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1119 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
    {
      kk_function_t _fun_unbox_x932 = _match_1119.clause;
      _x1163 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x932, (_fun_unbox_x932, m0, ev_818, kk_integer_box(a0_736), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1163);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift792_f_fun1168__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift792_f_fun1168(kk_function_t _fself, kk_box_t _b_944, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift792_f_fun1168(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift792_f_fun1168__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift792_f_fun1168__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift792_f_fun1168, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift792_f_fun1168(kk_function_t _fself, kk_box_t _b_944, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift792_f_fun1168__t* _self = kk_function_as(struct kk_test_float_bench2__mlift792_f_fun1168__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_944, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift792_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_768, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_941_939 = kk_integer_add(_y_768,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_821 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_941_939)),kk_context()));
  kk_box_t _x1167;
  if (kk_yielding(kk_context())) {
    _x1167 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift792_f_fun1168(i, _ctx), _ctx); /*1002*/
  }
  else {
    _x1167 = (kk_ref_get(i,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1167);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift793_f_fun1172__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift793_f_fun1172(kk_function_t _fself, kk_box_t _b_953, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift793_f_fun1172(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift793_f_fun1172__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift793_f_fun1172__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift793_f_fun1172, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift793_f_fun1172(kk_function_t _fself, kk_box_t _b_953, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift793_f_fun1172__t* _self = kk_function_as(struct kk_test_float_bench2__mlift793_f_fun1172__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1173;
  kk_integer_t _x1174 = kk_integer_unbox(_b_953); /*int*/
  _x1173 = kk_test_float_bench2__mlift792_f(c, i, _x1174, _ctx); /*int*/
  return kk_integer_box(_x1173);
}

kk_integer_t kk_test_float_bench2__mlift793_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_825;
  kk_box_t _x1169;
  kk_ref_t _x1170 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1169 = (kk_ref_get(_x1170,kk_context())); /*1000*/
  x_825 = kk_integer_unbox(_x1169); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_825, _ctx);
    kk_box_t _x1171 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift793_f_fun1172(c, i, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1171);
  }
  {
    return kk_test_float_bench2__mlift792_f(c, i, x_825, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift795_f_fun1177__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift795_f_fun1177(kk_function_t _fself, kk_box_t _b_964, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift795_f_fun1177(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift795_f_fun1177__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift795_f_fun1177__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift795_f_fun1177, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift795_f_fun1177(kk_function_t _fself, kk_box_t _b_964, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift795_f_fun1177__t* _self = kk_function_as(struct kk_test_float_bench2__mlift795_f_fun1177__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_964, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift795_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_772, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_961_959 = kk_integer_add(_y_772,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_827 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_961_959)),kk_context()));
  kk_box_t _x1176;
  if (kk_yielding(kk_context())) {
    _x1176 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift795_f_fun1177(i, _ctx), _ctx); /*1002*/
  }
  else {
    _x1176 = (kk_ref_get(i,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1176);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift796_f_fun1181__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift796_f_fun1181(kk_function_t _fself, kk_box_t _b_973, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift796_f_fun1181(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift796_f_fun1181__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift796_f_fun1181__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift796_f_fun1181, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift796_f_fun1181(kk_function_t _fself, kk_box_t _b_973, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift796_f_fun1181__t* _self = kk_function_as(struct kk_test_float_bench2__mlift796_f_fun1181__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1182;
  kk_integer_t _x1183 = kk_integer_unbox(_b_973); /*int*/
  _x1182 = kk_test_float_bench2__mlift795_f(c, i, _x1183, _ctx); /*int*/
  return kk_integer_box(_x1182);
}

kk_integer_t kk_test_float_bench2__mlift796_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_1 : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_831;
  kk_box_t _x1178;
  kk_ref_t _x1179 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1178 = (kk_ref_get(_x1179,kk_context())); /*1000*/
  x_831 = kk_integer_unbox(_x1178); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_831, _ctx);
    kk_box_t _x1180 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift796_f_fun1181(c, i, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1180);
  }
  {
    return kk_test_float_bench2__mlift795_f(c, i, x_831, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift799_f_fun1186__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift799_f_fun1186(kk_function_t _fself, kk_box_t _b_977, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift799_f_fun1186(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift799_f_fun1186, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift799_f_fun1186(kk_function_t _fself, kk_box_t _b_977, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1187 = kk_Unit;
  kk_integer_t _x1188 = kk_integer_unbox(_b_977); /*int*/
  kk_test_float_bench2__mlift798_f(_x1188, _ctx);
  return kk_unit_box(_x1187);
}

kk_unit_t kk_test_float_bench2__mlift799_f(kk_integer_t a2, kk_context_t* _ctx) { /* (a2 : int) -> count () */ 
  kk_integer_t x_833 = kk_test_float_bench2_one__(a2, _ctx); /*int*/;
  kk_integer_drop(x_833, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1185 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift799_f_fun1186(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1185); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift800_f_fun1190__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift800_f_fun1190(kk_function_t _fself, kk_box_t _b_980, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift800_f_fun1190(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift800_f_fun1190, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift800_f_fun1190(kk_function_t _fself, kk_box_t _b_980, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1191 = kk_Unit;
  kk_integer_t _x1192 = kk_integer_unbox(_b_980); /*int*/
  kk_test_float_bench2__mlift799_f(_x1192, _ctx);
  return kk_unit_box(_x1191);
}

kk_unit_t kk_test_float_bench2__mlift800_f(kk_integer_t a1, kk_context_t* _ctx) { /* (a1 : int) -> count () */ 
  kk_integer_t x_835 = kk_test_float_bench2_two__(a1, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_835, _ctx);
    kk_box_t _x1189 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift800_f_fun1190(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1189); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift799_f(x_835, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift801_f_fun1194__t {
  struct kk_function_s _base;
  kk_integer_t a00;
};
static kk_box_t kk_test_float_bench2__mlift801_f_fun1194(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift801_f_fun1194(kk_integer_t a00, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift801_f_fun1194__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift801_f_fun1194__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift801_f_fun1194, kk_context());
  _self->a00 = a00;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2__mlift801_f_fun1197__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift801_f_fun1197(kk_function_t _fself, kk_box_t _b_983, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift801_f_fun1197(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift801_f_fun1197, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift801_f_fun1197(kk_function_t _fself, kk_box_t _b_983, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1198 = kk_Unit;
  kk_integer_t _x1199 = kk_integer_unbox(_b_983); /*int*/
  kk_test_float_bench2__mlift800_f(_x1199, _ctx);
  return kk_unit_box(_x1198);
}
static kk_box_t kk_test_float_bench2__mlift801_f_fun1194(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift801_f_fun1194__t* _self = kk_function_as(struct kk_test_float_bench2__mlift801_f_fun1194__t*, _fself);
  kk_integer_t a00 = _self->a00; /* int */
  kk_drop_match(_self, {kk_integer_dup(a00);}, {}, _ctx)
  kk_unit_t _x1195 = kk_Unit;
  kk_integer_t x_837 = kk_test_float_bench2_one__(a00, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_837, _ctx);
    kk_box_t _x1196 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift801_f_fun1197(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1196);
  }
  else {
    kk_test_float_bench2__mlift800_f(x_837, _ctx);
  }
  return kk_unit_box(_x1195);
}

kk_unit_t kk_test_float_bench2__mlift801_f(kk_integer_t a00, kk_context_t* _ctx) { /* forall<h> (a00 : int) -> <local<h>,count,bra,div> () */ 
  kk_ssize_t _b_986_984 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_box_t _x1193 = kk_std_core_hnd__open_at0(_b_986_984, kk_test_float_bench2__new_mlift801_f_fun1194(a00, _ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1193); return kk_Unit;
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift802_f_fun1202__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift802_f_fun1202(kk_function_t _fself, kk_box_t _b_992, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift802_f_fun1202(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift802_f_fun1202, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift802_f_fun1202(kk_function_t _fself, kk_box_t _b_992, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1203 = kk_Unit;
  kk_integer_t _x1204 = kk_integer_unbox(_b_992); /*int*/
  kk_test_float_bench2__mlift801_f(_x1204, _ctx);
  return kk_unit_box(_x1203);
}

kk_unit_t kk_test_float_bench2__mlift802_f(kk_ref_t i, kk_unit_t wild__3, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_3 : ()) -> <bra,count,div,local<h>> () */ 
  kk_integer_t x_839;
  kk_box_t _x1200 = (kk_ref_get(i,kk_context())); /*1000*/
  x_839 = kk_integer_unbox(_x1200); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_839, _ctx);
    kk_box_t _x1201 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift802_f_fun1202(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1201); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift801_f(x_839, _ctx); return kk_Unit;
  }
}
 
// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// }


// lift anonymous function
struct kk_test_float_bench2_f_fun1209__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench2_f_fun1209(kk_function_t _fself, kk_std_core_hnd__marker _b_1006, kk_std_core_hnd__ev _b_1007, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1209(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1209__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1209__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1209, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1209(kk_function_t _fself, kk_std_core_hnd__marker _b_1006, kk_std_core_hnd__ev _b_1007, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1209__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1209__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_1007, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1210 = kk_Unit;
  kk_integer_t _b_1058_1004;
  kk_integer_t _x1211;
  kk_box_t _x1212 = (kk_ref_get(loc,kk_context())); /*1000*/
  _x1211 = kk_integer_unbox(_x1212); /*int*/
  kk_integer_t _x1213;
  kk_box_t _x1214 = (kk_ref_get(loc0,kk_context())); /*1000*/
  _x1213 = kk_integer_unbox(_x1214); /*int*/
  _b_1058_1004 = kk_integer_add(_x1211,_x1213,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_1058_1004)),kk_context()));
  return kk_unit_box(_x1210);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1215__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_box_t _b_1051, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1215(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1215, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_box_t _b_1051, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_1051;
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1217__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1217(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1217(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1217__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1217__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1217, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1221__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1221(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1221(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1221__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1221__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1221, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1225__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1225(kk_function_t _fself, kk_box_t _b_1015, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1225(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1225__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1225__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1225, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1230__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1230(kk_function_t _fself, kk_box_t _b_1013, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1230(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1230__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1230__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1230, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1230(kk_function_t _fself, kk_box_t _b_1013, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1230__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1230__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1231;
  kk_unit_t _x1232 = kk_Unit;
  kk_unit_unbox(_b_1013);
  _x1231 = kk_test_float_bench2__mlift793_f(loc0, loc, _x1232, _ctx); /*int*/
  return kk_integer_box(_x1231);
}
static kk_box_t kk_test_float_bench2_f_fun1225(kk_function_t _fself, kk_box_t _b_1015, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1225__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1225__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1226;
  kk_integer_t _b_1011_1009;
  kk_integer_t _x1227 = kk_integer_unbox(_b_1015); /*int*/
  _b_1011_1009 = kk_integer_add(_x1227,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_848 = kk_Unit;
  kk_ref_t _x1228 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1228,(kk_integer_box(_b_1011_1009)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1229 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1230(loc, loc0, _ctx), _ctx); /*1002*/
    _x1226 = kk_integer_unbox(_x1229); /*int*/
  }
  else {
    _x1226 = kk_test_float_bench2__mlift793_f(loc0, loc, x_848, _ctx); /*int*/
  }
  return kk_integer_box(_x1226);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1234__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1234(kk_function_t _fself, kk_box_t _b_1023, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1234(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1234__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1234__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1234, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1239__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1239(kk_function_t _fself, kk_box_t _b_1021, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1239(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1239__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1239__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1239, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1239(kk_function_t _fself, kk_box_t _b_1021, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1239__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1239__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1240;
  kk_unit_t _x1241 = kk_Unit;
  kk_unit_unbox(_b_1021);
  _x1240 = kk_test_float_bench2__mlift796_f(loc0, loc, _x1241, _ctx); /*int*/
  return kk_integer_box(_x1240);
}
static kk_box_t kk_test_float_bench2_f_fun1234(kk_function_t _fself, kk_box_t _b_1023, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1234__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1234__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1235;
  kk_integer_t _b_1019_1017;
  kk_integer_t _x1236 = kk_integer_unbox(_b_1023); /*int*/
  _b_1019_1017 = kk_integer_add(_x1236,(kk_integer_from_small(2)),kk_context()); /*int*/
  kk_unit_t x0_850 = kk_Unit;
  kk_ref_t _x1237 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1237,(kk_integer_box(_b_1019_1017)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1238 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1239(loc, loc0, _ctx), _ctx); /*1002*/
    _x1235 = kk_integer_unbox(_x1238); /*int*/
  }
  else {
    _x1235 = kk_test_float_bench2__mlift796_f(loc0, loc, x0_850, _ctx); /*int*/
  }
  return kk_integer_box(_x1235);
}
static kk_box_t kk_test_float_bench2_f_fun1221(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1221__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1221__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_test_float_bench2__hnd_count _x1222;
  kk_std_core_hnd__clause1 _x1223;
  kk_function_t _x1224;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1224 = kk_test_float_bench2_new_f_fun1225(loc, loc0, _ctx); /*(1003) -> 1001 1004*/
  _x1223 = kk_std_core_hnd_clause_tail1(_x1224, _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  kk_std_core_hnd__clause1 _x1233 = kk_std_core_hnd_clause_tail1(kk_test_float_bench2_new_f_fun1234(loc, loc0, _ctx), _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  _x1222 = kk_test_float_bench2__new_Hnd_count(kk_reuse_null, _x1223, _x1233, _ctx); /*test/float/bench2/.hnd-count<20,21>*/
  return kk_test_float_bench2__hnd_count_box(_x1222, _ctx);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1243__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1243(kk_function_t _fself, kk_box_t _b_1046, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1243(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1243__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1243__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1243, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1243(kk_function_t _fself, kk_box_t _b_1046, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1243__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1243__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_1046, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1244__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1244(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1244(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1244__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1244__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1244, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1247__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static bool kk_test_float_bench2_f_fun1247(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1247(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1247__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1247__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1247, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1250__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1250(kk_function_t _fself, kk_box_t _b_1029, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1250(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1250, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1250(kk_function_t _fself, kk_box_t _b_1029, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1251;
  kk_integer_t _x1252 = kk_integer_unbox(_b_1029); /*int*/
  kk_integer_t _x1253 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1251 = kk_integer_lt(_x1252,_x1253,kk_context()); /*bool*/
  return kk_bool_box(_x1251);
}
static bool kk_test_float_bench2_f_fun1247(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1247__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1247__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x1_852;
  kk_box_t _x1248 = (kk_ref_get(loc,kk_context())); /*1000*/
  x1_852 = kk_integer_unbox(_x1248); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x1_852, _ctx);
    kk_box_t _x1249 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1250(_ctx), _ctx); /*1002*/
    return kk_bool_unbox(_x1249);
  }
  {
    kk_integer_t _x1254 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x1_852,_x1254,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1255__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1255(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1255(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1255__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1255__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1255, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1257__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1257(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1257(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1257, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1257(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1258 = kk_Unit;
  kk_std_core_hnd__ev ev_857;
  kk_ssize_t _x1259 = ((kk_ssize_t)0); /*ssize_t*/
  ev_857 = kk_evv_at(_x1259,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1260;
  {
    struct kk_std_core_hnd_Ev* _con1261 = kk_std_core_hnd__as_Ev(ev_857);
    kk_std_core_hnd__marker m0 = _con1261->marker;
    kk_box_t _box_x1030 = _con1261->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x1030, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1107 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x1033 = _match_1107.clause;
      _x1260 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x1033, (_fun_unbox_x1033, m0, ev_857, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1260);
  return kk_unit_box(_x1258);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1264__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1264(kk_function_t _fself, kk_box_t _b_1041, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1264(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1264__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1264__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1264, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1264(kk_function_t _fself, kk_box_t _b_1041, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1264__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1264__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1265 = kk_Unit;
  kk_unit_t _x1266 = kk_Unit;
  kk_unit_unbox(_b_1041);
  kk_test_float_bench2__mlift802_f(loc, _x1266, _ctx);
  return kk_unit_box(_x1265);
}
static kk_unit_t kk_test_float_bench2_f_fun1255(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1255__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1255__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_1038_1036 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x2_855 = kk_Unit;
  kk_box_t _x1256 = kk_std_core_hnd__open_at0(_b_1038_1036, kk_test_float_bench2_new_f_fun1257(_ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1256);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1263 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1264(loc, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1263);
  }
  {
    return kk_test_float_bench2__mlift802_f(loc, x2_855, _ctx);
  }
}
static kk_box_t kk_test_float_bench2_f_fun1244(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1244__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1244__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1245 = kk_Unit;
  kk_function_t _x1246;
  kk_ref_dup(loc);
  _x1246 = kk_test_float_bench2_new_f_fun1247(loc, _ctx); /*() -> <div,local<722>,test/float/bench2/bra,test/float/bench2/count> bool*/
  kk_std_core_while(_x1246, kk_test_float_bench2_new_f_fun1255(loc, _ctx), _ctx);
  return kk_unit_box(_x1245);
}
static kk_box_t kk_test_float_bench2_f_fun1217(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1217__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1217__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1218;
  int32_t _b_1061_1042 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench2__hnd_count _b_1062_1043;
  kk_box_t _x1219;
  kk_function_t _x1220;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1220 = kk_test_float_bench2_new_f_fun1221(loc, loc0, _ctx); /*() -> 1002 1001*/
  _x1219 = kk_std_core_hnd__open_none0(_x1220, _ctx); /*1001*/
  _b_1062_1043 = kk_test_float_bench2__hnd_count_unbox(_x1219, _ctx); /*test/float/bench2/.hnd-count<<local<722>,test/float/bench2/bra,div>,int>*/
  kk_box_t _x1242 = kk_test_float_bench2__handle_count(_b_1061_1042, _b_1062_1043, kk_test_float_bench2_new_f_fun1243(loc0, _ctx), kk_test_float_bench2_new_f_fun1244(loc, _ctx), _ctx); /*171*/
  _x1218 = kk_integer_unbox(_x1242); /*int*/
  return kk_integer_box(_x1218);
}

kk_integer_t kk_test_float_bench2_f(kk_context_t* _ctx) { /* () -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  int32_t _b_1052_1047 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1205;
  kk_test_float_bench2__hnd_bra _x1206;
  kk_std_core_hnd__clause0 _x1207;
  kk_function_t _x1208;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1208 = kk_test_float_bench2_new_f_fun1209(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1011*/
  _x1207 = kk_std_core_hnd__new_Clause0(_x1208, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  _x1206 = kk_test_float_bench2__new_Hnd_bra(kk_reuse_null, _x1207, _ctx); /*test/float/bench2/.hnd-bra<6,7>*/
  kk_function_t _x1216;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1216 = kk_test_float_bench2_new_f_fun1217(loc, loc0, _ctx); /*() -> <test/float/bench2/bra|141> 140*/
  _x1205 = kk_test_float_bench2__handle_bra(_b_1052_1047, _x1206, kk_test_float_bench2_new_f_fun1215(_ctx), _x1216, _ctx); /*142*/
  res1 = kk_integer_unbox(_x1205); /*int*/
  kk_integer_t res0;
  kk_box_t _x1267 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*1002*/
  res0 = kk_integer_unbox(_x1267); /*int*/
  kk_integer_t res;
  kk_box_t _x1268 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*1002*/
  res = kk_integer_unbox(_x1268); /*int*/
  kk_box_t _x1269 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*1002*/
  return kk_integer_unbox(_x1269);
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
    kk_string_t _x1125;
    kk_define_string_literal(, _s1126, 10, "bra.bench2")
    _x1125 = kk_string_dup(_s1126); /*string*/
    kk_test_float_bench2__tag_bra = kk_std_core_hnd__new_Htag(_x1125, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  }
  {
    kk_string_t _x1128;
    kk_define_string_literal(, _s1129, 12, "count.bench2")
    _x1128 = kk_string_dup(_s1129); /*string*/
    kk_test_float_bench2__tag_count = kk_std_core_hnd__new_Htag(_x1128, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
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
