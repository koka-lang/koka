// Koka generated module: "test/float/bench2", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench2.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench2__handle_bra(int32_t cfc, kk_test_float_bench2__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1188 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_bra); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1188, cfc, kk_test_float_bench2__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench2__handle_count(int32_t cfc, kk_test_float_bench2__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1191 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_count); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1191, cfc, kk_test_float_bench2__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench2_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_795;
  kk_ssize_t _x1199 = ((kk_ssize_t)0); /*ssize_t*/
  ev_795 = kk_evv_at(_x1199,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1200;
  {
    struct kk_std_core_hnd_Ev* _con1201 = kk_std_core_hnd__as_Ev(ev_795);
    kk_std_core_hnd__marker m0 = _con1201->marker;
    kk_box_t _box_x899 = _con1201->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x899, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1184 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x902 = _match_1184.clause;
      _x1200 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x902, (_fun_unbox_x902, m0, ev_795, _ctx)); /*37*/
    }
  }
  kk_unit_unbox(_x1200); return kk_Unit;
}
 
// monadic lift

kk_integer_t kk_test_float_bench2__mlift777_one__(kk_integer_t _c_743, kk_context_t* _ctx) { /* (int) -> int */ 
  kk_std_core_hnd__ev ev_800;
  kk_ssize_t _x1207 = ((kk_ssize_t)0); /*ssize_t*/
  ev_800 = kk_evv_at(_x1207,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1208;
  {
    struct kk_std_core_hnd_Ev* _con1209 = kk_std_core_hnd__as_Ev(ev_800);
    kk_std_core_hnd__marker m0 = _con1209->marker;
    kk_box_t _box_x913 = _con1209->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x913, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1182 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
    {
      kk_function_t _fun_unbox_x917 = _match_1182.clause;
      _x1208 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x917, (_fun_unbox_x917, m0, ev_800, kk_integer_box(_c_743), _ctx)); /*52*/
    }
  }
  return kk_integer_unbox(_x1208);
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1212__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_one___fun1212(kk_function_t _fself, kk_box_t _b_923, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1212(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_one___fun1212, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_one___fun1212(kk_function_t _fself, kk_box_t _b_923, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1213;
  bool b_16563;
  kk_integer_t _x1214 = kk_integer_unbox(_b_923); /*int*/
  b_16563 = kk_integer_is_odd(_x1214,kk_context()); /*bool*/
  if (b_16563) {
    _x1213 = false; /*bool*/
  }
  else {
    _x1213 = true; /*bool*/
  }
  return kk_bool_box(_x1213);
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1218__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_one___fun1218(kk_function_t _fself, kk_box_t _b_928, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1218(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_one___fun1218, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_one___fun1218(kk_function_t _fself, kk_box_t _b_928, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x1219;
  kk_integer_t _x1220 = kk_integer_unbox(_b_928); /*int*/
  _x1219 = kk_test_float_bench2__mlift777_one__(_x1220, _ctx); /*int*/
  return kk_integer_box(_x1219);
}

kk_integer_t kk_test_float_bench2_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t x_803;
  bool _match_1181;
  kk_box_t _x1211;
  kk_box_t _x1215;
  kk_integer_t _x1216 = kk_integer_dup(a); /*int*/
  _x1215 = kk_integer_box(_x1216); /*3290*/
  _x1211 = kk_std_core_hnd__open_none1(kk_test_float_bench2_new_one___fun1212(_ctx), _x1215, _ctx); /*3291*/
  _match_1181 = kk_bool_unbox(_x1211); /*bool*/
  if (_match_1181) {
    x_803 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  else {
    x_803 = a; /*int*/
  }
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_803, _ctx);
    kk_box_t _x1217 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_one___fun1218(_ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1217);
  }
  {
    kk_std_core_hnd__ev ev_806;
    kk_ssize_t _x1221 = ((kk_ssize_t)0); /*ssize_t*/
    ev_806 = kk_evv_at(_x1221,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
    kk_box_t _x1222;
    {
      struct kk_std_core_hnd_Ev* _con1223 = kk_std_core_hnd__as_Ev(ev_806);
      kk_std_core_hnd__marker m0 = _con1223->marker;
      kk_box_t _box_x929 = _con1223->hnd;
      kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x929, NULL);
      kk_test_float_bench2__hnd_count_dup(h);
      kk_std_core_hnd__clause1 _match_1180 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
      {
        kk_function_t _fun_unbox_x933 = _match_1180.clause;
        _x1222 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x933, (_fun_unbox_x933, m0, ev_806, kk_integer_box(x_803), _ctx)); /*52*/
      }
    }
    return kk_integer_unbox(_x1222);
  }
}
 
// monadic lift

kk_integer_t kk_test_float_bench2__mlift778_two__(kk_integer_t _c_746, kk_context_t* _ctx) { /* (int) -> int */ 
  kk_std_core_hnd__ev ev_812;
  kk_ssize_t _x1229 = ((kk_ssize_t)0); /*ssize_t*/
  ev_812 = kk_evv_at(_x1229,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1230;
  {
    struct kk_std_core_hnd_Ev* _con1231 = kk_std_core_hnd__as_Ev(ev_812);
    kk_std_core_hnd__marker m0 = _con1231->marker;
    kk_box_t _box_x946 = _con1231->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x946, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1177 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
    {
      kk_function_t _fun_unbox_x950 = _match_1177.clause;
      _x1230 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x950, (_fun_unbox_x950, m0, ev_812, kk_integer_box(_c_746), _ctx)); /*52*/
    }
  }
  return kk_integer_unbox(_x1230);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1234__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_two___fun1234(kk_function_t _fself, kk_box_t _b_956, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1234(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_two___fun1234, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_two___fun1234(kk_function_t _fself, kk_box_t _b_956, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1235;
  bool b_16563;
  kk_integer_t _x1236 = kk_integer_unbox(_b_956); /*int*/
  b_16563 = kk_integer_is_odd(_x1236,kk_context()); /*bool*/
  if (b_16563) {
    _x1235 = false; /*bool*/
  }
  else {
    _x1235 = true; /*bool*/
  }
  return kk_bool_box(_x1235);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1240__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_two___fun1240(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1240(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_two___fun1240, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_two___fun1240(kk_function_t _fself, kk_box_t _b_961, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x1241;
  kk_integer_t _x1242 = kk_integer_unbox(_b_961); /*int*/
  _x1241 = kk_test_float_bench2__mlift778_two__(_x1242, _ctx); /*int*/
  return kk_integer_box(_x1241);
}

kk_integer_t kk_test_float_bench2_two__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t x_815;
  bool _match_1176;
  kk_box_t _x1233;
  kk_box_t _x1237;
  kk_integer_t _x1238 = kk_integer_dup(a); /*int*/
  _x1237 = kk_integer_box(_x1238); /*3290*/
  _x1233 = kk_std_core_hnd__open_none1(kk_test_float_bench2_new_two___fun1234(_ctx), _x1237, _ctx); /*3291*/
  _match_1176 = kk_bool_unbox(_x1233); /*bool*/
  if (_match_1176) {
    x_815 = kk_integer_add(a,(kk_integer_from_small(2)),kk_context()); /*int*/
  }
  else {
    x_815 = a; /*int*/
  }
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_815, _ctx);
    kk_box_t _x1239 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_two___fun1240(_ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1239);
  }
  {
    kk_std_core_hnd__ev ev_818;
    kk_ssize_t _x1243 = ((kk_ssize_t)0); /*ssize_t*/
    ev_818 = kk_evv_at(_x1243,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
    kk_box_t _x1244;
    {
      struct kk_std_core_hnd_Ev* _con1245 = kk_std_core_hnd__as_Ev(ev_818);
      kk_std_core_hnd__marker m0 = _con1245->marker;
      kk_box_t _box_x962 = _con1245->hnd;
      kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x962, NULL);
      kk_test_float_bench2__hnd_count_dup(h);
      kk_std_core_hnd__clause1 _match_1175 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
      {
        kk_function_t _fun_unbox_x966 = _match_1175.clause;
        _x1244 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x966, (_fun_unbox_x966, m0, ev_818, kk_integer_box(x_815), _ctx)); /*52*/
      }
    }
    return kk_integer_unbox(_x1244);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift780_f_fun1249__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift780_f_fun1249(kk_function_t _fself, kk_box_t _b_979, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift780_f_fun1249(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift780_f_fun1249__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift780_f_fun1249__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift780_f_fun1249, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift780_f_fun1249(kk_function_t _fself, kk_box_t _b_979, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift780_f_fun1249__t* _self = kk_function_as(struct kk_test_float_bench2__mlift780_f_fun1249__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_979, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift780_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_749, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_976_974 = kk_integer_add(_y_749,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_821 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_976_974)),kk_context()));
  kk_box_t _x1248;
  if (kk_yielding(kk_context())) {
    _x1248 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift780_f_fun1249(i, _ctx), _ctx); /*3860*/
  }
  else {
    _x1248 = (kk_ref_get(i,kk_context())); /*3860*/
  }
  return kk_integer_unbox(_x1248);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift781_f_fun1253__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift781_f_fun1253(kk_function_t _fself, kk_box_t _b_988, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift781_f_fun1253(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift781_f_fun1253__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift781_f_fun1253__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift781_f_fun1253, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift781_f_fun1253(kk_function_t _fself, kk_box_t _b_988, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift781_f_fun1253__t* _self = kk_function_as(struct kk_test_float_bench2__mlift781_f_fun1253__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1254;
  kk_integer_t _x1255 = kk_integer_unbox(_b_988); /*int*/
  _x1254 = kk_test_float_bench2__mlift780_f(c, i, _x1255, _ctx); /*int*/
  return kk_integer_box(_x1254);
}

kk_integer_t kk_test_float_bench2__mlift781_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_825;
  kk_box_t _x1250;
  kk_ref_t _x1251 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1250 = (kk_ref_get(_x1251,kk_context())); /*233*/
  x_825 = kk_integer_unbox(_x1250); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_825, _ctx);
    kk_box_t _x1252 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift781_f_fun1253(c, i, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1252);
  }
  {
    return kk_test_float_bench2__mlift780_f(c, i, x_825, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift783_f_fun1258__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift783_f_fun1258(kk_function_t _fself, kk_box_t _b_999, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift783_f_fun1258(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift783_f_fun1258__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift783_f_fun1258__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift783_f_fun1258, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift783_f_fun1258(kk_function_t _fself, kk_box_t _b_999, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift783_f_fun1258__t* _self = kk_function_as(struct kk_test_float_bench2__mlift783_f_fun1258__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_999, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench2__mlift783_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_753, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_996_994 = kk_integer_add(_y_753,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_827 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_996_994)),kk_context()));
  kk_box_t _x1257;
  if (kk_yielding(kk_context())) {
    _x1257 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift783_f_fun1258(i, _ctx), _ctx); /*3860*/
  }
  else {
    _x1257 = (kk_ref_get(i,kk_context())); /*3860*/
  }
  return kk_integer_unbox(_x1257);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift784_f_fun1262__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench2__mlift784_f_fun1262(kk_function_t _fself, kk_box_t _b_1008, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift784_f_fun1262(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift784_f_fun1262__t* _self = kk_function_alloc_as(struct kk_test_float_bench2__mlift784_f_fun1262__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2__mlift784_f_fun1262, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2__mlift784_f_fun1262(kk_function_t _fself, kk_box_t _b_1008, kk_context_t* _ctx) {
  struct kk_test_float_bench2__mlift784_f_fun1262__t* _self = kk_function_as(struct kk_test_float_bench2__mlift784_f_fun1262__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<722,int> */
  kk_ref_t i = _self->i; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1263;
  kk_integer_t _x1264 = kk_integer_unbox(_b_1008); /*int*/
  _x1263 = kk_test_float_bench2__mlift783_f(c, i, _x1264, _ctx); /*int*/
  return kk_integer_box(_x1263);
}

kk_integer_t kk_test_float_bench2__mlift784_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_1 : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_831;
  kk_box_t _x1259;
  kk_ref_t _x1260 = kk_ref_dup(c); /*local-var<722,int>*/
  _x1259 = (kk_ref_get(_x1260,kk_context())); /*233*/
  x_831 = kk_integer_unbox(_x1259); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_831, _ctx);
    kk_box_t _x1261 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift784_f_fun1262(c, i, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x1261);
  }
  {
    return kk_test_float_bench2__mlift783_f(c, i, x_831, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift787_f_fun1267__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift787_f_fun1267(kk_function_t _fself, kk_box_t _b_1014, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift787_f_fun1267(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift787_f_fun1267, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift787_f_fun1267(kk_function_t _fself, kk_box_t _b_1014, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x1268;
  kk_integer_t _x1269 = kk_integer_unbox(_b_1014); /*int*/
  _x1268 = kk_test_float_bench2_one__(_x1269, _ctx); /*int*/
  return kk_integer_box(_x1268);
}


// lift anonymous function
struct kk_test_float_bench2__mlift787_f_fun1271__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift787_f_fun1271(kk_function_t _fself, kk_box_t _b_1019, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift787_f_fun1271(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift787_f_fun1271, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift787_f_fun1271(kk_function_t _fself, kk_box_t _b_1019, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_1019, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_test_float_bench2__mlift787_f(kk_integer_t a2, kk_context_t* _ctx) { /* forall<h> (a2 : int) -> <count,local<h>,bra,div> () */ 
  kk_ssize_t _b_1015_1011 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_833;
  kk_box_t _x1266 = kk_std_core_hnd__open_at1(_b_1015_1011, kk_test_float_bench2__new_mlift787_f_fun1267(_ctx), kk_integer_box(a2), _ctx); /*5429*/
  x_833 = kk_integer_unbox(_x1266); /*int*/
  kk_integer_drop(x_833, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1270 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift787_f_fun1271(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1270); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift788_f_fun1273__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift788_f_fun1273(kk_function_t _fself, kk_box_t _b_1025, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift788_f_fun1273(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift788_f_fun1273, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift788_f_fun1273(kk_function_t _fself, kk_box_t _b_1025, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x1274;
  kk_integer_t _x1275 = kk_integer_unbox(_b_1025); /*int*/
  _x1274 = kk_test_float_bench2_two__(_x1275, _ctx); /*int*/
  return kk_integer_box(_x1274);
}


// lift anonymous function
struct kk_test_float_bench2__mlift788_f_fun1277__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift788_f_fun1277(kk_function_t _fself, kk_box_t _b_1030, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift788_f_fun1277(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift788_f_fun1277, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift788_f_fun1277(kk_function_t _fself, kk_box_t _b_1030, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1278 = kk_Unit;
  kk_integer_t _x1279 = kk_integer_unbox(_b_1030); /*int*/
  kk_test_float_bench2__mlift787_f(_x1279, _ctx);
  return kk_unit_box(_x1278);
}

kk_unit_t kk_test_float_bench2__mlift788_f(kk_integer_t a1, kk_context_t* _ctx) { /* forall<h> (a1 : int) -> <count,local<h>,bra,div> () */ 
  kk_ssize_t _b_1026_1022 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_836;
  kk_box_t _x1272 = kk_std_core_hnd__open_at1(_b_1026_1022, kk_test_float_bench2__new_mlift788_f_fun1273(_ctx), kk_integer_box(a1), _ctx); /*5429*/
  x_836 = kk_integer_unbox(_x1272); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_836, _ctx);
    kk_box_t _x1276 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift788_f_fun1277(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1276); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift787_f(x_836, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift789_f_fun1281__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift789_f_fun1281(kk_function_t _fself, kk_box_t _b_1036, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift789_f_fun1281(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift789_f_fun1281, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift789_f_fun1281(kk_function_t _fself, kk_box_t _b_1036, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x1282;
  kk_integer_t _x1283 = kk_integer_unbox(_b_1036); /*int*/
  _x1282 = kk_test_float_bench2_one__(_x1283, _ctx); /*int*/
  return kk_integer_box(_x1282);
}


// lift anonymous function
struct kk_test_float_bench2__mlift789_f_fun1285__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift789_f_fun1285(kk_function_t _fself, kk_box_t _b_1041, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift789_f_fun1285(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift789_f_fun1285, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift789_f_fun1285(kk_function_t _fself, kk_box_t _b_1041, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1286 = kk_Unit;
  kk_integer_t _x1287 = kk_integer_unbox(_b_1041); /*int*/
  kk_test_float_bench2__mlift788_f(_x1287, _ctx);
  return kk_unit_box(_x1286);
}

kk_unit_t kk_test_float_bench2__mlift789_f(kk_integer_t a00, kk_context_t* _ctx) { /* forall<h> (a00 : int) -> <local<h>,count,bra,div> () */ 
  kk_ssize_t _b_1037_1033 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x_838;
  kk_box_t _x1280 = kk_std_core_hnd__open_at1(_b_1037_1033, kk_test_float_bench2__new_mlift789_f_fun1281(_ctx), kk_integer_box(a00), _ctx); /*5429*/
  x_838 = kk_integer_unbox(_x1280); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_838, _ctx);
    kk_box_t _x1284 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift789_f_fun1285(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1284); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift788_f(x_838, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench2__mlift790_f_fun1290__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2__mlift790_f_fun1290(kk_function_t _fself, kk_box_t _b_1047, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2__new_mlift790_f_fun1290(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2__mlift790_f_fun1290, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2__mlift790_f_fun1290(kk_function_t _fself, kk_box_t _b_1047, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1291 = kk_Unit;
  kk_integer_t _x1292 = kk_integer_unbox(_b_1047); /*int*/
  kk_test_float_bench2__mlift789_f(_x1292, _ctx);
  return kk_unit_box(_x1291);
}

kk_unit_t kk_test_float_bench2__mlift790_f(kk_ref_t i, kk_unit_t wild__3, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_3 : ()) -> <bra,count,local<h>,div> () */ 
  kk_integer_t x_840;
  kk_box_t _x1288 = (kk_ref_get(i,kk_context())); /*233*/
  x_840 = kk_integer_unbox(_x1288); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_840, _ctx);
    kk_box_t _x1289 = kk_std_core_hnd_yield_extend(kk_test_float_bench2__new_mlift790_f_fun1290(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x1289); return kk_Unit;
  }
  {
    kk_test_float_bench2__mlift789_f(x_840, _ctx); return kk_Unit;
  }
}
 
// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// }


// lift anonymous function
struct kk_test_float_bench2_f_fun1297__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench2_f_fun1297(kk_function_t _fself, kk_std_core_hnd__marker _b_1061, kk_std_core_hnd__ev _b_1062, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1297(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1297__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1297__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1297, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1297(kk_function_t _fself, kk_std_core_hnd__marker _b_1061, kk_std_core_hnd__ev _b_1062, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1297__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1297__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_1062, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1298 = kk_Unit;
  kk_integer_t _b_1112_1059;
  kk_integer_t _x1299;
  kk_box_t _x1300 = (kk_ref_get(loc,kk_context())); /*233*/
  _x1299 = kk_integer_unbox(_x1300); /*int*/
  kk_integer_t _x1301;
  kk_box_t _x1302 = (kk_ref_get(loc0,kk_context())); /*233*/
  _x1301 = kk_integer_unbox(_x1302); /*int*/
  _b_1112_1059 = kk_integer_add(_x1299,_x1301,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_1112_1059)),kk_context()));
  return kk_unit_box(_x1298);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1303__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1303(kk_function_t _fself, kk_box_t _b_1105, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1303(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1303, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1303(kk_function_t _fself, kk_box_t _b_1105, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_1105;
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1305__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1305(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1305(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1305__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1305__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1305, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1309__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1309(kk_function_t _fself, kk_box_t _b_1070, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1309(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1309__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1309__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1309, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1314__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1314(kk_function_t _fself, kk_box_t _b_1068, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1314(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1314__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1314__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1314, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1314(kk_function_t _fself, kk_box_t _b_1068, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1314__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1314__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1315;
  kk_unit_t _x1316 = kk_Unit;
  kk_unit_unbox(_b_1068);
  _x1315 = kk_test_float_bench2__mlift781_f(loc0, loc, _x1316, _ctx); /*int*/
  return kk_integer_box(_x1315);
}
static kk_box_t kk_test_float_bench2_f_fun1309(kk_function_t _fself, kk_box_t _b_1070, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1309__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1309__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1310;
  kk_integer_t _b_1066_1064;
  kk_integer_t _x1311 = kk_integer_unbox(_b_1070); /*int*/
  _b_1066_1064 = kk_integer_add(_x1311,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_849 = kk_Unit;
  kk_ref_t _x1312 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1312,(kk_integer_box(_b_1066_1064)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1313 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1314(loc, loc0, _ctx), _ctx); /*3860*/
    _x1310 = kk_integer_unbox(_x1313); /*int*/
  }
  else {
    _x1310 = kk_test_float_bench2__mlift781_f(loc0, loc, x_849, _ctx); /*int*/
  }
  return kk_integer_box(_x1310);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1319__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1319(kk_function_t _fself, kk_box_t _b_1078, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1319(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1319__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1319__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1319, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1324__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1324(kk_function_t _fself, kk_box_t _b_1076, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1324(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1324__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1324__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1324, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1324(kk_function_t _fself, kk_box_t _b_1076, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1324__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1324__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1325;
  kk_unit_t _x1326 = kk_Unit;
  kk_unit_unbox(_b_1076);
  _x1325 = kk_test_float_bench2__mlift784_f(loc0, loc, _x1326, _ctx); /*int*/
  return kk_integer_box(_x1325);
}
static kk_box_t kk_test_float_bench2_f_fun1319(kk_function_t _fself, kk_box_t _b_1078, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1319__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1319__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1320;
  kk_integer_t _b_1074_1072;
  kk_integer_t _x1321 = kk_integer_unbox(_b_1078); /*int*/
  _b_1074_1072 = kk_integer_add(_x1321,(kk_integer_from_small(2)),kk_context()); /*int*/
  kk_unit_t x0_851 = kk_Unit;
  kk_ref_t _x1322 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1322,(kk_integer_box(_b_1074_1072)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1323 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1324(loc, loc0, _ctx), _ctx); /*3860*/
    _x1320 = kk_integer_unbox(_x1323); /*int*/
  }
  else {
    _x1320 = kk_test_float_bench2__mlift784_f(loc0, loc, x0_851, _ctx); /*int*/
  }
  return kk_integer_box(_x1320);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1328__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1328(kk_function_t _fself, kk_box_t _b_1100, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1328(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1328__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1328__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1328, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1328(kk_function_t _fself, kk_box_t _b_1100, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1328__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1328__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_1100, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1329__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1329(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1329(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1329__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1329__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1329, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1332__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static bool kk_test_float_bench2_f_fun1332(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1332(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1332__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1332__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1332, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1335__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1335(kk_function_t _fself, kk_box_t _b_1083, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1335(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1335, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1335(kk_function_t _fself, kk_box_t _b_1083, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1336;
  kk_integer_t _x1337 = kk_integer_unbox(_b_1083); /*int*/
  kk_integer_t _x1338 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1336 = kk_integer_lt(_x1337,_x1338,kk_context()); /*bool*/
  return kk_bool_box(_x1336);
}
static bool kk_test_float_bench2_f_fun1332(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1332__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1332__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x1_853;
  kk_box_t _x1333 = (kk_ref_get(loc,kk_context())); /*233*/
  x1_853 = kk_integer_unbox(_x1333); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x1_853, _ctx);
    kk_box_t _x1334 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1335(_ctx), _ctx); /*3860*/
    return kk_bool_unbox(_x1334);
  }
  {
    kk_integer_t _x1339 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x1_853,_x1339,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1340__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1340(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1340(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1340__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1340__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1340, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1342__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1342(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1342(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1342, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1342(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1343 = kk_Unit;
  kk_std_core_hnd__ev ev_858;
  kk_ssize_t _x1344 = ((kk_ssize_t)0); /*ssize_t*/
  ev_858 = kk_evv_at(_x1344,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1345;
  {
    struct kk_std_core_hnd_Ev* _con1346 = kk_std_core_hnd__as_Ev(ev_858);
    kk_std_core_hnd__marker m0 = _con1346->marker;
    kk_box_t _box_x1084 = _con1346->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x1084, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1162 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x1087 = _match_1162.clause;
      _x1345 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x1087, (_fun_unbox_x1087, m0, ev_858, _ctx)); /*37*/
    }
  }
  kk_unit_unbox(_x1345);
  return kk_unit_box(_x1343);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1349__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1349(kk_function_t _fself, kk_box_t _b_1095, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1349(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1349__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1349__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1349, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1349(kk_function_t _fself, kk_box_t _b_1095, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1349__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1349__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1350 = kk_Unit;
  kk_unit_t _x1351 = kk_Unit;
  kk_unit_unbox(_b_1095);
  kk_test_float_bench2__mlift790_f(loc, _x1351, _ctx);
  return kk_unit_box(_x1350);
}
static kk_unit_t kk_test_float_bench2_f_fun1340(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1340__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1340__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_1092_1090 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x2_856 = kk_Unit;
  kk_box_t _x1341 = kk_std_core_hnd__open_at0(_b_1092_1090, kk_test_float_bench2_new_f_fun1342(_ctx), _ctx); /*5322*/
  kk_unit_unbox(_x1341);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1348 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1349(loc, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x1348);
  }
  {
    return kk_test_float_bench2__mlift790_f(loc, x2_856, _ctx);
  }
}
static kk_box_t kk_test_float_bench2_f_fun1329(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1329__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1329__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1330 = kk_Unit;
  kk_function_t _x1331;
  kk_ref_dup(loc);
  _x1331 = kk_test_float_bench2_new_f_fun1332(loc, _ctx); /*() -> <div,local<722>,test/float/bench2/bra,test/float/bench2/count> bool*/
  kk_std_core_while(_x1331, kk_test_float_bench2_new_f_fun1340(loc, _ctx), _ctx);
  return kk_unit_box(_x1330);
}
static kk_box_t kk_test_float_bench2_f_fun1305(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1305__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1305__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1306;
  int32_t _b_1115_1096 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench2__hnd_count _b_1116_1097;
  kk_std_core_hnd__clause1 _x1307;
  kk_function_t _x1308;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1308 = kk_test_float_bench2_new_f_fun1309(loc, loc0, _ctx); /*(8873) -> 8871 8874*/
  _x1307 = kk_std_core_hnd_clause_tail1(_x1308, _ctx); /*std/core/hnd/clause1<8873,8874,8875,8871,8872>*/
  kk_std_core_hnd__clause1 _x1317;
  kk_function_t _x1318;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1318 = kk_test_float_bench2_new_f_fun1319(loc, loc0, _ctx); /*(8873) -> 8871 8874*/
  _b_1116_1097 = kk_test_float_bench2__new_Hnd_count(kk_reuse_null, _x1307, _x1317, _ctx); /*test/float/bench2/.hnd-count<<local<722>,test/float/bench2/bra,div>,int>*/
  kk_box_t _x1327 = kk_test_float_bench2__handle_count(_b_1115_1096, _b_1116_1097, kk_test_float_bench2_new_f_fun1328(loc0, _ctx), kk_test_float_bench2_new_f_fun1329(loc, _ctx), _ctx); /*171*/
  _x1306 = kk_integer_unbox(_x1327); /*int*/
  return kk_integer_box(_x1306);
}

kk_integer_t kk_test_float_bench2_f(kk_context_t* _ctx) { /* () -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  int32_t _b_1106_1101 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1293;
  kk_test_float_bench2__hnd_bra _x1294;
  kk_std_core_hnd__clause0 _x1295;
  kk_function_t _x1296;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1296 = kk_test_float_bench2_new_f_fun1297(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<39,40>, std/core/hnd/ev<38>) -> 39 37*/
  _x1295 = kk_std_core_hnd__new_Clause0(_x1296, _ctx); /*std/core/hnd/clause0<37,38,39,40>*/
  _x1294 = kk_test_float_bench2__new_Hnd_bra(kk_reuse_null, _x1295, _ctx); /*test/float/bench2/.hnd-bra<6,7>*/
  kk_function_t _x1304;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1304 = kk_test_float_bench2_new_f_fun1305(loc, loc0, _ctx); /*() -> <test/float/bench2/bra|141> 140*/
  _x1293 = kk_test_float_bench2__handle_bra(_b_1106_1101, _x1294, kk_test_float_bench2_new_f_fun1303(_ctx), _x1304, _ctx); /*142*/
  res1 = kk_integer_unbox(_x1293); /*int*/
  kk_integer_t res0;
  kk_box_t _x1352 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*9897*/
  res0 = kk_integer_unbox(_x1352); /*int*/
  kk_integer_t res;
  kk_box_t _x1353 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*9897*/
  res = kk_integer_unbox(_x1353); /*int*/
  kk_box_t _x1354 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*9897*/
  return kk_integer_unbox(_x1354);
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
    kk_string_t _x1186;
    kk_define_string_literal(, _s1187, 10, "bra.bench2")
    _x1186 = kk_string_dup(_s1187); /*string*/
    kk_test_float_bench2__tag_bra = kk_std_core_hnd__new_Htag(_x1186, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  }
  {
    kk_string_t _x1189;
    kk_define_string_literal(, _s1190, 12, "count.bench2")
    _x1189 = kk_string_dup(_s1190); /*string*/
    kk_test_float_bench2__tag_count = kk_std_core_hnd__new_Htag(_x1189, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
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
