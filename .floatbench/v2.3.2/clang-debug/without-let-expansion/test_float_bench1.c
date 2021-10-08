// Koka generated module: "test/float/bench1", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench1.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench1__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench1__handle_bra(int32_t cfc, kk_test_float_bench1__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1010 = kk_std_core_hnd__htag_dup(kk_test_float_bench1__tag_bra); /*std/core/hnd/htag<test/float/bench1/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1010, cfc, kk_test_float_bench1__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench1__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench1__handle_count(int32_t cfc, kk_test_float_bench1__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1013 = kk_std_core_hnd__htag_dup(kk_test_float_bench1__tag_count); /*std/core/hnd/htag<test/float/bench1/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1013, cfc, kk_test_float_bench1__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench1_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_734;
  kk_ssize_t _x1020 = ((kk_ssize_t)0); /*ssize_t*/
  ev_734 = kk_evv_at(_x1020,kk_context()); /*std/core/hnd/ev<test/float/bench1/.hnd-bra>*/
  kk_box_t _x1021;
  {
    struct kk_std_core_hnd_Ev* _con1022 = kk_std_core_hnd__as_Ev(ev_734);
    kk_std_core_hnd__marker m0 = _con1022->marker;
    kk_box_t _box_x811 = _con1022->hnd;
    kk_test_float_bench1__hnd_bra h = kk_test_float_bench1__hnd_bra_unbox(_box_x811, NULL);
    kk_test_float_bench1__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1006 = kk_test_float_bench1__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench1/.hnd-bra,161,162>*/;
    {
      kk_function_t _fun_unbox_x814 = _match_1006.clause;
      _x1021 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x814, (_fun_unbox_x814, m0, ev_734, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1021); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench1_one___fun1029__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench1_one___fun1029(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_one___fun1029(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench1_one___fun1029__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_one___fun1029__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_one___fun1029, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_one___fun1029(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_one___fun1029__t* _self = kk_function_as(struct kk_test_float_bench1_one___fun1029__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1030;
  bool b_16563;
  kk_integer_t _x1031 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1031,kk_context()); /*bool*/
  if (b_16563) {
    _x1030 = a; /*int*/
  }
  else {
    _x1030 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1030);
}

kk_integer_t kk_test_float_bench1_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_684;
  kk_box_t _x1028 = kk_std_core_hnd__open_none0(kk_test_float_bench1_new_one___fun1029(a, _ctx), _ctx); /*1001*/
  a0_684 = kk_integer_unbox(_x1028); /*int*/
  kk_std_core_hnd__ev ev_739;
  kk_ssize_t _x1032 = ((kk_ssize_t)0); /*ssize_t*/
  ev_739 = kk_evv_at(_x1032,kk_context()); /*std/core/hnd/ev<test/float/bench1/.hnd-count>*/
  kk_box_t _x1033;
  {
    struct kk_std_core_hnd_Ev* _con1034 = kk_std_core_hnd__as_Ev(ev_739);
    kk_std_core_hnd__marker m0 = _con1034->marker;
    kk_box_t _box_x827 = _con1034->hnd;
    kk_test_float_bench1__hnd_count h = kk_test_float_bench1__hnd_count_unbox(_box_x827, NULL);
    kk_test_float_bench1__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1004 = kk_test_float_bench1__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench1/.hnd-count,178,179>*/;
    {
      kk_function_t _fun_unbox_x831 = _match_1004.clause;
      _x1033 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x831, (_fun_unbox_x831, m0, ev_739, kk_integer_box(a0_684), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1033);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift720_f_fun1038__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench1__mlift720_f_fun1038(kk_function_t _fself, kk_box_t _b_843, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift720_f_fun1038(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift720_f_fun1038__t* _self = kk_function_alloc_as(struct kk_test_float_bench1__mlift720_f_fun1038__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1__mlift720_f_fun1038, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1__mlift720_f_fun1038(kk_function_t _fself, kk_box_t _b_843, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift720_f_fun1038__t* _self = kk_function_as(struct kk_test_float_bench1__mlift720_f_fun1038__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_box_drop(_b_843, _ctx);
  return (kk_ref_get(i,kk_context()));
}

kk_integer_t kk_test_float_bench1__mlift720_f(kk_ref_t c, kk_ref_t i, kk_integer_t _y_696, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, int) -> <local<h>,bra,div> int */ 
  kk_integer_t _b_840_838 = kk_integer_add(_y_696,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x_742 = kk_Unit;
  (kk_ref_set(c,(kk_integer_box(_b_840_838)),kk_context()));
  kk_box_t _x1037;
  if (kk_yielding(kk_context())) {
    _x1037 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift720_f_fun1038(i, _ctx), _ctx); /*1002*/
  }
  else {
    _x1037 = (kk_ref_get(i,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1037);
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift721_f_fun1042__t {
  struct kk_function_s _base;
  kk_ref_t c;
  kk_ref_t i;
};
static kk_box_t kk_test_float_bench1__mlift721_f_fun1042(kk_function_t _fself, kk_box_t _b_852, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift721_f_fun1042(kk_ref_t c, kk_ref_t i, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift721_f_fun1042__t* _self = kk_function_alloc_as(struct kk_test_float_bench1__mlift721_f_fun1042__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1__mlift721_f_fun1042, kk_context());
  _self->c = c;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1__mlift721_f_fun1042(kk_function_t _fself, kk_box_t _b_852, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift721_f_fun1042__t* _self = kk_function_as(struct kk_test_float_bench1__mlift721_f_fun1042__t*, _fself);
  kk_ref_t c = _self->c; /* local-var<575,int> */
  kk_ref_t i = _self->i; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(c);kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _x1043;
  kk_integer_t _x1044 = kk_integer_unbox(_b_852); /*int*/
  _x1043 = kk_test_float_bench1__mlift720_f(c, i, _x1044, _ctx); /*int*/
  return kk_integer_box(_x1043);
}

kk_integer_t kk_test_float_bench1__mlift721_f(kk_ref_t c, kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h> (c : local-var<h,int>, i : local-var<h,int>, wild_ : ()) -> <local<h>,bra,div> int */ 
  kk_integer_t x_746;
  kk_box_t _x1039;
  kk_ref_t _x1040 = kk_ref_dup(c); /*local-var<575,int>*/
  _x1039 = (kk_ref_get(_x1040,kk_context())); /*1000*/
  x_746 = kk_integer_unbox(_x1039); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_746, _ctx);
    kk_box_t _x1041 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift721_f_fun1042(c, i, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1041);
  }
  {
    return kk_test_float_bench1__mlift720_f(c, i, x_746, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift724_f_fun1047__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1__mlift724_f_fun1047(kk_function_t _fself, kk_box_t _b_856, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift724_f_fun1047(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__mlift724_f_fun1047, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1__mlift724_f_fun1047(kk_function_t _fself, kk_box_t _b_856, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1048 = kk_Unit;
  kk_integer_t _x1049 = kk_integer_unbox(_b_856); /*int*/
  kk_test_float_bench1__mlift723_f(_x1049, _ctx);
  return kk_unit_box(_x1048);
}

kk_unit_t kk_test_float_bench1__mlift724_f(kk_integer_t a3, kk_context_t* _ctx) { /* (a3 : int) -> count () */ 
  kk_integer_t x_748 = kk_test_float_bench1_one__(a3, _ctx); /*int*/;
  kk_integer_drop(x_748, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1046 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift724_f_fun1047(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1046); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift725_f_fun1051__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1__mlift725_f_fun1051(kk_function_t _fself, kk_box_t _b_859, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift725_f_fun1051(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__mlift725_f_fun1051, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1__mlift725_f_fun1051(kk_function_t _fself, kk_box_t _b_859, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1052 = kk_Unit;
  kk_integer_t _x1053 = kk_integer_unbox(_b_859); /*int*/
  kk_test_float_bench1__mlift724_f(_x1053, _ctx);
  return kk_unit_box(_x1052);
}

kk_unit_t kk_test_float_bench1__mlift725_f(kk_integer_t a2, kk_context_t* _ctx) { /* (a2 : int) -> count () */ 
  kk_integer_t x_750 = kk_test_float_bench1_one__(a2, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_750, _ctx);
    kk_box_t _x1050 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift725_f_fun1051(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1050); return kk_Unit;
  }
  {
    kk_test_float_bench1__mlift724_f(x_750, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift726_f_fun1055__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1__mlift726_f_fun1055(kk_function_t _fself, kk_box_t _b_862, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift726_f_fun1055(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__mlift726_f_fun1055, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1__mlift726_f_fun1055(kk_function_t _fself, kk_box_t _b_862, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1056 = kk_Unit;
  kk_integer_t _x1057 = kk_integer_unbox(_b_862); /*int*/
  kk_test_float_bench1__mlift725_f(_x1057, _ctx);
  return kk_unit_box(_x1056);
}

kk_unit_t kk_test_float_bench1__mlift726_f(kk_integer_t a1, kk_context_t* _ctx) { /* (a1 : int) -> count () */ 
  kk_integer_t x_752 = kk_test_float_bench1_one__(a1, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_752, _ctx);
    kk_box_t _x1054 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift726_f_fun1055(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1054); return kk_Unit;
  }
  {
    kk_test_float_bench1__mlift725_f(x_752, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift727_f_fun1059__t {
  struct kk_function_s _base;
  kk_integer_t a0;
};
static kk_box_t kk_test_float_bench1__mlift727_f_fun1059(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift727_f_fun1059(kk_integer_t a0, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift727_f_fun1059__t* _self = kk_function_alloc_as(struct kk_test_float_bench1__mlift727_f_fun1059__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1__mlift727_f_fun1059, kk_context());
  _self->a0 = a0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1__mlift727_f_fun1062__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1__mlift727_f_fun1062(kk_function_t _fself, kk_box_t _b_865, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift727_f_fun1062(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__mlift727_f_fun1062, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1__mlift727_f_fun1062(kk_function_t _fself, kk_box_t _b_865, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1063 = kk_Unit;
  kk_integer_t _x1064 = kk_integer_unbox(_b_865); /*int*/
  kk_test_float_bench1__mlift726_f(_x1064, _ctx);
  return kk_unit_box(_x1063);
}
static kk_box_t kk_test_float_bench1__mlift727_f_fun1059(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift727_f_fun1059__t* _self = kk_function_as(struct kk_test_float_bench1__mlift727_f_fun1059__t*, _fself);
  kk_integer_t a0 = _self->a0; /* int */
  kk_drop_match(_self, {kk_integer_dup(a0);}, {}, _ctx)
  kk_unit_t _x1060 = kk_Unit;
  kk_integer_t x_754 = kk_test_float_bench1_one__(a0, _ctx); /*int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_754, _ctx);
    kk_box_t _x1061 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift727_f_fun1062(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1061);
  }
  else {
    kk_test_float_bench1__mlift726_f(x_754, _ctx);
  }
  return kk_unit_box(_x1060);
}

kk_unit_t kk_test_float_bench1__mlift727_f(kk_integer_t a0, kk_context_t* _ctx) { /* forall<h> (a0 : int) -> <local<h>,count,bra,div> () */ 
  kk_ssize_t _b_868_866 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_box_t _x1058 = kk_std_core_hnd__open_at0(_b_868_866, kk_test_float_bench1__new_mlift727_f_fun1059(a0, _ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1058); return kk_Unit;
}
 
// monadic lift


// lift anonymous function
struct kk_test_float_bench1__mlift728_f_fun1067__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1__mlift728_f_fun1067(kk_function_t _fself, kk_box_t _b_874, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_mlift728_f_fun1067(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__mlift728_f_fun1067, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1__mlift728_f_fun1067(kk_function_t _fself, kk_box_t _b_874, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1068 = kk_Unit;
  kk_integer_t _x1069 = kk_integer_unbox(_b_874); /*int*/
  kk_test_float_bench1__mlift727_f(_x1069, _ctx);
  return kk_unit_box(_x1068);
}

kk_unit_t kk_test_float_bench1__mlift728_f(kk_ref_t i, kk_unit_t wild__1, kk_context_t* _ctx) { /* forall<h> (i : local-var<h,int>, wild_1 : ()) -> <bra,count,div,local<h>> () */ 
  kk_integer_t x_756;
  kk_box_t _x1065 = (kk_ref_get(i,kk_context())); /*1000*/
  x_756 = kk_integer_unbox(_x1065); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_756, _ctx);
    kk_box_t _x1066 = kk_std_core_hnd_yield_extend(kk_test_float_bench1__new_mlift728_f_fun1067(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x1066); return kk_Unit;
  }
  {
    kk_test_float_bench1__mlift727_f(x_756, _ctx); return kk_Unit;
  }
}
 
// fun g() : bra () { () }
// fun h() : <bra,count> () { () }


// lift anonymous function
struct kk_test_float_bench1_f_fun1074__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench1_f_fun1074(kk_function_t _fself, kk_std_core_hnd__marker _b_888, kk_std_core_hnd__ev _b_889, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1074(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1074__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1074__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1074, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_f_fun1074(kk_function_t _fself, kk_std_core_hnd__marker _b_888, kk_std_core_hnd__ev _b_889, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1074__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1074__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_889, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1075 = kk_Unit;
  kk_integer_t _b_932_886;
  kk_integer_t _x1076;
  kk_box_t _x1077 = (kk_ref_get(loc,kk_context())); /*1000*/
  _x1076 = kk_integer_unbox(_x1077); /*int*/
  kk_integer_t _x1078;
  kk_box_t _x1079 = (kk_ref_get(loc0,kk_context())); /*1000*/
  _x1078 = kk_integer_unbox(_x1079); /*int*/
  _b_932_886 = kk_integer_add(_x1076,_x1078,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_932_886)),kk_context()));
  return kk_unit_box(_x1075);
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1080__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1_f_fun1080(kk_function_t _fself, kk_box_t _b_925, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1080(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1_f_fun1080, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1_f_fun1080(kk_function_t _fself, kk_box_t _b_925, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_925;
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1082__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_integer_t n;
};
static kk_box_t kk_test_float_bench1_f_fun1082(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1082(kk_ref_t loc, kk_ref_t loc0, kk_integer_t n, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1082__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1082__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1082, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->n = n;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1086__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench1_f_fun1086(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1086(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1086__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1086__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1086, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1089__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench1_f_fun1089(kk_function_t _fself, kk_box_t _b_897, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1089(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1089__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1089__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1089, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1094__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench1_f_fun1094(kk_function_t _fself, kk_box_t _b_895, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1094(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1094__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1094__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1094, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_f_fun1094(kk_function_t _fself, kk_box_t _b_895, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1094__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1094__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1095;
  kk_unit_t _x1096 = kk_Unit;
  kk_unit_unbox(_b_895);
  _x1095 = kk_test_float_bench1__mlift721_f(loc0, loc, _x1096, _ctx); /*int*/
  return kk_integer_box(_x1095);
}
static kk_box_t kk_test_float_bench1_f_fun1089(kk_function_t _fself, kk_box_t _b_897, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1089__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1089__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1090;
  kk_integer_t _b_893_891;
  kk_integer_t _x1091 = kk_integer_unbox(_b_897); /*int*/
  _b_893_891 = kk_integer_add(_x1091,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_765 = kk_Unit;
  kk_ref_t _x1092 = kk_ref_dup(loc); /*local-var<575,int>*/
  (kk_ref_set(_x1092,(kk_integer_box(_b_893_891)),kk_context()));
  if (kk_yielding(kk_context())) {
    kk_box_t _x1093 = kk_std_core_hnd_yield_extend(kk_test_float_bench1_new_f_fun1094(loc, loc0, _ctx), _ctx); /*1002*/
    _x1090 = kk_integer_unbox(_x1093); /*int*/
  }
  else {
    _x1090 = kk_test_float_bench1__mlift721_f(loc0, loc, x_765, _ctx); /*int*/
  }
  return kk_integer_box(_x1090);
}
static kk_box_t kk_test_float_bench1_f_fun1086(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1086__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1086__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_test_float_bench1__hnd_count _x1087;
  kk_std_core_hnd__clause1 _x1088 = kk_std_core_hnd_clause_tail1(kk_test_float_bench1_new_f_fun1089(loc, loc0, _ctx), _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  _x1087 = kk_test_float_bench1__new_Hnd_count(kk_reuse_null, _x1088, _ctx); /*test/float/bench1/.hnd-count<15,16>*/
  return kk_test_float_bench1__hnd_count_box(_x1087, _ctx);
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1099__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench1_f_fun1099(kk_function_t _fself, kk_box_t _b_920, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1099(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1099__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1099__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1099, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_f_fun1099(kk_function_t _fself, kk_box_t _b_920, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1099__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1099__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_920, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1100__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_integer_t n;
};
static kk_box_t kk_test_float_bench1_f_fun1100(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1100(kk_ref_t loc, kk_ref_t loc0, kk_integer_t n, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1100__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1100__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1100, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->n = n;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1102__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
  kk_integer_t n;
};
static bool kk_test_float_bench1_f_fun1102(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1102(kk_ref_t loc0, kk_integer_t n, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1102__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1102__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1102, kk_context());
  _self->loc0 = loc0;
  _self->n = n;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1105__t {
  struct kk_function_s _base;
  kk_integer_t n;
};
static kk_box_t kk_test_float_bench1_f_fun1105(kk_function_t _fself, kk_box_t _b_903, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1105(kk_integer_t n, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1105__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1105__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1105, kk_context());
  _self->n = n;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_f_fun1105(kk_function_t _fself, kk_box_t _b_903, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1105__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1105__t*, _fself);
  kk_integer_t n = _self->n; /* int */
  kk_drop_match(_self, {kk_integer_dup(n);}, {}, _ctx)
  bool _x1106;
  kk_integer_t _x1107 = kk_integer_unbox(_b_903); /*int*/
  kk_integer_t _x1108 = kk_integer_mul(n,(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1106 = kk_integer_lt(_x1107,_x1108,kk_context()); /*bool*/
  return kk_bool_box(_x1106);
}
static bool kk_test_float_bench1_f_fun1102(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1102__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1102__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_integer_t n = _self->n; /* int */
  kk_drop_match(_self, {kk_ref_dup(loc0);kk_integer_dup(n);}, {}, _ctx)
  kk_integer_t x0_767;
  kk_box_t _x1103 = (kk_ref_get(loc0,kk_context())); /*1000*/
  x0_767 = kk_integer_unbox(_x1103); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x0_767, _ctx);
    kk_box_t _x1104 = kk_std_core_hnd_yield_extend(kk_test_float_bench1_new_f_fun1105(n, _ctx), _ctx); /*1002*/
    return kk_bool_unbox(_x1104);
  }
  {
    kk_integer_t _x1109 = kk_integer_mul(n,(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x0_767,_x1109,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1110__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench1_f_fun1110(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1110(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1110__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1110__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1110, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench1_f_fun1112__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1_f_fun1112(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1112(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1_f_fun1112, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1_f_fun1112(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1113 = kk_Unit;
  kk_std_core_hnd__ev ev_773;
  kk_ssize_t _x1114 = ((kk_ssize_t)0); /*ssize_t*/
  ev_773 = kk_evv_at(_x1114,kk_context()); /*std/core/hnd/ev<test/float/bench1/.hnd-bra>*/
  kk_box_t _x1115;
  {
    struct kk_std_core_hnd_Ev* _con1116 = kk_std_core_hnd__as_Ev(ev_773);
    kk_std_core_hnd__marker m0 = _con1116->marker;
    kk_box_t _box_x904 = _con1116->hnd;
    kk_test_float_bench1__hnd_bra h = kk_test_float_bench1__hnd_bra_unbox(_box_x904, NULL);
    kk_test_float_bench1__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_994 = kk_test_float_bench1__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench1/.hnd-bra,161,162>*/;
    {
      kk_function_t _fun_unbox_x907 = _match_994.clause;
      _x1115 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x907, (_fun_unbox_x907, m0, ev_773, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1115);
  return kk_unit_box(_x1113);
}


// lift anonymous function
struct kk_test_float_bench1_f_fun1119__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench1_f_fun1119(kk_function_t _fself, kk_box_t _b_915, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_f_fun1119(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1119__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_f_fun1119__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_f_fun1119, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_f_fun1119(kk_function_t _fself, kk_box_t _b_915, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1119__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1119__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1120 = kk_Unit;
  kk_unit_t _x1121 = kk_Unit;
  kk_unit_unbox(_b_915);
  kk_test_float_bench1__mlift728_f(loc, _x1121, _ctx);
  return kk_unit_box(_x1120);
}
static kk_unit_t kk_test_float_bench1_f_fun1110(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1110__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1110__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_912_910 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x1_771 = kk_Unit;
  kk_box_t _x1111 = kk_std_core_hnd__open_at0(_b_912_910, kk_test_float_bench1_new_f_fun1112(_ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1111);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1118 = kk_std_core_hnd_yield_extend(kk_test_float_bench1_new_f_fun1119(loc, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1118);
  }
  {
    return kk_test_float_bench1__mlift728_f(loc, x1_771, _ctx);
  }
}
static kk_box_t kk_test_float_bench1_f_fun1100(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1100__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1100__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_integer_t n = _self->n; /* int */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_integer_dup(n);}, {}, _ctx)
  kk_unit_t _x1101 = kk_Unit;
  kk_std_core_while(kk_test_float_bench1_new_f_fun1102(loc0, n, _ctx), kk_test_float_bench1_new_f_fun1110(loc, _ctx), _ctx);
  return kk_unit_box(_x1101);
}
static kk_box_t kk_test_float_bench1_f_fun1082(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_f_fun1082__t* _self = kk_function_as(struct kk_test_float_bench1_f_fun1082__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<575,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<575,int> */
  kk_integer_t n = _self->n; /* int */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_integer_dup(n);}, {}, _ctx)
  kk_integer_t _x1083;
  int32_t _b_935_916 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench1__hnd_count _b_936_917;
  kk_box_t _x1084;
  kk_function_t _x1085;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1085 = kk_test_float_bench1_new_f_fun1086(loc, loc0, _ctx); /*() -> 1002 1001*/
  _x1084 = kk_std_core_hnd__open_none0(_x1085, _ctx); /*1001*/
  _b_936_917 = kk_test_float_bench1__hnd_count_unbox(_x1084, _ctx); /*test/float/bench1/.hnd-count<<local<575>,test/float/bench1/bra,div>,int>*/
  kk_box_t _x1097;
  kk_function_t _x1098;
  kk_ref_dup(loc0);
  _x1098 = kk_test_float_bench1_new_f_fun1099(loc0, _ctx); /*(143) -> 144 1000*/
  _x1097 = kk_test_float_bench1__handle_count(_b_935_916, _b_936_917, _x1098, kk_test_float_bench1_new_f_fun1100(loc, loc0, n, _ctx), _ctx); /*145*/
  _x1083 = kk_integer_unbox(_x1097); /*int*/
  return kk_integer_box(_x1083);
}

kk_integer_t kk_test_float_bench1_f(kk_integer_t n, kk_context_t* _ctx) { /* (n : int) -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<575,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<575,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<575,int>*/;
  int32_t _b_926_921 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1070;
  kk_test_float_bench1__hnd_bra _x1071;
  kk_std_core_hnd__clause0 _x1072;
  kk_function_t _x1073;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1073 = kk_test_float_bench1_new_f_fun1074(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1011*/
  _x1072 = kk_std_core_hnd__new_Clause0(_x1073, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  _x1071 = kk_test_float_bench1__new_Hnd_bra(kk_reuse_null, _x1072, _ctx); /*test/float/bench1/.hnd-bra<6,7>*/
  kk_function_t _x1081;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1081 = kk_test_float_bench1_new_f_fun1082(loc, loc0, n, _ctx); /*() -> <test/float/bench1/bra|115> 114*/
  _x1070 = kk_test_float_bench1__handle_bra(_b_926_921, _x1071, kk_test_float_bench1_new_f_fun1080(_ctx), _x1081, _ctx); /*116*/
  res1 = kk_integer_unbox(_x1070); /*int*/
  kk_integer_t res0;
  kk_box_t _x1122 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*1002*/
  res0 = kk_integer_unbox(_x1122); /*int*/
  kk_integer_t res;
  kk_box_t _x1123 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*1002*/
  res = kk_integer_unbox(_x1123); /*int*/
  kk_box_t _x1124 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*1002*/
  return kk_integer_unbox(_x1124);
}
extern kk_box_t kk_test_float_bench1__mlift729_main_fun1126(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1__mlift729_main_fun1126__t* _self = kk_function_as(struct kk_test_float_bench1__mlift729_main_fun1126__t*, _fself);
  kk_integer_t n = _self->n; /* int */
  kk_drop_match(_self, {kk_integer_dup(n);}, {}, _ctx)
  kk_unit_t _x1127 = kk_Unit;
  kk_integer_t __ = kk_test_float_bench1_f(n, _ctx); /*int*/;
  kk_integer_drop(__, _ctx);
  kk_Unit;
  return kk_unit_box(_x1127);
}


// lift anonymous function
struct kk_test_float_bench1_main_fun1129__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1_main_fun1129(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_main_fun1129(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1_main_fun1129, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1_main_fun1129(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__list _x1130 = kk_std_os_env_get_args(_ctx); /*list<string>*/
  return kk_std_core__list_box(_x1130, _ctx);
}


// lift anonymous function
struct kk_test_float_bench1_main_fun1132__t {
  struct kk_function_s _base;
  kk_std_core__list args;
};
static kk_box_t kk_test_float_bench1_main_fun1132(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_main_fun1132(kk_std_core__list args, kk_context_t* _ctx) {
  struct kk_test_float_bench1_main_fun1132__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_main_fun1132__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_main_fun1132, kk_context());
  _self->args = args;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_main_fun1132(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_main_fun1132__t* _self = kk_function_as(struct kk_test_float_bench1_main_fun1132__t*, _fself);
  kk_std_core__list args = _self->args; /* list<string> */
  kk_drop_match(_self, {kk_std_core__list_dup(args);}, {}, _ctx)
  kk_std_core_types__maybe _x1133;
  kk_std_core_types__maybe _b_972_970 = kk_std_core_head_1(args, _ctx); /*maybe<string>*/;
  kk_string_t s_687;
  kk_box_t _x1134;
  kk_box_t _x1135;
  kk_string_t _x1136;
  kk_define_string_literal(, _s1137, 6, "100000")
  _x1136 = kk_string_dup(_s1137); /*string*/
  _x1135 = kk_string_box(_x1136); /*1001*/
  _x1134 = kk_std_core_maybe_1(_b_972_970, _x1135, _ctx); /*1001*/
  s_687 = kk_string_unbox(_x1134); /*string*/
  kk_string_t _x1138;
  kk_string_t _x1139 = kk_string_trim_left(s_687,kk_context()); /*string*/
  _x1138 = kk_string_trim_right(_x1139,kk_context()); /*string*/
  bool _x1140;
  kk_std_core_types__optional _match_992 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_992)) {
    kk_box_t _box_x974 = _match_992._cons.Optional.value;
    bool _hex_15080 = kk_bool_unbox(_box_x974);
    kk_std_core_types__optional_drop(_match_992, _ctx);
    _x1140 = _hex_15080; /*bool*/
    goto _match1141;
  }
  {
    _x1140 = false; /*bool*/
  }
  _match1141: ;
  _x1133 = kk_std_core_xparse_int(_x1138, _x1140, _ctx); /*maybe<int>*/
  return kk_std_core_types__maybe_box(_x1133, _ctx);
}


// lift anonymous function
struct kk_test_float_bench1_main_fun1145__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench1_main_fun1145(kk_function_t _fself, kk_box_t _b_980, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_main_fun1145(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1_main_fun1145, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench1_main_fun1145(kk_function_t _fself, kk_box_t _b_980, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1146 = kk_Unit;
  kk_integer_t _x1147 = kk_integer_unbox(_b_980); /*int*/
  kk_test_float_bench1__mlift729_main(_x1147, _ctx);
  return kk_unit_box(_x1146);
}


// lift anonymous function
struct kk_test_float_bench1_main_fun1148__t {
  struct kk_function_s _base;
  kk_integer_t x_775;
};
static kk_box_t kk_test_float_bench1_main_fun1148(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1_new_main_fun1148(kk_integer_t x_775, kk_context_t* _ctx) {
  struct kk_test_float_bench1_main_fun1148__t* _self = kk_function_alloc_as(struct kk_test_float_bench1_main_fun1148__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench1_main_fun1148, kk_context());
  _self->x_775 = x_775;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench1_main_fun1148(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench1_main_fun1148__t* _self = kk_function_as(struct kk_test_float_bench1_main_fun1148__t*, _fself);
  kk_integer_t x_775 = _self->x_775; /* int */
  kk_drop_match(_self, {kk_integer_dup(x_775);}, {}, _ctx)
  kk_unit_t _x1149 = kk_Unit;
  kk_integer_t __ = kk_test_float_bench1_f(x_775, _ctx); /*int*/;
  kk_integer_drop(__, _ctx);
  kk_Unit;
  return kk_unit_box(_x1149);
}

kk_unit_t kk_test_float_bench1_main(kk_context_t* _ctx) { /* () -> <pure,ndet> () */ 
  kk_std_core__list args;
  kk_box_t _x1128 = kk_std_core_hnd__open_none0(kk_test_float_bench1_new_main_fun1129(_ctx), _ctx); /*1001*/
  args = kk_std_core__list_unbox(_x1128, _ctx); /*list<string>*/
  kk_std_core_types__maybe _b_977_976;
  kk_box_t _x1131 = kk_std_core_hnd__open_none0(kk_test_float_bench1_new_main_fun1132(args, _ctx), _ctx); /*1001*/
  _b_977_976 = kk_std_core_types__maybe_unbox(_x1131, _ctx); /*maybe<int>*/
  kk_integer_t x_775;
  kk_box_t _x1143 = kk_std_core_unjust(_b_977_976, _ctx); /*1001*/
  x_775 = kk_integer_unbox(_x1143); /*int*/
  kk_box_t _x1144;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_775, _ctx);
    _x1144 = kk_std_core_hnd_yield_extend(kk_test_float_bench1_new_main_fun1145(_ctx), _ctx); /*1002*/
  }
  else {
    _x1144 = kk_std_core_hnd__open_none0(kk_test_float_bench1_new_main_fun1148(x_775, _ctx), _ctx); /*1002*/
  }
  kk_unit_unbox(_x1144); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench1__hmain_fun1150__t {
  struct kk_function_s _base;
};
static kk_unit_t kk_test_float_bench1__hmain_fun1150(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench1__new_hmain_fun1150(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench1__hmain_fun1150, _ctx)
  return kk_function_dup(_fself);
}

static kk_unit_t kk_test_float_bench1__hmain_fun1150(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_test_float_bench1_main(_ctx);
}

kk_unit_t kk_test_float_bench1__hmain(kk_context_t* _ctx) { /* () -> <console,div,ndet> () */ 
  kk_std_core__default_exn(kk_test_float_bench1__new_hmain_fun1150(_ctx), _ctx); return kk_Unit;
}

// main exit
static void _kk_main_exit(void) {
  kk_context_t* _ctx = kk_get_context();
  kk_test_float_bench1__done(_ctx);
}

// main entry
int main(int argc, char** argv) {
  kk_assert(sizeof(size_t)==8 && sizeof(void*)==8);
  kk_context_t* _ctx = kk_main_start(argc, argv);
  kk_test_float_bench1__init(_ctx);
  atexit(&_kk_main_exit);
  kk_test_float_bench1__hmain(_ctx);
  kk_test_float_bench1__done(_ctx);
  kk_main_end(_ctx);
  return 0;
}

// initialization
void kk_test_float_bench1__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  kk_std_core_types__init(_ctx);
  kk_std_core_hnd__init(_ctx);
  kk_std_core__init(_ctx);
  kk_std_text_parse__init(_ctx);
  kk_std_os_path__init(_ctx);
  kk_std_os_env__init(_ctx);
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
  {
    kk_string_t _x1008;
    kk_define_string_literal(, _s1009, 10, "bra.bench1")
    _x1008 = kk_string_dup(_s1009); /*string*/
    kk_test_float_bench1__tag_bra = kk_std_core_hnd__new_Htag(_x1008, _ctx); /*std/core/hnd/htag<test/float/bench1/.hnd-bra>*/
  }
  {
    kk_string_t _x1011;
    kk_define_string_literal(, _s1012, 12, "count.bench1")
    _x1011 = kk_string_dup(_s1012); /*string*/
    kk_test_float_bench1__tag_count = kk_std_core_hnd__new_Htag(_x1011, _ctx); /*std/core/hnd/htag<test/float/bench1/.hnd-count>*/
  }
}

// termination
void kk_test_float_bench1__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_core_hnd__htag_drop(kk_test_float_bench1__tag_count, _ctx);
  kk_std_core_hnd__htag_drop(kk_test_float_bench1__tag_bra, _ctx);
  kk_std_os_env__done(_ctx);
  kk_std_os_path__done(_ctx);
  kk_std_text_parse__done(_ctx);
  kk_std_core__done(_ctx);
  kk_std_core_hnd__done(_ctx);
  kk_std_core_types__done(_ctx);
}
