// Koka generated module: "test/float/bench2", koka version: 2.3.2, platform: 64-bit
#include "test_float_bench2.h"
 
// runtime tag for the `:bra` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_bra;
 
// handler for the `:bra` effect

kk_box_t kk_test_float_bench2__handle_bra(int32_t cfc, kk_test_float_bench2__hnd_bra hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-bra<e,b>, ret : (res : a) -> e b, action : () -> <bra|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1088 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_bra); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  return kk_std_core_hnd__hhandle(_x1088, cfc, kk_test_float_bench2__hnd_bra_box(hnd, _ctx), ret, action, _ctx);
}
 
// runtime tag for the `:count` effect

kk_std_core_hnd__htag kk_test_float_bench2__tag_count;
 
// handler for the `:count` effect

kk_box_t kk_test_float_bench2__handle_count(int32_t cfc, kk_test_float_bench2__hnd_count hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-count<e,b>, ret : (res : a) -> e b, action : () -> <count|e> a) -> e b */ 
  kk_std_core_hnd__htag _x1091 = kk_std_core_hnd__htag_dup(kk_test_float_bench2__tag_count); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
  return kk_std_core_hnd__hhandle(_x1091, cfc, kk_test_float_bench2__hnd_count_box(hnd, _ctx), ret, action, _ctx);
}

kk_unit_t kk_test_float_bench2_k(kk_context_t* _ctx) { /* () -> bra () */ 
  kk_std_core_hnd__ev ev_780;
  kk_ssize_t _x1099 = ((kk_ssize_t)0); /*ssize_t*/
  ev_780 = kk_evv_at(_x1099,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1100;
  {
    struct kk_std_core_hnd_Ev* _con1101 = kk_std_core_hnd__as_Ev(ev_780);
    kk_std_core_hnd__marker m0 = _con1101->marker;
    kk_box_t _box_x856 = _con1101->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x856, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1084 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x859 = _match_1084.clause;
      _x1100 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x859, (_fun_unbox_x859, m0, ev_780, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1100); return kk_Unit;
}


// lift anonymous function
struct kk_test_float_bench2_one___fun1108__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_one___fun1108(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_one___fun1108(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1108__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_one___fun1108__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_one___fun1108, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_one___fun1108(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_one___fun1108__t* _self = kk_function_as(struct kk_test_float_bench2_one___fun1108__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1109;
  bool b_16563;
  kk_integer_t _x1110 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1110,kk_context()); /*bool*/
  if (b_16563) {
    _x1109 = a; /*int*/
  }
  else {
    _x1109 = kk_integer_add(a,(kk_integer_from_small(1)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1109);
}

kk_integer_t kk_test_float_bench2_one__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_735;
  kk_box_t _x1107 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_one___fun1108(a, _ctx), _ctx); /*1001*/
  a0_735 = kk_integer_unbox(_x1107); /*int*/
  kk_std_core_hnd__ev ev_785;
  kk_ssize_t _x1111 = ((kk_ssize_t)0); /*ssize_t*/
  ev_785 = kk_evv_at(_x1111,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1112;
  {
    struct kk_std_core_hnd_Ev* _con1113 = kk_std_core_hnd__as_Ev(ev_785);
    kk_std_core_hnd__marker m0 = _con1113->marker;
    kk_box_t _box_x872 = _con1113->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x872, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1082 = kk_test_float_bench2__select_one(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,204,205>*/;
    {
      kk_function_t _fun_unbox_x876 = _match_1082.clause;
      _x1112 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x876, (_fun_unbox_x876, m0, ev_785, kk_integer_box(a0_735), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1112);
}


// lift anonymous function
struct kk_test_float_bench2_two___fun1120__t {
  struct kk_function_s _base;
  kk_integer_t a;
};
static kk_box_t kk_test_float_bench2_two___fun1120(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_two___fun1120(kk_integer_t a, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1120__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_two___fun1120__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_two___fun1120, kk_context());
  _self->a = a;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_two___fun1120(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_two___fun1120__t* _self = kk_function_as(struct kk_test_float_bench2_two___fun1120__t*, _fself);
  kk_integer_t a = _self->a; /* int */
  kk_drop_match(_self, {kk_integer_dup(a);}, {}, _ctx)
  kk_integer_t _x1121;
  bool b_16563;
  kk_integer_t _x1122 = kk_integer_dup(a); /*int*/
  b_16563 = kk_integer_is_odd(_x1122,kk_context()); /*bool*/
  if (b_16563) {
    _x1121 = a; /*int*/
  }
  else {
    _x1121 = kk_integer_add(a,(kk_integer_from_small(2)),kk_context()); /*int*/
  }
  return kk_integer_box(_x1121);
}

kk_integer_t kk_test_float_bench2_two__(kk_integer_t a, kk_context_t* _ctx) { /* (a : int) -> count int */ 
  kk_integer_t a0_736;
  kk_box_t _x1119 = kk_std_core_hnd__open_none0(kk_test_float_bench2_new_two___fun1120(a, _ctx), _ctx); /*1001*/
  a0_736 = kk_integer_unbox(_x1119); /*int*/
  kk_std_core_hnd__ev ev_791;
  kk_ssize_t _x1123 = ((kk_ssize_t)0); /*ssize_t*/
  ev_791 = kk_evv_at(_x1123,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-count>*/
  kk_box_t _x1124;
  {
    struct kk_std_core_hnd_Ev* _con1125 = kk_std_core_hnd__as_Ev(ev_791);
    kk_std_core_hnd__marker m0 = _con1125->marker;
    kk_box_t _box_x890 = _con1125->hnd;
    kk_test_float_bench2__hnd_count h = kk_test_float_bench2__hnd_count_unbox(_box_x890, NULL);
    kk_test_float_bench2__hnd_count_dup(h);
    kk_std_core_hnd__clause1 _match_1080 = kk_test_float_bench2__select_two(h, _ctx); /*std/core/hnd/clause1<int,int,test/float/bench2/.hnd-count,221,222>*/;
    {
      kk_function_t _fun_unbox_x894 = _match_1080.clause;
      _x1124 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x894, (_fun_unbox_x894, m0, ev_791, kk_integer_box(a0_736), _ctx)); /*1011*/
    }
  }
  return kk_integer_unbox(_x1124);
}
 
// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// }


// lift anonymous function
struct kk_test_float_bench2_f_fun1131__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
  kk_ref_t loc1;
};
static kk_box_t kk_test_float_bench2_f_fun1131(kk_function_t _fself, kk_std_core_hnd__marker _b_909, kk_std_core_hnd__ev _b_910, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1131(kk_ref_t loc, kk_ref_t loc0, kk_ref_t loc1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1131__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1131__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1131, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  _self->loc1 = loc1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1131(kk_function_t _fself, kk_std_core_hnd__marker _b_909, kk_std_core_hnd__ev _b_910, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1131__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1131__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_ref_t loc1 = _self->loc1; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);kk_ref_dup(loc1);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_910, ((int32_t)KI32(3)), _ctx);
  kk_unit_t _x1132 = kk_Unit;
  kk_integer_t _b_1022_907;
  kk_integer_t _x1133;
  kk_box_t _x1134 = (kk_ref_get(loc,kk_context())); /*1000*/
  _x1133 = kk_integer_unbox(_x1134); /*int*/
  kk_integer_t _x1135;
  kk_box_t _x1136 = (kk_ref_get(loc0,kk_context())); /*1000*/
  _x1135 = kk_integer_unbox(_x1136); /*int*/
  _b_1022_907 = kk_integer_add(_x1133,_x1135,kk_context()); /*int*/
  (kk_ref_set(loc1,(kk_integer_box(_b_1022_907)),kk_context()));
  return kk_unit_box(_x1132);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1137__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1137(kk_function_t _fself, kk_box_t _b_1015, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1137(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1137, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1137(kk_function_t _fself, kk_box_t _b_1015, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_1015;
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1139__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1139(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1139(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1139__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1139__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1139, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1143__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1143(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1143(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1143__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1143__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1143, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1147__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1147(kk_function_t _fself, kk_box_t _b_935, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1147(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1147__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1147__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1147, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1151__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_integer_t kk_test_float_bench2_f_fun1151(kk_function_t _fself, kk_unit_t __, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1151(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1151__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1151__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1151, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1154__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_integer_t kk_test_float_bench2_f_fun1154(kk_function_t _fself, kk_integer_t _y_749, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1154(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1154__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1154__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1154, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1156__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1156(kk_function_t _fself, kk_box_t _b_923, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1156(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1156__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1156__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1156, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1156(kk_function_t _fself, kk_box_t _b_923, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1156__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1156__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_box_drop(_b_923, _ctx);
  return (kk_ref_get(loc,kk_context()));
}
static kk_integer_t kk_test_float_bench2_f_fun1154(kk_function_t _fself, kk_integer_t _y_749, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1154__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1154__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _b_920_918 = kk_integer_add(_y_749,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x1_805 = kk_Unit;
  (kk_ref_set(loc0,(kk_integer_box(_b_920_918)),kk_context()));
  kk_box_t _x1155;
  if (kk_yielding(kk_context())) {
    _x1155 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1156(loc, _ctx), _ctx); /*1002*/
  }
  else {
    _x1155 = (kk_ref_get(loc,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1155);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1158__t {
  struct kk_function_s _base;
  kk_function_t next0_804;
};
static kk_box_t kk_test_float_bench2_f_fun1158(kk_function_t _fself, kk_box_t _b_930, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1158(kk_function_t next0_804, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1158__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1158__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1158, kk_context());
  _self->next0_804 = next0_804;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1158(kk_function_t _fself, kk_box_t _b_930, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1158__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1158__t*, _fself);
  kk_function_t next0_804 = _self->next0_804; /* (int) -> <local<722>,test/float/bench2/bra,div> int */
  kk_drop_match(_self, {kk_function_dup(next0_804);}, {}, _ctx)
  kk_integer_t _x1159;
  kk_integer_t _x1160 = kk_integer_unbox(_b_930); /*int*/
  _x1159 = kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_804, (next0_804, _x1160, _ctx)); /*int*/
  return kk_integer_box(_x1159);
}
static kk_integer_t kk_test_float_bench2_f_fun1151(kk_function_t _fself, kk_unit_t __, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1151__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1151__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t x0_803;
  kk_box_t _x1152;
  kk_ref_t _x1153 = kk_ref_dup(loc0); /*local-var<722,int>*/
  _x1152 = (kk_ref_get(_x1153,kk_context())); /*1000*/
  x0_803 = kk_integer_unbox(_x1152); /*int*/
  kk_function_t next0_804 = kk_test_float_bench2_new_f_fun1154(loc, loc0, _ctx); /*(int) -> <local<722>,test/float/bench2/bra,div> int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x0_803, _ctx);
    kk_box_t _x1157 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1158(next0_804, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1157);
  }
  {
    return kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_804, (next0_804, x0_803, _ctx));
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1162__t {
  struct kk_function_s _base;
  kk_function_t next_802;
};
static kk_box_t kk_test_float_bench2_f_fun1162(kk_function_t _fself, kk_box_t _b_933, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1162(kk_function_t next_802, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1162__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1162__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1162, kk_context());
  _self->next_802 = next_802;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1162(kk_function_t _fself, kk_box_t _b_933, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1162__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1162__t*, _fself);
  kk_function_t next_802 = _self->next_802; /* (()) -> <local<722>,test/float/bench2/bra,div> int */
  kk_drop_match(_self, {kk_function_dup(next_802);}, {}, _ctx)
  kk_integer_t _x1163;
  kk_unit_t _x1164 = kk_Unit;
  kk_unit_unbox(_b_933);
  _x1163 = kk_function_call(kk_integer_t, (kk_function_t, kk_unit_t, kk_context_t*), next_802, (next_802, _x1164, _ctx)); /*int*/
  return kk_integer_box(_x1163);
}
static kk_box_t kk_test_float_bench2_f_fun1147(kk_function_t _fself, kk_box_t _b_935, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1147__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1147__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1148;
  kk_integer_t _b_914_912;
  kk_integer_t _x1149 = kk_integer_unbox(_b_935); /*int*/
  _b_914_912 = kk_integer_add(_x1149,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t x_801 = kk_Unit;
  kk_ref_t _x1150 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1150,(kk_integer_box(_b_914_912)),kk_context()));
  kk_function_t next_802 = kk_test_float_bench2_new_f_fun1151(loc, loc0, _ctx); /*(()) -> <local<722>,test/float/bench2/bra,div> int*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x1161 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1162(next_802, _ctx), _ctx); /*1002*/
    _x1148 = kk_integer_unbox(_x1161); /*int*/
  }
  else {
    _x1148 = kk_function_call(kk_integer_t, (kk_function_t, kk_unit_t, kk_context_t*), next_802, (next_802, x_801, _ctx)); /*int*/
  }
  return kk_integer_box(_x1148);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1166__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1166(kk_function_t _fself, kk_box_t _b_960, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1166(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1166__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1166__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1166, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1170__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_integer_t kk_test_float_bench2_f_fun1170(kk_function_t _fself, kk_unit_t __1, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1170(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1170__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1170__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1170, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1173__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_ref_t loc0;
};
static kk_integer_t kk_test_float_bench2_f_fun1173(kk_function_t _fself, kk_integer_t _y_753, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1173(kk_ref_t loc, kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1173__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1173__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1173, kk_context());
  _self->loc = loc;
  _self->loc0 = loc0;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1175__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1175(kk_function_t _fself, kk_box_t _b_948, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1175(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1175__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1175__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1175, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1175(kk_function_t _fself, kk_box_t _b_948, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1175__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1175__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_box_drop(_b_948, _ctx);
  return (kk_ref_get(loc,kk_context()));
}
static kk_integer_t kk_test_float_bench2_f_fun1173(kk_function_t _fself, kk_integer_t _y_753, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1173__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1173__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _b_945_943 = kk_integer_add(_y_753,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_unit_t x4_811 = kk_Unit;
  (kk_ref_set(loc0,(kk_integer_box(_b_945_943)),kk_context()));
  kk_box_t _x1174;
  if (kk_yielding(kk_context())) {
    _x1174 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1175(loc, _ctx), _ctx); /*1002*/
  }
  else {
    _x1174 = (kk_ref_get(loc,kk_context())); /*1002*/
  }
  return kk_integer_unbox(_x1174);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1177__t {
  struct kk_function_s _base;
  kk_function_t next3_810;
};
static kk_box_t kk_test_float_bench2_f_fun1177(kk_function_t _fself, kk_box_t _b_955, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1177(kk_function_t next3_810, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1177__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1177__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1177, kk_context());
  _self->next3_810 = next3_810;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1177(kk_function_t _fself, kk_box_t _b_955, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1177__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1177__t*, _fself);
  kk_function_t next3_810 = _self->next3_810; /* (int) -> <local<722>,test/float/bench2/bra,div> int */
  kk_drop_match(_self, {kk_function_dup(next3_810);}, {}, _ctx)
  kk_integer_t _x1178;
  kk_integer_t _x1179 = kk_integer_unbox(_b_955); /*int*/
  _x1178 = kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next3_810, (next3_810, _x1179, _ctx)); /*int*/
  return kk_integer_box(_x1178);
}
static kk_integer_t kk_test_float_bench2_f_fun1170(kk_function_t _fself, kk_unit_t __1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1170__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1170__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t x3_809;
  kk_box_t _x1171;
  kk_ref_t _x1172 = kk_ref_dup(loc0); /*local-var<722,int>*/
  _x1171 = (kk_ref_get(_x1172,kk_context())); /*1000*/
  x3_809 = kk_integer_unbox(_x1171); /*int*/
  kk_function_t next3_810 = kk_test_float_bench2_new_f_fun1173(loc, loc0, _ctx); /*(int) -> <local<722>,test/float/bench2/bra,div> int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x3_809, _ctx);
    kk_box_t _x1176 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1177(next3_810, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x1176);
  }
  {
    return kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next3_810, (next3_810, x3_809, _ctx));
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1181__t {
  struct kk_function_s _base;
  kk_function_t next2_808;
};
static kk_box_t kk_test_float_bench2_f_fun1181(kk_function_t _fself, kk_box_t _b_958, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1181(kk_function_t next2_808, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1181__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1181__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1181, kk_context());
  _self->next2_808 = next2_808;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1181(kk_function_t _fself, kk_box_t _b_958, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1181__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1181__t*, _fself);
  kk_function_t next2_808 = _self->next2_808; /* (()) -> <local<722>,test/float/bench2/bra,div> int */
  kk_drop_match(_self, {kk_function_dup(next2_808);}, {}, _ctx)
  kk_integer_t _x1182;
  kk_unit_t _x1183 = kk_Unit;
  kk_unit_unbox(_b_958);
  _x1182 = kk_function_call(kk_integer_t, (kk_function_t, kk_unit_t, kk_context_t*), next2_808, (next2_808, _x1183, _ctx)); /*int*/
  return kk_integer_box(_x1182);
}
static kk_box_t kk_test_float_bench2_f_fun1166(kk_function_t _fself, kk_box_t _b_960, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1166__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1166__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1167;
  kk_integer_t _b_939_937;
  kk_integer_t _x1168 = kk_integer_unbox(_b_960); /*int*/
  _b_939_937 = kk_integer_add(_x1168,(kk_integer_from_small(2)),kk_context()); /*int*/
  kk_unit_t x2_807 = kk_Unit;
  kk_ref_t _x1169 = kk_ref_dup(loc); /*local-var<722,int>*/
  (kk_ref_set(_x1169,(kk_integer_box(_b_939_937)),kk_context()));
  kk_function_t next2_808 = kk_test_float_bench2_new_f_fun1170(loc, loc0, _ctx); /*(()) -> <local<722>,test/float/bench2/bra,div> int*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x1180 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1181(next2_808, _ctx), _ctx); /*1002*/
    _x1167 = kk_integer_unbox(_x1180); /*int*/
  }
  else {
    _x1167 = kk_function_call(kk_integer_t, (kk_function_t, kk_unit_t, kk_context_t*), next2_808, (next2_808, x2_807, _ctx)); /*int*/
  }
  return kk_integer_box(_x1167);
}
static kk_box_t kk_test_float_bench2_f_fun1143(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1143__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1143__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_test_float_bench2__hnd_count _x1144;
  kk_std_core_hnd__clause1 _x1145;
  kk_function_t _x1146;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1146 = kk_test_float_bench2_new_f_fun1147(loc, loc0, _ctx); /*(1003) -> 1001 1004*/
  _x1145 = kk_std_core_hnd_clause_tail1(_x1146, _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  kk_std_core_hnd__clause1 _x1165 = kk_std_core_hnd_clause_tail1(kk_test_float_bench2_new_f_fun1166(loc, loc0, _ctx), _ctx); /*std/core/hnd/clause1<1003,1004,1005,1001,1002>*/
  _x1144 = kk_test_float_bench2__new_Hnd_count(kk_reuse_null, _x1145, _x1165, _ctx); /*test/float/bench2/.hnd-count<20,21>*/
  return kk_test_float_bench2__hnd_count_box(_x1144, _ctx);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1185__t {
  struct kk_function_s _base;
  kk_ref_t loc0;
};
static kk_box_t kk_test_float_bench2_f_fun1185(kk_function_t _fself, kk_box_t _b_1010, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1185(kk_ref_t loc0, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1185__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1185__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1185, kk_context());
  _self->loc0 = loc0;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1185(kk_function_t _fself, kk_box_t _b_1010, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1185__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1185__t*, _fself);
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc0);}, {}, _ctx)
  kk_box_drop(_b_1010, _ctx);
  return (kk_ref_get(loc0,kk_context()));
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1186__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_test_float_bench2_f_fun1186(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1186(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1186__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1186__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1186, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1189__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static bool kk_test_float_bench2_f_fun1189(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1189(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1189__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1189__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1189, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1192__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1192(kk_function_t _fself, kk_box_t _b_966, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1192(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1192, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1192(kk_function_t _fself, kk_box_t _b_966, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x1193;
  kk_integer_t _x1194 = kk_integer_unbox(_b_966); /*int*/
  kk_integer_t _x1195 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
  _x1193 = kk_integer_lt(_x1194,_x1195,kk_context()); /*bool*/
  return kk_bool_box(_x1193);
}
static bool kk_test_float_bench2_f_fun1189(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1189__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1189__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x5_813;
  kk_box_t _x1190 = (kk_ref_get(loc,kk_context())); /*1000*/
  x5_813 = kk_integer_unbox(_x1190); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x5_813, _ctx);
    kk_box_t _x1191 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1192(_ctx), _ctx); /*1002*/
    return kk_bool_unbox(_x1191);
  }
  {
    kk_integer_t _x1196 = kk_integer_mul((kk_integer_from_int(10000000, _ctx)),(kk_integer_from_small(4)),kk_context()); /*int*/
    return kk_integer_lt(x5_813,_x1196,kk_context());
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1197__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1197(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1197(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1197__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1197__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1197, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1199__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1199(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1199(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1199, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1199(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x1200 = kk_Unit;
  kk_std_core_hnd__ev ev_817;
  kk_ssize_t _x1201 = ((kk_ssize_t)0); /*ssize_t*/
  ev_817 = kk_evv_at(_x1201,kk_context()); /*std/core/hnd/ev<test/float/bench2/.hnd-bra>*/
  kk_box_t _x1202;
  {
    struct kk_std_core_hnd_Ev* _con1203 = kk_std_core_hnd__as_Ev(ev_817);
    kk_std_core_hnd__marker m0 = _con1203->marker;
    kk_box_t _box_x967 = _con1203->hnd;
    kk_test_float_bench2__hnd_bra h = kk_test_float_bench2__hnd_bra_unbox(_box_x967, NULL);
    kk_test_float_bench2__hnd_bra_dup(h);
    kk_std_core_hnd__clause0 _match_1072 = kk_test_float_bench2__select_brara(h, _ctx); /*std/core/hnd/clause0<(),test/float/bench2/.hnd-bra,187,188>*/;
    {
      kk_function_t _fun_unbox_x970 = _match_1072.clause;
      _x1202 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x970, (_fun_unbox_x970, m0, ev_817, _ctx)); /*1006*/
    }
  }
  kk_unit_unbox(_x1202);
  return kk_unit_box(_x1200);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1205__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_unit_t kk_test_float_bench2_f_fun1205(kk_function_t _fself, kk_unit_t __3, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1205(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1205__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1205__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1205, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1207__t {
  struct kk_function_s _base;
};
static kk_unit_t kk_test_float_bench2_f_fun1207(kk_function_t _fself, kk_integer_t a00, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1207(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1207, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1209__t {
  struct kk_function_s _base;
  kk_integer_t a00;
};
static kk_box_t kk_test_float_bench2_f_fun1209(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1209(kk_integer_t a00, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1209__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1209__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1209, kk_context());
  _self->a00 = a00;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1209(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1209__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1209__t*, _fself);
  kk_integer_t a00 = _self->a00; /* int */
  kk_drop_match(_self, {kk_integer_dup(a00);}, {}, _ctx)
  kk_integer_t _x1210 = kk_test_float_bench2_one__(a00, _ctx); /*int*/
  return kk_integer_box(_x1210);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1211__t {
  struct kk_function_s _base;
};
static kk_unit_t kk_test_float_bench2_f_fun1211(kk_function_t _fself, kk_integer_t a1, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1211(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1211, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1213__t {
  struct kk_function_s _base;
  kk_integer_t a1;
};
static kk_box_t kk_test_float_bench2_f_fun1213(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1213(kk_integer_t a1, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1213__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1213__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1213, kk_context());
  _self->a1 = a1;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1213(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1213__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1213__t*, _fself);
  kk_integer_t a1 = _self->a1; /* int */
  kk_drop_match(_self, {kk_integer_dup(a1);}, {}, _ctx)
  kk_integer_t _x1214 = kk_test_float_bench2_two__(a1, _ctx); /*int*/
  return kk_integer_box(_x1214);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1215__t {
  struct kk_function_s _base;
};
static kk_unit_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_integer_t a2, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1215(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1215, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_test_float_bench2_f_fun1217__t {
  struct kk_function_s _base;
  kk_integer_t a2;
};
static kk_box_t kk_test_float_bench2_f_fun1217(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1217(kk_integer_t a2, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1217__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1217__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1217, kk_context());
  _self->a2 = a2;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1217(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1217__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1217__t*, _fself);
  kk_integer_t a2 = _self->a2; /* int */
  kk_drop_match(_self, {kk_integer_dup(a2);}, {}, _ctx)
  kk_integer_t _x1218 = kk_test_float_bench2_one__(a2, _ctx); /*int*/
  return kk_integer_box(_x1218);
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1220__t {
  struct kk_function_s _base;
};
static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_box_t _b_992, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1220(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_test_float_bench2_f_fun1220, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_test_float_bench2_f_fun1220(kk_function_t _fself, kk_box_t _b_992, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_992, _ctx);
  return kk_unit_box(kk_Unit);
}
static kk_unit_t kk_test_float_bench2_f_fun1215(kk_function_t _fself, kk_integer_t a2, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_ssize_t _b_989_987 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x10_825;
  kk_box_t _x1216 = kk_std_core_hnd__open_at0(_b_989_987, kk_test_float_bench2_new_f_fun1217(a2, _ctx), _ctx); /*1001*/
  x10_825 = kk_integer_unbox(_x1216); /*int*/
  kk_integer_drop(x10_825, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x1219 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1220(_ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1219);
  }
  {
    return kk_Unit;
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1222__t {
  struct kk_function_s _base;
  kk_function_t next9_824;
};
static kk_box_t kk_test_float_bench2_f_fun1222(kk_function_t _fself, kk_box_t _b_996, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1222(kk_function_t next9_824, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1222__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1222__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1222, kk_context());
  _self->next9_824 = next9_824;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1222(kk_function_t _fself, kk_box_t _b_996, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1222__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1222__t*, _fself);
  kk_function_t next9_824 = _self->next9_824; /* (int) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> () */
  kk_drop_match(_self, {kk_function_dup(next9_824);}, {}, _ctx)
  kk_unit_t _x1223 = kk_Unit;
  kk_integer_t _x1224 = kk_integer_unbox(_b_996); /*int*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next9_824, (next9_824, _x1224, _ctx));
  return kk_unit_box(_x1223);
}
static kk_unit_t kk_test_float_bench2_f_fun1211(kk_function_t _fself, kk_integer_t a1, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_ssize_t _b_985_983 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x9_823;
  kk_box_t _x1212 = kk_std_core_hnd__open_at0(_b_985_983, kk_test_float_bench2_new_f_fun1213(a1, _ctx), _ctx); /*1001*/
  x9_823 = kk_integer_unbox(_x1212); /*int*/
  kk_function_t next9_824 = kk_test_float_bench2_new_f_fun1215(_ctx); /*(int) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> ()*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x9_823, _ctx);
    kk_box_t _x1221 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1222(next9_824, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1221);
  }
  {
    return kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next9_824, (next9_824, x9_823, _ctx));
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1226__t {
  struct kk_function_s _base;
  kk_function_t next8_822;
};
static kk_box_t kk_test_float_bench2_f_fun1226(kk_function_t _fself, kk_box_t _b_999, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1226(kk_function_t next8_822, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1226__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1226__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1226, kk_context());
  _self->next8_822 = next8_822;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1226(kk_function_t _fself, kk_box_t _b_999, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1226__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1226__t*, _fself);
  kk_function_t next8_822 = _self->next8_822; /* (int) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> () */
  kk_drop_match(_self, {kk_function_dup(next8_822);}, {}, _ctx)
  kk_unit_t _x1227 = kk_Unit;
  kk_integer_t _x1228 = kk_integer_unbox(_b_999); /*int*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next8_822, (next8_822, _x1228, _ctx));
  return kk_unit_box(_x1227);
}
static kk_unit_t kk_test_float_bench2_f_fun1207(kk_function_t _fself, kk_integer_t a00, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_ssize_t _b_981_979 = ((kk_ssize_t)1); /*std/core/hnd/ev-index*/;
  kk_integer_t x8_821;
  kk_box_t _x1208 = kk_std_core_hnd__open_at0(_b_981_979, kk_test_float_bench2_new_f_fun1209(a00, _ctx), _ctx); /*1001*/
  x8_821 = kk_integer_unbox(_x1208); /*int*/
  kk_function_t next8_822 = kk_test_float_bench2_new_f_fun1211(_ctx); /*(int) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> ()*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x8_821, _ctx);
    kk_box_t _x1225 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1226(next8_822, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1225);
  }
  {
    return kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next8_822, (next8_822, x8_821, _ctx));
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1230__t {
  struct kk_function_s _base;
  kk_function_t next7_820;
};
static kk_box_t kk_test_float_bench2_f_fun1230(kk_function_t _fself, kk_box_t _b_1002, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1230(kk_function_t next7_820, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1230__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1230__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1230, kk_context());
  _self->next7_820 = next7_820;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1230(kk_function_t _fself, kk_box_t _b_1002, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1230__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1230__t*, _fself);
  kk_function_t next7_820 = _self->next7_820; /* (int) -> <local<722>,test/float/bench2/count,test/float/bench2/bra,div> () */
  kk_drop_match(_self, {kk_function_dup(next7_820);}, {}, _ctx)
  kk_unit_t _x1231 = kk_Unit;
  kk_integer_t _x1232 = kk_integer_unbox(_b_1002); /*int*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next7_820, (next7_820, _x1232, _ctx));
  return kk_unit_box(_x1231);
}
static kk_unit_t kk_test_float_bench2_f_fun1205(kk_function_t _fself, kk_unit_t __3, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1205__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1205__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x7_819;
  kk_box_t _x1206 = (kk_ref_get(loc,kk_context())); /*1000*/
  x7_819 = kk_integer_unbox(_x1206); /*int*/
  kk_function_t next7_820 = kk_test_float_bench2_new_f_fun1207(_ctx); /*(int) -> <local<722>,test/float/bench2/count,test/float/bench2/bra,div> ()*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x7_819, _ctx);
    kk_box_t _x1229 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1230(next7_820, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1229);
  }
  {
    return kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next7_820, (next7_820, x7_819, _ctx));
  }
}


// lift anonymous function
struct kk_test_float_bench2_f_fun1234__t {
  struct kk_function_s _base;
  kk_function_t next6_816;
};
static kk_box_t kk_test_float_bench2_f_fun1234(kk_function_t _fself, kk_box_t _b_1005, kk_context_t* _ctx);
static kk_function_t kk_test_float_bench2_new_f_fun1234(kk_function_t next6_816, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1234__t* _self = kk_function_alloc_as(struct kk_test_float_bench2_f_fun1234__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_test_float_bench2_f_fun1234, kk_context());
  _self->next6_816 = next6_816;
  return &_self->_base;
}

static kk_box_t kk_test_float_bench2_f_fun1234(kk_function_t _fself, kk_box_t _b_1005, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1234__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1234__t*, _fself);
  kk_function_t next6_816 = _self->next6_816; /* (()) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> () */
  kk_drop_match(_self, {kk_function_dup(next6_816);}, {}, _ctx)
  kk_unit_t _x1235 = kk_Unit;
  kk_unit_t _x1236 = kk_Unit;
  kk_unit_unbox(_b_1005);
  kk_function_call(kk_unit_t, (kk_function_t, kk_unit_t, kk_context_t*), next6_816, (next6_816, _x1236, _ctx));
  return kk_unit_box(_x1235);
}
static kk_unit_t kk_test_float_bench2_f_fun1197(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1197__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1197__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_ssize_t _b_975_973 = ((kk_ssize_t)0); /*std/core/hnd/ev-index*/;
  kk_unit_t x6_815 = kk_Unit;
  kk_box_t _x1198 = kk_std_core_hnd__open_at0(_b_975_973, kk_test_float_bench2_new_f_fun1199(_ctx), _ctx); /*1001*/
  kk_unit_unbox(_x1198);
  kk_function_t next6_816 = kk_test_float_bench2_new_f_fun1205(loc, _ctx); /*(()) -> <test/float/bench2/bra,test/float/bench2/count,div,local<722>> ()*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x1233 = kk_std_core_hnd_yield_extend(kk_test_float_bench2_new_f_fun1234(next6_816, _ctx), _ctx); /*1002*/
    return kk_unit_unbox(_x1233);
  }
  {
    return kk_function_call(kk_unit_t, (kk_function_t, kk_unit_t, kk_context_t*), next6_816, (next6_816, x6_815, _ctx));
  }
}
static kk_box_t kk_test_float_bench2_f_fun1186(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1186__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1186__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_unit_t _x1187 = kk_Unit;
  kk_function_t _x1188;
  kk_ref_dup(loc);
  _x1188 = kk_test_float_bench2_new_f_fun1189(loc, _ctx); /*() -> <div,local<722>,test/float/bench2/bra,test/float/bench2/count> bool*/
  kk_std_core_while(_x1188, kk_test_float_bench2_new_f_fun1197(loc, _ctx), _ctx);
  return kk_unit_box(_x1187);
}
static kk_box_t kk_test_float_bench2_f_fun1139(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_test_float_bench2_f_fun1139__t* _self = kk_function_as(struct kk_test_float_bench2_f_fun1139__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<722,int> */
  kk_ref_t loc0 = _self->loc0; /* local-var<722,int> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_ref_dup(loc0);}, {}, _ctx)
  kk_integer_t _x1140;
  int32_t _b_1025_1006 = ((int32_t)KI32(1)); /*int32*/;
  kk_test_float_bench2__hnd_count _b_1026_1007;
  kk_box_t _x1141;
  kk_function_t _x1142;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1142 = kk_test_float_bench2_new_f_fun1143(loc, loc0, _ctx); /*() -> 1002 1001*/
  _x1141 = kk_std_core_hnd__open_none0(_x1142, _ctx); /*1001*/
  _b_1026_1007 = kk_test_float_bench2__hnd_count_unbox(_x1141, _ctx); /*test/float/bench2/.hnd-count<<local<722>,test/float/bench2/bra,div>,int>*/
  kk_box_t _x1184 = kk_test_float_bench2__handle_count(_b_1025_1006, _b_1026_1007, kk_test_float_bench2_new_f_fun1185(loc0, _ctx), kk_test_float_bench2_new_f_fun1186(loc, _ctx), _ctx); /*171*/
  _x1140 = kk_integer_unbox(_x1184); /*int*/
  return kk_integer_box(_x1140);
}

kk_integer_t kk_test_float_bench2_f(kk_context_t* _ctx) { /* () -> div int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc0 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  kk_ref_t loc1 = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<722,int>*/;
  int32_t _b_1016_1011 = ((int32_t)KI32(1)); /*int32*/;
  kk_integer_t res1;
  kk_box_t _x1127;
  kk_test_float_bench2__hnd_bra _x1128;
  kk_std_core_hnd__clause0 _x1129;
  kk_function_t _x1130;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  kk_ref_dup(loc1);
  _x1130 = kk_test_float_bench2_new_f_fun1131(loc, loc0, loc1, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1011*/
  _x1129 = kk_std_core_hnd__new_Clause0(_x1130, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  _x1128 = kk_test_float_bench2__new_Hnd_bra(kk_reuse_null, _x1129, _ctx); /*test/float/bench2/.hnd-bra<6,7>*/
  kk_function_t _x1138;
  kk_ref_dup(loc);
  kk_ref_dup(loc0);
  _x1138 = kk_test_float_bench2_new_f_fun1139(loc, loc0, _ctx); /*() -> <test/float/bench2/bra|141> 140*/
  _x1127 = kk_test_float_bench2__handle_bra(_b_1016_1011, _x1128, kk_test_float_bench2_new_f_fun1137(_ctx), _x1138, _ctx); /*142*/
  res1 = kk_integer_unbox(_x1127); /*int*/
  kk_integer_t res0;
  kk_box_t _x1237 = kk_std_core_hnd_prompt_local_var(loc1, kk_integer_box(res1), _ctx); /*1002*/
  res0 = kk_integer_unbox(_x1237); /*int*/
  kk_integer_t res;
  kk_box_t _x1238 = kk_std_core_hnd_prompt_local_var(loc0, kk_integer_box(res0), _ctx); /*1002*/
  res = kk_integer_unbox(_x1238); /*int*/
  kk_box_t _x1239 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*1002*/
  return kk_integer_unbox(_x1239);
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
    kk_string_t _x1086;
    kk_define_string_literal(, _s1087, 10, "bra.bench2")
    _x1086 = kk_string_dup(_s1087); /*string*/
    kk_test_float_bench2__tag_bra = kk_std_core_hnd__new_Htag(_x1086, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-bra>*/
  }
  {
    kk_string_t _x1089;
    kk_define_string_literal(, _s1090, 12, "count.bench2")
    _x1089 = kk_string_dup(_s1090); /*string*/
    kk_test_float_bench2__tag_count = kk_std_core_hnd__new_Htag(_x1089, _ctx); /*std/core/hnd/htag<test/float/bench2/.hnd-count>*/
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
