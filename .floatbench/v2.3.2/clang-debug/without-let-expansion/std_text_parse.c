// Koka generated module: "std/text/parse", koka version: 2.3.2, platform: 64-bit
#include "std_text_parse.h"
 
// runtime tag for the `:parse` effect

kk_std_core_hnd__htag kk_std_text_parse__tag_parse;
 
// handler for the `:parse` effect

kk_box_t kk_std_text_parse__handle_parse(int32_t cfc, kk_std_text_parse__hnd_parse hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-parse<e,b>, ret : (res : a) -> e b, action : () -> <parse|e> a) -> e b */ 
  kk_std_core_hnd__htag _x3397 = kk_std_core_hnd__htag_dup(kk_std_text_parse__tag_parse); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
  return kk_std_core_hnd__hhandle(_x3397, cfc, kk_std_text_parse__hnd_parse_box(hnd, _ctx), ret, action, _ctx);
}

kk_std_core_types__either kk_std_text_parse_either(kk_std_text_parse__parse_error perr, kk_context_t* _ctx) { /* forall<a> (perr : parse-error<a>) -> either<string,a> */ 
  if (kk_std_text_parse__is_ParseOk(perr)) {
    struct kk_std_text_parse_ParseOk* _con3402 = kk_std_text_parse__as_ParseOk(perr);
    kk_box_t x = _con3402->result;
    kk_std_core__sslice _pat0 = _con3402->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(perr))) {
      kk_std_core__sslice_drop(_pat0, _ctx);
      kk_std_text_parse__parse_error_free(perr);
    }
    else {
      kk_box_dup(x);
      kk_std_text_parse__parse_error_decref(perr, _ctx);
    }
    return kk_std_core_types__new_Right(x, _ctx);
  }
  {
    struct kk_std_text_parse_ParseError* _con3403 = kk_std_text_parse__as_ParseError(perr);
    kk_string_t msg = _con3403->msg;
    kk_std_core__sslice _pat5 = _con3403->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(perr))) {
      kk_std_core__sslice_drop(_pat5, _ctx);
      kk_std_text_parse__parse_error_free(perr);
    }
    else {
      kk_string_dup(msg);
      kk_std_text_parse__parse_error_decref(perr, _ctx);
    }
    return kk_std_core_types__new_Left(kk_string_box(msg), _ctx);
  }
}
extern kk_box_t kk_std_text_parse_satisfy_fun3411(kk_function_t _fself, kk_box_t _b_2637, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fun3411__t* _self = kk_function_as(struct kk_std_text_parse_satisfy_fun3411__t*, _fself);
  kk_function_t pred = _self->pred; /* (sslice) -> total maybe<(521, sslice)> */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  kk_std_core_types__maybe _x3412;
  kk_std_core__sslice _x3413 = kk_std_core__sslice_unbox(_b_2637, _ctx); /*sslice*/
  _x3412 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_std_core__sslice, kk_context_t*), pred, (pred, _x3413, _ctx)); /*maybe<(521, sslice)>*/
  return kk_std_core_types__maybe_box(_x3412, _ctx);
}
 
// monadic lift

kk_box_t kk_std_text_parse__mlift2415_satisfy_fail(kk_string_t msg, kk_std_core_types__maybe _y_2328, kk_context_t* _ctx) { /* forall<a> (msg : string, maybe<a>) -> parse a */ 
  if (kk_std_core_types__is_Nothing(_y_2328)) {
    kk_std_core_hnd__ev ev_2445;
    kk_ssize_t _x3414 = ((kk_ssize_t)0); /*ssize_t*/
    ev_2445 = kk_evv_at(_x3414,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
    {
      struct kk_std_core_hnd_Ev* _con3415 = kk_std_core_hnd__as_Ev(ev_2445);
      kk_std_core_hnd__marker m0 = _con3415->marker;
      kk_box_t _box_x2638 = _con3415->hnd;
      kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2638, NULL);
      kk_std_text_parse__hnd_parse_dup(h);
      kk_std_core_hnd__clause1 _match_3388 = kk_std_text_parse__select_fail(h, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
      {
        kk_function_t _fun_unbox_x2642 = _match_3388.clause;
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2642, (_fun_unbox_x2642, m0, ev_2445, kk_string_box(msg), _ctx));
      }
    }
  }
  {
    kk_box_t x0 = _y_2328._cons.Just.value;
    kk_string_drop(msg, _ctx);
    return x0;
  }
}


// lift anonymous function
struct kk_std_text_parse_satisfy_fail_fun3421__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_box_t kk_std_text_parse_satisfy_fail_fun3421(kk_function_t _fself, kk_box_t _b_2659, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_satisfy_fail_fun3421(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fail_fun3421__t* _self = kk_function_alloc_as(struct kk_std_text_parse_satisfy_fail_fun3421__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_satisfy_fail_fun3421, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_satisfy_fail_fun3421(kk_function_t _fself, kk_box_t _b_2659, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fail_fun3421__t* _self = kk_function_as(struct kk_std_text_parse_satisfy_fail_fun3421__t*, _fself);
  kk_function_t pred = _self->pred; /* (sslice) -> maybe<(547, sslice)> */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  kk_std_core_types__maybe _x3422;
  kk_std_core__sslice _x3423 = kk_std_core__sslice_unbox(_b_2659, _ctx); /*sslice*/
  _x3422 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_std_core__sslice, kk_context_t*), pred, (pred, _x3423, _ctx)); /*maybe<(547, sslice)>*/
  return kk_std_core_types__maybe_box(_x3422, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_satisfy_fail_fun3424__t {
  struct kk_function_s _base;
  kk_string_t msg;
};
static kk_box_t kk_std_text_parse_satisfy_fail_fun3424(kk_function_t _fself, kk_box_t _b_2661, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_satisfy_fail_fun3424(kk_string_t msg, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fail_fun3424__t* _self = kk_function_alloc_as(struct kk_std_text_parse_satisfy_fail_fun3424__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_satisfy_fail_fun3424, kk_context());
  _self->msg = msg;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_satisfy_fail_fun3424(kk_function_t _fself, kk_box_t _b_2661, kk_context_t* _ctx) {
  struct kk_std_text_parse_satisfy_fail_fun3424__t* _self = kk_function_as(struct kk_std_text_parse_satisfy_fail_fun3424__t*, _fself);
  kk_string_t msg = _self->msg; /* string */
  kk_drop_match(_self, {kk_string_dup(msg);}, {}, _ctx)
  kk_std_core_types__maybe _x3425 = kk_std_core_types__maybe_unbox(_b_2661, _ctx); /*maybe<547>*/
  return kk_std_text_parse__mlift2415_satisfy_fail(msg, _x3425, _ctx);
}

kk_box_t kk_std_text_parse_satisfy_fail(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx) { /* forall<a> (msg : string, pred : (sslice) -> maybe<(a, sslice)>) -> parse a */ 
  kk_std_core_hnd__ev ev_2451;
  kk_ssize_t _x3417 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2451 = kk_evv_at(_x3417,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_std_core_types__maybe x_2448;
  kk_box_t _x3418;
  {
    struct kk_std_core_hnd_Ev* _con3419 = kk_std_core_hnd__as_Ev(ev_2451);
    kk_std_core_hnd__marker m0 = _con3419->marker;
    kk_box_t _box_x2646 = _con3419->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2646, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause1 _match_3387 = kk_std_text_parse__select_satisfy(h, _ctx); /*std/core/hnd/clause1<(sslice) -> total maybe<(438, sslice)>,maybe<438>,std/text/parse/.hnd-parse,439,440>*/;
    {
      kk_function_t _fun_unbox_x2653 = _match_3387.clause;
      _x3418 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2653, (_fun_unbox_x2653, m0, ev_2451, kk_function_box(kk_std_text_parse_new_satisfy_fail_fun3421(pred, _ctx)), _ctx)); /*1011*/
    }
  }
  x_2448 = kk_std_core_types__maybe_unbox(_x3418, _ctx); /*maybe<547>*/
  if (kk_yielding(kk_context())) {
    kk_std_core_types__maybe_drop(x_2448, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_text_parse_new_satisfy_fail_fun3424(msg, _ctx), _ctx);
  }
  if (kk_std_core_types__is_Nothing(x_2448)) {
    kk_std_core_hnd__ev ev0_2454;
    kk_ssize_t _x3426 = ((kk_ssize_t)0); /*ssize_t*/
    ev0_2454 = kk_evv_at(_x3426,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
    {
      struct kk_std_core_hnd_Ev* _con3427 = kk_std_core_hnd__as_Ev(ev0_2454);
      kk_std_core_hnd__marker m00 = _con3427->marker;
      kk_box_t _box_x2662 = _con3427->hnd;
      kk_std_text_parse__hnd_parse h0 = kk_std_text_parse__hnd_parse_unbox(_box_x2662, NULL);
      kk_std_text_parse__hnd_parse_dup(h0);
      kk_std_core_hnd__clause1 _match_3386 = kk_std_text_parse__select_fail(h0, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
      {
        kk_function_t _fun_unbox_x2666 = _match_3386.clause;
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2666, (_fun_unbox_x2666, m00, ev0_2454, kk_string_box(msg), _ctx));
      }
    }
  }
  {
    kk_box_t x2 = x_2448._cons.Just.value;
    kk_string_drop(msg, _ctx);
    return x2;
  }
}


// lift anonymous function
struct kk_std_text_parse_char_is_fun3430__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_text_parse_char_is_fun3430(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_char_is_fun3430(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_text_parse_char_is_fun3430__t* _self = kk_function_alloc_as(struct kk_std_text_parse_char_is_fun3430__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_char_is_fun3430, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_char_is_fun3430(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_char_is_fun3430__t* _self = kk_function_as(struct kk_std_text_parse_char_is_fun3430__t*, _fself);
  kk_function_t pred = _self->pred; /* (char) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  kk_std_core_types__maybe _match_3384 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3384)) {
    kk_box_t _box_x2672 = _match_3384._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2672, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2673 = _pat0.fst;
      kk_box_t _box_x2674 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2673, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2674, NULL);
      kk_function_t _x3434 = kk_function_dup(pred); /*(char) -> bool*/
      if (kk_function_call(bool, (kk_function_t, kk_char_t, kk_context_t*), _x3434, (_x3434, c, _ctx))) {
        kk_function_drop(pred, _ctx);
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3384, _ctx);
        kk_box_t _x3435;
        kk_std_core_types__tuple2_ _x3436 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3435 = kk_std_core_types__tuple2__box(_x3436, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3435, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3384, _ctx);
    kk_function_drop(pred, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_char_is(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx) { /* (msg : string, pred : (char) -> bool) -> parse char */ 
  kk_box_t _x3429 = kk_std_text_parse_satisfy_fail(msg, kk_std_text_parse_new_char_is_fun3430(pred, _ctx), _ctx); /*547*/
  return kk_char_unbox(_x3429, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_alpha_fun3440__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_alpha_fun3440(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_alpha_fun3440(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_alpha_fun3440, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_alpha_fun3440(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3383 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3383)) {
    kk_box_t _box_x2685 = _match_3383._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2685, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2686 = _pat0.fst;
      kk_box_t _box_x2687 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2686, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2687, NULL);
      if (kk_std_core_is_alpha(c, _ctx)) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3383, _ctx);
        kk_box_t _x3444;
        kk_std_core_types__tuple2_ _x3445 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3444 = kk_std_core_types__tuple2__box(_x3445, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3444, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3383, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_alpha(kk_context_t* _ctx) { /* () -> parse char */ 
  kk_box_t _x3437;
  kk_string_t _x3438;
  kk_define_string_literal(, _s3439, 5, "alpha")
  _x3438 = kk_string_dup(_s3439); /*string*/
  _x3437 = kk_std_text_parse_satisfy_fail(_x3438, kk_std_text_parse_new_alpha_fun3440(_ctx), _ctx); /*547*/
  return kk_char_unbox(_x3437, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_alpha_num_fun3449__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_alpha_num_fun3449(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_alpha_num_fun3449(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_alpha_num_fun3449, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_alpha_num_fun3449(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3382 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3382)) {
    kk_box_t _box_x2698 = _match_3382._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2698, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2699 = _pat0.fst;
      kk_box_t _box_x2700 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2699, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2700, NULL);
      if (kk_std_core_is_alpha_num(c, _ctx)) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3382, _ctx);
        kk_box_t _x3453;
        kk_std_core_types__tuple2_ _x3454 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3453 = kk_std_core_types__tuple2__box(_x3454, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3453, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3382, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_alpha_num(kk_context_t* _ctx) { /* () -> parse char */ 
  kk_box_t _x3446;
  kk_string_t _x3447;
  kk_define_string_literal(, _s3448, 9, "alpha-num")
  _x3447 = kk_string_dup(_s3448); /*string*/
  _x3446 = kk_std_text_parse_satisfy_fail(_x3447, kk_std_text_parse_new_alpha_num_fun3449(_ctx), _ctx); /*547*/
  return kk_char_unbox(_x3446, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_char_fun3462__t {
  struct kk_function_s _base;
  kk_char_t c;
};
static kk_std_core_types__maybe kk_std_text_parse_char_fun3462(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_char_fun3462(kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_text_parse_char_fun3462__t* _self = kk_function_alloc_as(struct kk_std_text_parse_char_fun3462__t, 1, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_char_fun3462, kk_context());
  _self->c = c;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_char_fun3462(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_char_fun3462__t* _self = kk_function_as(struct kk_std_text_parse_char_fun3462__t*, _fself);
  kk_char_t c = _self->c; /* char */
  kk_drop_match(_self, {;}, {}, _ctx)
  kk_std_core_types__maybe _match_3381 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3381)) {
    kk_box_t _box_x2711 = _match_3381._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2711, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2712 = _pat0.fst;
      kk_box_t _box_x2713 = _pat0.snd;
      kk_char_t c0 = kk_char_unbox(_box_x2712, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2713, NULL);
      if (c == c0) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3381, _ctx);
        kk_box_t _x3466;
        kk_std_core_types__tuple2_ _x3467 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c0, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3466 = kk_std_core_types__tuple2__box(_x3467, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3466, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3381, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_char(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> parse char */ 
  kk_string_t msg_2262;
  kk_string_t _x3455;
  kk_define_string_literal(, _s3456, 1, "\'")
  _x3455 = kk_string_dup(_s3456); /*string*/
  kk_string_t _x3457;
  kk_string_t _x3458 = kk_std_core_show_char(c, _ctx); /*string*/
  kk_string_t _x3459;
  kk_define_string_literal(, _s3460, 1, "\'")
  _x3459 = kk_string_dup(_s3460); /*string*/
  _x3457 = kk_std_core__lp__plus__plus__1_rp_(_x3458, _x3459, _ctx); /*string*/
  msg_2262 = kk_std_core__lp__plus__plus__1_rp_(_x3455, _x3457, _ctx); /*string*/
  kk_box_t _x3461 = kk_std_text_parse_satisfy_fail(msg_2262, kk_std_text_parse_new_char_fun3462(c, _ctx), _ctx); /*547*/
  return kk_char_unbox(_x3461, _ctx);
}

kk_std_core_types__tuple2_ kk_std_text_parse_next_while0(kk_std_core__sslice slice, kk_function_t pred, kk_std_core__list acc, kk_context_t* _ctx) { /* (slice : sslice, pred : (char) -> bool, acc : list<char>) -> (list<char>, sslice) */ 
  kk__tailcall: ;
  kk_std_core_types__maybe _match_3380;
  kk_std_core__sslice _x3468 = kk_std_core__sslice_dup(slice); /*sslice*/
  _match_3380 = kk_std_core_next(_x3468, _ctx); /*maybe<(char, sslice)>*/
  if (kk_std_core_types__is_Just(_match_3380)) {
    kk_box_t _box_x2724 = _match_3380._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2724, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2725 = _pat0.fst;
      kk_box_t _box_x2726 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2725, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2726, NULL);
      kk_function_t _x3472 = kk_function_dup(pred); /*(char) -> bool*/
      if (kk_function_call(bool, (kk_function_t, kk_char_t, kk_context_t*), _x3472, (_x3472, c, _ctx))) {
        kk_std_core__sslice_drop(slice, _ctx);
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3380, _ctx);
        { // tailcall
          kk_std_core__list _x3473 = kk_std_core__new_Cons(kk_reuse_null, kk_char_box(c, _ctx), acc, _ctx); /*list<1009>*/
          slice = rest0;
          acc = _x3473;
          goto kk__tailcall;
        }
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3380, _ctx);
    kk_function_drop(pred, _ctx);
    kk_std_core__list _b_2733_2729 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc, _ctx); /*list<char>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_2733_2729, _ctx), kk_std_core__sslice_box(slice, _ctx), _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_chars_are_fun3475__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_text_parse_chars_are_fun3475(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_chars_are_fun3475(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_text_parse_chars_are_fun3475__t* _self = kk_function_alloc_as(struct kk_std_text_parse_chars_are_fun3475__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_chars_are_fun3475, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_chars_are_fun3475(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_chars_are_fun3475__t* _self = kk_function_as(struct kk_std_text_parse_chars_are_fun3475__t*, _fself);
  kk_function_t pred = _self->pred; /* (char) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  kk_std_core_types__tuple2_ _match_3379 = kk_std_text_parse_next_while0(slice, pred, kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x2735 = _match_3379.fst;
    kk_box_t _box_x2736 = _match_3379.snd;
    kk_std_core__list _pat0 = kk_std_core__list_unbox(_box_x2735, NULL);
    kk_std_core__sslice _pat1 = kk_std_core__sslice_unbox(_box_x2736, NULL);
    if (kk_std_core__is_Nil(_pat0)) {
      kk_std_core_types__tuple2__drop(_match_3379, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x2737 = _match_3379.fst;
    kk_box_t _box_x2738 = _match_3379.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x2737, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2738, NULL);
    kk_box_t _x3480;
    kk_std_core_types__tuple2_ _x3481 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3480 = kk_std_core_types__tuple2__box(_x3481, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3480, _ctx);
  }
}

kk_std_core__list kk_std_text_parse_chars_are(kk_string_t msg, kk_function_t pred, kk_context_t* _ctx) { /* (msg : string, pred : (char) -> bool) -> parse list<char> */ 
  kk_box_t _x3474 = kk_std_text_parse_satisfy_fail(msg, kk_std_text_parse_new_chars_are_fun3475(pred, _ctx), _ctx); /*547*/
  return kk_std_core__list_unbox(_x3474, _ctx);
}
 
// monadic lift

kk_box_t kk_std_text_parse__mlift2416_choose(kk_function_t p0, kk_std_core__list pp, bool _y_2341, kk_context_t* _ctx) { /* forall<a,e> (p0 : parser<e,a>, pp : list<parser<e,a>>, bool) -> <parse|e> a */ 
  if (_y_2341) {
    kk_std_core__list_drop(pp, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p0, (p0, _ctx));
  }
  {
    kk_function_drop(p0, _ctx);
    return kk_std_text_parse_choose(pp, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_choose_fun3488__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_choose_fun3488(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_choose_fun3488(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_choose_fun3488, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_choose_fun3488(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_hnd__ev ev_2460;
  kk_ssize_t _x3489 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2460 = kk_evv_at(_x3489,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  {
    struct kk_std_core_hnd_Ev* _con3490 = kk_std_core_hnd__as_Ev(ev_2460);
    kk_std_core_hnd__marker m0 = _con3490->marker;
    kk_box_t _box_x2755 = _con3490->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2755, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause1 _match_3377 = kk_std_text_parse__select_fail(h, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
    {
      kk_function_t _fun_unbox_x2759 = _match_3377.clause;
      kk_box_t _x3492;
      kk_string_t _x3493;
      kk_define_string_literal(, _s3494, 23, "no further alternatives")
      _x3493 = kk_string_dup(_s3494); /*string*/
      _x3492 = kk_string_box(_x3493); /*1010*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2759, (_fun_unbox_x2759, m0, ev_2460, _x3492, _ctx));
    }
  }
}


// lift anonymous function
struct kk_std_text_parse_choose_fun3500__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_choose_fun3500(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_choose_fun3500(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_choose_fun3500, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_choose_fun3500(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x3501;
  kk_std_core_hnd__ev ev0_2466;
  kk_ssize_t _x3502 = ((kk_ssize_t)0); /*ssize_t*/
  ev0_2466 = kk_evv_at(_x3502,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_box_t _x3503;
  {
    struct kk_std_core_hnd_Ev* _con3504 = kk_std_core_hnd__as_Ev(ev0_2466);
    kk_std_core_hnd__marker m00 = _con3504->marker;
    kk_box_t _box_x2769 = _con3504->hnd;
    kk_std_text_parse__hnd_parse h0 = kk_std_text_parse__hnd_parse_unbox(_box_x2769, NULL);
    kk_std_text_parse__hnd_parse_dup(h0);
    kk_std_core_hnd__clause0 _match_3376 = kk_std_text_parse__select_pick(h0, _ctx); /*std/core/hnd/clause0<bool,std/text/parse/.hnd-parse,417,418>*/;
    {
      kk_function_t _fun_unbox_x2772 = _match_3376.clause;
      _x3503 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x2772, (_fun_unbox_x2772, m00, ev0_2466, _ctx)); /*1006*/
    }
  }
  _x3501 = kk_bool_unbox(_x3503); /*bool*/
  return kk_bool_box(_x3501);
}


// lift anonymous function
struct kk_std_text_parse_choose_fun3506__t {
  struct kk_function_s _base;
  kk_function_t p00;
  kk_std_core__list pp0;
};
static kk_box_t kk_std_text_parse_choose_fun3506(kk_function_t _fself, kk_box_t _b_2780, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_choose_fun3506(kk_function_t p00, kk_std_core__list pp0, kk_context_t* _ctx) {
  struct kk_std_text_parse_choose_fun3506__t* _self = kk_function_alloc_as(struct kk_std_text_parse_choose_fun3506__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_choose_fun3506, kk_context());
  _self->p00 = p00;
  _self->pp0 = pp0;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_choose_fun3506(kk_function_t _fself, kk_box_t _b_2780, kk_context_t* _ctx) {
  struct kk_std_text_parse_choose_fun3506__t* _self = kk_function_as(struct kk_std_text_parse_choose_fun3506__t*, _fself);
  kk_function_t p00 = _self->p00; /* std/text/parse/parser<771,770> */
  kk_std_core__list pp0 = _self->pp0; /* list<std/text/parse/parser<771,770>> */
  kk_drop_match(_self, {kk_function_dup(p00);kk_std_core__list_dup(pp0);}, {}, _ctx)
  bool _x3507 = kk_bool_unbox(_b_2780); /*bool*/
  return kk_std_text_parse__mlift2416_choose(p00, pp0, _x3507, _ctx);
}

kk_box_t kk_std_text_parse_choose(kk_std_core__list ps, kk_context_t* _ctx) { /* forall<a,e> (ps : list<parser<e,a>>) -> <parse|e> a */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(ps)) {
    kk_ssize_t _x3486;
    kk_std_core_hnd__htag _x3487 = kk_std_core_hnd__htag_dup(kk_std_text_parse__tag_parse); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
    _x3486 = kk_std_core_hnd__evv_index(_x3487, _ctx); /*std/core/hnd/ev-index*/
    return kk_std_core_hnd__open_at0(_x3486, kk_std_text_parse_new_choose_fun3488(_ctx), _ctx);
  }
  {
    struct kk_std_core_Cons* _con3495 = kk_std_core__as_Cons(ps);
    kk_box_t _fun_unbox_x2764 = _con3495->head;
    kk_std_core__list _pat10 = _con3495->tail;
    if (kk_std_core__is_Nil(_pat10)) {
      if (kk_likely(kk_std_core__list_is_unique(ps))) {
        kk_std_core__list_free(ps);
      }
      else {
        kk_box_dup(_fun_unbox_x2764);
        kk_std_core__list_decref(ps, _ctx);
      }
      kk_function_t _x3496 = kk_function_unbox(_fun_unbox_x2764); /*() -> <std/text/parse/parse|771> 2765*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _x3496, (_x3496, _ctx));
    }
  }
  {
    struct kk_std_core_Cons* _con3497 = kk_std_core__as_Cons(ps);
    kk_box_t _fun_unbox_x2767 = _con3497->head;
    kk_std_core__list pp0 = _con3497->tail;
    if (kk_likely(kk_std_core__list_is_unique(ps))) {
      kk_std_core__list_free(ps);
    }
    else {
      kk_box_dup(_fun_unbox_x2767);
      kk_std_core__list_dup(pp0);
      kk_std_core__list_decref(ps, _ctx);
    }
    kk_function_t p00 = kk_function_unbox(_fun_unbox_x2767); /*std/text/parse/parser<771,770>*/;
    kk_ssize_t _b_2777_2775;
    kk_std_core_hnd__htag _x3498 = kk_std_core_hnd__htag_dup(kk_std_text_parse__tag_parse); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
    _b_2777_2775 = kk_std_core_hnd__evv_index(_x3498, _ctx); /*std/core/hnd/ev-index*/
    bool x0_2463;
    kk_box_t _x3499 = kk_std_core_hnd__open_at0(_b_2777_2775, kk_std_text_parse_new_choose_fun3500(_ctx), _ctx); /*1001*/
    x0_2463 = kk_bool_unbox(_x3499); /*bool*/
    if (kk_yielding(kk_context())) {
      return kk_std_core_hnd_yield_extend(kk_std_text_parse_new_choose_fun3506(p00, pp0, _ctx), _ctx);
    }
    if (x0_2463) {
      kk_std_core__list_drop(pp0, _ctx);
      return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p00, (p00, _ctx));
    }
    {
      kk_function_drop(p00, _ctx);
      { // tailcall
        ps = pp0;
        goto kk__tailcall;
      }
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_text_parse__mlift2417_count_acc(kk_std_core__list acc, kk_integer_t n, kk_function_t p, kk_box_t x, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, n : int, p : parser<e,a>, x : a) -> <parse|e> list<a> */ 
  kk_integer_t _x3508 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_std_core__list _x3509 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<1009>*/
  return kk_std_text_parse_count_acc(_x3508, _x3509, p, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_count_acc_fun3513__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_integer_t n0;
  kk_function_t p0;
};
static kk_box_t kk_std_text_parse_count_acc_fun3513(kk_function_t _fself, kk_box_t _b_2784, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_count_acc_fun3513(kk_std_core__list acc0, kk_integer_t n0, kk_function_t p0, kk_context_t* _ctx) {
  struct kk_std_text_parse_count_acc_fun3513__t* _self = kk_function_alloc_as(struct kk_std_text_parse_count_acc_fun3513__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_count_acc_fun3513, kk_context());
  _self->acc0 = acc0;
  _self->n0 = n0;
  _self->p0 = p0;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_count_acc_fun3513(kk_function_t _fself, kk_box_t _b_2784, kk_context_t* _ctx) {
  struct kk_std_text_parse_count_acc_fun3513__t* _self = kk_function_as(struct kk_std_text_parse_count_acc_fun3513__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<825> */
  kk_integer_t n0 = _self->n0; /* int */
  kk_function_t p0 = _self->p0; /* std/text/parse/parser<826,825> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_integer_dup(n0);kk_function_dup(p0);}, {}, _ctx)
  kk_std_core__list _x3514 = kk_std_text_parse__mlift2417_count_acc(acc0, n0, p0, _b_2784, _ctx); /*list<825>*/
  return kk_std_core__list_box(_x3514, _ctx);
}

kk_std_core__list kk_std_text_parse_count_acc(kk_integer_t n0, kk_std_core__list acc0, kk_function_t p0, kk_context_t* _ctx) { /* forall<a,e> (n : int, acc : list<a>, p : parser<e,a>) -> <parse|e> list<a> */ 
  kk__tailcall: ;
  bool _match_3373;
  kk_integer_t _x3510 = kk_integer_dup(n0); /*int*/
  _match_3373 = kk_integer_lte(_x3510,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_3373) {
    kk_integer_drop(n0, _ctx);
    kk_function_drop(p0, _ctx);
    return kk_std_core_reverse(acc0, _ctx);
  }
  {
    kk_box_t x0_2468;
    kk_function_t _x3511 = kk_function_dup(p0); /*std/text/parse/parser<826,825>*/
    x0_2468 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _x3511, (_x3511, _ctx)); /*825*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_2468, _ctx);
      kk_box_t _x3512 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_count_acc_fun3513(acc0, n0, p0, _ctx), _ctx); /*1002*/
      return kk_std_core__list_unbox(_x3512, _ctx);
    }
    { // tailcall
      kk_integer_t _x3515 = kk_integer_sub(n0,(kk_integer_from_small(1)),kk_context()); /*int*/
      kk_std_core__list _x3516 = kk_std_core__new_Cons(kk_reuse_null, x0_2468, acc0, _ctx); /*list<1009>*/
      n0 = _x3515;
      acc0 = _x3516;
      goto kk__tailcall;
    }
  }
}


// lift anonymous function
struct kk_std_text_parse_digit_fun3528__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_digit_fun3528(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digit_fun3528(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digit_fun3528, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_digit_fun3528(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3370 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3370)) {
    kk_box_t _box_x2793 = _match_3370._cons.Just.value;
    kk_std_core_types__tuple2_ _pat00 = kk_std_core_types__tuple2__unbox(_box_x2793, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat00)) {
      kk_box_t _box_x2794 = _pat00.fst;
      kk_box_t _box_x2795 = _pat00.snd;
      kk_char_t c = kk_char_unbox(_box_x2794, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2795, NULL);
      bool _match_3371 = (c >= ('0')); /*bool*/;
      bool _x3532;
      if (_match_3371) {
        _x3532 = (c <= ('9')); /*bool*/
      }
      else {
        _x3532 = false; /*bool*/
      }
      if (_x3532) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3370, _ctx);
        kk_box_t _x3533;
        kk_std_core_types__tuple2_ _x3534 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3533 = kk_std_core_types__tuple2__box(_x3534, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3533, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3370, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_digit_fun3536__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_digit_fun3536(kk_function_t _fself, kk_box_t _b_2807, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digit_fun3536(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digit_fun3536, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_digit_fun3536(kk_function_t _fself, kk_box_t _b_2807, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x3537;
  kk_char_t _x3538 = kk_char_unbox(_b_2807, _ctx); /*char*/
  _x3537 = kk_std_text_parse__mlift2418_digit(_x3538, _ctx); /*int*/
  return kk_integer_box(_x3537);
}

kk_integer_t kk_std_text_parse_digit(kk_context_t* _ctx) { /* () -> parse int */ 
  kk_char_t x_2473;
  kk_box_t _x3525;
  kk_string_t _x3526;
  kk_define_string_literal(, _s3527, 5, "digit")
  _x3526 = kk_string_dup(_s3527); /*string*/
  _x3525 = kk_std_text_parse_satisfy_fail(_x3526, kk_std_text_parse_new_digit_fun3528(_ctx), _ctx); /*547*/
  x_2473 = kk_char_unbox(_x3525, _ctx); /*char*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x3535 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_digit_fun3536(_ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x3535);
  }
  {
    kk_char_t _x3539;
    kk_integer_t _x3540;
    kk_integer_t _x3541 = kk_integer_from_int(x_2473,kk_context()); /*int*/
    kk_integer_t _x3542 = kk_integer_from_int(('0'),kk_context()); /*int*/
    _x3540 = kk_integer_sub(_x3541,_x3542,kk_context()); /*int*/
    _x3539 = kk_integer_clamp32(_x3540,kk_context()); /*char*/
    return kk_integer_from_int(_x3539,kk_context());
  }
}


// lift anonymous function
struct kk_std_text_parse_digits_fun3546__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_digits_fun3546(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits_fun3546(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits_fun3546, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_digits_fun3548__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_digits_fun3548(kk_function_t _fself, kk_char_t _x13547, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits_fun3548(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits_fun3548, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_digits_fun3548(kk_function_t _fself, kk_char_t _x13547, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_digit(_x13547, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_digits_fun3546(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3368 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_digits_fun3548(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x2809 = _match_3368.fst;
    kk_box_t _box_x2810 = _match_3368.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x2809, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x2810, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3368, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x2811 = _match_3368.fst;
    kk_box_t _box_x2812 = _match_3368.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x2811, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2812, NULL);
    kk_box_t _x3553;
    kk_std_core_types__tuple2_ _x3554 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3553 = kk_std_core_types__tuple2__box(_x3554, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3553, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_digits_fun3556__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_digits_fun3556(kk_function_t _fself, kk_box_t _b_2824, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits_fun3556(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits_fun3556, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_digits_fun3556(kk_function_t _fself, kk_box_t _b_2824, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3557;
  kk_std_core__list _x3558 = kk_std_core__list_unbox(_b_2824, _ctx); /*list<char>*/
  _x3557 = kk_std_core_string_2(_x3558, _ctx); /*string*/
  return kk_string_box(_x3557);
}

kk_string_t kk_std_text_parse_digits(kk_context_t* _ctx) { /* () -> parse string */ 
  kk_std_core__list x_2476;
  kk_box_t _x3543;
  kk_string_t _x3544;
  kk_define_string_literal(, _s3545, 5, "digit")
  _x3544 = kk_string_dup(_s3545); /*string*/
  _x3543 = kk_std_text_parse_satisfy_fail(_x3544, kk_std_text_parse_new_digits_fun3546(_ctx), _ctx); /*547*/
  x_2476 = kk_std_core__list_unbox(_x3543, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2476, _ctx);
    kk_box_t _x3555 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_digits_fun3556(_ctx), _ctx); /*1002*/
    return kk_string_unbox(_x3555);
  }
  {
    return kk_std_core_string_2(x_2476, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse__lp__bar__bar__fun3561__t_rp_ {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse__lp__bar__bar__fun3561_rp_(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_dash__bar__bar__fun3561(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse__lp__bar__bar__fun3561_rp_, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse__lp__bar__bar__fun3561_rp_(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x3562;
  kk_std_core_hnd__ev ev_2481;
  kk_ssize_t _x3563 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2481 = kk_evv_at(_x3563,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_box_t _x3564;
  {
    struct kk_std_core_hnd_Ev* _con3565 = kk_std_core_hnd__as_Ev(ev_2481);
    kk_std_core_hnd__marker m0 = _con3565->marker;
    kk_box_t _box_x2826 = _con3565->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2826, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause0 _match_3366 = kk_std_text_parse__select_pick(h, _ctx); /*std/core/hnd/clause0<bool,std/text/parse/.hnd-parse,417,418>*/;
    {
      kk_function_t _fun_unbox_x2829 = _match_3366.clause;
      _x3564 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_context_t*), _fun_unbox_x2829, (_fun_unbox_x2829, m0, ev_2481, _ctx)); /*1006*/
    }
  }
  _x3562 = kk_bool_unbox(_x3564); /*bool*/
  return kk_bool_box(_x3562);
}


// lift anonymous function
struct kk_std_text_parse__lp__bar__bar__fun3567__t_rp_ {
  struct kk_function_s _base;
  kk_function_t p1;
  kk_function_t p2;
};
static kk_box_t kk_std_text_parse__lp__bar__bar__fun3567_rp_(kk_function_t _fself, kk_box_t _b_2837, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_dash__bar__bar__fun3567(kk_function_t p1, kk_function_t p2, kk_context_t* _ctx) {
  struct kk_std_text_parse__lp__bar__bar__fun3567__t_rp_* _self = kk_function_alloc_as(struct kk_std_text_parse__lp__bar__bar__fun3567__t_rp_, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__lp__bar__bar__fun3567_rp_, kk_context());
  _self->p1 = p1;
  _self->p2 = p2;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__lp__bar__bar__fun3567_rp_(kk_function_t _fself, kk_box_t _b_2837, kk_context_t* _ctx) {
  struct kk_std_text_parse__lp__bar__bar__fun3567__t_rp_* _self = kk_function_as(struct kk_std_text_parse__lp__bar__bar__fun3567__t_rp_*, _fself);
  kk_function_t p1 = _self->p1; /* std/text/parse/parser<949,948> */
  kk_function_t p2 = _self->p2; /* std/text/parse/parser<949,948> */
  kk_drop_match(_self, {kk_function_dup(p1);kk_function_dup(p2);}, {}, _ctx)
  bool _match_3365 = kk_bool_unbox(_b_2837); /*bool*/;
  if (_match_3365) {
    kk_function_drop(p2, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p1, (p1, _ctx));
  }
  {
    kk_function_drop(p1, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p2, (p2, _ctx));
  }
}

kk_box_t kk_std_text_parse__lp__bar__bar__rp_(kk_function_t p1, kk_function_t p2, kk_context_t* _ctx) { /* forall<a,e> (p1 : parser<e,a>, p2 : parser<e,a>) -> <parse|e> a */ 
  kk_ssize_t _b_2834_2832;
  kk_std_core_hnd__htag _x3559 = kk_std_core_hnd__htag_dup(kk_std_text_parse__tag_parse); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
  _b_2834_2832 = kk_std_core_hnd__evv_index(_x3559, _ctx); /*std/core/hnd/ev-index*/
  bool x_2478;
  kk_box_t _x3560 = kk_std_core_hnd__open_at0(_b_2834_2832, kk_std_text_parse_new_dash__bar__bar__fun3561(_ctx), _ctx); /*1001*/
  x_2478 = kk_bool_unbox(_x3560); /*bool*/
  if (kk_yielding(kk_context())) {
    return kk_std_core_hnd_yield_extend(kk_std_text_parse_new_dash__bar__bar__fun3567(p1, p2, _ctx), _ctx);
  }
  if (x_2478) {
    kk_function_drop(p2, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p1, (p1, _ctx));
  }
  {
    kk_function_drop(p1, _ctx);
    return kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p2, (p2, _ctx));
  }
}
extern kk_box_t kk_std_text_parse_optional_fun3568(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_optional_fun3568__t* _self = kk_function_as(struct kk_std_text_parse_optional_fun3568__t*, _fself);
  kk_box_t kkloc_default = _self->kkloc_default; /* 990 */
  kk_drop_match(_self, {kk_box_dup(kkloc_default);}, {}, _ctx)
  return kkloc_default;
}


// lift anonymous function
struct kk_std_text_parse_digits0_fun3570__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_digits0_fun3570(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits0_fun3570(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits0_fun3570, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_digits0_fun3575__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_digits0_fun3575(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits0_fun3575(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits0_fun3575, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_digits0_fun3577__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_digits0_fun3577(kk_function_t _fself, kk_char_t _x13576, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits0_fun3577(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits0_fun3577, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_digits0_fun3577(kk_function_t _fself, kk_char_t _x13576, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_digit(_x13576, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_digits0_fun3575(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3363 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_digits0_fun3577(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x2840 = _match_3363.fst;
    kk_box_t _box_x2841 = _match_3363.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x2840, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x2841, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3363, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x2842 = _match_3363.fst;
    kk_box_t _box_x2843 = _match_3363.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x2842, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2843, NULL);
    kk_box_t _x3582;
    kk_std_core_types__tuple2_ _x3583 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3582 = kk_std_core_types__tuple2__box(_x3583, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3582, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_digits0_fun3585__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_digits0_fun3585(kk_function_t _fself, kk_box_t _b_2855, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits0_fun3585(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits0_fun3585, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_digits0_fun3585(kk_function_t _fself, kk_box_t _b_2855, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3586;
  kk_std_core__list _x3587 = kk_std_core__list_unbox(_b_2855, _ctx); /*list<char>*/
  _x3586 = kk_std_core_string_2(_x3587, _ctx); /*string*/
  return kk_string_box(_x3586);
}
static kk_box_t kk_std_text_parse_digits0_fun3570(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3571;
  kk_std_core__list x_2486;
  kk_box_t _x3572;
  kk_string_t _x3573;
  kk_define_string_literal(, _s3574, 5, "digit")
  _x3573 = kk_string_dup(_s3574); /*string*/
  _x3572 = kk_std_text_parse_satisfy_fail(_x3573, kk_std_text_parse_new_digits0_fun3575(_ctx), _ctx); /*547*/
  x_2486 = kk_std_core__list_unbox(_x3572, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2486, _ctx);
    kk_box_t _x3584 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_digits0_fun3585(_ctx), _ctx); /*1002*/
    _x3571 = kk_string_unbox(_x3584); /*string*/
  }
  else {
    _x3571 = kk_std_core_string_2(x_2486, _ctx); /*string*/
  }
  return kk_string_box(_x3571);
}


// lift anonymous function
struct kk_std_text_parse_digits0_fun3588__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_digits0_fun3588(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_digits0_fun3588(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_digits0_fun3588, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_digits0_fun3588(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3589;
  kk_define_string_literal(, _s3590, 1, "0")
  _x3589 = kk_string_dup(_s3590); /*string*/
  return kk_string_box(_x3589);
}

kk_string_t kk_std_text_parse_digits0(kk_context_t* _ctx) { /* () -> parse string */ 
  kk_box_t _x3569 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_text_parse_new_digits0_fun3570(_ctx), kk_std_text_parse_new_digits0_fun3588(_ctx), _ctx); /*948*/
  return kk_string_unbox(_x3569);
}
 
// monadic lift

kk_unit_t kk_std_text_parse__mlift2420_eof(kk_std_core_types__maybe _y_2361, kk_context_t* _ctx) { /* (maybe<()>) -> parse () */ 
  if (kk_std_core_types__is_Nothing(_y_2361)) {
    kk_std_core_hnd__ev ev_2488;
    kk_ssize_t _x3591 = ((kk_ssize_t)0); /*ssize_t*/
    ev_2488 = kk_evv_at(_x3591,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
    kk_box_t _x3592;
    {
      struct kk_std_core_hnd_Ev* _con3593 = kk_std_core_hnd__as_Ev(ev_2488);
      kk_std_core_hnd__marker m0 = _con3593->marker;
      kk_box_t _box_x2861 = _con3593->hnd;
      kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2861, NULL);
      kk_std_text_parse__hnd_parse_dup(h);
      kk_std_core_hnd__clause1 _match_3361 = kk_std_text_parse__select_fail(h, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
      {
        kk_function_t _fun_unbox_x2865 = _match_3361.clause;
        kk_box_t _x3595;
        kk_string_t _x3596;
        kk_define_string_literal(, _s3597, 22, "expecting end-of-input")
        _x3596 = kk_string_dup(_s3597); /*string*/
        _x3595 = kk_string_box(_x3596); /*1010*/
        _x3592 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2865, (_fun_unbox_x2865, m0, ev_2488, _x3595, _ctx)); /*1011*/
      }
    }
    kk_unit_unbox(_x3592); return kk_Unit;
  }
  {
    kk_box_t _box_x2869 = _y_2361._cons.Just.value;
    kk_unit_t _pat30 = kk_unit_unbox(_box_x2869);
    kk_std_core_types__maybe_drop(_y_2361, _ctx);
    kk_Unit; return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_text_parse_eof_fun3603__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_eof_fun3603(kk_function_t _fself, kk_box_t _b_2883, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_eof_fun3603(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_eof_fun3603, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_eof_fun3603(kk_function_t _fself, kk_box_t _b_2883, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _x3604;
  bool b_16603;
  kk_ssize_t _x3605;
  kk_std_core__sslice _match_3360;
  kk_box_t _x3606 = kk_box_dup(_b_2883); /*2881*/
  _match_3360 = kk_std_core__sslice_unbox(_x3606, _ctx); /*sslice*/
  {
    kk_ssize_t _x = _match_3360.len;
    kk_std_core__sslice_drop(_match_3360, _ctx);
    _x3605 = _x; /*ssize_t*/
  }
  b_16603 = (_x3605 > 0); /*bool*/
  if (b_16603) {
    kk_box_drop(_b_2883, _ctx);
    _x3604 = kk_std_core_types__new_Nothing(_ctx); /*forall<a> maybe<a>*/
  }
  else {
    kk_box_t _x3607;
    kk_std_core_types__tuple2_ _x3608 = kk_std_core_types__new_dash__lp__comma__rp_(kk_unit_box(kk_Unit), _b_2883, _ctx); /*(1004, 1005)*/
    _x3607 = kk_std_core_types__tuple2__box(_x3608, _ctx); /*1034*/
    _x3604 = kk_std_core_types__new_Just(_x3607, _ctx); /*forall<a> maybe<a>*/
  }
  return kk_std_core_types__maybe_box(_x3604, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_eof_fun3610__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_eof_fun3610(kk_function_t _fself, kk_box_t _b_2892, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_eof_fun3610(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_eof_fun3610, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_eof_fun3610(kk_function_t _fself, kk_box_t _b_2892, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x3611 = kk_Unit;
  kk_std_core_types__maybe _x3612 = kk_std_core_types__maybe_unbox(_b_2892, _ctx); /*maybe<()>*/
  kk_std_text_parse__mlift2420_eof(_x3612, _ctx);
  return kk_unit_box(_x3611);
}

kk_unit_t kk_std_text_parse_eof(kk_context_t* _ctx) { /* () -> parse () */ 
  kk_std_core_hnd__ev ev_2494;
  kk_ssize_t _x3599 = ((kk_ssize_t)0); /*ssize_t*/
  ev_2494 = kk_evv_at(_x3599,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
  kk_std_core_types__maybe x_2491;
  kk_box_t _x3600;
  {
    struct kk_std_core_hnd_Ev* _con3601 = kk_std_core_hnd__as_Ev(ev_2494);
    kk_std_core_hnd__marker m0 = _con3601->marker;
    kk_box_t _box_x2870 = _con3601->hnd;
    kk_std_text_parse__hnd_parse h = kk_std_text_parse__hnd_parse_unbox(_box_x2870, NULL);
    kk_std_text_parse__hnd_parse_dup(h);
    kk_std_core_hnd__clause1 _match_3359 = kk_std_text_parse__select_satisfy(h, _ctx); /*std/core/hnd/clause1<(sslice) -> total maybe<(438, sslice)>,maybe<438>,std/text/parse/.hnd-parse,439,440>*/;
    {
      kk_function_t _fun_unbox_x2877 = _match_3359.clause;
      _x3600 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2877, (_fun_unbox_x2877, m0, ev_2494, kk_function_box(kk_std_text_parse_new_eof_fun3603(_ctx)), _ctx)); /*1011*/
    }
  }
  x_2491 = kk_std_core_types__maybe_unbox(_x3600, _ctx); /*maybe<()>*/
  if (kk_yielding(kk_context())) {
    kk_std_core_types__maybe_drop(x_2491, _ctx);
    kk_box_t _x3609 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_eof_fun3610(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x3609); return kk_Unit;
  }
  if (kk_std_core_types__is_Nothing(x_2491)) {
    kk_std_core_hnd__ev ev0_2497;
    kk_ssize_t _x3613 = ((kk_ssize_t)0); /*ssize_t*/
    ev0_2497 = kk_evv_at(_x3613,kk_context()); /*std/core/hnd/ev<std/text/parse/.hnd-parse>*/
    kk_box_t _x3614;
    {
      struct kk_std_core_hnd_Ev* _con3615 = kk_std_core_hnd__as_Ev(ev0_2497);
      kk_std_core_hnd__marker m00 = _con3615->marker;
      kk_box_t _box_x2893 = _con3615->hnd;
      kk_std_text_parse__hnd_parse h0 = kk_std_text_parse__hnd_parse_unbox(_box_x2893, NULL);
      kk_std_text_parse__hnd_parse_dup(h0);
      kk_std_core_hnd__clause1 _match_3358 = kk_std_text_parse__select_fail(h0, _ctx); /*std/core/hnd/clause1<string,399,std/text/parse/.hnd-parse,400,401>*/;
      {
        kk_function_t _fun_unbox_x2897 = _match_3358.clause;
        kk_box_t _x3617;
        kk_string_t _x3618;
        kk_define_string_literal(, _s3619, 22, "expecting end-of-input")
        _x3618 = kk_string_dup(_s3619); /*string*/
        _x3617 = kk_string_box(_x3618); /*1010*/
        _x3614 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x2897, (_fun_unbox_x2897, m00, ev0_2497, _x3617, _ctx)); /*1011*/
      }
    }
    kk_unit_unbox(_x3614); return kk_Unit;
  }
  {
    kk_box_t _box_x2901 = x_2491._cons.Just.value;
    kk_unit_t _pat31 = kk_unit_unbox(_box_x2901);
    kk_std_core_types__maybe_drop(x_2491, _ctx);
    kk_Unit; return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_text_parse_hex_digits_fun3624__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_hex_digits_fun3624(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_hex_digits_fun3624(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_hex_digits_fun3624, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_hex_digits_fun3626__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_hex_digits_fun3626(kk_function_t _fself, kk_char_t _x13625, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_hex_digits_fun3626(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_hex_digits_fun3626, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_hex_digits_fun3626(kk_function_t _fself, kk_char_t _x13625, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_hex_digit(_x13625, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_hex_digits_fun3624(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3356 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_hex_digits_fun3626(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x2903 = _match_3356.fst;
    kk_box_t _box_x2904 = _match_3356.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x2903, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x2904, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3356, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x2905 = _match_3356.fst;
    kk_box_t _box_x2906 = _match_3356.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x2905, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2906, NULL);
    kk_box_t _x3631;
    kk_std_core_types__tuple2_ _x3632 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3631 = kk_std_core_types__tuple2__box(_x3632, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3631, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_hex_digits_fun3634__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_hex_digits_fun3634(kk_function_t _fself, kk_box_t _b_2918, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_hex_digits_fun3634(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_hex_digits_fun3634, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_hex_digits_fun3634(kk_function_t _fself, kk_box_t _b_2918, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3635;
  kk_std_core__list _x3636 = kk_std_core__list_unbox(_b_2918, _ctx); /*list<char>*/
  _x3635 = kk_std_core_string_2(_x3636, _ctx); /*string*/
  return kk_string_box(_x3635);
}

kk_string_t kk_std_text_parse_hex_digits(kk_context_t* _ctx) { /* () -> parse string */ 
  kk_std_core__list x_2500;
  kk_box_t _x3621;
  kk_string_t _x3622;
  kk_define_string_literal(, _s3623, 5, "digit")
  _x3622 = kk_string_dup(_s3623); /*string*/
  _x3621 = kk_std_text_parse_satisfy_fail(_x3622, kk_std_text_parse_new_hex_digits_fun3624(_ctx), _ctx); /*547*/
  x_2500 = kk_std_core__list_unbox(_x3621, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2500, _ctx);
    kk_box_t _x3633 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_hex_digits_fun3634(_ctx), _ctx); /*1002*/
    return kk_string_unbox(_x3633);
  }
  {
    return kk_std_core_string_2(x_2500, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2421_many_acc_fun3639__t {
  struct kk_function_s _base;
  kk_function_t p;
};
static kk_box_t kk_std_text_parse__mlift2421_many_acc_fun3639(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2421_many_acc_fun3639(kk_function_t p, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2421_many_acc_fun3639__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2421_many_acc_fun3639__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2421_many_acc_fun3639, kk_context());
  _self->p = p;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2421_many_acc_fun3639(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2421_many_acc_fun3639__t* _self = kk_function_as(struct kk_std_text_parse__mlift2421_many_acc_fun3639__t*, _fself);
  kk_function_t p = _self->p; /* std/text/parse/parser<1152,1151> */
  kk_drop_match(_self, {kk_function_dup(p);}, {}, _ctx)
  return kk_function_box(p);
}

kk_std_core__list kk_std_text_parse__mlift2421_many_acc(kk_std_core__list acc, kk_function_t p, kk_box_t x, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, p : parser<e,a>, x : a) -> <parse|e> list<a> */ 
  kk_function_t _x3637;
  kk_box_t _x3638 = kk_std_core_hnd__open_none0(kk_std_text_parse__new_mlift2421_many_acc_fun3639(p, _ctx), _ctx); /*1001*/
  _x3637 = kk_function_unbox(_x3638); /*() -> <std/text/parse/parse|1152> 2922*/
  kk_std_core__list _x3640 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<1009>*/
  return kk_std_text_parse_many_acc(_x3637, _x3640, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_many_acc_fun3643__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t p0;
};
static kk_box_t kk_std_text_parse_many_acc_fun3643(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_many_acc_fun3643(kk_std_core__list acc0, kk_function_t p0, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3643__t* _self = kk_function_alloc_as(struct kk_std_text_parse_many_acc_fun3643__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_many_acc_fun3643, kk_context());
  _self->acc0 = acc0;
  _self->p0 = p0;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_many_acc_fun3647__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t p0;
};
static kk_box_t kk_std_text_parse_many_acc_fun3647(kk_function_t _fself, kk_box_t _b_2925, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_many_acc_fun3647(kk_std_core__list acc0, kk_function_t p0, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3647__t* _self = kk_function_alloc_as(struct kk_std_text_parse_many_acc_fun3647__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_many_acc_fun3647, kk_context());
  _self->acc0 = acc0;
  _self->p0 = p0;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_many_acc_fun3647(kk_function_t _fself, kk_box_t _b_2925, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3647__t* _self = kk_function_as(struct kk_std_text_parse_many_acc_fun3647__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<1151> */
  kk_function_t p0 = _self->p0; /* std/text/parse/parser<1152,1151> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(p0);}, {}, _ctx)
  kk_std_core__list _x3648 = kk_std_text_parse__mlift2421_many_acc(acc0, p0, _b_2925, _ctx); /*list<1151>*/
  return kk_std_core__list_box(_x3648, _ctx);
}
static kk_box_t kk_std_text_parse_many_acc_fun3643(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3643__t* _self = kk_function_as(struct kk_std_text_parse_many_acc_fun3643__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<1151> */
  kk_function_t p0 = _self->p0; /* std/text/parse/parser<1152,1151> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(p0);}, {}, _ctx)
  kk_std_core__list _x3644;
  kk_box_t x0_2502;
  kk_function_t _x3645 = kk_function_dup(p0); /*std/text/parse/parser<1152,1151>*/
  x0_2502 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _x3645, (_x3645, _ctx)); /*1151*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x0_2502, _ctx);
    kk_box_t _x3646 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_many_acc_fun3647(acc0, p0, _ctx), _ctx); /*1002*/
    _x3644 = kk_std_core__list_unbox(_x3646, _ctx); /*list<1151>*/
  }
  else {
    _x3644 = kk_std_text_parse__mlift2421_many_acc(acc0, p0, x0_2502, _ctx); /*list<1151>*/
  }
  return kk_std_core__list_box(_x3644, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_many_acc_fun3649__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
};
static kk_box_t kk_std_text_parse_many_acc_fun3649(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_many_acc_fun3649(kk_std_core__list acc0, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3649__t* _self = kk_function_alloc_as(struct kk_std_text_parse_many_acc_fun3649__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_many_acc_fun3649, kk_context());
  _self->acc0 = acc0;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_many_acc_fun3649(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_many_acc_fun3649__t* _self = kk_function_as(struct kk_std_text_parse_many_acc_fun3649__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<1151> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);}, {}, _ctx)
  kk_std_core__list _x3650 = kk_std_core_reverse(acc0, _ctx); /*list<1001>*/
  return kk_std_core__list_box(_x3650, _ctx);
}

kk_std_core__list kk_std_text_parse_many_acc(kk_function_t p0, kk_std_core__list acc0, kk_context_t* _ctx) { /* forall<a,e> (p : parser<e,a>, acc : list<a>) -> <parse|e> list<a> */ 
  kk_box_t _x3641;
  kk_function_t _x3642;
  kk_std_core__list_dup(acc0);
  _x3642 = kk_std_text_parse_new_many_acc_fun3643(acc0, p0, _ctx); /*() -> <std/text/parse/parse|949> 948*/
  _x3641 = kk_std_text_parse__lp__bar__bar__rp_(_x3642, kk_std_text_parse_new_many_acc_fun3649(acc0, _ctx), _ctx); /*948*/
  return kk_std_core__list_unbox(_x3641, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2423_many1_fun3652__t {
  struct kk_function_s _base;
  kk_box_t _y_2369;
};
static kk_box_t kk_std_text_parse__mlift2423_many1_fun3652(kk_function_t _fself, kk_box_t _b_2933, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2423_many1_fun3652(kk_box_t _y_2369, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2423_many1_fun3652__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2423_many1_fun3652__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2423_many1_fun3652, kk_context());
  _self->_y_2369 = _y_2369;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2423_many1_fun3652(kk_function_t _fself, kk_box_t _b_2933, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2423_many1_fun3652__t* _self = kk_function_as(struct kk_std_text_parse__mlift2423_many1_fun3652__t*, _fself);
  kk_box_t _y_2369 = _self->_y_2369; /* 1188 */
  kk_drop_match(_self, {kk_box_dup(_y_2369);}, {}, _ctx)
  kk_std_core__list _x3653;
  kk_std_core__list _x3654 = kk_std_core__list_unbox(_b_2933, _ctx); /*list<1188>*/
  _x3653 = kk_std_core__new_Cons(kk_reuse_null, _y_2369, _x3654, _ctx); /*list<1009>*/
  return kk_std_core__list_box(_x3653, _ctx);
}

kk_std_core__list kk_std_text_parse__mlift2423_many1(kk_function_t p, kk_box_t _y_2369, kk_context_t* _ctx) { /* forall<a,e> (p : parser<e,a>, a) -> <parse|e> list<a> */ 
  kk_std_core__list x_2504 = kk_std_text_parse_many_acc(p, kk_std_core__new_Nil(_ctx), _ctx); /*list<1188>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2504, _ctx);
    kk_box_t _x3651 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2423_many1_fun3652(_y_2369, _ctx), _ctx); /*1002*/
    return kk_std_core__list_unbox(_x3651, _ctx);
  }
  {
    return kk_std_core__new_Cons(kk_reuse_null, _y_2369, x_2504, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_many1_fun3657__t {
  struct kk_function_s _base;
  kk_function_t p;
};
static kk_box_t kk_std_text_parse_many1_fun3657(kk_function_t _fself, kk_box_t _b_2937, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_many1_fun3657(kk_function_t p, kk_context_t* _ctx) {
  struct kk_std_text_parse_many1_fun3657__t* _self = kk_function_alloc_as(struct kk_std_text_parse_many1_fun3657__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_many1_fun3657, kk_context());
  _self->p = p;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_many1_fun3657(kk_function_t _fself, kk_box_t _b_2937, kk_context_t* _ctx) {
  struct kk_std_text_parse_many1_fun3657__t* _self = kk_function_as(struct kk_std_text_parse_many1_fun3657__t*, _fself);
  kk_function_t p = _self->p; /* std/text/parse/parser<1189,1188> */
  kk_drop_match(_self, {kk_function_dup(p);}, {}, _ctx)
  kk_std_core__list _x3658 = kk_std_text_parse__mlift2423_many1(p, _b_2937, _ctx); /*list<1188>*/
  return kk_std_core__list_box(_x3658, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_many1_fun3660__t {
  struct kk_function_s _base;
  kk_box_t x_2508;
};
static kk_box_t kk_std_text_parse_many1_fun3660(kk_function_t _fself, kk_box_t _b_2939, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_many1_fun3660(kk_box_t x_2508, kk_context_t* _ctx) {
  struct kk_std_text_parse_many1_fun3660__t* _self = kk_function_alloc_as(struct kk_std_text_parse_many1_fun3660__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_many1_fun3660, kk_context());
  _self->x_2508 = x_2508;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_many1_fun3660(kk_function_t _fself, kk_box_t _b_2939, kk_context_t* _ctx) {
  struct kk_std_text_parse_many1_fun3660__t* _self = kk_function_as(struct kk_std_text_parse_many1_fun3660__t*, _fself);
  kk_box_t x_2508 = _self->x_2508; /* 1188 */
  kk_drop_match(_self, {kk_box_dup(x_2508);}, {}, _ctx)
  kk_std_core__list _x3661;
  kk_std_core__list _x3662 = kk_std_core__list_unbox(_b_2939, _ctx); /*list<1188>*/
  _x3661 = kk_std_core__new_Cons(kk_reuse_null, x_2508, _x3662, _ctx); /*list<1009>*/
  return kk_std_core__list_box(_x3661, _ctx);
}

kk_std_core__list kk_std_text_parse_many1(kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (p : parser<e,a>) -> <parse|e> list<a> */ 
  kk_box_t x_2508;
  kk_function_t _x3655 = kk_function_dup(p); /*std/text/parse/parser<1189,1188>*/
  x_2508 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _x3655, (_x3655, _ctx)); /*1188*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_2508, _ctx);
    kk_box_t _x3656 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_many1_fun3657(p, _ctx), _ctx); /*1002*/
    return kk_std_core__list_unbox(_x3656, _ctx);
  }
  {
    kk_std_core__list x0_2511 = kk_std_text_parse_many_acc(p, kk_std_core__new_Nil(_ctx), _ctx); /*list<1188>*/;
    if (kk_yielding(kk_context())) {
      kk_std_core__list_drop(x0_2511, _ctx);
      kk_box_t _x3659 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_many1_fun3660(x_2508, _ctx), _ctx); /*1002*/
      return kk_std_core__list_unbox(_x3659, _ctx);
    }
    {
      return kk_std_core__new_Cons(kk_reuse_null, x_2508, x0_2511, _ctx);
    }
  }
}

kk_std_core_types__maybe kk_std_text_parse_maybe(kk_std_text_parse__parse_error perr, kk_context_t* _ctx) { /* forall<a> (perr : parse-error<a>) -> maybe<a> */ 
  if (kk_std_text_parse__is_ParseOk(perr)) {
    struct kk_std_text_parse_ParseOk* _con3663 = kk_std_text_parse__as_ParseOk(perr);
    kk_box_t x0 = _con3663->result;
    kk_std_core__sslice _pat00 = _con3663->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(perr))) {
      kk_std_core__sslice_drop(_pat00, _ctx);
      kk_std_text_parse__parse_error_free(perr);
    }
    else {
      kk_box_dup(x0);
      kk_std_text_parse__parse_error_decref(perr, _ctx);
    }
    return kk_std_core_types__new_Just(x0, _ctx);
  }
  {
    struct kk_std_text_parse_ParseError* _con3664 = kk_std_text_parse__as_ParseError(perr);
    kk_std_core__sslice _pat5 = _con3664->rest;
    kk_std_text_parse__parse_error_dropn(perr, ((int32_t)KI32(2)), _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_text_parse_next_match(kk_std_core__sslice slice, kk_std_core__list cs, kk_context_t* _ctx) { /* (slice : sslice, cs : list<char>) -> maybe<sslice> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(cs)) {
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(slice, _ctx), _ctx);
  }
  {
    struct kk_std_core_Cons* _con3665 = kk_std_core__as_Cons(cs);
    kk_box_t _box_x2945 = _con3665->head;
    kk_std_core__list cc = _con3665->tail;
    kk_char_t c = kk_char_unbox(_box_x2945, NULL);
    if (kk_likely(kk_std_core__list_is_unique(cs))) {
      kk_std_core__list_free(cs);
    }
    else {
      kk_std_core__list_dup(cc);
      kk_std_core__list_decref(cs, _ctx);
    }
    kk_std_core_types__maybe _match_3350 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
    if (kk_std_core_types__is_Just(_match_3350)) {
      kk_box_t _box_x2946 = _match_3350._cons.Just.value;
      kk_std_core_types__tuple2_ _pat2 = kk_std_core_types__tuple2__unbox(_box_x2946, NULL);
      if (kk_std_core_types__is_dash__lp__comma__rp_(_pat2)) {
        kk_box_t _box_x2947 = _pat2.fst;
        kk_box_t _box_x2948 = _pat2.snd;
        kk_char_t d = kk_char_unbox(_box_x2947, NULL);
        kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2948, NULL);
        if (c == d) { // tailcall
                      slice = rest0;
                      cs = cc;
                      goto kk__tailcall;
        }
      }
    }
    {
      kk_std_core_types__maybe_drop(_match_3350, _ctx);
      kk_std_core__list_drop(cc, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
}


// lift anonymous function
struct kk_std_text_parse_no_digit_fun3673__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_no_digit_fun3673(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_no_digit_fun3673(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_no_digit_fun3673, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_no_digit_fun3673(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3348 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3348)) {
    kk_box_t _box_x2950 = _match_3348._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2950, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2951 = _pat0.fst;
      kk_box_t _box_x2952 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2951, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2952, NULL);
      bool b_2283;
      bool _match_3349 = (c >= ('0')); /*bool*/;
      if (_match_3349) {
        b_2283 = (c <= ('9')); /*bool*/
      }
      else {
        b_2283 = false; /*bool*/
      }
      bool _x3677;
      if (b_2283) {
        _x3677 = false; /*bool*/
      }
      else {
        _x3677 = true; /*bool*/
      }
      if (_x3677) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3348, _ctx);
        kk_box_t _x3678;
        kk_std_core_types__tuple2_ _x3679 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3678 = kk_std_core_types__tuple2__box(_x3679, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3678, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3348, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_no_digit(kk_context_t* _ctx) { /* () -> parse char */ 
  kk_box_t _x3670;
  kk_string_t _x3671;
  kk_define_string_literal(, _s3672, 11, "not a digit")
  _x3671 = kk_string_dup(_s3672); /*string*/
  _x3670 = kk_std_text_parse_satisfy_fail(_x3671, kk_std_text_parse_new_no_digit_fun3673(_ctx), _ctx); /*547*/
  return kk_char_unbox(_x3670, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_none_of_fun3683__t {
  struct kk_function_s _base;
  kk_string_t chars;
};
static kk_std_core_types__maybe kk_std_text_parse_none_of_fun3683(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_none_of_fun3683(kk_string_t chars, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_fun3683__t* _self = kk_function_alloc_as(struct kk_std_text_parse_none_of_fun3683__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_none_of_fun3683, kk_context());
  _self->chars = chars;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_none_of_fun3683(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_fun3683__t* _self = kk_function_as(struct kk_std_text_parse_none_of_fun3683__t*, _fself);
  kk_string_t chars = _self->chars; /* string */
  kk_drop_match(_self, {kk_string_dup(chars);}, {}, _ctx)
  kk_std_core_types__maybe _match_3347 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3347)) {
    kk_box_t _box_x2963 = _match_3347._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2963, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2964 = _pat0.fst;
      kk_box_t _box_x2965 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2964, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2965, NULL);
      bool b_2287;
      kk_string_t _x3687 = kk_string_dup(chars); /*string*/
      kk_string_t _x3688 = kk_std_core_string(c, _ctx); /*string*/
      b_2287 = kk_string_contains(_x3687,_x3688,kk_context()); /*bool*/
      bool _x3689;
      if (b_2287) {
        _x3689 = false; /*bool*/
      }
      else {
        _x3689 = true; /*bool*/
      }
      if (_x3689) {
        kk_string_drop(chars, _ctx);
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3347, _ctx);
        kk_box_t _x3690;
        kk_std_core_types__tuple2_ _x3691 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3690 = kk_std_core_types__tuple2__box(_x3691, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3690, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3347, _ctx);
    kk_string_drop(chars, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_none_of(kk_string_t chars, kk_context_t* _ctx) { /* (chars : string) -> parse char */ 
  kk_box_t _x3680;
  kk_string_t _x3681 = kk_string_empty(); /*string*/
  _x3680 = kk_std_text_parse_satisfy_fail(_x3681, kk_std_text_parse_new_none_of_fun3683(chars, _ctx), _ctx); /*547*/
  return kk_char_unbox(_x3680, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_none_of_many1_fun3695__t {
  struct kk_function_s _base;
  kk_string_t chars;
};
static kk_std_core_types__maybe kk_std_text_parse_none_of_many1_fun3695(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_none_of_many1_fun3695(kk_string_t chars, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_many1_fun3695__t* _self = kk_function_alloc_as(struct kk_std_text_parse_none_of_many1_fun3695__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_none_of_many1_fun3695, kk_context());
  _self->chars = chars;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_none_of_many1_fun3696__t {
  struct kk_function_s _base;
  kk_string_t chars;
};
static bool kk_std_text_parse_none_of_many1_fun3696(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_none_of_many1_fun3696(kk_string_t chars, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_many1_fun3696__t* _self = kk_function_alloc_as(struct kk_std_text_parse_none_of_many1_fun3696__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_none_of_many1_fun3696, kk_context());
  _self->chars = chars;
  return &_self->_base;
}

static bool kk_std_text_parse_none_of_many1_fun3696(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_many1_fun3696__t* _self = kk_function_as(struct kk_std_text_parse_none_of_many1_fun3696__t*, _fself);
  kk_string_t chars = _self->chars; /* string */
  kk_drop_match(_self, {kk_string_dup(chars);}, {}, _ctx)
  bool b_2290;
  kk_string_t _x3697 = kk_std_core_string(c, _ctx); /*string*/
  b_2290 = kk_string_contains(chars,_x3697,kk_context()); /*bool*/
  if (b_2290) {
    return false;
  }
  {
    return true;
  }
}
static kk_std_core_types__maybe kk_std_text_parse_none_of_many1_fun3695(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_none_of_many1_fun3695__t* _self = kk_function_as(struct kk_std_text_parse_none_of_many1_fun3695__t*, _fself);
  kk_string_t chars = _self->chars; /* string */
  kk_drop_match(_self, {kk_string_dup(chars);}, {}, _ctx)
  kk_std_core_types__tuple2_ _match_3346 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_none_of_many1_fun3696(chars, _ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x2976 = _match_3346.fst;
    kk_box_t _box_x2977 = _match_3346.snd;
    kk_std_core__list _pat01 = kk_std_core__list_unbox(_box_x2976, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x2977, NULL);
    if (kk_std_core__is_Nil(_pat01)) {
      kk_std_core_types__tuple2__drop(_match_3346, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x2978 = _match_3346.fst;
    kk_box_t _box_x2979 = _match_3346.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x2978, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2979, NULL);
    kk_box_t _x3702;
    kk_std_core_types__tuple2_ _x3703 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3702 = kk_std_core_types__tuple2__box(_x3703, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3702, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_none_of_many1_fun3705__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_none_of_many1_fun3705(kk_function_t _fself, kk_box_t _b_2991, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_none_of_many1_fun3705(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_none_of_many1_fun3705, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_none_of_many1_fun3705(kk_function_t _fself, kk_box_t _b_2991, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3706;
  kk_std_core__list _x3707 = kk_std_core__list_unbox(_b_2991, _ctx); /*list<char>*/
  _x3706 = kk_std_core_string_2(_x3707, _ctx); /*string*/
  return kk_string_box(_x3706);
}

kk_string_t kk_std_text_parse_none_of_many1(kk_string_t chars, kk_context_t* _ctx) { /* (chars : string) -> parse string */ 
  kk_std_core__list x_2516;
  kk_box_t _x3692;
  kk_string_t _x3693 = kk_string_empty(); /*string*/
  _x3692 = kk_std_text_parse_satisfy_fail(_x3693, kk_std_text_parse_new_none_of_many1_fun3695(chars, _ctx), _ctx); /*547*/
  x_2516 = kk_std_core__list_unbox(_x3692, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2516, _ctx);
    kk_box_t _x3704 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_none_of_many1_fun3705(_ctx), _ctx); /*1002*/
    return kk_string_unbox(_x3704);
  }
  {
    return kk_std_core_string_2(x_2516, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_one_of_fun3710__t {
  struct kk_function_s _base;
  kk_string_t chars;
};
static kk_std_core_types__maybe kk_std_text_parse_one_of_fun3710(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_one_of_fun3710(kk_string_t chars, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_fun3710__t* _self = kk_function_alloc_as(struct kk_std_text_parse_one_of_fun3710__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_one_of_fun3710, kk_context());
  _self->chars = chars;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_one_of_fun3710(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_fun3710__t* _self = kk_function_as(struct kk_std_text_parse_one_of_fun3710__t*, _fself);
  kk_string_t chars = _self->chars; /* string */
  kk_drop_match(_self, {kk_string_dup(chars);}, {}, _ctx)
  kk_std_core_types__maybe _match_3344 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3344)) {
    kk_box_t _box_x2993 = _match_3344._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x2993, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x2994 = _pat0.fst;
      kk_box_t _box_x2995 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x2994, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x2995, NULL);
      kk_string_t _x3714 = kk_string_dup(chars); /*string*/
      kk_string_t _x3715 = kk_std_core_string(c, _ctx); /*string*/
      if (kk_string_contains(_x3714,_x3715,kk_context())) {
        kk_string_drop(chars, _ctx);
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3344, _ctx);
        kk_box_t _x3716;
        kk_std_core_types__tuple2_ _x3717 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3716 = kk_std_core_types__tuple2__box(_x3717, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3716, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3344, _ctx);
    kk_string_drop(chars, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_one_of(kk_string_t chars, kk_context_t* _ctx) { /* (chars : string) -> parse char */ 
  kk_box_t _x3708;
  kk_string_t _x3709 = kk_string_dup(chars); /*string*/
  _x3708 = kk_std_text_parse_satisfy_fail(_x3709, kk_std_text_parse_new_one_of_fun3710(chars, _ctx), _ctx); /*547*/
  return kk_char_unbox(_x3708, _ctx);
}
extern kk_box_t kk_std_text_parse_one_of_or_fun3719(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_or_fun3719__t* _self = kk_function_as(struct kk_std_text_parse_one_of_or_fun3719__t*, _fself);
  kk_string_t chars = _self->chars; /* string */
  kk_drop_match(_self, {kk_string_dup(chars);}, {}, _ctx)
  kk_char_t _x3720 = kk_std_text_parse_one_of(chars, _ctx); /*char*/
  return kk_char_box(_x3720, _ctx);
}
extern kk_box_t kk_std_text_parse_one_of_or_fun3721(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_one_of_or_fun3721__t* _self = kk_function_as(struct kk_std_text_parse_one_of_or_fun3721__t*, _fself);
  kk_char_t kkloc_default = _self->kkloc_default; /* char */
  kk_drop_match(_self, {;}, {}, _ctx)
  return kk_char_box(kkloc_default, _ctx);
}
 
// monadic lift

kk_std_text_parse__parse_error kk_std_text_parse__mlift2425_parse(kk_std_text_parse__parse_error err1, kk_std_text_parse__parse_error _y_2382, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (err1 : parse-error<a>, parse-error<a>) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  if (kk_std_text_parse__is_ParseOk(_y_2382)) {
    struct kk_std_text_parse_ParseOk* _con3722 = kk_std_text_parse__as_ParseOk(_y_2382);
    kk_box_t x2 = _con3722->result;
    kk_std_core__sslice rest2 = _con3722->rest;
    kk_reuse_t _ru_3313 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_y_2382))) {
      _ru_3313 = (kk_std_text_parse__parse_error_reuse(_y_2382));
    }
    else {
      kk_std_core__sslice_dup(rest2);
      kk_box_dup(x2);
      kk_std_text_parse__parse_error_decref(_y_2382, _ctx);
      _ru_3313 = kk_reuse_null;
    }
    kk_std_text_parse__parse_error_drop(err1, _ctx);
    if (kk_likely(_ru_3313!=NULL)) {
      struct kk_std_text_parse_ParseOk* _con3723 = (struct kk_std_text_parse_ParseOk*)_ru_3313;
      return kk_std_text_parse__base_ParseOk(_con3723);
    }
    {
      return kk_std_text_parse__new_ParseOk(kk_reuse_null, x2, rest2, _ctx);
    }
  }
  {
    kk_std_text_parse__parse_error_drop(_y_2382, _ctx);
    return err1;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2426_parse_fun3725__t {
  struct kk_function_s _base;
  kk_std_text_parse__parse_error err1;
};
static kk_box_t kk_std_text_parse__mlift2426_parse_fun3725(kk_function_t _fself, kk_box_t _b_3011, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2426_parse_fun3725(kk_std_text_parse__parse_error err1, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2426_parse_fun3725__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2426_parse_fun3725__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2426_parse_fun3725, kk_context());
  _self->err1 = err1;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2426_parse_fun3725(kk_function_t _fself, kk_box_t _b_3011, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2426_parse_fun3725__t* _self = kk_function_as(struct kk_std_text_parse__mlift2426_parse_fun3725__t*, _fself);
  kk_std_text_parse__parse_error err1 = _self->err1; /* std/text/parse/parse-error<2037> */
  kk_drop_match(_self, {kk_std_text_parse__parse_error_dup(err1);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3726;
  kk_std_text_parse__parse_error _y_3013_2382 = kk_std_text_parse__parse_error_unbox(_b_3011, _ctx); /*std/text/parse/parse-error<2037>*/;
  if (kk_std_text_parse__is_ParseOk(_y_3013_2382)) {
    struct kk_std_text_parse_ParseOk* _con3728 = kk_std_text_parse__as_ParseOk(_y_3013_2382);
    kk_box_t x2 = _con3728->result;
    kk_std_core__sslice rest2 = _con3728->rest;
    kk_reuse_t _ru_3314 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_y_3013_2382))) {
      _ru_3314 = (kk_std_text_parse__parse_error_reuse(_y_3013_2382));
    }
    else {
      kk_std_core__sslice_dup(rest2);
      kk_box_dup(x2);
      kk_std_text_parse__parse_error_decref(_y_3013_2382, _ctx);
      _ru_3314 = kk_reuse_null;
    }
    kk_std_text_parse__parse_error_drop(err1, _ctx);
    if (kk_likely(_ru_3314!=NULL)) {
      struct kk_std_text_parse_ParseOk* _con3729 = (struct kk_std_text_parse_ParseOk*)_ru_3314;
      _x3726 = kk_std_text_parse__base_ParseOk(_con3729); /*std/text/parse/parse-error<39>*/
      goto _match3727;
    }
    {
      _x3726 = kk_std_text_parse__new_ParseOk(kk_reuse_null, x2, rest2, _ctx); /*std/text/parse/parse-error<39>*/
      goto _match3727;
    }
  }
  {
    kk_std_text_parse__parse_error_drop(_y_3013_2382, _ctx);
    _x3726 = err1; /*std/text/parse/parse-error<39>*/
  }
  _match3727: ;
  return kk_std_text_parse__parse_error_box(_x3726, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse__mlift2426_parse(kk_std_text_parse__parse_error err1, kk_function_t resume, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (err1 : parse-error<a>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, wild_ : ()) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  kk_std_text_parse__parse_error x_2518 = kk_function_call(kk_std_text_parse__parse_error, (kk_function_t, bool, kk_context_t*), resume, (resume, false, _ctx)); /*std/text/parse/parse-error<2037>*/;
  if (kk_yielding(kk_context())) {
    kk_std_text_parse__parse_error_drop(x_2518, _ctx);
    kk_box_t _x3724 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2426_parse_fun3725(err1, _ctx), _ctx); /*1002*/
    return kk_std_text_parse__parse_error_unbox(_x3724, _ctx);
  }
  {
    kk_std_text_parse__parse_error _y_3014_2382 = x_2518; /*std/text/parse/parse-error<2037>*/;
    if (kk_std_text_parse__is_ParseOk(_y_3014_2382)) {
      struct kk_std_text_parse_ParseOk* _con3730 = kk_std_text_parse__as_ParseOk(_y_3014_2382);
      kk_box_t x2 = _con3730->result;
      kk_std_core__sslice rest2 = _con3730->rest;
      kk_reuse_t _ru_3315 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_text_parse__parse_error_is_unique(_y_3014_2382))) {
        _ru_3315 = (kk_std_text_parse__parse_error_reuse(_y_3014_2382));
      }
      else {
        kk_std_core__sslice_dup(rest2);
        kk_box_dup(x2);
        kk_std_text_parse__parse_error_decref(_y_3014_2382, _ctx);
        _ru_3315 = kk_reuse_null;
      }
      kk_std_text_parse__parse_error_drop(err1, _ctx);
      if (kk_likely(_ru_3315!=NULL)) {
        struct kk_std_text_parse_ParseOk* _con3731 = (struct kk_std_text_parse_ParseOk*)_ru_3315;
        return kk_std_text_parse__base_ParseOk(_con3731);
      }
      {
        return kk_std_text_parse__new_ParseOk(kk_reuse_null, x2, rest2, _ctx);
      }
    }
    {
      kk_std_text_parse__parse_error_drop(_y_3014_2382, _ctx);
      return err1;
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2427_parse_fun3735__t {
  struct kk_function_s _base;
  kk_std_text_parse__parse_error _y_2380;
  kk_function_t resume;
};
static kk_box_t kk_std_text_parse__mlift2427_parse_fun3735(kk_function_t _fself, kk_box_t _b_3020, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2427_parse_fun3735(kk_std_text_parse__parse_error _y_2380, kk_function_t resume, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2427_parse_fun3735__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2427_parse_fun3735__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2427_parse_fun3735, kk_context());
  _self->_y_2380 = _y_2380;
  _self->resume = resume;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2427_parse_fun3735(kk_function_t _fself, kk_box_t _b_3020, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2427_parse_fun3735__t* _self = kk_function_as(struct kk_std_text_parse__mlift2427_parse_fun3735__t*, _fself);
  kk_std_text_parse__parse_error _y_2380 = _self->_y_2380; /* std/text/parse/parse-error<2037> */
  kk_function_t resume = _self->resume; /* (bool) -> <local<2029>,local<_253>|2038> std/text/parse/parse-error<2037> */
  kk_drop_match(_self, {kk_std_text_parse__parse_error_dup(_y_2380);kk_function_dup(resume);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3736;
  kk_unit_t _x3737 = kk_Unit;
  kk_unit_unbox(_b_3020);
  _x3736 = kk_std_text_parse__mlift2426_parse(_y_2380, resume, _x3737, _ctx); /*std/text/parse/parse-error<2037>*/
  return kk_std_text_parse__parse_error_box(_x3736, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse__mlift2427_parse(kk_ref_t input, kk_function_t resume, kk_std_core__sslice save, kk_std_text_parse__parse_error _y_2380, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (input : local-var<h1,sslice>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, save : sslice, parse-error<a>) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  if (kk_std_text_parse__is_ParseOk(_y_2380)) {
    struct kk_std_text_parse_ParseOk* _con3732 = kk_std_text_parse__as_ParseOk(_y_2380);
    kk_box_t x1 = _con3732->result;
    kk_std_core__sslice rest1 = _con3732->rest;
    kk_reuse_t _ru_3316 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_y_2380))) {
      _ru_3316 = (kk_std_text_parse__parse_error_reuse(_y_2380));
    }
    else {
      kk_std_core__sslice_dup(rest1);
      kk_box_dup(x1);
      kk_std_text_parse__parse_error_decref(_y_2380, _ctx);
      _ru_3316 = kk_reuse_null;
    }
    kk_std_core__sslice_drop(save, _ctx);
    kk_function_drop(resume, _ctx);
    kk_ref_drop(input, _ctx);
    if (kk_likely(_ru_3316!=NULL)) {
      struct kk_std_text_parse_ParseOk* _con3733 = (struct kk_std_text_parse_ParseOk*)_ru_3316;
      return kk_std_text_parse__base_ParseOk(_con3733);
    }
    {
      return kk_std_text_parse__new_ParseOk(kk_reuse_null, x1, rest1, _ctx);
    }
  }
  {
    kk_unit_t x_2522 = kk_Unit;
    (kk_ref_set(input,(kk_std_core__sslice_box(save, _ctx)),kk_context()));
    if (kk_yielding(kk_context())) {
      kk_box_t _x3734 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2427_parse_fun3735(_y_2380, resume, _ctx), _ctx); /*1002*/
      return kk_std_text_parse__parse_error_unbox(_x3734, _ctx);
    }
    {
      return kk_std_text_parse__mlift2426_parse(_y_2380, resume, x_2522, _ctx);
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2428_parse_fun3740__t {
  struct kk_function_s _base;
  kk_ref_t input;
  kk_function_t resume;
  kk_std_core__sslice save;
};
static kk_box_t kk_std_text_parse__mlift2428_parse_fun3740(kk_function_t _fself, kk_box_t _b_3024, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2428_parse_fun3740(kk_ref_t input, kk_function_t resume, kk_std_core__sslice save, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2428_parse_fun3740__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2428_parse_fun3740__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2428_parse_fun3740, kk_context());
  _self->input = input;
  _self->resume = resume;
  _self->save = save;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2428_parse_fun3740(kk_function_t _fself, kk_box_t _b_3024, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2428_parse_fun3740__t* _self = kk_function_as(struct kk_std_text_parse__mlift2428_parse_fun3740__t*, _fself);
  kk_ref_t input = _self->input; /* local-var<2029,sslice> */
  kk_function_t resume = _self->resume; /* (bool) -> <local<2029>,local<_253>|2038> std/text/parse/parse-error<2037> */
  kk_std_core__sslice save = _self->save; /* sslice */
  kk_drop_match(_self, {kk_ref_dup(input);kk_function_dup(resume);kk_std_core__sslice_dup(save);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3741;
  kk_std_text_parse__parse_error _x3742 = kk_std_text_parse__parse_error_unbox(_b_3024, _ctx); /*std/text/parse/parse-error<2037>*/
  _x3741 = kk_std_text_parse__mlift2427_parse(input, resume, save, _x3742, _ctx); /*std/text/parse/parse-error<2037>*/
  return kk_std_text_parse__parse_error_box(_x3741, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse__mlift2428_parse(kk_ref_t input, kk_function_t resume, kk_std_core__sslice save, kk_context_t* _ctx) { /* forall<_h,h1,a,e> (input : local-var<h1,sslice>, resume : (bool) -> <local<h1>,local<_h>|e> parse-error<a>, save : sslice) -> <local<h1>,local<_h>|e> parse-error<a> */ 
  kk_std_text_parse__parse_error x_2524;
  kk_function_t _x3738 = kk_function_dup(resume); /*(bool) -> <local<2029>,local<_253>|2038> std/text/parse/parse-error<2037>*/
  x_2524 = kk_function_call(kk_std_text_parse__parse_error, (kk_function_t, bool, kk_context_t*), _x3738, (_x3738, true, _ctx)); /*std/text/parse/parse-error<2037>*/
  if (kk_yielding(kk_context())) {
    kk_std_text_parse__parse_error_drop(x_2524, _ctx);
    kk_box_t _x3739 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2428_parse_fun3740(input, resume, save, _ctx), _ctx); /*1002*/
    return kk_std_text_parse__parse_error_unbox(_x3739, _ctx);
  }
  {
    return kk_std_text_parse__mlift2427_parse(input, resume, save, x_2524, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2430_parse_fun3746__t {
  struct kk_function_s _base;
  kk_box_t x;
};
static kk_box_t kk_std_text_parse__mlift2430_parse_fun3746(kk_function_t _fself, kk_box_t _b_3034, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2430_parse_fun3746(kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2430_parse_fun3746__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2430_parse_fun3746__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2430_parse_fun3746, kk_context());
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2430_parse_fun3746(kk_function_t _fself, kk_box_t _b_3034, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2430_parse_fun3746__t* _self = kk_function_as(struct kk_std_text_parse__mlift2430_parse_fun3746__t*, _fself);
  kk_box_t x = _self->x; /* 1987 */
  kk_drop_match(_self, {kk_box_dup(x);}, {}, _ctx)
  kk_box_drop(_b_3034, _ctx);
  kk_std_core_types__maybe _x3747 = kk_std_core_types__new_Just(x, _ctx); /*maybe<1034>*/
  return kk_std_core_types__maybe_box(_x3747, _ctx);
}

kk_std_core_types__maybe kk_std_text_parse__mlift2430_parse(kk_ref_t input, kk_function_t pred, kk_std_core__sslice inp, kk_context_t* _ctx) { /* forall<_h,a,h1,e> (input : local-var<h1,sslice>, pred : (sslice) -> total maybe<(a, sslice)>, inp : sslice) -> <local<h1>,local<_h>|e> maybe<a> */ 
  kk_std_core_types__maybe _match_3339 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_std_core__sslice, kk_context_t*), pred, (pred, inp, _ctx)); /*maybe<(1987, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3339)) {
    kk_box_t _box_x3027 = _match_3339._cons.Just.value;
    kk_std_core_types__tuple2_ _pat9 = kk_std_core_types__tuple2__unbox(_box_x3027, NULL);
    kk_box_t x = _pat9.fst;
    kk_box_t _box_x3028 = _pat9.snd;
    kk_std_core__sslice cap = kk_std_core__sslice_unbox(_box_x3028, NULL);
    kk_std_core__sslice_dup(cap);
    kk_box_dup(x);
    kk_std_core_types__maybe_drop(_match_3339, _ctx);
    kk_unit_t x0_2526 = kk_Unit;
    (kk_ref_set(input,(kk_std_core__sslice_box(cap, _ctx)),kk_context()));
    if (kk_yielding(kk_context())) {
      kk_box_t _x3745 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2430_parse_fun3746(x, _ctx), _ctx); /*1002*/
      return kk_std_core_types__maybe_unbox(_x3745, _ctx);
    }
    {
      return kk_std_core_types__new_Just(x, _ctx);
    }
  }
  {
    kk_ref_drop(input, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3752__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3752(kk_function_t _fself, kk_std_core_hnd__marker _b_3041, kk_std_core_hnd__ev _b_3042, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3752(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3752__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3752__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3752, kk_context());
  _self->loc = loc;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_parse_fun3752(kk_function_t _fself, kk_std_core_hnd__marker _b_3041, kk_std_core_hnd__ev _b_3042, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3752__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3752__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_3042, ((int32_t)KI32(3)), _ctx);
  return (kk_ref_get(loc,kk_context()));
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3755__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3755(kk_function_t _fself, kk_std_core_hnd__marker _b_3052, kk_std_core_hnd__ev _b_3053, kk_box_t _b_3054, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3755(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3755__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3755__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3755, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3756__t {
  struct kk_function_s _base;
  kk_box_t _b_3054;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3756(kk_function_t _fself, kk_function_t _b_3049, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3756(kk_box_t _b_3054, kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3756__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3756__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3756, kk_context());
  _self->_b_3054 = _b_3054;
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3760__t {
  struct kk_function_s _base;
  kk_box_t _b_3054;
};
static kk_box_t kk_std_text_parse_parse_fun3760(kk_function_t _fself, kk_box_t _b_3046, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3760(kk_box_t _b_3054, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3760__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3760__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3760, kk_context());
  _self->_b_3054 = _b_3054;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_parse_fun3760(kk_function_t _fself, kk_box_t _b_3046, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3760__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3760__t*, _fself);
  kk_box_t _b_3054 = _self->_b_3054; /* 1016 */
  kk_drop_match(_self, {kk_box_dup(_b_3054);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3761;
  kk_string_t _x3762 = kk_string_unbox(_b_3054); /*string*/
  kk_std_core__sslice _x3763 = kk_std_core__sslice_unbox(_b_3046, _ctx); /*sslice*/
  _x3761 = kk_std_text_parse__new_ParseError(kk_reuse_null, _x3762, _x3763, _ctx); /*std/text/parse/parse-error<39>*/
  return kk_std_text_parse__parse_error_box(_x3761, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3756(kk_function_t _fself, kk_function_t _b_3049, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3756__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3756__t*, _fself);
  kk_box_t _b_3054 = _self->_b_3054; /* 1016 */
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_box_dup(_b_3054);kk_ref_dup(loc);}, {}, _ctx)
  kk_function_drop(_b_3049, _ctx);
  kk_std_text_parse__parse_error _x3757;
  kk_std_core__sslice x0_2534;
  kk_box_t _x3758 = (kk_ref_get(loc,kk_context())); /*1000*/
  x0_2534 = kk_std_core__sslice_unbox(_x3758, _ctx); /*sslice*/
  if (kk_yielding(kk_context())) {
    kk_std_core__sslice_drop(x0_2534, _ctx);
    kk_box_t _x3759 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_parse_fun3760(_b_3054, _ctx), _ctx); /*1002*/
    _x3757 = kk_std_text_parse__parse_error_unbox(_x3759, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  else {
    kk_string_t _x3764 = kk_string_unbox(_b_3054); /*string*/
    _x3757 = kk_std_text_parse__new_ParseError(kk_reuse_null, _x3764, x0_2534, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  return kk_std_text_parse__parse_error_box(_x3757, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3755(kk_function_t _fself, kk_std_core_hnd__marker _b_3052, kk_std_core_hnd__ev _b_3053, kk_box_t _b_3054, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3755__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3755__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_3053, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_3052, kk_std_text_parse_new_parse_fun3756(_b_3054, loc, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3767__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3767(kk_function_t _fself, kk_std_core_hnd__marker _b_3071, kk_std_core_hnd__ev _b_3072, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3767(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3767__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3767__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3767, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3768__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3768(kk_function_t _fself, kk_function_t _b_3068, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3768(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3768__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3768__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3768, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3769__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3769(kk_function_t _fself, kk_box_t _b_3062, kk_function_t _b_3063, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3769(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3769__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3769__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3769, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3771__t {
  struct kk_function_s _base;
  kk_function_t _b_3063;
};
static kk_std_text_parse__parse_error kk_std_text_parse_parse_fun3771(kk_function_t _fself, bool _b_3064, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3771(kk_function_t _b_3063, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3771__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3771__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3771, kk_context());
  _self->_b_3063 = _b_3063;
  return &_self->_base;
}

static kk_std_text_parse__parse_error kk_std_text_parse_parse_fun3771(kk_function_t _fself, bool _b_3064, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3771__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3771__t*, _fself);
  kk_function_t _b_3063 = _self->_b_3063; /* (1002) -> 1003 1004 */
  kk_drop_match(_self, {kk_function_dup(_b_3063);}, {}, _ctx)
  kk_box_t _x3772 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _b_3063, (_b_3063, kk_bool_box(_b_3064), _ctx)); /*1004*/
  return kk_std_text_parse__parse_error_unbox(_x3772, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3776__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_function_t r_3122;
};
static kk_box_t kk_std_text_parse_parse_fun3776(kk_function_t _fself, kk_box_t _b_3058, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3776(kk_ref_t loc, kk_function_t r_3122, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3776__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3776__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3776, kk_context());
  _self->loc = loc;
  _self->r_3122 = r_3122;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_parse_fun3776(kk_function_t _fself, kk_box_t _b_3058, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3776__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3776__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_function_t r_3122 = _self->r_3122; /* (bool) -> <local<2029>,local<_253>|2038> std/text/parse/parse-error<2037> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_function_dup(r_3122);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3777;
  kk_std_core__sslice _x3778 = kk_std_core__sslice_unbox(_b_3058, _ctx); /*sslice*/
  _x3777 = kk_std_text_parse__mlift2428_parse(loc, r_3122, _x3778, _ctx); /*std/text/parse/parse-error<2037>*/
  return kk_std_text_parse__parse_error_box(_x3777, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3769(kk_function_t _fself, kk_box_t _b_3062, kk_function_t _b_3063, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3769__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3769__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_box_drop(_b_3062, _ctx);
  kk_std_text_parse__parse_error _x3770;
  kk_function_t r_3122 = kk_std_text_parse_new_parse_fun3771(_b_3063, _ctx); /*(bool) -> <local<2029>,local<_253>|2038> std/text/parse/parse-error<2037>*/;
  kk_std_core__sslice x1_2539;
  kk_box_t _x3773;
  kk_ref_t _x3774 = kk_ref_dup(loc); /*local-var<2029,sslice>*/
  _x3773 = (kk_ref_get(_x3774,kk_context())); /*1000*/
  x1_2539 = kk_std_core__sslice_unbox(_x3773, _ctx); /*sslice*/
  if (kk_yielding(kk_context())) {
    kk_std_core__sslice_drop(x1_2539, _ctx);
    kk_box_t _x3775 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_parse_fun3776(loc, r_3122, _ctx), _ctx); /*1002*/
    _x3770 = kk_std_text_parse__parse_error_unbox(_x3775, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  else {
    _x3770 = kk_std_text_parse__mlift2428_parse(loc, r_3122, x1_2539, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  return kk_std_text_parse__parse_error_box(_x3770, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3768(kk_function_t _fself, kk_function_t _b_3068, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3768__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3768__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  return kk_std_core_hnd_protect(kk_unit_box(kk_Unit), kk_std_text_parse_new_parse_fun3769(loc, _ctx), _b_3068, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3767(kk_function_t _fself, kk_std_core_hnd__marker _b_3071, kk_std_core_hnd__ev _b_3072, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3767__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3767__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_3072, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to(_b_3071, kk_std_text_parse_new_parse_fun3768(loc, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3781__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3781(kk_function_t _fself, kk_std_core_hnd__marker _b_3078, kk_std_core_hnd__ev _b_3079, kk_box_t _b_3080, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3781(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3781__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3781__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3781, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3783__t {
  struct kk_function_s _base;
  kk_box_t _b_3080;
};
static kk_std_core_types__maybe kk_std_text_parse_parse_fun3783(kk_function_t _fself, kk_std_core__sslice _b_3083, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3783(kk_box_t _b_3080, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3783__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3783__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3783, kk_context());
  _self->_b_3080 = _b_3080;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_parse_fun3783(kk_function_t _fself, kk_std_core__sslice _b_3083, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3783__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3783__t*, _fself);
  kk_box_t _b_3080 = _self->_b_3080; /* 1016 */
  kk_drop_match(_self, {kk_box_dup(_b_3080);}, {}, _ctx)
  kk_box_t _x3784;
  kk_function_t _x3785 = kk_function_unbox(_b_3080); /*(3081) -> total 3082*/
  _x3784 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x3785, (_x3785, kk_std_core__sslice_box(_b_3083, _ctx), _ctx)); /*3082*/
  return kk_std_core_types__maybe_unbox(_x3784, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3789__t {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_function_t x2_3126;
};
static kk_box_t kk_std_text_parse_parse_fun3789(kk_function_t _fself, kk_box_t _b_3076, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3789(kk_ref_t loc, kk_function_t x2_3126, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3789__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3789__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3789, kk_context());
  _self->loc = loc;
  _self->x2_3126 = x2_3126;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_parse_fun3789(kk_function_t _fself, kk_box_t _b_3076, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3789__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3789__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_function_t x2_3126 = _self->x2_3126; /* (sslice) -> total maybe<(1987, sslice)> */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_function_dup(x2_3126);}, {}, _ctx)
  kk_std_core_types__maybe _x3790;
  kk_std_core__sslice _x3791 = kk_std_core__sslice_unbox(_b_3076, _ctx); /*sslice*/
  _x3790 = kk_std_text_parse__mlift2430_parse(loc, x2_3126, _x3791, _ctx); /*maybe<1987>*/
  return kk_std_core_types__maybe_box(_x3790, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3781(kk_function_t _fself, kk_std_core_hnd__marker _b_3078, kk_std_core_hnd__ev _b_3079, kk_box_t _b_3080, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3781__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3781__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_3079, ((int32_t)KI32(3)), _ctx);
  kk_std_core_types__maybe _x3782;
  kk_function_t x2_3126 = kk_std_text_parse_new_parse_fun3783(_b_3080, _ctx); /*(sslice) -> total maybe<(1987, sslice)>*/;
  kk_std_core__sslice x3_2542;
  kk_box_t _x3786;
  kk_ref_t _x3787 = kk_ref_dup(loc); /*local-var<2029,sslice>*/
  _x3786 = (kk_ref_get(_x3787,kk_context())); /*1000*/
  x3_2542 = kk_std_core__sslice_unbox(_x3786, _ctx); /*sslice*/
  if (kk_yielding(kk_context())) {
    kk_std_core__sslice_drop(x3_2542, _ctx);
    kk_box_t _x3788 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_parse_fun3789(loc, x2_3126, _ctx), _ctx); /*1002*/
    _x3782 = kk_std_core_types__maybe_unbox(_x3788, _ctx); /*maybe<1987>*/
  }
  else {
    _x3782 = kk_std_text_parse__mlift2430_parse(loc, x2_3126, x3_2542, _ctx); /*maybe<1987>*/
  }
  return kk_std_core_types__maybe_box(_x3782, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_fun3793__t {
  struct kk_function_s _base;
  kk_ref_t loc;
};
static kk_box_t kk_std_text_parse_parse_fun3793(kk_function_t _fself, kk_box_t _b_3092, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3793(kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3793__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3793__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3793, kk_context());
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_fun3797__t {
  struct kk_function_s _base;
  kk_box_t _b_3092;
};
static kk_box_t kk_std_text_parse_parse_fun3797(kk_function_t _fself, kk_box_t _b_3087, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_fun3797(kk_box_t _b_3092, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3797__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_fun3797__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_fun3797, kk_context());
  _self->_b_3092 = _b_3092;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_parse_fun3797(kk_function_t _fself, kk_box_t _b_3087, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3797__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3797__t*, _fself);
  kk_box_t _b_3092 = _self->_b_3092; /* 360 */
  kk_drop_match(_self, {kk_box_dup(_b_3092);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3798;
  kk_std_core__sslice _x3799 = kk_std_core__sslice_unbox(_b_3087, _ctx); /*sslice*/
  _x3798 = kk_std_text_parse__new_ParseOk(kk_reuse_null, _b_3092, _x3799, _ctx); /*std/text/parse/parse-error<39>*/
  return kk_std_text_parse__parse_error_box(_x3798, _ctx);
}
static kk_box_t kk_std_text_parse_parse_fun3793(kk_function_t _fself, kk_box_t _b_3092, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_fun3793__t* _self = kk_function_as(struct kk_std_text_parse_parse_fun3793__t*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<2029,sslice> */
  kk_drop_match(_self, {kk_ref_dup(loc);}, {}, _ctx)
  kk_std_text_parse__parse_error _x3794;
  kk_std_core__sslice x4_2544;
  kk_box_t _x3795 = (kk_ref_get(loc,kk_context())); /*1000*/
  x4_2544 = kk_std_core__sslice_unbox(_x3795, _ctx); /*sslice*/
  if (kk_yielding(kk_context())) {
    kk_std_core__sslice_drop(x4_2544, _ctx);
    kk_box_t _x3796 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_parse_fun3797(_b_3092, _ctx), _ctx); /*1002*/
    _x3794 = kk_std_text_parse__parse_error_unbox(_x3796, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  else {
    _x3794 = kk_std_text_parse__new_ParseOk(kk_reuse_null, _b_3092, x4_2544, _ctx); /*std/text/parse/parse-error<2037>*/
  }
  return kk_std_text_parse__parse_error_box(_x3794, _ctx);
}

kk_std_text_parse__parse_error kk_std_text_parse_parse(kk_std_core__sslice input0, kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (input0 : sslice, p : () -> <parse|e> a) -> e parse-error<a> */ 
  kk_ref_t loc = kk_ref_alloc((kk_std_core__sslice_box(input0, _ctx)),kk_context()); /*local-var<2029,sslice>*/;
  int32_t _b_3093_3088 = ((int32_t)KI32(3)); /*int32*/;
  kk_std_text_parse__parse_error res;
  kk_box_t _x3748;
  kk_std_text_parse__hnd_parse _x3749;
  kk_std_core_hnd__clause0 _x3750;
  kk_function_t _x3751;
  kk_ref_dup(loc);
  _x3751 = kk_std_text_parse_new_parse_fun3752(loc, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1000*/
  _x3750 = kk_std_core_hnd__new_Clause0(_x3751, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  kk_std_core_hnd__clause1 _x3753;
  kk_function_t _x3754;
  kk_ref_dup(loc);
  _x3754 = kk_std_text_parse_new_parse_fun3755(loc, _ctx); /*(std/core/hnd/marker<1019,1020>, std/core/hnd/ev<1018>, 1016) -> 1019 1001*/
  _x3753 = kk_std_core_hnd__new_Clause1(_x3754, _ctx); /*std/core/hnd/clause1<1016,1017,1018,1019,1020>*/
  kk_std_core_hnd__clause0 _x3765;
  kk_function_t _x3766;
  kk_ref_dup(loc);
  _x3766 = kk_std_text_parse_new_parse_fun3767(loc, _ctx); /*(std/core/hnd/marker<1013,1014>, std/core/hnd/ev<1012>) -> 1013 1001*/
  _x3765 = kk_std_core_hnd__new_Clause0(_x3766, _ctx); /*std/core/hnd/clause0<1011,1012,1013,1014>*/
  kk_std_core_hnd__clause1 _x3779;
  kk_function_t _x3780;
  kk_ref_dup(loc);
  _x3780 = kk_std_text_parse_new_parse_fun3781(loc, _ctx); /*(std/core/hnd/marker<1019,1020>, std/core/hnd/ev<1018>, 1016) -> 1019 1017*/
  _x3779 = kk_std_core_hnd__new_Clause1(_x3780, _ctx); /*std/core/hnd/clause1<1016,1017,1018,1019,1020>*/
  _x3749 = kk_std_text_parse__new_Hnd_parse(kk_reuse_null, _x3750, _x3753, _x3765, _x3779, _ctx); /*std/text/parse/.hnd-parse<27,28>*/
  kk_function_t _x3792;
  kk_ref_dup(loc);
  _x3792 = kk_std_text_parse_new_parse_fun3793(loc, _ctx); /*(360) -> 361 362*/
  _x3748 = kk_std_text_parse__handle_parse(_b_3093_3088, _x3749, _x3792, p, _ctx); /*362*/
  res = kk_std_text_parse__parse_error_unbox(_x3748, _ctx); /*std/text/parse/parse-error<2037>*/
  kk_box_t _x3800 = kk_std_core_hnd_prompt_local_var(loc, kk_std_text_parse__parse_error_box(res, _ctx), _ctx); /*1002*/
  return kk_std_text_parse__parse_error_unbox(_x3800, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2433_parse_eof_fun3803__t {
  struct kk_function_s _base;
  kk_box_t x;
};
static kk_box_t kk_std_text_parse__mlift2433_parse_eof_fun3803(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2433_parse_eof_fun3803(kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2433_parse_eof_fun3803__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2433_parse_eof_fun3803__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2433_parse_eof_fun3803, kk_context());
  _self->x = x;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse__mlift2433_parse_eof_fun3804__t {
  struct kk_function_s _base;
  kk_box_t x;
};
static kk_box_t kk_std_text_parse__mlift2433_parse_eof_fun3804(kk_function_t _fself, kk_box_t _b_3138, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2433_parse_eof_fun3804(kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2433_parse_eof_fun3804__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2433_parse_eof_fun3804__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2433_parse_eof_fun3804, kk_context());
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2433_parse_eof_fun3804(kk_function_t _fself, kk_box_t _b_3138, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2433_parse_eof_fun3804__t* _self = kk_function_as(struct kk_std_text_parse__mlift2433_parse_eof_fun3804__t*, _fself);
  kk_box_t x = _self->x; /* 2062 */
  kk_drop_match(_self, {kk_box_dup(x);}, {}, _ctx)
  kk_box_drop(_b_3138, _ctx);
  return x;
}
static kk_box_t kk_std_text_parse__mlift2433_parse_eof_fun3803(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2433_parse_eof_fun3803__t* _self = kk_function_as(struct kk_std_text_parse__mlift2433_parse_eof_fun3803__t*, _fself);
  kk_box_t x = _self->x; /* 2062 */
  kk_drop_match(_self, {kk_box_dup(x);}, {}, _ctx)
  kk_unit_t x0_2550 = kk_Unit;
  kk_std_text_parse_eof(_ctx);
  if (kk_yielding(kk_context())) {
    return kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2433_parse_eof_fun3804(x, _ctx), _ctx);
  }
  {
    return x;
  }
}

kk_box_t kk_std_text_parse__mlift2433_parse_eof(kk_box_t x, kk_context_t* _ctx) { /* forall<a,e> (x : a) -> <parse|e> a */ 
  kk_ssize_t _x3801;
  kk_std_core_hnd__htag _x3802 = kk_std_core_hnd__htag_dup(kk_std_text_parse__tag_parse); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
  _x3801 = kk_std_core_hnd__evv_index(_x3802, _ctx); /*std/core/hnd/ev-index*/
  return kk_std_core_hnd__open_at0(_x3801, kk_std_text_parse__new_mlift2433_parse_eof_fun3803(x, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_parse_eof_fun3805__t {
  struct kk_function_s _base;
  kk_function_t p;
};
static kk_box_t kk_std_text_parse_parse_eof_fun3805(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_eof_fun3805(kk_function_t p, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_eof_fun3805__t* _self = kk_function_alloc_as(struct kk_std_text_parse_parse_eof_fun3805__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_parse_eof_fun3805, kk_context());
  _self->p = p;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_parse_eof_fun3807__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_parse_eof_fun3807(kk_function_t _fself, kk_box_t _x13806, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_parse_eof_fun3807(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_parse_eof_fun3807, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_parse_eof_fun3807(kk_function_t _fself, kk_box_t _x13806, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_text_parse__mlift2433_parse_eof(_x13806, _ctx);
}
static kk_box_t kk_std_text_parse_parse_eof_fun3805(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_parse_eof_fun3805__t* _self = kk_function_as(struct kk_std_text_parse_parse_eof_fun3805__t*, _fself);
  kk_function_t p = _self->p; /* () -> <std/text/parse/parse|2063> 2062 */
  kk_drop_match(_self, {kk_function_dup(p);}, {}, _ctx)
  kk_box_t x_2554 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), p, (p, _ctx)); /*2062*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_2554, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_text_parse_new_parse_eof_fun3807(_ctx), _ctx);
  }
  {
    return kk_std_text_parse__mlift2433_parse_eof(x_2554, _ctx);
  }
}

kk_std_text_parse__parse_error kk_std_text_parse_parse_eof(kk_std_core__sslice input, kk_function_t p, kk_context_t* _ctx) { /* forall<a,e> (input : sslice, p : () -> <parse|e> a) -> e parse-error<a> */ 
  return kk_std_text_parse_parse(input, kk_std_text_parse_new_parse_eof_fun3805(p, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_pnat_fun3813__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_pnat_fun3813(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pnat_fun3813(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pnat_fun3813, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_pnat_fun3815__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_pnat_fun3815(kk_function_t _fself, kk_char_t _x13814, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pnat_fun3815(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pnat_fun3815, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_pnat_fun3815(kk_function_t _fself, kk_char_t _x13814, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_digit(_x13814, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_pnat_fun3813(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3332 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_pnat_fun3815(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x3143 = _match_3332.fst;
    kk_box_t _box_x3144 = _match_3332.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x3143, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x3144, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3332, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x3145 = _match_3332.fst;
    kk_box_t _box_x3146 = _match_3332.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x3145, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3146, NULL);
    kk_box_t _x3820;
    kk_std_core_types__tuple2_ _x3821 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3820 = kk_std_core_types__tuple2__box(_x3821, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3820, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_pnat_fun3823__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_pnat_fun3823(kk_function_t _fself, kk_box_t _b_3158, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pnat_fun3823(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pnat_fun3823, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_pnat_fun3823(kk_function_t _fself, kk_box_t _b_3158, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x3824;
  kk_std_core__list _x3825 = kk_std_core__list_unbox(_b_3158, _ctx); /*list<char>*/
  _x3824 = kk_std_text_parse__mlift2434_pnat(_x3825, _ctx); /*int*/
  return kk_integer_box(_x3824);
}

kk_integer_t kk_std_text_parse_pnat(kk_context_t* _ctx) { /* () -> parse int */ 
  kk_std_core__list x_2556;
  kk_box_t _x3810;
  kk_string_t _x3811;
  kk_define_string_literal(, _s3812, 5, "digit")
  _x3811 = kk_string_dup(_s3812); /*string*/
  _x3810 = kk_std_text_parse_satisfy_fail(_x3811, kk_std_text_parse_new_pnat_fun3813(_ctx), _ctx); /*547*/
  x_2556 = kk_std_core__list_unbox(_x3810, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2556, _ctx);
    kk_box_t _x3822 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_pnat_fun3823(_ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x3822);
  }
  {
    kk_string_t _x3826 = kk_std_core_string_2(x_2556, _ctx); /*string*/
    kk_std_core_types__optional _x3827 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(0)), _ctx); /*optional<1035>*/
    return kk_std_core_parse_int_default(_x3826, _x3827, kk_std_core_types__new_None(_ctx), _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_sign_fun3829__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_sign_fun3829(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_sign_fun3829(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_sign_fun3829, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_sign_fun3832__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_sign_fun3832(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_sign_fun3832(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_sign_fun3832, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_sign_fun3832(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3330 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3330)) {
    kk_box_t _box_x3162 = _match_3330._cons.Just.value;
    kk_std_core_types__tuple2_ _pat00 = kk_std_core_types__tuple2__unbox(_box_x3162, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat00)) {
      kk_box_t _box_x3163 = _pat00.fst;
      kk_box_t _box_x3164 = _pat00.snd;
      kk_char_t c = kk_char_unbox(_box_x3163, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3164, NULL);
      kk_string_t _x3836;
      kk_define_string_literal(, _s3837, 2, "+-")
      _x3836 = kk_string_dup(_s3837); /*string*/
      kk_string_t _x3838 = kk_std_core_string(c, _ctx); /*string*/
      if (kk_string_contains(_x3836,_x3838,kk_context())) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3330, _ctx);
        kk_box_t _x3839;
        kk_std_core_types__tuple2_ _x3840 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3839 = kk_std_core_types__tuple2__box(_x3840, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3839, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3330, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
static kk_box_t kk_std_text_parse_sign_fun3829(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3830;
  kk_define_string_literal(, _s3831, 2, "+-")
  _x3830 = kk_string_dup(_s3831); /*string*/
  return kk_std_text_parse_satisfy_fail(_x3830, kk_std_text_parse_new_sign_fun3832(_ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_sign_fun3841__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_sign_fun3841(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_sign_fun3841(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_sign_fun3841, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_sign_fun3841(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_char_box('+', _ctx);
}


// lift anonymous function
struct kk_std_text_parse_sign_fun3843__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_sign_fun3843(kk_function_t _fself, kk_box_t _b_3180, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_sign_fun3843(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_sign_fun3843, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_sign_fun3843(kk_function_t _fself, kk_box_t _b_3180, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x3844;
  kk_char_t _x3845 = kk_char_unbox(_b_3180, _ctx); /*char*/
  _x3844 = kk_std_text_parse__mlift2435_sign(_x3845, _ctx); /*bool*/
  return kk_bool_box(_x3844);
}

bool kk_std_text_parse_sign(kk_context_t* _ctx) { /* () -> parse bool */ 
  kk_char_t x_2559;
  kk_box_t _x3828 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_text_parse_new_sign_fun3829(_ctx), kk_std_text_parse_new_sign_fun3841(_ctx), _ctx); /*948*/
  x_2559 = kk_char_unbox(_x3828, _ctx); /*char*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x3842 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_sign_fun3843(_ctx), _ctx); /*1002*/
    return kk_bool_unbox(_x3842);
  }
  {
    return (x_2559 == ('-'));
  }
}
extern kk_box_t kk_std_text_parse__mlift2436_pint_fun3847(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2436_pint_fun3847__t* _self = kk_function_as(struct kk_std_text_parse__mlift2436_pint_fun3847__t*, _fself);
  kk_integer_t i = _self->i; /* int */
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {kk_integer_dup(i);;}, {}, _ctx)
  kk_integer_t _x3848;
  if (neg) {
    _x3848 = kk_integer_neg(i,kk_context()); /*int*/
  }
  else {
    _x3848 = i; /*int*/
  }
  return kk_integer_box(_x3848);
}
 
// monadic lift


// lift anonymous function
struct kk_std_text_parse__mlift2437_pint_fun3849__t {
  struct kk_function_s _base;
  bool neg;
};
static kk_integer_t kk_std_text_parse__mlift2437_pint_fun3849(kk_function_t _fself, kk_integer_t i, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2437_pint_fun3849(bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3849__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2437_pint_fun3849__t, 1, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2437_pint_fun3849, kk_context());
  _self->neg = neg;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse__mlift2437_pint_fun3851__t {
  struct kk_function_s _base;
  kk_integer_t i;
  bool neg;
};
static kk_box_t kk_std_text_parse__mlift2437_pint_fun3851(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2437_pint_fun3851(kk_integer_t i, bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3851__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2437_pint_fun3851__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2437_pint_fun3851, kk_context());
  _self->i = i;
  _self->neg = neg;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2437_pint_fun3851(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3851__t* _self = kk_function_as(struct kk_std_text_parse__mlift2437_pint_fun3851__t*, _fself);
  kk_integer_t i = _self->i; /* int */
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {kk_integer_dup(i);;}, {}, _ctx)
  kk_integer_t _x3852;
  if (neg) {
    _x3852 = kk_integer_neg(i,kk_context()); /*int*/
  }
  else {
    _x3852 = i; /*int*/
  }
  return kk_integer_box(_x3852);
}
static kk_integer_t kk_std_text_parse__mlift2437_pint_fun3849(kk_function_t _fself, kk_integer_t i, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3849__t* _self = kk_function_as(struct kk_std_text_parse__mlift2437_pint_fun3849__t*, _fself);
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {;}, {}, _ctx)
  kk_box_t _x3850 = kk_std_core_hnd__open_none0(kk_std_text_parse__new_mlift2437_pint_fun3851(i, neg, _ctx), _ctx); /*1001*/
  return kk_integer_unbox(_x3850);
}


// lift anonymous function
struct kk_std_text_parse__mlift2437_pint_fun3854__t {
  struct kk_function_s _base;
  kk_function_t next_2563;
};
static kk_box_t kk_std_text_parse__mlift2437_pint_fun3854(kk_function_t _fself, kk_box_t _b_3187, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse__new_mlift2437_pint_fun3854(kk_function_t next_2563, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3854__t* _self = kk_function_alloc_as(struct kk_std_text_parse__mlift2437_pint_fun3854__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse__mlift2437_pint_fun3854, kk_context());
  _self->next_2563 = next_2563;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse__mlift2437_pint_fun3854(kk_function_t _fself, kk_box_t _b_3187, kk_context_t* _ctx) {
  struct kk_std_text_parse__mlift2437_pint_fun3854__t* _self = kk_function_as(struct kk_std_text_parse__mlift2437_pint_fun3854__t*, _fself);
  kk_function_t next_2563 = _self->next_2563; /* (int) -> std/text/parse/parse int */
  kk_drop_match(_self, {kk_function_dup(next_2563);}, {}, _ctx)
  kk_integer_t _x3855;
  kk_integer_t _x3856 = kk_integer_unbox(_b_3187); /*int*/
  _x3855 = kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next_2563, (next_2563, _x3856, _ctx)); /*int*/
  return kk_integer_box(_x3855);
}

kk_integer_t kk_std_text_parse__mlift2437_pint(kk_char_t c0, kk_context_t* _ctx) { /* (c0 : char) -> parse int */ 
  bool neg = (c0 == ('-')); /*bool*/;
  kk_integer_t x_2562 = kk_std_text_parse_pnat(_ctx); /*int*/;
  kk_function_t next_2563 = kk_std_text_parse__new_mlift2437_pint_fun3849(neg, _ctx); /*(int) -> std/text/parse/parse int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_2562, _ctx);
    kk_box_t _x3853 = kk_std_core_hnd_yield_extend(kk_std_text_parse__new_mlift2437_pint_fun3854(next_2563, _ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x3853);
  }
  {
    return kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next_2563, (next_2563, x_2562, _ctx));
  }
}


// lift anonymous function
struct kk_std_text_parse_pint_fun3858__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_pint_fun3858(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3858(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pint_fun3858, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_pint_fun3861__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_pint_fun3861(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3861(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pint_fun3861, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_pint_fun3861(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3327 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3327)) {
    kk_box_t _box_x3189 = _match_3327._cons.Just.value;
    kk_std_core_types__tuple2_ _pat00 = kk_std_core_types__tuple2__unbox(_box_x3189, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat00)) {
      kk_box_t _box_x3190 = _pat00.fst;
      kk_box_t _box_x3191 = _pat00.snd;
      kk_char_t c = kk_char_unbox(_box_x3190, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3191, NULL);
      kk_string_t _x3865;
      kk_define_string_literal(, _s3866, 2, "+-")
      _x3865 = kk_string_dup(_s3866); /*string*/
      kk_string_t _x3867 = kk_std_core_string(c, _ctx); /*string*/
      if (kk_string_contains(_x3865,_x3867,kk_context())) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3327, _ctx);
        kk_box_t _x3868;
        kk_std_core_types__tuple2_ _x3869 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3868 = kk_std_core_types__tuple2__box(_x3869, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3868, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3327, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
static kk_box_t kk_std_text_parse_pint_fun3858(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3859;
  kk_define_string_literal(, _s3860, 2, "+-")
  _x3859 = kk_string_dup(_s3860); /*string*/
  return kk_std_text_parse_satisfy_fail(_x3859, kk_std_text_parse_new_pint_fun3861(_ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_pint_fun3870__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_pint_fun3870(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3870(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pint_fun3870, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_pint_fun3870(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_char_box('+', _ctx);
}


// lift anonymous function
struct kk_std_text_parse_pint_fun3872__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_pint_fun3872(kk_function_t _fself, kk_box_t _b_3207, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3872(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_pint_fun3872, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_pint_fun3872(kk_function_t _fself, kk_box_t _b_3207, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x3873;
  kk_char_t _x3874 = kk_char_unbox(_b_3207, _ctx); /*char*/
  _x3873 = kk_std_text_parse__mlift2437_pint(_x3874, _ctx); /*int*/
  return kk_integer_box(_x3873);
}


// lift anonymous function
struct kk_std_text_parse_pint_fun3876__t {
  struct kk_function_s _base;
  bool neg;
};
static kk_box_t kk_std_text_parse_pint_fun3876(kk_function_t _fself, kk_box_t _b_3210, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3876(bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3876__t* _self = kk_function_alloc_as(struct kk_std_text_parse_pint_fun3876__t, 1, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_pint_fun3876, kk_context());
  _self->neg = neg;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_text_parse_pint_fun3877__t {
  struct kk_function_s _base;
  kk_box_t _b_3210;
  bool neg;
};
static kk_box_t kk_std_text_parse_pint_fun3877(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3877(kk_box_t _b_3210, bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3877__t* _self = kk_function_alloc_as(struct kk_std_text_parse_pint_fun3877__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_pint_fun3877, kk_context());
  _self->_b_3210 = _b_3210;
  _self->neg = neg;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_pint_fun3877(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3877__t* _self = kk_function_as(struct kk_std_text_parse_pint_fun3877__t*, _fself);
  kk_box_t _b_3210 = _self->_b_3210; /* 1001 */
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {kk_box_dup(_b_3210);;}, {}, _ctx)
  kk_integer_t _x3878;
  if (neg) {
    kk_integer_t _x3879 = kk_integer_unbox(_b_3210); /*int*/
    _x3878 = kk_integer_neg(_x3879,kk_context()); /*int*/
  }
  else {
    _x3878 = kk_integer_unbox(_b_3210); /*int*/
  }
  return kk_integer_box(_x3878);
}
static kk_box_t kk_std_text_parse_pint_fun3876(kk_function_t _fself, kk_box_t _b_3210, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3876__t* _self = kk_function_as(struct kk_std_text_parse_pint_fun3876__t*, _fself);
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {;}, {}, _ctx)
  return kk_std_core_hnd__open_none0(kk_std_text_parse_new_pint_fun3877(_b_3210, neg, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_text_parse_pint_fun3880__t {
  struct kk_function_s _base;
  kk_integer_t x0_2569;
  bool neg;
};
static kk_box_t kk_std_text_parse_pint_fun3880(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pint_fun3880(kk_integer_t x0_2569, bool neg, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3880__t* _self = kk_function_alloc_as(struct kk_std_text_parse_pint_fun3880__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_pint_fun3880, kk_context());
  _self->x0_2569 = x0_2569;
  _self->neg = neg;
  return &_self->_base;
}

static kk_box_t kk_std_text_parse_pint_fun3880(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_text_parse_pint_fun3880__t* _self = kk_function_as(struct kk_std_text_parse_pint_fun3880__t*, _fself);
  kk_integer_t x0_2569 = _self->x0_2569; /* int */
  bool neg = _self->neg; /* bool */
  kk_drop_match(_self, {kk_integer_dup(x0_2569);;}, {}, _ctx)
  kk_integer_t _x3881;
  if (neg) {
    _x3881 = kk_integer_neg(x0_2569,kk_context()); /*int*/
  }
  else {
    _x3881 = x0_2569; /*int*/
  }
  return kk_integer_box(_x3881);
}

kk_integer_t kk_std_text_parse_pint(kk_context_t* _ctx) { /* () -> parse int */ 
  kk_char_t x_2566;
  kk_box_t _x3857 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_text_parse_new_pint_fun3858(_ctx), kk_std_text_parse_new_pint_fun3870(_ctx), _ctx); /*948*/
  x_2566 = kk_char_unbox(_x3857, _ctx); /*char*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x3871 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_pint_fun3872(_ctx), _ctx); /*1002*/
    return kk_integer_unbox(_x3871);
  }
  {
    bool neg = (x_2566 == ('-')); /*bool*/;
    kk_integer_t x0_2569 = kk_std_text_parse_pnat(_ctx); /*int*/;
    kk_box_t _x3875;
    if (kk_yielding(kk_context())) {
      kk_integer_drop(x0_2569, _ctx);
      _x3875 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_pint_fun3876(neg, _ctx), _ctx); /*1002*/
    }
    else {
      _x3875 = kk_std_core_hnd__open_none0(kk_std_text_parse_new_pint_fun3880(x0_2569, neg, _ctx), _ctx); /*1002*/
    }
    return kk_integer_unbox(_x3875);
  }
}


// lift anonymous function
struct kk_std_text_parse_pstring_fun3884__t {
  struct kk_function_s _base;
  kk_string_t s;
};
static kk_std_core_types__maybe kk_std_text_parse_pstring_fun3884(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_pstring_fun3884(kk_string_t s, kk_context_t* _ctx) {
  struct kk_std_text_parse_pstring_fun3884__t* _self = kk_function_alloc_as(struct kk_std_text_parse_pstring_fun3884__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_text_parse_pstring_fun3884, kk_context());
  _self->s = s;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_text_parse_pstring_fun3884(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  struct kk_std_text_parse_pstring_fun3884__t* _self = kk_function_as(struct kk_std_text_parse_pstring_fun3884__t*, _fself);
  kk_string_t s = _self->s; /* string */
  kk_drop_match(_self, {kk_string_dup(s);}, {}, _ctx)
  kk_std_core_types__maybe _match_3324;
  kk_std_core__list _x3885;
  kk_string_t _x3886 = kk_string_dup(s); /*string*/
  _x3885 = kk_std_core_list_4(_x3886, _ctx); /*list<char>*/
  _match_3324 = kk_std_text_parse_next_match(slice, _x3885, _ctx); /*maybe<sslice>*/
  if (kk_std_core_types__is_Just(_match_3324)) {
    kk_box_t _box_x3217 = _match_3324._cons.Just.value;
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3217, NULL);
    kk_box_t _x3888;
    kk_std_core_types__tuple2_ _x3889 = kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(s), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3888 = kk_std_core_types__tuple2__box(_x3889, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3888, _ctx);
  }
  {
    kk_string_drop(s, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_string_t kk_std_text_parse_pstring(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> parse string */ 
  kk_box_t _x3882;
  kk_string_t _x3883 = kk_string_dup(s); /*string*/
  _x3882 = kk_std_text_parse_satisfy_fail(_x3883, kk_std_text_parse_new_pstring_fun3884(s, _ctx), _ctx); /*547*/
  return kk_string_unbox(_x3882);
}

kk_std_core_types__maybe kk_std_text_parse_starts_with(kk_string_t s, kk_function_t p, kk_context_t* _ctx) { /* forall<a> (s : string, p : () -> parse a) -> maybe<(a, sslice)> */ 
  kk_std_text_parse__parse_error _match_3323;
  kk_std_core__sslice _x3890;
  kk_string_t _x3891 = kk_string_dup(s); /*string*/
  kk_ssize_t _x3892 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x3893 = kk_string_len(s,kk_context()); /*ssize_t*/
  _x3890 = kk_std_core__new_Sslice(_x3891, _x3892, _x3893, _ctx); /*sslice*/
  _match_3323 = kk_std_text_parse_parse(_x3890, p, _ctx); /*std/text/parse/parse-error<2037>*/
  if (kk_std_text_parse__is_ParseOk(_match_3323)) {
    struct kk_std_text_parse_ParseOk* _con3894 = kk_std_text_parse__as_ParseOk(_match_3323);
    kk_box_t x = _con3894->result;
    kk_std_core__sslice rest0 = _con3894->rest;
    if (kk_likely(kk_std_text_parse__parse_error_is_unique(_match_3323))) {
      kk_std_text_parse__parse_error_free(_match_3323);
    }
    else {
      kk_std_core__sslice_dup(rest0);
      kk_box_dup(x);
      kk_std_text_parse__parse_error_decref(_match_3323, _ctx);
    }
    kk_box_t _x3895;
    kk_std_core_types__tuple2_ _x3896 = kk_std_core_types__new_dash__lp__comma__rp_(x, kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3895 = kk_std_core_types__tuple2__box(_x3896, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3895, _ctx);
  }
  {
    kk_std_text_parse__parse_error_drop(_match_3323, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_white_fun3900__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_white_fun3900(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_white_fun3900(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_white_fun3900, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_text_parse_white_fun3900(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _match_3322 = kk_std_core_next(slice, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Just(_match_3322)) {
    kk_box_t _box_x3234 = _match_3322._cons.Just.value;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x3234, NULL);
    if (kk_std_core_types__is_dash__lp__comma__rp_(_pat0)) {
      kk_box_t _box_x3235 = _pat0.fst;
      kk_box_t _box_x3236 = _pat0.snd;
      kk_char_t c = kk_char_unbox(_box_x3235, NULL);
      kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3236, NULL);
      if (kk_std_core_is_white(c, _ctx)) {
        kk_std_core__sslice_dup(rest0);
        kk_std_core_types__maybe_drop(_match_3322, _ctx);
        kk_box_t _x3904;
        kk_std_core_types__tuple2_ _x3905 = kk_std_core_types__new_dash__lp__comma__rp_(kk_char_box(c, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
        _x3904 = kk_std_core_types__tuple2__box(_x3905, _ctx); /*1034*/
        return kk_std_core_types__new_Just(_x3904, _ctx);
      }
    }
  }
  {
    kk_std_core_types__maybe_drop(_match_3322, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_char_t kk_std_text_parse_white(kk_context_t* _ctx) { /* () -> parse char */ 
  kk_box_t _x3897;
  kk_string_t _x3898 = kk_string_empty(); /*string*/
  _x3897 = kk_std_text_parse_satisfy_fail(_x3898, kk_std_text_parse_new_white_fun3900(_ctx), _ctx); /*547*/
  return kk_char_unbox(_x3897, _ctx);
}


// lift anonymous function
struct kk_std_text_parse_whitespace_fun3909__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_whitespace_fun3909(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace_fun3909(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace_fun3909, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_whitespace_fun3911__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_whitespace_fun3911(kk_function_t _fself, kk_char_t _x13910, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace_fun3911(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace_fun3911, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_whitespace_fun3911(kk_function_t _fself, kk_char_t _x13910, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_white(_x13910, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_whitespace_fun3909(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3321 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_whitespace_fun3911(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x3247 = _match_3321.fst;
    kk_box_t _box_x3248 = _match_3321.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x3247, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x3248, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3321, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x3249 = _match_3321.fst;
    kk_box_t _box_x3250 = _match_3321.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x3249, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3250, NULL);
    kk_box_t _x3916;
    kk_std_core_types__tuple2_ _x3917 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3916 = kk_std_core_types__tuple2__box(_x3917, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3916, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_whitespace_fun3919__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_whitespace_fun3919(kk_function_t _fself, kk_box_t _b_3262, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace_fun3919(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace_fun3919, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_whitespace_fun3919(kk_function_t _fself, kk_box_t _b_3262, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3920;
  kk_std_core__list _x3921 = kk_std_core__list_unbox(_b_3262, _ctx); /*list<char>*/
  _x3920 = kk_std_core_string_2(_x3921, _ctx); /*string*/
  return kk_string_box(_x3920);
}

kk_string_t kk_std_text_parse_whitespace(kk_context_t* _ctx) { /* () -> parse string */ 
  kk_std_core__list x_2574;
  kk_box_t _x3906;
  kk_string_t _x3907 = kk_string_empty(); /*string*/
  _x3906 = kk_std_text_parse_satisfy_fail(_x3907, kk_std_text_parse_new_whitespace_fun3909(_ctx), _ctx); /*547*/
  x_2574 = kk_std_core__list_unbox(_x3906, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2574, _ctx);
    kk_box_t _x3918 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_whitespace_fun3919(_ctx), _ctx); /*1002*/
    return kk_string_unbox(_x3918);
  }
  {
    return kk_std_core_string_2(x_2574, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_whitespace0_fun3923__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_whitespace0_fun3923(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace0_fun3923(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace0_fun3923, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_whitespace0_fun3928__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_text_parse_whitespace0_fun3928(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace0_fun3928(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace0_fun3928, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_text_parse_whitespace0_fun3930__t {
  struct kk_function_s _base;
};
static bool kk_std_text_parse_whitespace0_fun3930(kk_function_t _fself, kk_char_t _x13929, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace0_fun3930(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace0_fun3930, _ctx)
  return kk_function_dup(_fself);
}

static bool kk_std_text_parse_whitespace0_fun3930(kk_function_t _fself, kk_char_t _x13929, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_is_white(_x13929, _ctx);
}
static kk_std_core_types__maybe kk_std_text_parse_whitespace0_fun3928(kk_function_t _fself, kk_std_core__sslice slice, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__tuple2_ _match_3319 = kk_std_text_parse_next_while0(slice, kk_std_text_parse_new_whitespace0_fun3930(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*(list<char>, sslice)*/;
  {
    kk_box_t _box_x3264 = _match_3319.fst;
    kk_box_t _box_x3265 = _match_3319.snd;
    kk_std_core__list _pat00 = kk_std_core__list_unbox(_box_x3264, NULL);
    kk_std_core__sslice _pat10 = kk_std_core__sslice_unbox(_box_x3265, NULL);
    if (kk_std_core__is_Nil(_pat00)) {
      kk_std_core_types__tuple2__drop(_match_3319, _ctx);
      return kk_std_core_types__new_Nothing(_ctx);
    }
  }
  {
    kk_box_t _box_x3266 = _match_3319.fst;
    kk_box_t _box_x3267 = _match_3319.snd;
    kk_std_core__list xs = kk_std_core__list_unbox(_box_x3266, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x3267, NULL);
    kk_box_t _x3935;
    kk_std_core_types__tuple2_ _x3936 = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(xs, _ctx), kk_std_core__sslice_box(rest0, _ctx), _ctx); /*(1004, 1005)*/
    _x3935 = kk_std_core_types__tuple2__box(_x3936, _ctx); /*1034*/
    return kk_std_core_types__new_Just(_x3935, _ctx);
  }
}


// lift anonymous function
struct kk_std_text_parse_whitespace0_fun3938__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_whitespace0_fun3938(kk_function_t _fself, kk_box_t _b_3279, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace0_fun3938(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace0_fun3938, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_whitespace0_fun3938(kk_function_t _fself, kk_box_t _b_3279, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3939;
  kk_std_core__list _x3940 = kk_std_core__list_unbox(_b_3279, _ctx); /*list<char>*/
  _x3939 = kk_std_core_string_2(_x3940, _ctx); /*string*/
  return kk_string_box(_x3939);
}
static kk_box_t kk_std_text_parse_whitespace0_fun3923(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3924;
  kk_std_core__list x_2576;
  kk_box_t _x3925;
  kk_string_t _x3926 = kk_string_empty(); /*string*/
  _x3925 = kk_std_text_parse_satisfy_fail(_x3926, kk_std_text_parse_new_whitespace0_fun3928(_ctx), _ctx); /*547*/
  x_2576 = kk_std_core__list_unbox(_x3925, _ctx); /*list<char>*/
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_2576, _ctx);
    kk_box_t _x3937 = kk_std_core_hnd_yield_extend(kk_std_text_parse_new_whitespace0_fun3938(_ctx), _ctx); /*1002*/
    _x3924 = kk_string_unbox(_x3937); /*string*/
  }
  else {
    _x3924 = kk_std_core_string_2(x_2576, _ctx); /*string*/
  }
  return kk_string_box(_x3924);
}


// lift anonymous function
struct kk_std_text_parse_whitespace0_fun3941__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_text_parse_whitespace0_fun3941(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_text_parse_new_whitespace0_fun3941(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_text_parse_whitespace0_fun3941, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_text_parse_whitespace0_fun3941(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x3942 = kk_string_empty(); /*string*/
  return kk_string_box(_x3942);
}

kk_string_t kk_std_text_parse_whitespace0(kk_context_t* _ctx) { /* () -> parse string */ 
  kk_box_t _x3922 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_text_parse_new_whitespace0_fun3923(_ctx), kk_std_text_parse_new_whitespace0_fun3941(_ctx), _ctx); /*948*/
  return kk_string_unbox(_x3922);
}

// initialization
void kk_std_text_parse__init(kk_context_t* _ctx){
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
    kk_string_t _x3395;
    kk_define_string_literal(, _s3396, 11, "parse.parse")
    _x3395 = kk_string_dup(_s3396); /*string*/
    kk_std_text_parse__tag_parse = kk_std_core_hnd__new_Htag(_x3395, _ctx); /*std/core/hnd/htag<std/text/parse/.hnd-parse>*/
  }
}

// termination
void kk_std_text_parse__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_core_hnd__htag_drop(kk_std_text_parse__tag_parse, _ctx);
  kk_std_core__done(_ctx);
  kk_std_core_hnd__done(_ctx);
  kk_std_core_types__done(_ctx);
}
