// Koka generated module: "std/core", koka version: 2.3.2, platform: 64-bit
#include "std_core.h"
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_std_core__list kk_vector_to_list(kk_vector_t v, kk_std_core__list tail, kk_context_t* ctx) {
  // todo: avoid boxed_dup if v is unique
  kk_ssize_t n;
  kk_box_t* p = kk_vector_buf_borrow(v, &n);
  if (n <= 0) {
    kk_vector_drop(v,ctx);
    return tail;
  }
  kk_std_core__list nil  = kk_std_core__new_Nil(ctx);
  struct kk_std_core_Cons* cons = NULL;
  kk_std_core__list list = kk_std_core__new_Nil(ctx);
  for( kk_ssize_t i = 0; i < n; i++ ) {
    kk_std_core__list hd = kk_std_core__new_Cons(kk_reuse_null,kk_box_dup(p[i]), nil, ctx);
    if (cons==NULL) {
      list = hd;
    }
    else {
      cons->tail = hd;
    }
    cons = kk_std_core__as_Cons(hd);
  }
  if (cons == NULL) { list = tail; } 
               else { cons->tail = tail; }
  kk_vector_drop(v,ctx);
  return list;
}

kk_vector_t kk_list_to_vector(kk_std_core__list xs, kk_context_t* ctx) {
  // todo: avoid boxed_dup if xs is unique
  // find the length
  kk_ssize_t len = 0;
  kk_std_core__list ys = xs;
  while (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(ys);
    len++;
    ys = cons->tail;
  }
  // alloc the vector and copy
  kk_box_t* p;
  kk_vector_t v = kk_vector_alloc_uninit(len, &p, ctx);  
  ys = xs;
  for( kk_ssize_t i = 0; i < len; i++) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(ys);
    ys = cons->tail;
    p[i] = kk_box_dup(cons->head);
  }
  kk_std_core__list_drop(xs,ctx);  // todo: drop while visiting?
  return v;
}

kk_vector_t kk_vector_init( kk_ssize_t n, kk_function_t init, kk_context_t* ctx) {
  kk_box_t* p;
  kk_vector_t v = kk_vector_alloc_uninit(n, &p, ctx);  
  for(kk_ssize_t i = 0; i < n; i++) {
    kk_function_dup(init);
    p[i] = kk_function_call(kk_box_t,(kk_function_t,kk_ssize_t,kk_context_t*),init,(init,i,ctx));
  }
  kk_function_drop(init,ctx);
  return v;
}

kk_box_t kk_main_console( kk_function_t action, kk_context_t* ctx ) {
  return kk_function_call(kk_box_t,(kk_function_t,kk_unit_t,kk_context_t*),action,(action,kk_Unit,ctx));
}


kk_std_core__list kk_string_to_list(kk_string_t s, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* p = kk_string_buf_borrow(s,&len);
  const uint8_t* const end = p + len;
  kk_std_core__list nil  = kk_std_core__new_Nil(ctx);
  kk_std_core__list list = nil;
  struct kk_std_core_Cons* tl = NULL;
  kk_ssize_t count;
  while( p < end ) {
    kk_char_t c = kk_utf8_read(p,&count);
    p += count;
    kk_std_core__list cons = kk_std_core__new_Cons(kk_reuse_null,kk_char_box(c,ctx), nil, ctx);
    if (tl!=NULL) {
      tl->tail = cons;
    }
    else {
      list = cons;
    }
    tl = kk_std_core__as_Cons(cons);
  }
  kk_string_drop(s,ctx);
  return list;
}

kk_string_t kk_string_from_list(kk_std_core__list cs, kk_context_t* ctx) {
  // TODO: optimize for short strings to write directly into a local buffer?
  // find total UTF8 length
  kk_ssize_t len = 0;
  kk_std_core__list xs = cs;
  while (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(xs);
    len += kk_utf8_len(kk_char_unbox(cons->head,ctx));
    xs = cons->tail;
  }
  // allocate and copy the characters
  uint8_t* p;
  kk_string_t s = kk_unsafe_string_alloc_buf(len,&p,ctx);  // must be initialized
  xs = cs;
  while (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(xs);
    kk_ssize_t count;
    kk_utf8_write( kk_char_unbox(cons->head,ctx), p, &count );
    p += count;
    xs = cons->tail;
  }
  kk_assert_internal(*p == 0 && (p - kk_string_buf_borrow(s,NULL)) == len);
  kk_std_core__list_drop(cs,ctx);  // todo: drop while visiting?
  return s;
}

static inline void kk_sslice_start_end_borrowx( kk_std_core__sslice sslice, const uint8_t** start, const uint8_t** end, const uint8_t** sstart, const uint8_t** send) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(sslice.str,&slen);
  *start = s + sslice.start;
  *end = s + sslice.start + sslice.len;
  if (sstart != NULL) *sstart = s;
  if (send != NULL) *send = s + slen;
  kk_assert_internal(*start >= s && *start <= *end);
  kk_assert_internal(*end >= *start && *end <= s + slen);
}

static inline void kk_sslice_start_end_borrow( kk_std_core__sslice sslice, const uint8_t** start, const uint8_t** end) {
  kk_sslice_start_end_borrowx(sslice,start,end,NULL,NULL);
}

kk_integer_t kk_slice_count( kk_std_core__sslice sslice, kk_context_t* ctx ) {
  // TODO: optimize this by extending kk_string_count
  const uint8_t* start;
  const uint8_t* end;
  kk_sslice_start_end_borrow(sslice, &start, &end);
  kk_ssize_t count = 0;
  while( start < end && *start != 0 ) {
    const uint8_t* next = kk_utf8_next(start);
    count++;
    start = next;
  }
  kk_std_core__sslice_drop(sslice,ctx);
  return kk_integer_from_ssize_t(count,ctx);
}

kk_string_t kk_slice_to_string( kk_std_core__sslice  sslice, kk_context_t* ctx ) {
  const uint8_t* start;
  const uint8_t* end;
  kk_sslice_start_end_borrow(sslice, &start, &end);
  // is it the full string?
  if (sslice.start == 0 && sslice.len == kk_string_len_borrow(sslice.str)) {
    // TODO: drop sslice and dup sslice.str?
    return sslice.str;
  }
  else {
    // if not, we copy len bytes
    kk_string_t s = kk_string_alloc_dupn_valid_utf8(sslice.len, start, ctx);
    kk_std_core__sslice_drop(sslice,ctx);
    return s;
  }
}

kk_std_core__sslice kk_slice_first( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen);
  const uint8_t* next = (slen > 0 ? kk_utf8_next(s) : s);
  return kk_std_core__new_Sslice(str, 0, (next - s), ctx);
}

kk_std_core__sslice kk_slice_last( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen);
  const uint8_t* end = s + slen;
  const uint8_t* prev = (s==end ? s : kk_utf8_prev(end));
  return kk_std_core__new_Sslice(str, (prev - s), (end - prev), ctx);
}

kk_std_core__sslice kk_slice_between( struct kk_std_core_Sslice slice1, struct kk_std_core_Sslice slice2, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow( slice1.str, NULL );
  const uint8_t* s2 = kk_string_buf_borrow( slice2.str, NULL );
  if (s1 != s2) {
    kk_info_message("between: not equal slices: %p vs. %p\n", s1, s2);
    return kk_std_core__new_Sslice(kk_string_empty(), 0, -1, ctx); // invalid slice
  }
  kk_ssize_t start = (slice1.start <= slice2.start ? slice1.start : slice2.start);
  kk_ssize_t len   = (slice1.start <= slice2.start ? slice2.start - slice1.start : slice1.start - slice2.start);
  return kk_std_core__new_Sslice(slice1.str, start, len, ctx);
}

kk_std_core_types__maybe kk_slice_next( struct kk_std_core_Sslice slice, kk_context_t* ctx ) {
  if (slice.len <= 0) {
    kk_std_core__sslice_drop(slice,ctx);
    return kk_std_core_types__new_Nothing(ctx);
  }
  const uint8_t* start;
  const uint8_t* end;
  kk_sslice_start_end_borrow(slice, &start, &end);
  kk_ssize_t clen;
  const kk_char_t c = kk_utf8_read(start,&clen);
  kk_assert_internal(clen > 0 && clen <= slice.len);
  if (clen > slice.len) clen = slice.len;
  // TODO: specialize type to avoid boxing
  kk_std_core__sslice snext = kk_std_core__new_Sslice(slice.str, slice.start + clen, slice.len - clen, ctx);
  kk_std_core_types__tuple2_ res = kk_std_core_types__new_dash__lp__comma__rp_( kk_char_box(c,ctx), kk_std_core__sslice_box(snext,ctx), ctx);
  return kk_std_core_types__new_Just( kk_std_core_types__tuple2__box(res,ctx), ctx );
}

struct kk_std_core_Sslice kk_slice_extend( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
  kk_ssize_t cnt = kk_integer_clamp(count,ctx);
  if (cnt==0 || (slice.len <= 0 && cnt<0)) return slice;
  const uint8_t* s0;
  const uint8_t* s1;
  kk_sslice_start_end_borrow(slice,&s0,&s1);
  const uint8_t* t  = s1;
  if (cnt >= 0) {
    do {
      t = kk_utf8_next(t);
      cnt--;
    } while (cnt > 0 && *t != 0);
  }
  else {  // cnt < 0
    const uint8_t* sstart = s0 - slice.start;
    do {
      t = kk_utf8_prev(t);
      cnt++;
    } while (cnt < 0 && t > sstart);
  }
  if (t == s1) return slice;  // length is unchanged
  return kk_std_core__new_Sslice(slice.str, slice.start, (t < s0 ? 0 : (t - s0)), ctx);
}

struct kk_std_core_Sslice kk_slice_advance( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
  const kk_ssize_t cnt0 = kk_integer_clamp(count,ctx);
  kk_ssize_t cnt = cnt0;
  if (cnt==0 || (slice.start == 0 && cnt<0)) return slice;
  const uint8_t* sstart;
  const uint8_t* s0;
  const uint8_t* s1;
  const uint8_t* send;
  kk_sslice_start_end_borrowx(slice,&s0,&s1,&sstart,&send);
  // advance the start
  const uint8_t* t0  = s0;
  if (cnt >= 0) {
    do {
      t0 = kk_utf8_next(t0);
      cnt--;
    } while (cnt > 0 && t0 < send);
  }
  else {  // cnt < 0
    do {
      t0 = kk_utf8_prev(t0);
      cnt++;
    } while (cnt < 0 && t0 > sstart);
  }
  if (t0 == s0) return slice;  // start is unchanged
  // "t0" points to the new start, now advance the end by the same amount of codepoints
  const uint8_t* t1 = s1;
  cnt = cnt0;
  if (cnt >= 0) {
    do {
      t1 = kk_utf8_next(t1);
      cnt--;
    } while (cnt > 0 && t1 < send);
  }
  else {  // cnt < 0
    do {
      t1 = kk_utf8_prev(t1);
      cnt++;
    } while (cnt < 0 && t1 > sstart);
  }
  // t1 points to the new end
  kk_assert_internal(t1 >= t0);
  return kk_std_core__new_Sslice(slice.str, (t0 - sstart), (t1 - t0), ctx);
}

struct kk_std_core_Sslice kk_slice_common_prefix( kk_string_t str1, kk_string_t str2, kk_integer_t iupto, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow(str1,NULL);
  const uint8_t* s2 = kk_string_buf_borrow(str2,NULL);
  kk_ssize_t upto = kk_integer_clamp_ssize_t(iupto,ctx);
  kk_ssize_t count;
  for(count = 0; count < upto && *s1 != 0 && *s2 != 0; count++, s1++, s2++ ) {
    if (*s1 != *s2) break;
  }
  kk_string_drop(str2,ctx);
  return kk_std_core__new_Sslice(str1, 0, count, ctx);
}


kk_std_core__error kk_error_ok( kk_box_t result, kk_context_t* ctx ) {
  return kk_std_core__new_Ok( result, ctx );
}

kk_std_core__error kk_error_from_errno( int err, kk_context_t* ctx ) {  
  kk_string_t msg;
  #if defined(__GLIBC__) && !defined(WIN32) && !defined(__APPLE__) && !defined(__FreeBSD__)
    // GNU version of strerror_r
    char buf[256];
    char* serr = strerror_r(err, buf, 255); buf[255] = 0;
    msg = kk_string_alloc_from_qutf8( serr, ctx );
  #elif (/* _POSIX_C_SOURCE >= 200112L ||*/ !defined(WIN32) && (_XOPEN_SOURCE >= 600 || defined(__APPLE__) || defined(__FreeBSD__) || defined(__MUSL__)))
    // XSI version of strerror_r
    char buf[256];
    strerror_r(err, buf, 255); buf[255] = 0;
    msg = kk_string_alloc_from_qutf8( buf, ctx );
  #elif defined(_MSC_VER) || (__STDC_VERSION__ >= 201112L || __cplusplus >= 201103L)
    // MSVC, or C/C++ 11
    char buf[256];
    strerror_s(buf, 255, err); buf[255] = 0;
    msg = kk_string_alloc_from_qutf8( buf, ctx );
  #else
    // Old style
    msg = kk_string_alloc_from_qutf8( strerror(err), ctx );
  #endif
  return kk_std_core__new_Error( kk_std_core__new_Exception( msg, kk_std_core__new_ExnSystem(kk_reuse_null, kk_integer_from_int(err,ctx), ctx), ctx), ctx );  
}


kk_unit_t kk_assert_fail( kk_string_t msg, kk_context_t* ctx ) {
  kk_fatal_error(EINVAL, "assertion failed: %s\n", kk_string_cbuf_borrow(msg,NULL));
  kk_string_drop(msg,ctx);
  return kk_Unit;
}
kk_define_string_literal(, kk_std_core__tag_ExnError, 17, "std/core/ExnError")
kk_define_string_literal(, kk_std_core__tag_ExnAssert, 18, "std/core/ExnAssert")
kk_define_string_literal(, kk_std_core__tag_ExnTodo, 16, "std/core/ExnTodo")
kk_define_string_literal(, kk_std_core__tag_ExnRange, 17, "std/core/ExnRange")
kk_define_string_literal(, kk_std_core__tag_ExnPattern, 19, "std/core/ExnPattern")
kk_define_string_literal(, kk_std_core__tag_ExnSystem, 18, "std/core/ExnSystem")
kk_define_string_literal(, kk_std_core__tag_ExnInternal, 20, "std/core/ExnInternal")

kk_std_core__exception kk_std_core__copy(kk_std_core__exception _this, kk_std_core_types__optional message0, kk_std_core_types__optional info0, kk_context_t* _ctx) { /* (exception, message : optional<string>, info : optional<exception-info>) -> exception */ 
  kk_string_t _x19174;
  if (kk_std_core_types__is_Optional(message0)) {
    kk_box_t _box_x17637 = message0._cons.Optional.value;
    kk_string_t _message_1683 = kk_string_unbox(_box_x17637);
    kk_string_dup(_message_1683);
    kk_std_core_types__optional_drop(message0, _ctx);
    _x19174 = _message_1683; /*string*/
    goto _match19175;
  }
  {
    kk_string_t _x = _this.message;
    kk_string_dup(_x);
    _x19174 = _x; /*string*/
  }
  _match19175: ;
  kk_std_core__exception_info _x19177;
  if (kk_std_core_types__is_Optional(info0)) {
    kk_box_t _box_x17638 = info0._cons.Optional.value;
    kk_std_core__exception_info _info_1689 = kk_std_core__exception_info_unbox(_box_x17638, NULL);
    kk_std_core__exception_info_dup(_info_1689);
    kk_std_core_types__optional_drop(info0, _ctx);
    kk_std_core__exception_drop(_this, _ctx);
    _x19177 = _info_1689; /*exception-info*/
    goto _match19178;
  }
  {
    kk_std_core__exception_info _x0 = _this.info;
    kk_std_core__exception_info_dup(_x0);
    kk_std_core__exception_drop(_this, _ctx);
    _x19177 = _x0; /*exception-info*/
  }
  _match19178: ;
  return kk_std_core__new_Exception(_x19174, _x19177, _ctx);
}

kk_std_core__sslice kk_std_core__copy_2(kk_std_core__sslice _this, kk_std_core_types__optional str0, kk_std_core_types__optional start0, kk_std_core_types__optional len0, kk_context_t* _ctx) { /* (sslice, str : optional<string>, start : optional<ssize_t>, len : optional<ssize_t>) -> sslice */ 
  kk_string_t _x19184;
  if (kk_std_core_types__is_Optional(str0)) {
    kk_box_t _box_x17640 = str0._cons.Optional.value;
    kk_string_t _str_1821 = kk_string_unbox(_box_x17640);
    kk_string_dup(_str_1821);
    kk_std_core_types__optional_drop(str0, _ctx);
    _x19184 = _str_1821; /*string*/
    goto _match19185;
  }
  {
    kk_string_t _x = _this.str;
    kk_string_dup(_x);
    _x19184 = _x; /*string*/
  }
  _match19185: ;
  kk_ssize_t _x19187;
  if (kk_std_core_types__is_Optional(start0)) {
    kk_box_t _box_x17641 = start0._cons.Optional.value;
    kk_ssize_t _start_1827 = kk_ssize_unbox(_box_x17641, NULL);
    kk_std_core_types__optional_drop(start0, _ctx);
    _x19187 = _start_1827; /*ssize_t*/
    goto _match19188;
  }
  {
    kk_ssize_t _x0 = _this.start;
    _x19187 = _x0; /*ssize_t*/
  }
  _match19188: ;
  kk_ssize_t _x19190;
  if (kk_std_core_types__is_Optional(len0)) {
    kk_box_t _box_x17642 = len0._cons.Optional.value;
    kk_ssize_t _len_1833 = kk_ssize_unbox(_box_x17642, NULL);
    kk_std_core_types__optional_drop(len0, _ctx);
    kk_std_core__sslice_drop(_this, _ctx);
    _x19190 = _len_1833; /*ssize_t*/
    goto _match19191;
  }
  {
    kk_ssize_t _x1 = _this.len;
    kk_std_core__sslice_drop(_this, _ctx);
    _x19190 = _x1; /*ssize_t*/
  }
  _match19191: ;
  return kk_std_core__new_Sslice(_x19184, _x19187, _x19190, _ctx);
}

kk_std_core__stream kk_std_core__copy_3(kk_std_core__stream _this, kk_std_core_types__optional head0, kk_std_core_types__optional tail0, kk_context_t* _ctx) { /* forall<a> (stream<a>, head : optional<a>, tail : optional<stream<a>>) -> stream<a> */ 
  kk_box_t _x19195;
  if (kk_std_core_types__is_Optional(head0)) {
    kk_box_t _head_1879 = head0._cons.Optional.value;
    _x19195 = _head_1879; /*1896*/
  }
  else {
    struct kk_std_core_Next* _con19196 = kk_std_core__as_Next(_this);
    kk_box_t _x = _con19196->head;
    kk_box_dup(_x);
    _x19195 = _x; /*1896*/
  }
  kk_std_core__stream _x19197;
  if (kk_std_core_types__is_Optional(tail0)) {
    kk_box_t _box_x17643 = tail0._cons.Optional.value;
    kk_std_core__stream _tail_1886 = kk_std_core__stream_unbox(_box_x17643, NULL);
    kk_std_core__stream_dup(_tail_1886);
    kk_std_core_types__optional_drop(tail0, _ctx);
    kk_std_core__stream_drop(_this, _ctx);
    _x19197 = _tail_1886; /*stream<1896>*/
    goto _match19198;
  }
  {
    struct kk_std_core_Next* _con19200 = kk_std_core__as_Next(_this);
    kk_box_t _pat01 = _con19200->head;
    kk_std_core__stream _x0 = _con19200->tail;
    if (kk_likely(kk_std_core__stream_is_unique(_this))) {
      kk_box_drop(_pat01, _ctx);
      kk_std_core__stream_free(_this);
    }
    else {
      kk_std_core__stream_dup(_x0);
      kk_std_core__stream_decref(_this, _ctx);
    }
    _x19197 = _x0; /*stream<1896>*/
  }
  _match19198: ;
  return kk_std_core__new_Next(kk_reuse_null, _x19195, _x19197, _ctx);
}
 
// runtime tag for the `:exn` effect

kk_std_core_hnd__htag kk_std_core__tag_exn;
 
// handler for the `:exn` effect

kk_box_t kk_std_core__handle_exn(int32_t cfc, kk_std_core__hnd_exn hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-exn<e,b>, ret : (res : a) -> e b, action : () -> <exn|e> a) -> e b */ 
  kk_std_core_hnd__htag _x19204 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
  return kk_std_core_hnd__hhandle(_x19204, cfc, kk_std_core__hnd_exn_box(hnd, _ctx), ret, action, _ctx);
}
 
// Unsafe: transform any type to a `null` type; used internally by the compiler.

kk_std_core__null kk_std_core__null_any(kk_box_t x, kk_context_t* _ctx) { /* forall<a> (x : a) -> null<a> */ 
  return ((x).box == kk_box_null.box ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox(x));
}

kk_ref_t kk_std_core_redirect;
 
// Print a string to the console, including a final newline character.

kk_unit_t kk_std_core_xprintsln(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_println(s,kk_context()); return kk_Unit;
}

kk_integer_t kk_std_core_string_compare(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> int */ 
  return kk_string_cmp_int(x,y,kk_context());
}
 
// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.

int32_t kk_std_core_int32(kk_integer_t _arg1, kk_context_t* _ctx) { /* (int) -> int32 */ 
  return kk_integer_clamp32(_arg1,kk_context());
}
 
// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.

kk_ssize_t kk_std_core_ssize__t(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> ssize_t */ 
  return kk_integer_clamp_ssize_t(i,kk_context());
}
 
// Convert a character to a string

kk_string_t kk_std_core_string(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> string */ 
  return kk_string_from_char(c,kk_context());
}
 
// Convert a vector of characters to a string.

kk_string_t kk_std_core_string_1(kk_vector_t _arg1, kk_context_t* _ctx) { /* (vector<char>) -> string */ 
  return kk_string_from_chars(_arg1,kk_context());
}
 
// Convert a list of characters to a string

kk_string_t kk_std_core_string_2(kk_std_core__list cs, kk_context_t* _ctx) { /* (cs : list<char>) -> total string */ 
  return kk_string_from_list(cs,kk_context());
}
 
// O(n). Copy the `slice` argument into a fresh string.
// Takes O(1) time if the slice covers the entire string.

kk_string_t kk_std_core_string_3(kk_std_core__sslice slice0, kk_context_t* _ctx) { /* (slice : sslice) -> string */ 
  return kk_slice_to_string(slice0,kk_context());
}
 
// Convert a vector to a list with an optional tail.

kk_std_core__list kk_std_core_vlist(kk_vector_t v, kk_std_core_types__optional tail0, kk_context_t* _ctx) { /* forall<a> (v : vector<a>, tail : optional<list<a>>) -> list<a> */ 
  kk_std_core__list _x19207;
  if (kk_std_core_types__is_Optional(tail0)) {
    kk_box_t _box_x17657 = tail0._cons.Optional.value;
    kk_std_core__list _tail_2090 = kk_std_core__list_unbox(_box_x17657, NULL);
    kk_std_core__list_dup(_tail_2090);
    kk_std_core_types__optional_drop(tail0, _ctx);
    _x19207 = _tail_2090; /*list<2100>*/
    goto _match19208;
  }
  {
    _x19207 = kk_std_core__new_Nil(_ctx); /*list<2100>*/
  }
  _match19208: ;
  return kk_vector_to_list(v,_x19207,kk_context());
}

kk_string_t kk_std_core_int_show_hex(kk_integer_t i, bool use_capitals, kk_context_t* _ctx) { /* (i : int, use-capitals : bool) -> string */ 
  return kk_integer_to_hex_string(i,use_capitals,kk_context());
}

kk_string_t kk_std_core_repeatz(kk_string_t s, kk_ssize_t n, kk_context_t* _ctx) { /* (s : string, n : ssize_t) -> string */ 
  return kk_string_repeat(s,n,kk_context());
}

kk_string_t kk_std_core_show_expx(double d, int32_t prec, kk_context_t* _ctx) { /* (d : double, prec : int32) -> string */ 
  return kk_double_show_exp(d,prec,kk_context());
}

kk_string_t kk_std_core_show_fixedx(double d, int32_t prec, kk_context_t* _ctx) { /* (d : double, prec : int32) -> string */ 
  return kk_double_show_fixed(d,prec,kk_context());
}
 
// Print a string to the console

kk_unit_t kk_std_core_xprints(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_print(s,kk_context()); return kk_Unit;
}
 
// Raise an integer `i` to the power of `exp`.

kk_integer_t kk_std_core_pow(kk_integer_t i, kk_integer_t exp, kk_context_t* _ctx) { /* (i : int, exp : int) -> int */ 
  return kk_integer_pow(i,exp,kk_context());
}
 
// O(`count`). Advance the start position of a string slice by `count` characters
// up to the end of the string.
// A negative `count` advances the start position backwards upto the first position
// in a string.
// Maintains the character count of the original slice upto the end of the string.
// For example:
//
// * `"abc".first.advance(1).string == "b"`,
// * `"abc".first.advance(3).string == ""`,
// * `"abc".last.advance(-1).string == "b"`.
//

kk_std_core__sslice kk_std_core_advance(kk_std_core__sslice slice0, kk_integer_t count, kk_context_t* _ctx) { /* (slice : sslice, count : int) -> sslice */ 
  return kk_slice_advance(slice0,count,kk_context());
}

kk_unit_t kk_std_core_unsafe_assert_fail(kk_string_t msg, kk_context_t* _ctx) { /* (msg : string) -> () */ 
  kk_assert_fail(msg,kk_context()); return kk_Unit;
}
 
// clamp an `:int` to fit in a `:byte`.

uint8_t kk_std_core_byte(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> byte */ 
  return kk_integer_clamp_byte(i,kk_context());
}
 
// O(`count`). Extend a string slice by `count` characters up to the end of the string.
// A negative `count` shrinks the slice up to the empty slice.
// For example:
//
// * `"abc".first.extend(1).string == "ab"`
// * `"abc".last.extend(-1).string == ""`
//

kk_std_core__sslice kk_std_core_extend(kk_std_core__sslice slice0, kk_integer_t count, kk_context_t* _ctx) { /* (slice : sslice, count : int) -> sslice */ 
  return kk_slice_extend(slice0,count,kk_context());
}

kk_std_core__sslice kk_std_core_first1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> sslice */ 
  return kk_slice_first(s,kk_context());
}
 
// Convert a string to upper-case

kk_string_t kk_std_core_to_upper(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  return kk_string_to_upper(s,kk_context());
}

kk_integer_t kk_std_core_cdiv_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> int */ 
  return kk_integer_cdiv_pow10(i,n,kk_context());
}

kk_integer_t kk_std_core_mul_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> int */ 
  return kk_integer_mul_pow10(i,n,kk_context());
}
 
// Return the common prefix of two strings (upto `upto` characters (default is minimum length of the two strings))

kk_std_core__sslice kk_std_core_common_prefix(kk_string_t s, kk_string_t t, kk_std_core_types__optional upto, kk_context_t* _ctx) { /* (s : string, t : string, upto : optional<int>) -> sslice */ 
  kk_integer_t _x19210;
  if (kk_std_core_types__is_Optional(upto)) {
    kk_box_t _box_x17658 = upto._cons.Optional.value;
    kk_integer_t _upto_2194 = kk_integer_unbox(_box_x17658);
    kk_integer_dup(_upto_2194);
    kk_std_core_types__optional_drop(upto, _ctx);
    _x19210 = _upto_2194; /*int*/
    goto _match19211;
  }
  {
    _x19210 = kk_integer_from_small(-1); /*int*/
  }
  _match19211: ;
  return kk_slice_common_prefix(s,t,_x19210,kk_context());
}
 
// lifted

kk_std_core__list kk_std_core__ctail_lift16732_concat(kk_std_core__list ys, kk_std_core__list zss, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, zss : list<list<a>>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19213 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19213->head;
    kk_std_core__list yy = _con19213->tail;
    kk_reuse_t _ru_18854 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18854 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18854 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16795 = kk_std_core__list_hole(); /*list<2242>*/;
    kk_std_core__list _ctail_16796;
    if (kk_likely(_ru_18854!=NULL)) {
      struct kk_std_core_Cons* _con19214 = (struct kk_std_core_Cons*)_ru_18854;
      _con19214->tail = _ctail_16795;
      _ctail_16796 = kk_std_core__base_Cons(_con19214); /*list<2242>*/
    }
    else {
      _ctail_16796 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16795, _ctx); /*list<2242>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19215;
      kk_box_t* _b_17670_17664 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16796)->tail)); /*cfield<list<2242>>*/;
      _x19215 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16796, _ctx)),_b_17670_17664); /*ctail<0>*/
      ys = yy;
      _acc = _x19215;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zss)) {
    struct kk_std_core_Cons* _con19216 = kk_std_core__as_Cons(zss);
    kk_box_t _box_x17665 = _con19216->head;
    kk_std_core__list zzs = _con19216->tail;
    kk_std_core__list zs = kk_std_core__list_unbox(_box_x17665, NULL);
    if (kk_likely(kk_std_core__list_is_unique(zss))) {
      kk_std_core__list_free(zss);
    }
    else {
      kk_std_core__list_dup(zs);
      kk_std_core__list_dup(zzs);
      kk_std_core__list_decref(zss, _ctx);
    }
    { // tailcall
      ys = zs;
      zss = zzs;
      goto kk__tailcall;
    }
  }
  {
    kk_box_t _x19218 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19218, _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16732_concat(kk_std_core__list ys0, kk_std_core__list zss0, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, zss : list<list<a>>) -> list<a> */ 
  kk_std_core_types__ctail _x19219 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_lift16732_concat(ys0, zss0, _x19219, _ctx);
}
extern kk_box_t kk_std_core_const_fun19220_1(kk_function_t _fself, kk_box_t ___wildcard__122__7, kk_context_t* _ctx) {
  struct kk_std_core_const_fun19220__t_1* _self = kk_function_as(struct kk_std_core_const_fun19220__t_1*, _fself);
  kk_box_t default0 = _self->default0; /* 2270 */
  kk_drop_match(_self, {kk_box_dup(default0);}, {}, _ctx)
  kk_box_drop(___wildcard__122__7, _ctx);
  return default0;
}
 
// If the slice is not empty,
// return the first character, and a new slice that is advanced by 1.

kk_std_core_types__maybe kk_std_core_next(kk_std_core__sslice slice0, kk_context_t* _ctx) { /* (slice : sslice) -> maybe<(char, sslice)> */ 
  return kk_slice_next(slice0,kk_context());
}
 
// Return the number of decimal digits of `i`. Return `0` when `i==0`.

kk_integer_t kk_std_core_count_digits(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> int */ 
  return kk_integer_count_digits(i,kk_context());
}
 
// Transform an `:error` type to an `:either` value.

kk_std_core_types__either kk_std_core_either(kk_std_core__error t, kk_context_t* _ctx) { /* forall<a> (t : error<a>) -> either<exception,a> */ 
  if (kk_std_core__is_Error(t)) {
    kk_std_core__exception exn0 = t._cons.Error.exception;
    kk_std_core__exception_dup(exn0);
    kk_std_core__error_drop(t, _ctx);
    return kk_std_core_types__new_Left(kk_std_core__exception_box(exn0, _ctx), _ctx);
  }
  {
    kk_box_t x = t._cons.Ok.result;
    return kk_std_core_types__new_Right(x, _ctx);
  }
}

bool kk_std_core_xends_with(kk_string_t s, kk_string_t post, kk_context_t* _ctx) { /* (s : string, post : string) -> bool */ 
  return kk_string_ends_with(s,post,kk_context());
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17136_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list zz, kk_std_core__list ys1_16762, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e list<b>, zz : list<a>, ys1.16762 : list<b>) -> e list<b> */ 
  return kk_std_core__ctail_lift16733_flatmap(f, ys1_16762, zz, _acc, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17137_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list zz0, kk_std_core__list ys1_167620, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e list<b>, zz : list<a>, ys1.16762 : list<b>) -> e list<b> */ 
  return kk_std_core__ctailm_lift16733_flatmap(f0, ys1_167620, zz0, _accm, _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctail_lift16733_flatmap_fun19227__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list zz1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_lift16733_flatmap_fun19227(kk_function_t _fself, kk_box_t _b_17685, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_lift16733_flatmap_fun19227(kk_function_t f1, kk_std_core__list zz1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16733_flatmap_fun19227__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_lift16733_flatmap_fun19227__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_lift16733_flatmap_fun19227, kk_context());
  _self->f1 = f1;
  _self->zz1 = zz1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_lift16733_flatmap_fun19227(kk_function_t _fself, kk_box_t _b_17685, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16733_flatmap_fun19227__t* _self = kk_function_as(struct kk_std_core__ctail_lift16733_flatmap_fun19227__t*, _fself);
  kk_function_t f1 = _self->f1; /* (2419) -> 2421 list<2420> */
  kk_std_core__list zz1 = _self->zz1; /* list<2419> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<2420>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(zz1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19228;
  kk_std_core__list _x19229 = kk_std_core__list_unbox(_b_17685, _ctx); /*list<2420>*/
  _x19228 = kk_std_core__mlift17136_op(_acc0, f1, zz1, _x19229, _ctx); /*list<2420>*/
  return kk_std_core__list_box(_x19228, _ctx);
}

kk_std_core__list kk_std_core__ctail_lift16733_flatmap(kk_function_t f1, kk_std_core__list ys, kk_std_core__list zs, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19221 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19221->head;
    kk_std_core__list yy = _con19221->tail;
    kk_reuse_t _ru_18856 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18856 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18856 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16797 = kk_std_core__list_hole(); /*list<2420>*/;
    kk_std_core__list _ctail_16798;
    if (kk_likely(_ru_18856!=NULL)) {
      struct kk_std_core_Cons* _con19222 = (struct kk_std_core_Cons*)_ru_18856;
      _con19222->tail = _ctail_16797;
      _ctail_16798 = kk_std_core__base_Cons(_con19222); /*list<2420>*/
    }
    else {
      _ctail_16798 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16797, _ctx); /*list<2420>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19223;
      kk_box_t* _b_17690_17683 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16798)->tail)); /*cfield<list<2420>>*/;
      _x19223 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_16798, _ctx)),_b_17690_17683); /*ctail<0>*/
      ys = yy;
      _acc0 = _x19223;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zs)) {
    struct kk_std_core_Cons* _con19224 = kk_std_core__as_Cons(zs);
    kk_box_t z = _con19224->head;
    kk_std_core__list zz1 = _con19224->tail;
    if (kk_likely(kk_std_core__list_is_unique(zs))) {
      kk_std_core__list_free(zs);
    }
    else {
      kk_box_dup(z);
      kk_std_core__list_dup(zz1);
      kk_std_core__list_decref(zs, _ctx);
    }
    kk_std_core__list x_17207;
    kk_function_t _x19225 = kk_function_dup(f1); /*(2419) -> 2421 list<2420>*/
    x_17207 = kk_function_call(kk_std_core__list, (kk_function_t, kk_box_t, kk_context_t*), _x19225, (_x19225, z, _ctx)); /*list<2420>*/
    if (kk_yielding(kk_context())) {
      kk_std_core__list_drop(x_17207, _ctx);
      kk_box_t _x19226 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_lift16733_flatmap_fun19227(f1, zz1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19226, _ctx);
    }
    { // tailcall
      ys = x_17207;
      zs = zz1;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19230 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19230, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctailm_lift16733_flatmap_fun19233__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__ctailm_lift16733_flatmap_fun19233(kk_function_t _fself, kk_std_core__list _ctail_16800, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16733_flatmap_fun19233(kk_function_t _accm0, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19233__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19233__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16733_flatmap_fun19233, kk_context());
  _self->_accm0 = _accm0;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_lift16733_flatmap_fun19233(kk_function_t _fself, kk_std_core__list _ctail_16800, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19233__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19233__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2420>) -> list<2420> */
  kk_box_t y0 = _self->y0; /* 2420 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x19234 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16800, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19234, _ctx));
}


// lift anonymous function
struct kk_std_core__ctailm_lift16733_flatmap_fun19238__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list zz2;
};
static kk_box_t kk_std_core__ctailm_lift16733_flatmap_fun19238(kk_function_t _fself, kk_box_t _b_17699, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16733_flatmap_fun19238(kk_function_t _accm0, kk_function_t f2, kk_std_core__list zz2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19238__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19238__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16733_flatmap_fun19238, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->zz2 = zz2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_lift16733_flatmap_fun19238(kk_function_t _fself, kk_box_t _b_17699, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19238__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19238__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2420>) -> list<2420> */
  kk_function_t f2 = _self->f2; /* (2419) -> 2421 list<2420> */
  kk_std_core__list zz2 = _self->zz2; /* list<2419> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(zz2);}, {}, _ctx)
  kk_std_core__list _x19239;
  kk_std_core__list _x19240 = kk_std_core__list_unbox(_b_17699, _ctx); /*list<2420>*/
  _x19239 = kk_std_core__mlift17137_op(_accm0, f2, zz2, _x19240, _ctx); /*list<2420>*/
  return kk_std_core__list_box(_x19239, _ctx);
}

kk_std_core__list kk_std_core__ctailm_lift16733_flatmap(kk_function_t f2, kk_std_core__list ys0, kk_std_core__list zs0, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con19231 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con19231->head;
    kk_std_core__list yy0 = _con19231->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_box_dup(y0);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys0, _ctx);
    }
    { // tailcall
      kk_function_t _x19232 = kk_std_core__new_ctailm_lift16733_flatmap_fun19233(_accm0, y0, _ctx); /*(list<2420>) -> list<2420>*/
      ys0 = yy0;
      _accm0 = _x19232;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zs0)) {
    struct kk_std_core_Cons* _con19235 = kk_std_core__as_Cons(zs0);
    kk_box_t z0 = _con19235->head;
    kk_std_core__list zz2 = _con19235->tail;
    if (kk_likely(kk_std_core__list_is_unique(zs0))) {
      kk_std_core__list_free(zs0);
    }
    else {
      kk_box_dup(z0);
      kk_std_core__list_dup(zz2);
      kk_std_core__list_decref(zs0, _ctx);
    }
    kk_std_core__list x0_17210;
    kk_function_t _x19236 = kk_function_dup(f2); /*(2419) -> 2421 list<2420>*/
    x0_17210 = kk_function_call(kk_std_core__list, (kk_function_t, kk_box_t, kk_context_t*), _x19236, (_x19236, z0, _ctx)); /*list<2420>*/
    if (kk_yielding(kk_context())) {
      kk_std_core__list_drop(x0_17210, _ctx);
      kk_box_t _x19237 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_lift16733_flatmap_fun19238(_accm0, f2, zz2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19237, _ctx);
    }
    { // tailcall
      ys0 = x0_17210;
      zs0 = zz2;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16733_flatmap_fun19242__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core__lift16733_flatmap_fun19242(kk_function_t _fself, kk_std_core__list _ctail_16799, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16733_flatmap_fun19242(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__lift16733_flatmap_fun19242, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core__lift16733_flatmap_fun19242(kk_function_t _fself, kk_std_core__list _ctail_16799, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16799;
}

kk_std_core__list kk_std_core__lift16733_flatmap(kk_function_t f3, kk_std_core__list ys1, kk_std_core__list zs1, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>) -> e list<b> */ 
  bool _match_19162 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19162) {
    kk_std_core_types__ctail _x19241 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_lift16733_flatmap(f3, ys1, zs1, _x19241, _ctx);
  }
  {
    return kk_std_core__ctailm_lift16733_flatmap(f3, ys1, zs1, kk_std_core__new_lift16733_flatmap_fun19242(_ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16734_reverse_append(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19243 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con19243->head;
    kk_std_core__list xx = _con19243->tail;
    kk_reuse_t _ru_18860 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18860 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18860 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x19244;
      if (kk_likely(_ru_18860!=NULL)) {
        struct kk_std_core_Cons* _con19245 = (struct kk_std_core_Cons*)_ru_18860;
        _con19245->tail = acc;
        _x19244 = kk_std_core__base_Cons(_con19245); /*list<61>*/
      }
      else {
        _x19244 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x19244;
      ys = xx;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17139_force_fun19249__t {
  struct kk_function_s _base;
  kk_ref_t r;
};
static kk_box_t kk_std_core__mlift17139_force_fun19249(kk_function_t _fself, kk_box_t x0, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17139_force_fun19249(kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core__mlift17139_force_fun19249__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17139_force_fun19249__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17139_force_fun19249, kk_context());
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17139_force_fun19249(kk_function_t _fself, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17139_force_fun19249__t* _self = kk_function_as(struct kk_std_core__mlift17139_force_fun19249__t*, _fself);
  kk_ref_t r = _self->r; /* ref<global,either<() -> 2556 2555,2555>> */
  kk_drop_match(_self, {kk_ref_dup(r);}, {}, _ctx)
  kk_unit_t __ = kk_Unit;
  kk_box_t _x19250;
  kk_std_core_types__either _x19251;
  kk_box_t _x19252 = kk_box_dup(x0); /*2555*/
  _x19251 = kk_std_core_types__new_Right(_x19252, _ctx); /*either<72,73>*/
  _x19250 = kk_std_core_types__either_box(_x19251, _ctx); /*171*/
  kk_ref_set(r,_x19250,kk_context());
  return x0;
}

kk_box_t kk_std_core__mlift17139_force(kk_ref_t r, kk_function_t _y_16871, kk_context_t* _ctx) { /* forall<a,e> (r : ref<global,either<() -> e a,a>>, () -> <st<global>,div|e> a) -> <alloc<global>,div,read<global>,write<global>|e> a */ 
  kk_box_t x_17213 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _y_16871, (_y_16871, _ctx)); /*2555*/;
  kk_function_t next0_17214 = kk_std_core__new_mlift17139_force_fun19249(r, _ctx); /*(2555) -> <st<global>,div|2556> 2555*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17213, _ctx);
    return kk_std_core_hnd_yield_extend(next0_17214, _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), next0_17214, (next0_17214, x_17213, _ctx));
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17140_force_fun19253__t {
  struct kk_function_s _base;
  kk_ref_t r;
};
static kk_box_t kk_std_core__mlift17140_force_fun19253(kk_function_t _fself, kk_box_t _b_17714, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17140_force_fun19253(kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core__mlift17140_force_fun19253__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17140_force_fun19253__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17140_force_fun19253, kk_context());
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17140_force_fun19253(kk_function_t _fself, kk_box_t _b_17714, kk_context_t* _ctx) {
  struct kk_std_core__mlift17140_force_fun19253__t* _self = kk_function_as(struct kk_std_core__mlift17140_force_fun19253__t*, _fself);
  kk_ref_t r = _self->r; /* ref<global,either<() -> 2556 2555,2555>> */
  kk_drop_match(_self, {kk_ref_dup(r);}, {}, _ctx)
  kk_function_t _x19254 = kk_function_unbox(_b_17714); /*() -> <st<global>,div|2556> 17715*/
  return kk_std_core__mlift17139_force(r, _x19254, _ctx);
}

kk_box_t kk_std_core__mlift17140_force(kk_ref_t r, kk_std_core_types__either _y_16869, kk_context_t* _ctx) { /* forall<a,e> (r : ref<global,either<() -> e a,a>>, either<() -> e a,a>) -> <read<global>,div,alloc<global>,write<global>|e> a */ 
  if (kk_std_core_types__is_Right(_y_16869)) {
    kk_box_t x = _y_16869._cons.Right.right;
    kk_ref_drop(r, _ctx);
    return x;
  }
  {
    kk_box_t _fun_unbox_x17711 = _y_16869._cons.Left.left;
    if (kk_yielding(kk_context())) {
      kk_box_drop(_fun_unbox_x17711, _ctx);
      return kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17140_force_fun19253(r, _ctx), _ctx);
    }
    {
      kk_function_t _x19255 = kk_function_unbox(_fun_unbox_x17711); /*() -> 2556 17712*/
      return kk_std_core__mlift17139_force(r, _x19255, _ctx);
    }
  }
}
 
// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.


// lift anonymous function
struct kk_std_core_force_fun19258__t {
  struct kk_function_s _base;
  kk_std_core__delayed delayed;
};
static kk_box_t kk_std_core_force_fun19258(kk_function_t _fself, kk_box_t _b_17721, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_force_fun19258(kk_std_core__delayed delayed, kk_context_t* _ctx) {
  struct kk_std_core_force_fun19258__t* _self = kk_function_alloc_as(struct kk_std_core_force_fun19258__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_force_fun19258, kk_context());
  _self->delayed = delayed;
  return &_self->_base;
}

static kk_box_t kk_std_core_force_fun19258(kk_function_t _fself, kk_box_t _b_17721, kk_context_t* _ctx) {
  struct kk_std_core_force_fun19258__t* _self = kk_function_as(struct kk_std_core_force_fun19258__t*, _fself);
  kk_std_core__delayed delayed = _self->delayed; /* delayed<2556,2555> */
  kk_drop_match(_self, {kk_std_core__delayed_dup(delayed);}, {}, _ctx)
  kk_std_core_types__either _y_17723_16869 = kk_std_core_types__either_unbox(_b_17721, _ctx); /*either<() -> 2556 2555,2555>*/;
  kk_ref_t _x19259;
  {
    kk_ref_t _x0 = delayed.dref;
    _x19259 = _x0; /*ref<global,either<() -> 2556 2555,2555>>*/
  }
  return kk_std_core__mlift17140_force(_x19259, _y_17723_16869, _ctx);
}

kk_box_t kk_std_core_force(kk_std_core__delayed delayed, kk_context_t* _ctx) { /* forall<a,e> (delayed : delayed<e,a>) -> e a */ 
  kk_std_core_types__either x_17222;
  kk_box_t _x19256;
  kk_ref_t _x19257;
  {
    kk_ref_t _x = delayed.dref;
    kk_ref_dup(_x);
    _x19257 = _x; /*ref<global,either<() -> 2556 2555,2555>>*/
  }
  _x19256 = kk_ref_get(_x19257,kk_context()); /*184*/
  x_17222 = kk_std_core_types__either_unbox(_x19256, _ctx); /*either<() -> 2556 2555,2555>*/
  if (kk_yielding(kk_context())) {
    kk_std_core_types__either_drop(x_17222, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_core_new_force_fun19258(delayed, _ctx), _ctx);
  }
  {
    kk_std_core_types__either _y_17724_16869 = x_17222; /*either<() -> 2556 2555,2555>*/;
    kk_ref_t _x19260;
    {
      kk_ref_t _x0 = delayed.dref;
      _x19260 = _x0; /*ref<global,either<() -> 2556 2555,2555>>*/
    }
    return kk_std_core__mlift17140_force(_x19260, _y_17724_16869, _ctx);
  }
}
 
// Generic show: shows the internal representation of an object as a string
// Note: this breaks parametricity so it should not be public

kk_string_t kk_std_core_gshow(kk_box_t _arg1, kk_context_t* _ctx) { /* forall<a> (a) -> string */ 
  return kk_show_any(_arg1,kk_context());
}
 
// Return the host environment: `dotnet`, `browser`, `webworker`, `node`, or `libc`.

kk_string_t kk_std_core_host(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_get_host(kk_context());
}
 
// clamp an `:int` to fit in an `:int64_t`.

int64_t kk_std_core_int64(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> int64 */ 
  return kk_integer_clamp64(i,kk_context());
}
 
// lifted

kk_std_core__list kk_std_core__ctail_lift16735_intersperse(kk_std_core__list ys, kk_box_t s, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, s : a, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19261 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19261->head;
    kk_std_core__list yy = _con19261->tail;
    kk_reuse_t _ru_18861 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18861 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18861 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16801;
    kk_std_core__list _ru_19165 = kk_std_core__list_hole(); /*list<2587>*/;
    if (kk_likely(_ru_18861!=NULL)) {
      struct kk_std_core_Cons* _con19262 = (struct kk_std_core_Cons*)_ru_18861;
      _con19262->tail = _ru_19165;
      _ctail_16801 = kk_std_core__base_Cons(_con19262); /*list<2587>*/
    }
    else {
      _ctail_16801 = kk_std_core__new_Cons(kk_reuse_null, y, _ru_19165, _ctx); /*list<2587>*/
    }
    { // tailcall
      kk_box_t _x19263 = kk_box_dup(s); /*2587*/
      kk_std_core_types__ctail _x19264;
      kk_box_t* _b_17735_17730 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16801)->tail)); /*cfield<list<2587>>*/;
      kk_box_t _x19265;
      kk_std_core__list _x19266 = kk_std_core__new_Cons(kk_reuse_null, s, _ctail_16801, _ctx); /*list<61>*/
      _x19265 = kk_std_core__list_box(_x19266, _ctx); /*0*/
      _x19264 = kk_ctail_link(_acc,_x19265,_b_17735_17730); /*ctail<0>*/
      ys = yy;
      s = _x19263;
      _acc = _x19264;
      goto kk__tailcall;
    }
  }
  {
    kk_box_drop(s, _ctx);
    kk_box_t _x19267 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19267, _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16735_intersperse(kk_std_core__list ys0, kk_box_t s0, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, s : a) -> list<a> */ 
  kk_std_core_types__ctail _x19268 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_lift16735_intersperse(ys0, s0, _x19268, _ctx);
}
 
// Insert a separator `sep`  between all elements of a list `xs` .

kk_std_core__list kk_std_core_intersperse(kk_std_core__list xs, kk_box_t sep, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, sep : a) -> list<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19269 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19269->head;
    kk_std_core__list xx = _con19269->tail;
    kk_reuse_t _ru_18862 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18862 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18862 = kk_reuse_null;
    }
    kk_std_core__list _ru_19166 = kk_std_core__lift16735_intersperse(xx, sep, _ctx); /*list<2587>*/;
    if (kk_likely(_ru_18862!=NULL)) {
      struct kk_std_core_Cons* _con19270 = (struct kk_std_core_Cons*)_ru_18862;
      _con19270->tail = _ru_19166;
      return kk_std_core__base_Cons(_con19270);
    }
    {
      return kk_std_core__new_Cons(kk_reuse_null, x, _ru_19166, _ctx);
    }
  }
  {
    kk_box_drop(sep, _ctx);
    return kk_std_core__new_Nil(_ctx);
  }
}
 
// clamp an `:int` to fit in an `:intptr_t`.

intptr_t kk_std_core_intptr__t(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> intptr_t */ 
  return kk_integer_clamp_intptr_t(i,kk_context());
}
 
// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.

kk_integer_t kk_std_core_is_exp10(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> int */ 
  return kk_integer_ctz(i,kk_context());
}

kk_std_core__sslice kk_std_core_last1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> sslice */ 
  return kk_slice_last(s,kk_context());
}
 
// Used by the compiler to wrap main console applications

kk_box_t kk_std_core_main_console(kk_function_t main, kk_context_t* _ctx) { /* forall<a,e> (main : () -> e a) -> e a */ 
  return kk_main_console(main,kk_context());
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17141_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list yy, kk_box_t _ctail_16803, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */ 
  kk_std_core__list _ctail_16804 = kk_std_core__list_hole(); /*list<2728>*/;
  kk_std_core__list _ctail_16805 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16803, _ctail_16804, _ctx); /*list<2728>*/;
  kk_std_core_types__ctail _x19271;
  kk_box_t* _b_17749_17746 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16805)->tail)); /*cfield<list<2728>>*/;
  _x19271 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16805, _ctx)),_b_17749_17746); /*ctail<0>*/
  return kk_std_core__ctail_lift16736_map_peek(f, yy, _x19271, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17142_op_fun19272__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16808;
};
static kk_std_core__list kk_std_core__mlift17142_op_fun19272(kk_function_t _fself, kk_std_core__list _ctail_16807, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17142_op_fun19272(kk_function_t _accm, kk_box_t _ctail_16808, kk_context_t* _ctx) {
  struct kk_std_core__mlift17142_op_fun19272__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17142_op_fun19272__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17142_op_fun19272, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16808 = _ctail_16808;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17142_op_fun19272(kk_function_t _fself, kk_std_core__list _ctail_16807, kk_context_t* _ctx) {
  struct kk_std_core__mlift17142_op_fun19272__t* _self = kk_function_as(struct kk_std_core__mlift17142_op_fun19272__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<2728>) -> list<2728> */
  kk_box_t _ctail_16808 = _self->_ctail_16808; /* 2728 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16808);}, {}, _ctx)
  kk_std_core__list _x19273 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16808, _ctail_16807, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x19273, _ctx));
}

kk_std_core__list kk_std_core__mlift17142_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list yy0, kk_box_t _ctail_16808, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */ 
  return kk_std_core__ctailm_lift16736_map_peek(f0, yy0, kk_std_core__new_mlift17142_op_fun19272(_accm, _ctail_16808, _ctx), _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctail_lift16736_map_peek_fun19278__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list yy1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_lift16736_map_peek_fun19278(kk_function_t _fself, kk_box_t _b_17754, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_lift16736_map_peek_fun19278(kk_function_t f1, kk_std_core__list yy1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16736_map_peek_fun19278__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_lift16736_map_peek_fun19278__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_lift16736_map_peek_fun19278, kk_context());
  _self->f1 = f1;
  _self->yy1 = yy1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_lift16736_map_peek_fun19278(kk_function_t _fself, kk_box_t _b_17754, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16736_map_peek_fun19278__t* _self = kk_function_as(struct kk_std_core__ctail_lift16736_map_peek_fun19278__t*, _fself);
  kk_function_t f1 = _self->f1; /* (value : 2727, rest : list<2727>) -> 2729 2728 */
  kk_std_core__list yy1 = _self->yy1; /* list<2727> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<2728>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(yy1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19279 = kk_std_core__mlift17141_op(_acc0, f1, yy1, _b_17754, _ctx); /*list<2728>*/
  return kk_std_core__list_box(_x19279, _ctx);
}

kk_std_core__list kk_std_core__ctail_lift16736_map_peek(kk_function_t f1, kk_std_core__list ys, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19274 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19274->head;
    kk_std_core__list yy1 = _con19274->tail;
    kk_reuse_t _ru_18863 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18863 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy1);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18863 = kk_reuse_null;
    }
    kk_box_t x_17224;
    kk_function_t _x19276 = kk_function_dup(f1); /*(value : 2727, rest : list<2727>) -> 2729 2728*/
    kk_std_core__list _x19275 = kk_std_core__list_dup(yy1); /*list<2727>*/
    x_17224 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_std_core__list, kk_context_t*), _x19276, (_x19276, y, _x19275, _ctx)); /*2728*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18863, _ctx);
      kk_box_drop(x_17224, _ctx);
      kk_box_t _x19277 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_lift16736_map_peek_fun19278(f1, yy1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19277, _ctx);
    }
    {
      kk_std_core__list _ctail_168040 = kk_std_core__list_hole(); /*list<2728>*/;
      kk_std_core__list _ctail_168050 = kk_std_core__new_Cons(_ru_18863, x_17224, _ctail_168040, _ctx); /*list<2728>*/;
      { // tailcall
        kk_std_core_types__ctail _x19280;
        kk_box_t* _b_17766_17760 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168050)->tail)); /*cfield<list<2728>>*/;
        _x19280 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168050, _ctx)),_b_17766_17760); /*ctail<0>*/
        ys = yy1;
        _acc0 = _x19280;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19281 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19281, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctailm_lift16736_map_peek_fun19286__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list yy2;
};
static kk_box_t kk_std_core__ctailm_lift16736_map_peek_fun19286(kk_function_t _fself, kk_box_t _b_17774, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16736_map_peek_fun19286(kk_function_t _accm0, kk_function_t f2, kk_std_core__list yy2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19286__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19286__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16736_map_peek_fun19286, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->yy2 = yy2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_lift16736_map_peek_fun19286(kk_function_t _fself, kk_box_t _b_17774, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19286__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19286__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2728>) -> list<2728> */
  kk_function_t f2 = _self->f2; /* (value : 2727, rest : list<2727>) -> 2729 2728 */
  kk_std_core__list yy2 = _self->yy2; /* list<2727> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(yy2);}, {}, _ctx)
  kk_std_core__list _x19287 = kk_std_core__mlift17142_op(_accm0, f2, yy2, _b_17774, _ctx); /*list<2728>*/
  return kk_std_core__list_box(_x19287, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_lift16736_map_peek_fun19289__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x0_17227;
};
static kk_std_core__list kk_std_core__ctailm_lift16736_map_peek_fun19289(kk_function_t _fself, kk_std_core__list _ctail_168070, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16736_map_peek_fun19289(kk_function_t _accm0, kk_box_t x0_17227, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19289__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19289__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16736_map_peek_fun19289, kk_context());
  _self->_accm0 = _accm0;
  _self->x0_17227 = x0_17227;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_lift16736_map_peek_fun19289(kk_function_t _fself, kk_std_core__list _ctail_168070, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19289__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19289__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2728>) -> list<2728> */
  kk_box_t x0_17227 = _self->x0_17227; /* 2728 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x0_17227);}, {}, _ctx)
  kk_std_core__list _x19290 = kk_std_core__new_Cons(kk_reuse_null, x0_17227, _ctail_168070, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19290, _ctx));
}

kk_std_core__list kk_std_core__ctailm_lift16736_map_peek(kk_function_t f2, kk_std_core__list ys0, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con19282 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con19282->head;
    kk_std_core__list yy2 = _con19282->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_box_dup(y0);
      kk_std_core__list_dup(yy2);
      kk_std_core__list_decref(ys0, _ctx);
    }
    kk_box_t x0_17227;
    kk_function_t _x19284 = kk_function_dup(f2); /*(value : 2727, rest : list<2727>) -> 2729 2728*/
    kk_std_core__list _x19283 = kk_std_core__list_dup(yy2); /*list<2727>*/
    x0_17227 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_std_core__list, kk_context_t*), _x19284, (_x19284, y0, _x19283, _ctx)); /*2728*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17227, _ctx);
      kk_box_t _x19285 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_lift16736_map_peek_fun19286(_accm0, f2, yy2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19285, _ctx);
    }
    { // tailcall
      kk_function_t _x19288 = kk_std_core__new_ctailm_lift16736_map_peek_fun19289(_accm0, x0_17227, _ctx); /*(list<2728>) -> list<2728>*/
      ys0 = yy2;
      _accm0 = _x19288;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16736_map_peek_fun19292__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core__lift16736_map_peek_fun19292(kk_function_t _fself, kk_std_core__list _ctail_16806, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16736_map_peek_fun19292(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__lift16736_map_peek_fun19292, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core__lift16736_map_peek_fun19292(kk_function_t _fself, kk_std_core__list _ctail_16806, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16806;
}

kk_std_core__list kk_std_core__lift16736_map_peek(kk_function_t f3, kk_std_core__list ys1, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>) -> e list<b> */ 
  bool _match_19156 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19156) {
    kk_std_core_types__ctail _x19291 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_lift16736_map_peek(f3, ys1, _x19291, _ctx);
  }
  {
    return kk_std_core__ctailm_lift16736_map_peek(f3, ys1, kk_std_core__new_lift16736_map_peek_fun19292(_ctx), _ctx);
  }
}

kk_integer_t kk_std_core_maxListStack;

kk_ref_t kk_std_core_trace_enabled;
 
// Compose two funs `f` and `g`.


// lift anonymous function
struct kk_std_core_o_fun19294__t {
  struct kk_function_s _base;
  kk_function_t f;
  kk_function_t g;
};
static kk_box_t kk_std_core_o_fun19294(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_o_fun19294(kk_function_t f, kk_function_t g, kk_context_t* _ctx) {
  struct kk_std_core_o_fun19294__t* _self = kk_function_alloc_as(struct kk_std_core_o_fun19294__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_o_fun19294, kk_context());
  _self->f = f;
  _self->g = g;
  return &_self->_base;
}

static kk_box_t kk_std_core_o_fun19294(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_o_fun19294__t* _self = kk_function_as(struct kk_std_core_o_fun19294__t*, _fself);
  kk_function_t f = _self->f; /* (2771) -> 2768 2769 */
  kk_function_t g = _self->g; /* (2767) -> 2768 2771 */
  kk_drop_match(_self, {kk_function_dup(f);kk_function_dup(g);}, {}, _ctx)
  kk_box_t x0_17230 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), g, (g, x, _ctx)); /*2771*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x0_17230, _ctx);
    return kk_std_core_hnd_yield_extend(f, _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x0_17230, _ctx));
  }
}

kk_function_t kk_std_core_o(kk_function_t f, kk_function_t g, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (a) -> e b, g : (c) -> e a) -> ((x : c) -> e b) */ 
  return kk_std_core_new_o_fun19294(f, g, _ctx);
}
 
// monadic lift

kk_box_t kk_std_core__mlift17143_once(kk_function_t calc, kk_ref_t r, kk_std_core_types__maybe _y_16890, kk_context_t* _ctx) { /* forall<_h,_e,a> (calc : () -> a, r : ref<_h,maybe<a>>, maybe<a>) -> <read<_h>,write<_h>,div|_e> a */ 
  if (kk_std_core_types__is_Just(_y_16890)) {
    kk_box_t x = _y_16890._cons.Just.value;
    kk_function_drop(calc, _ctx);
    kk_ref_drop(r, _ctx);
    return x;
  }
  {
    kk_box_t x0 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), calc, (calc, _ctx)); /*2892*/;
    kk_unit_t __ = kk_Unit;
    kk_box_t _x19296;
    kk_std_core_types__maybe _x19297;
    kk_box_t _x19298 = kk_box_dup(x0); /*2892*/
    _x19297 = kk_std_core_types__new_Just(_x19298, _ctx); /*maybe<105>*/
    _x19296 = kk_std_core_types__maybe_box(_x19297, _ctx); /*171*/
    kk_ref_set(r,_x19296,kk_context());
    return x0;
  }
}
 
// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.


// lift anonymous function
struct kk_std_core_once_fun19299__t {
  struct kk_function_s _base;
  kk_function_t calc;
  kk_ref_t r;
};
static kk_box_t kk_std_core_once_fun19299(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19299(kk_function_t calc, kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19299__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19299__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19299, kk_context());
  _self->calc = calc;
  _self->r = r;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_once_fun19302__t {
  struct kk_function_s _base;
  kk_function_t calc;
  kk_ref_t r;
};
static kk_box_t kk_std_core_once_fun19302(kk_function_t _fself, kk_std_core_types__maybe _y_16890, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19302(kk_function_t calc, kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19302__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19302__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19302, kk_context());
  _self->calc = calc;
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core_once_fun19302(kk_function_t _fself, kk_std_core_types__maybe _y_16890, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19302__t* _self = kk_function_as(struct kk_std_core_once_fun19302__t*, _fself);
  kk_function_t calc = _self->calc; /* () -> 2892 */
  kk_ref_t r = _self->r; /* ref<_2800,maybe<2892>> */
  kk_drop_match(_self, {kk_function_dup(calc);kk_ref_dup(r);}, {}, _ctx)
  if (kk_std_core_types__is_Just(_y_16890)) {
    kk_box_t x0 = _y_16890._cons.Just.value;
    kk_function_drop(calc, _ctx);
    kk_ref_drop(r, _ctx);
    return x0;
  }
  {
    kk_box_t x00 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), calc, (calc, _ctx)); /*2892*/;
    kk_unit_t __ = kk_Unit;
    kk_box_t _x19303;
    kk_std_core_types__maybe _x19304;
    kk_box_t _x19305 = kk_box_dup(x00); /*2892*/
    _x19304 = kk_std_core_types__new_Just(_x19305, _ctx); /*maybe<105>*/
    _x19303 = kk_std_core_types__maybe_box(_x19304, _ctx); /*171*/
    kk_ref_set(r,_x19303,kk_context());
    return x00;
  }
}


// lift anonymous function
struct kk_std_core_once_fun19306__t {
  struct kk_function_s _base;
  kk_function_t next0_17235;
};
static kk_box_t kk_std_core_once_fun19306(kk_function_t _fself, kk_box_t _b_17793, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19306(kk_function_t next0_17235, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19306__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19306__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19306, kk_context());
  _self->next0_17235 = next0_17235;
  return &_self->_base;
}

static kk_box_t kk_std_core_once_fun19306(kk_function_t _fself, kk_box_t _b_17793, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19306__t* _self = kk_function_as(struct kk_std_core_once_fun19306__t*, _fself);
  kk_function_t next0_17235 = _self->next0_17235; /* (maybe<2892>) -> <read<_2800>,write<_2800>,div|_2884> 2892 */
  kk_drop_match(_self, {kk_function_dup(next0_17235);}, {}, _ctx)
  kk_std_core_types__maybe _x19307 = kk_std_core_types__maybe_unbox(_b_17793, _ctx); /*maybe<2892>*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_types__maybe, kk_context_t*), next0_17235, (next0_17235, _x19307, _ctx));
}
static kk_box_t kk_std_core_once_fun19299(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19299__t* _self = kk_function_as(struct kk_std_core_once_fun19299__t*, _fself);
  kk_function_t calc = _self->calc; /* () -> 2892 */
  kk_ref_t r = _self->r; /* ref<_2800,maybe<2892>> */
  kk_drop_match(_self, {kk_function_dup(calc);kk_ref_dup(r);}, {}, _ctx)
  kk_std_core_types__maybe x_17234;
  kk_box_t _x19300;
  kk_ref_t _x19301 = kk_ref_dup(r); /*ref<_2800,maybe<2892>>*/
  _x19300 = kk_ref_get(_x19301,kk_context()); /*184*/
  x_17234 = kk_std_core_types__maybe_unbox(_x19300, _ctx); /*maybe<2892>*/
  kk_function_t next0_17235 = kk_std_core_new_once_fun19302(calc, r, _ctx); /*(maybe<2892>) -> <read<_2800>,write<_2800>,div|_2884> 2892*/;
  if (kk_yielding(kk_context())) {
    kk_std_core_types__maybe_drop(x_17234, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_core_new_once_fun19306(next0_17235, _ctx), _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_types__maybe, kk_context_t*), next0_17235, (next0_17235, x_17234, _ctx));
  }
}

kk_function_t kk_std_core_once(kk_function_t calc, kk_context_t* _ctx) { /* forall<a> (calc : () -> a) -> (() -> a) */ 
  kk_ref_t r = kk_ref_alloc((kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx)),kk_context()); /*ref<_2800,maybe<2892>>*/;
  return kk_std_core_new_once_fun19299(calc, r, _ctx);
}

kk_std_core_types__maybe kk_std_core_xparse_int(kk_string_t s, bool hex, kk_context_t* _ctx) { /* (s : string, hex : bool) -> maybe<int> */ 
  return kk_integer_xparse(s,hex,kk_context());
}

kk_box_t kk_std_core_phantom(kk_context_t* _ctx) { /* forall<a> () -> a */ 
  return kk_box_null;
}
 
// Convert a string to lower-case

kk_string_t kk_std_core_to_lower(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  return kk_string_to_lower(s,kk_context());
}

kk_unit_t kk_std_core_xtrace(kk_string_t message0, kk_context_t* _ctx) { /* (message : string) -> () */ 
  kk_trace(message0,kk_context()); return kk_Unit;
}

kk_unit_t kk_std_core_xtrace_any(kk_string_t message0, kk_box_t x, kk_context_t* _ctx) { /* forall<a> (message : string, x : a) -> () */ 
  kk_trace_any(message0,x,kk_context()); return kk_Unit;
}

kk_ref_t kk_std_core_unique_count;

kk_vector_t kk_std_core_unvlist(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> vector<a> */ 
  return kk_list_to_vector(xs,kk_context());
}
 
// Create a new vector of length `n`  with initial elements given by function `f` .

kk_vector_t kk_std_core_vector_initz(kk_ssize_t n, kk_function_t f, kk_context_t* _ctx) { /* forall<a> (n : ssize_t, f : (ssize_t) -> a) -> vector<a> */ 
  return kk_vector_init(n,f,kk_context());
}

bool kk_std_core__lp__excl__eq__4_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19311;
  if (kk_std_core_types__is_Lt(x)) {
    _x19311 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19312;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19311 = kk_integer_from_small(0); /*int*/
    goto _match19312;
  }
  {
    _x19311 = kk_integer_from_small(1); /*int*/
  }
  _match19312: ;
  kk_integer_t _x19313;
  if (kk_std_core_types__is_Lt(y)) {
    _x19313 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19314;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19313 = kk_integer_from_small(0); /*int*/
    goto _match19314;
  }
  {
    _x19313 = kk_integer_from_small(1); /*int*/
  }
  _match19314: ;
  return kk_integer_neq(_x19311,_x19313,kk_context());
}
 
// Append two lists.

kk_std_core__list kk_std_core__ctail_append(kk_std_core__list xs, kk_std_core__list ys, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ys : list<a>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19318 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19318->head;
    kk_std_core__list xx = _con19318->tail;
    kk_reuse_t _ru_18865 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18865 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18865 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16809 = kk_std_core__list_hole(); /*list<3284>*/;
    kk_std_core__list _ctail_16810;
    if (kk_likely(_ru_18865!=NULL)) {
      struct kk_std_core_Cons* _con19319 = (struct kk_std_core_Cons*)_ru_18865;
      _con19319->tail = _ctail_16809;
      _ctail_16810 = kk_std_core__base_Cons(_con19319); /*list<3284>*/
    }
    else {
      _ctail_16810 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16809, _ctx); /*list<3284>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19320;
      kk_box_t* _b_17807_17802 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16810)->tail)); /*cfield<list<3284>>*/;
      _x19320 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16810, _ctx)),_b_17807_17802); /*ctail<0>*/
      xs = xx;
      _acc = _x19320;
      goto kk__tailcall;
    }
  }
  {
    kk_box_t _x19321 = kk_ctail_resolve(_acc,(kk_std_core__list_box(ys, _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19321, _ctx);
  }
}
 
// Append two lists.

kk_std_core__list kk_std_core_append(kk_std_core__list xs0, kk_std_core__list ys0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ys : list<a>) -> list<a> */ 
  kk_std_core_types__ctail _x19322 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_append(xs0, ys0, _x19322, _ctx);
}
 
// Append two strings.

kk_string_t kk_std_core__lp__plus__plus__1_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> string */ 
  return kk_string_cat(x,y,kk_context());
}

kk_unit_t kk_std_core_printsln(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_std_core_types__maybe _match_19153;
  kk_box_t _x19323;
  kk_ref_t _x19324 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  _x19323 = kk_ref_get(_x19324,kk_context()); /*184*/
  _match_19153 = kk_std_core_types__maybe_unbox(_x19323, _ctx); /*maybe<(string) -> console ()>*/
  if (kk_std_core_types__is_Nothing(_match_19153)) {
    kk_std_core_xprintsln(s, _ctx); return kk_Unit;
  }
  {
    kk_box_t _fun_unbox_x17817 = _match_19153._cons.Just.value;
    kk_string_t _b_17820;
    kk_string_t _x19325;
    kk_define_string_literal(, _s19326, 1, "\n")
    _x19325 = kk_string_dup(_s19326); /*string*/
    _b_17820 = kk_std_core__lp__plus__plus__1_rp_(s, _x19325, _ctx); /*string*/
    kk_box_t _x19327;
    kk_function_t _x19328 = kk_function_unbox(_fun_unbox_x17817); /*(17818) -> console 17819*/
    _x19327 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19328, (_x19328, kk_string_box(_b_17820), _ctx)); /*17819*/
    kk_unit_unbox(_x19327); return kk_Unit;
  }
}

bool kk_std_core__lp__eq__eq__4_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19329;
  if (kk_std_core_types__is_Lt(x)) {
    _x19329 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19330;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19329 = kk_integer_from_small(0); /*int*/
    goto _match19330;
  }
  {
    _x19329 = kk_integer_from_small(1); /*int*/
  }
  _match19330: ;
  kk_integer_t _x19331;
  if (kk_std_core_types__is_Lt(y)) {
    _x19331 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19332;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19331 = kk_integer_from_small(0); /*int*/
    goto _match19332;
  }
  {
    _x19331 = kk_integer_from_small(1); /*int*/
  }
  _match19332: ;
  return kk_integer_eq(_x19329,_x19331,kk_context());
}

bool kk_std_core__lp__lt__5_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19333;
  if (kk_std_core_types__is_Lt(x)) {
    _x19333 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19334;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19333 = kk_integer_from_small(0); /*int*/
    goto _match19334;
  }
  {
    _x19333 = kk_integer_from_small(1); /*int*/
  }
  _match19334: ;
  kk_integer_t _x19335;
  if (kk_std_core_types__is_Lt(y)) {
    _x19335 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19336;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19335 = kk_integer_from_small(0); /*int*/
    goto _match19336;
  }
  {
    _x19335 = kk_integer_from_small(1); /*int*/
  }
  _match19336: ;
  return kk_integer_lt(_x19333,_x19335,kk_context());
}

kk_std_core_types__order kk_std_core_order(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> order */ 
  bool _match_19151;
  kk_integer_t _x19337 = kk_integer_dup(i); /*int*/
  _match_19151 = kk_integer_lt(_x19337,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19151) {
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Lt(_ctx);
  }
  {
    bool _match_19152 = kk_integer_gt(i,(kk_integer_from_small(0)),kk_context()); /*bool*/;
    if (_match_19152) {
      return kk_std_core_types__new_Gt(_ctx);
    }
    {
      return kk_std_core_types__new_Eq(_ctx);
    }
  }
}
 
// Compare two strings.
// Uses the character codes directly for comparison

kk_std_core_types__order kk_std_core_compare_4(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> order */ 
  kk_integer_t i_16577 = kk_std_core_string_compare(x, y, _ctx); /*int*/;
  bool _match_19149;
  kk_integer_t _x19338 = kk_integer_dup(i_16577); /*int*/
  _match_19149 = kk_integer_lt(_x19338,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19149) {
    kk_integer_drop(i_16577, _ctx);
    return kk_std_core_types__new_Lt(_ctx);
  }
  {
    bool _match_19150 = kk_integer_gt(i_16577,(kk_integer_from_small(0)),kk_context()); /*bool*/;
    if (_match_19150) {
      return kk_std_core_types__new_Gt(_ctx);
    }
    {
      return kk_std_core_types__new_Eq(_ctx);
    }
  }
}

bool kk_std_core__lp__lt__7_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> bool */ 
  kk_std_core_types__order x0_16713 = kk_std_core_compare_4(x, y, _ctx); /*order*/;
  kk_integer_t _x19339;
  if (kk_std_core_types__is_Lt(x0_16713)) {
    _x19339 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19340;
  }
  if (kk_std_core_types__is_Eq(x0_16713)) {
    _x19339 = kk_integer_from_small(0); /*int*/
    goto _match19340;
  }
  {
    _x19339 = kk_integer_from_small(1); /*int*/
  }
  _match19340: ;
  kk_integer_t _x19341 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_eq(_x19339,_x19341,kk_context());
}

bool kk_std_core__lp__gt__3_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19342;
  if (kk_std_core_types__is_Lt(x)) {
    _x19342 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19343;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19342 = kk_integer_from_small(0); /*int*/
    goto _match19343;
  }
  {
    _x19342 = kk_integer_from_small(1); /*int*/
  }
  _match19343: ;
  kk_integer_t _x19344;
  if (kk_std_core_types__is_Lt(y)) {
    _x19344 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19345;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19344 = kk_integer_from_small(0); /*int*/
    goto _match19345;
  }
  {
    _x19344 = kk_integer_from_small(1); /*int*/
  }
  _match19345: ;
  return kk_integer_gt(_x19342,_x19344,kk_context());
}

kk_std_core_types__order kk_std_core_compare_2(bool x, bool y, kk_context_t* _ctx) { /* (x : bool, y : bool) -> order */ 
  if (x) {
    if (x) {
      if (y) {
        return kk_std_core_types__new_Eq(_ctx);
      }
      {
        return kk_std_core_types__new_Gt(_ctx);
      }
    }
    {
      return kk_std_core_types__new_Eq(_ctx);
    }
  }
  if (y) {
    return kk_std_core_types__new_Lt(_ctx);
  }
  if (x) {
    if (y) {
      return kk_std_core_types__new_Eq(_ctx);
    }
    {
      return kk_std_core_types__new_Gt(_ctx);
    }
  }
  {
    return kk_std_core_types__new_Eq(_ctx);
  }
}

bool kk_std_core__lp__gt__eq__4_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19348;
  if (kk_std_core_types__is_Lt(x)) {
    _x19348 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19349;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19348 = kk_integer_from_small(0); /*int*/
    goto _match19349;
  }
  {
    _x19348 = kk_integer_from_small(1); /*int*/
  }
  _match19349: ;
  kk_integer_t _x19350;
  if (kk_std_core_types__is_Lt(y)) {
    _x19350 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19351;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19350 = kk_integer_from_small(0); /*int*/
    goto _match19351;
  }
  {
    _x19350 = kk_integer_from_small(1); /*int*/
  }
  _match19351: ;
  return kk_integer_gte(_x19348,_x19350,kk_context());
}

bool kk_std_core__lp__gt__eq__6_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> bool */ 
  kk_std_core_types__order x0_16717 = kk_std_core_compare_4(x, y, _ctx); /*order*/;
  kk_integer_t _x19352;
  if (kk_std_core_types__is_Lt(x0_16717)) {
    _x19352 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19353;
  }
  if (kk_std_core_types__is_Eq(x0_16717)) {
    _x19352 = kk_integer_from_small(0); /*int*/
    goto _match19353;
  }
  {
    _x19352 = kk_integer_from_small(1); /*int*/
  }
  _match19353: ;
  kk_integer_t _x19354 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_gt(_x19352,_x19354,kk_context());
}
 
// lifted

kk_string_t kk_std_core__lift16737_joinsep(kk_string_t sep, kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (sep : string, ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19355 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x17822 = _con19355->head;
    kk_std_core__list yy = _con19355->tail;
    kk_string_t y = kk_string_unbox(_box_x17822);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    kk_string_t acc0_16766;
    kk_string_t _x19357;
    kk_string_t _x19358 = kk_string_dup(sep); /*string*/
    _x19357 = kk_std_core__lp__plus__plus__1_rp_(_x19358, y, _ctx); /*string*/
    acc0_16766 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19357, _ctx); /*string*/
    { // tailcall
      ys = yy;
      acc = acc0_16766;
      goto kk__tailcall;
    }
  }
  {
    kk_string_drop(sep, _ctx);
    return acc;
  }
}
 
// Concatenate all strings in a list

kk_string_t kk_std_core_joinsep(kk_std_core__list xs, kk_string_t sep, kk_context_t* _ctx) { /* (xs : list<string>, sep : string) -> string */ 
  if (kk_std_core__is_Nil(xs)) {
    kk_string_drop(sep, _ctx);
    return kk_string_empty();
  }
  {
    struct kk_std_core_Cons* _con19360 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17823 = _con19360->head;
    kk_std_core__list xx = _con19360->tail;
    kk_string_t x = kk_string_unbox(_box_x17823);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core__lift16737_joinsep(sep, xx, x, _ctx);
  }
}
 
// lifted

kk_string_t kk_std_core__lift16738_join_2(kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19362 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x17824 = _con19362->head;
    kk_std_core__list yy = _con19362->tail;
    kk_string_t y = kk_string_unbox(_box_x17824);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x19364;
      kk_string_t _x19365;
      kk_string_t _x19366 = kk_string_empty(); /*string*/
      _x19365 = kk_std_core__lp__plus__plus__1_rp_(_x19366, y, _ctx); /*string*/
      _x19364 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19365, _ctx); /*string*/
      ys = yy;
      acc = _x19364;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// Concatenate all strings in a list

kk_string_t kk_std_core_join_2(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<string>) -> string */ 
  if (kk_std_core__is_Nil(xs)) {
    return kk_string_empty();
  }
  {
    struct kk_std_core_Cons* _con19369 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17825 = _con19369->head;
    kk_std_core__list xx = _con19369->tail;
    kk_string_t x = kk_string_unbox(_box_x17825);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core__lift16738_join_2(xx, x, _ctx);
  }
}
 
// Concatenate all strings in a list using a specific separator

kk_string_t kk_std_core_join_3(kk_std_core__list xs, kk_string_t sep, kk_context_t* _ctx) { /* (xs : list<string>, sep : string) -> string */ 
  if (kk_std_core__is_Nil(xs)) {
    kk_string_drop(sep, _ctx);
    return kk_string_empty();
  }
  {
    struct kk_std_core_Cons* _con19372 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17826 = _con19372->head;
    kk_std_core__list xx = _con19372->tail;
    kk_string_t x = kk_string_unbox(_box_x17826);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core__lift16737_joinsep(sep, xx, x, _ctx);
  }
}

kk_ssize_t kk_std_core_decr_1(kk_ssize_t i, kk_context_t* _ctx) { /* (i : ssize_t) -> ssize_t */ 
  return (i - 1);
}

bool kk_std_core__lp__lt__eq__5_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19374;
  if (kk_std_core_types__is_Lt(x)) {
    _x19374 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19375;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19374 = kk_integer_from_small(0); /*int*/
    goto _match19375;
  }
  {
    _x19374 = kk_integer_from_small(1); /*int*/
  }
  _match19375: ;
  kk_integer_t _x19376;
  if (kk_std_core_types__is_Lt(y)) {
    _x19376 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19377;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19376 = kk_integer_from_small(0); /*int*/
    goto _match19377;
  }
  {
    _x19376 = kk_integer_from_small(1); /*int*/
  }
  _match19377: ;
  return kk_integer_lte(_x19374,_x19376,kk_context());
}

kk_ssize_t kk_std_core_incr_1(kk_ssize_t i, kk_context_t* _ctx) { /* (i : ssize_t) -> ssize_t */ 
  return (i + 1);
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17144_op(kk_function_t action, kk_ssize_t end, kk_ssize_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : (ssize_t) -> e (), end : ssize_t, i : ssize_t, wild_ : ()) -> e () */ 
  kk_ssize_t i0_16769 = kk_std_core_incr_1(i, _ctx); /*ssize_t*/;
  kk_std_core__lift16739_forz(action, end, i0_16769, _ctx); return kk_Unit;
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16739_forz_fun19382__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_ssize_t end0;
  kk_ssize_t i0;
};
static kk_box_t kk_std_core__lift16739_forz_fun19382(kk_function_t _fself, kk_box_t _b_17828, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16739_forz_fun19382(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16739_forz_fun19382__t* _self = kk_function_alloc_as(struct kk_std_core__lift16739_forz_fun19382__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16739_forz_fun19382, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16739_forz_fun19382(kk_function_t _fself, kk_box_t _b_17828, kk_context_t* _ctx) {
  struct kk_std_core__lift16739_forz_fun19382__t* _self = kk_function_as(struct kk_std_core__lift16739_forz_fun19382__t*, _fself);
  kk_function_t action0 = _self->action0; /* (ssize_t) -> 4408 () */
  kk_ssize_t end0 = _self->end0; /* ssize_t */
  kk_ssize_t i0 = _self->i0; /* ssize_t */
  kk_drop_match(_self, {kk_function_dup(action0);;;}, {}, _ctx)
  kk_unit_t _x19383 = kk_Unit;
  kk_unit_t _x19384 = kk_Unit;
  kk_unit_unbox(_b_17828);
  kk_std_core__mlift17144_op(action0, end0, i0, _x19384, _ctx);
  return kk_unit_box(_x19383);
}

kk_unit_t kk_std_core__lift16739_forz(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) { /* forall<e> (action : (ssize_t) -> e (), end : ssize_t, i : ssize_t) -> e () */ 
  kk__tailcall: ;
  bool _match_19143 = (i0 <= end0); /*bool*/;
  if (_match_19143) {
    kk_unit_t x_17243 = kk_Unit;
    kk_function_t _x19380 = kk_function_dup(action0); /*(ssize_t) -> 4408 ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_ssize_t, kk_context_t*), _x19380, (_x19380, i0, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x19381 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16739_forz_fun19382(action0, end0, i0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x19381); return kk_Unit;
    }
    {
      kk_ssize_t i0_167690 = kk_std_core_incr_1(i0, _ctx); /*ssize_t*/;
      { // tailcall
        i0 = i0_167690;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(action0, _ctx);
    kk_Unit; return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_core_foreach_indexedz_fun19387__t {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_indexedz_fun19387(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexedz_fun19387(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexedz_fun19387__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexedz_fun19387__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexedz_fun19387, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_indexedz_fun19387(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexedz_fun19387__t* _self = kk_function_as(struct kk_std_core_foreach_indexedz_fun19387__t*, _fself);
  kk_function_t f = _self->f; /* (4442, ssize_t) -> 4443 () */
  kk_vector_t v = _self->v; /* vector<4442> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t _x19388 = kk_vector_at(v,i,kk_context()); /*223*/
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_ssize_t, kk_context_t*), f, (f, _x19388, i, _ctx));
}

kk_unit_t kk_std_core_foreach_indexedz(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a, ssize_t) -> e ()) -> e () */ 
  kk_ssize_t start0_17246 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17247;
  kk_ssize_t _x19385;
  kk_vector_t _x19386 = kk_vector_dup(v); /*vector<4442>*/
  _x19385 = kk_vector_len(_x19386,kk_context()); /*ssize_t*/
  end_17247 = kk_std_core_decr_1(_x19385, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_indexedz_fun19387(f, v, _ctx), end_17247, start0_17246, _ctx); return kk_Unit;
}
 
// lifted

kk_integer_t kk_std_core__lift16740_length_1(kk_std_core__list ys, kk_integer_t acc, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, acc : int) -> int */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19389 = kk_std_core__as_Cons(ys);
    kk_box_t _pat0 = _con19389->head;
    kk_std_core__list yy = _con19389->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_box_drop(_pat0, _ctx);
      kk_std_core__list_free(ys);
    }
    else {
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_integer_t _x19390 = kk_integer_add(acc,(kk_integer_from_small(1)),kk_context()); /*int*/
      ys = yy;
      acc = _x19390;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16741_list(kk_integer_t low, kk_integer_t high, kk_std_core__list acc, kk_context_t* _ctx) { /* (low : int, high : int, acc : list<int>) -> list<int> */ 
  kk__tailcall: ;
  bool _match_19142;
  kk_integer_t _x19392 = kk_integer_dup(low); /*int*/
  kk_integer_t _x19393 = kk_integer_dup(high); /*int*/
  _match_19142 = kk_integer_gt(_x19392,_x19393,kk_context()); /*bool*/
  if (_match_19142) {
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low, _ctx);
    return acc;
  }
  { // tailcall
    kk_integer_t _x19394;
    kk_integer_t _x19395 = kk_integer_dup(high); /*int*/
    _x19394 = kk_integer_sub(_x19395,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_std_core__list _x19396 = kk_std_core__new_Cons(kk_reuse_null, kk_integer_box(high), acc, _ctx); /*list<61>*/
    high = _x19394;
    acc = _x19396;
    goto kk__tailcall;
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17145_op(kk_std_core__list acc, kk_function_t f, kk_integer_t high0_16771, kk_integer_t low, kk_box_t _y_16901, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, f : (int) -> e a, high0.16771 : int, low : int, a) -> e list<a> */ 
  kk_std_core__list _x19397 = kk_std_core__new_Cons(kk_reuse_null, _y_16901, acc, _ctx); /*list<61>*/
  return kk_std_core__lift16742_list_1(f, low, high0_16771, _x19397, _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16742_list_fun19403__t_1 {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t f0;
  kk_integer_t high0_167710;
  kk_integer_t low0;
};
static kk_box_t kk_std_core__lift16742_list_fun19403_1(kk_function_t _fself, kk_box_t _b_17836, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16742_list_fun19403_1(kk_std_core__list acc0, kk_function_t f0, kk_integer_t high0_167710, kk_integer_t low0, kk_context_t* _ctx) {
  struct kk_std_core__lift16742_list_fun19403__t_1* _self = kk_function_alloc_as(struct kk_std_core__lift16742_list_fun19403__t_1, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16742_list_fun19403_1, kk_context());
  _self->acc0 = acc0;
  _self->f0 = f0;
  _self->high0_167710 = high0_167710;
  _self->low0 = low0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16742_list_fun19403_1(kk_function_t _fself, kk_box_t _b_17836, kk_context_t* _ctx) {
  struct kk_std_core__lift16742_list_fun19403__t_1* _self = kk_function_as(struct kk_std_core__lift16742_list_fun19403__t_1*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<4816> */
  kk_function_t f0 = _self->f0; /* (int) -> 4817 4816 */
  kk_integer_t high0_167710 = _self->high0_167710; /* int */
  kk_integer_t low0 = _self->low0; /* int */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(f0);kk_integer_dup(high0_167710);kk_integer_dup(low0);}, {}, _ctx)
  kk_std_core__list _x19404 = kk_std_core__mlift17145_op(acc0, f0, high0_167710, low0, _b_17836, _ctx); /*list<4816>*/
  return kk_std_core__list_box(_x19404, _ctx);
}

kk_std_core__list kk_std_core__lift16742_list_1(kk_function_t f0, kk_integer_t low0, kk_integer_t high, kk_std_core__list acc0, kk_context_t* _ctx) { /* forall<a,e> (f : (int) -> e a, low : int, high : int, acc : list<a>) -> e list<a> */ 
  kk__tailcall: ;
  bool _match_19140;
  kk_integer_t _x19398 = kk_integer_dup(low0); /*int*/
  kk_integer_t _x19399 = kk_integer_dup(high); /*int*/
  _match_19140 = kk_integer_gt(_x19398,_x19399,kk_context()); /*bool*/
  if (_match_19140) {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low0, _ctx);
    return acc0;
  }
  {
    kk_integer_t high0_167710;
    kk_integer_t _x19400 = kk_integer_dup(high); /*int*/
    high0_167710 = kk_integer_sub(_x19400,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_box_t x_17249;
    kk_function_t _x19401 = kk_function_dup(f0); /*(int) -> 4817 4816*/
    x_17249 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_context_t*), _x19401, (_x19401, high, _ctx)); /*4816*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x_17249, _ctx);
      kk_box_t _x19402 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16742_list_fun19403_1(acc0, f0, high0_167710, low0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19402, _ctx);
    }
    { // tailcall
      kk_std_core__list _x19405 = kk_std_core__new_Cons(kk_reuse_null, x_17249, acc0, _ctx); /*list<61>*/
      high = high0_167710;
      acc0 = _x19405;
      goto kk__tailcall;
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17146_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_box_t _ctail_16811, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */ 
  kk_std_core__list _ctail_16812 = kk_std_core__list_hole(); /*list<5916>*/;
  kk_std_core__list _ctail_16813 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16811, _ctail_16812, _ctx); /*list<5916>*/;
  kk_std_core_types__ctail _x19406;
  kk_box_t* _b_17847_17844 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16813)->tail)); /*cfield<list<5916>>*/;
  _x19406 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16813, _ctx)),_b_17847_17844); /*ctail<0>*/
  return kk_std_core__ctail_map_5(xx, f, _x19406, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17147_op_fun19407__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16816;
};
static kk_std_core__list kk_std_core__mlift17147_op_fun19407(kk_function_t _fself, kk_std_core__list _ctail_16815, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17147_op_fun19407(kk_function_t _accm, kk_box_t _ctail_16816, kk_context_t* _ctx) {
  struct kk_std_core__mlift17147_op_fun19407__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17147_op_fun19407__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17147_op_fun19407, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16816 = _ctail_16816;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17147_op_fun19407(kk_function_t _fself, kk_std_core__list _ctail_16815, kk_context_t* _ctx) {
  struct kk_std_core__mlift17147_op_fun19407__t* _self = kk_function_as(struct kk_std_core__mlift17147_op_fun19407__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<5916>) -> list<5916> */
  kk_box_t _ctail_16816 = _self->_ctail_16816; /* 5916 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16816);}, {}, _ctx)
  kk_std_core__list _x19408 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16816, _ctail_16815, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x19408, _ctx));
}

kk_std_core__list kk_std_core__mlift17147_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_box_t _ctail_16816, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */ 
  return kk_std_core__ctailm_map_5(xx0, f0, kk_std_core__new_mlift17147_op_fun19407(_accm, _ctail_16816, _ctx), _ctx);
}
 
// Apply a function `f`  to each element of the input list in sequence.


// lift anonymous function
struct kk_std_core__ctail_map_fun19412__t_5 {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_map_fun19412_5(kk_function_t _fself, kk_box_t _b_17852, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_map_fun19412_5(kk_function_t f1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_fun19412__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctail_map_fun19412__t_5, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_map_fun19412_5, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_map_fun19412_5(kk_function_t _fself, kk_box_t _b_17852, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_fun19412__t_5* _self = kk_function_as(struct kk_std_core__ctail_map_fun19412__t_5*, _fself);
  kk_function_t f1 = _self->f1; /* (5915) -> 5917 5916 */
  kk_std_core__list xx1 = _self->xx1; /* list<5915> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<5916>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19413 = kk_std_core__mlift17146_op(_acc0, f1, xx1, _b_17852, _ctx); /*list<5916>*/
  return kk_std_core__list_box(_x19413, _ctx);
}

kk_std_core__list kk_std_core__ctail_map_5(kk_std_core__list xs, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19409 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19409->head;
    kk_std_core__list xx1 = _con19409->tail;
    kk_reuse_t _ru_18872 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18872 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18872 = kk_reuse_null;
    }
    kk_box_t x0_17252;
    kk_function_t _x19410 = kk_function_dup(f1); /*(5915) -> 5917 5916*/
    x0_17252 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19410, (_x19410, x, _ctx)); /*5916*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18872, _ctx);
      kk_box_drop(x0_17252, _ctx);
      kk_box_t _x19411 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_map_fun19412_5(f1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19411, _ctx);
    }
    {
      kk_std_core__list _ctail_168120 = kk_std_core__list_hole(); /*list<5916>*/;
      kk_std_core__list _ctail_168130 = kk_std_core__new_Cons(_ru_18872, x0_17252, _ctail_168120, _ctx); /*list<5916>*/;
      { // tailcall
        kk_std_core_types__ctail _x19414;
        kk_box_t* _b_17864_17858 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168130)->tail)); /*cfield<list<5916>>*/;
        _x19414 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168130, _ctx)),_b_17864_17858); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x19414;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19415 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19415, _ctx);
  }
}
 
// Apply a function `f`  to each element of the input list in sequence.


// lift anonymous function
struct kk_std_core__ctailm_map_fun19419__t_5 {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_map_fun19419_5(kk_function_t _fself, kk_box_t _b_17872, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_fun19419_5(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19419__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_fun19419__t_5, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_fun19419_5, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_map_fun19419_5(kk_function_t _fself, kk_box_t _b_17872, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19419__t_5* _self = kk_function_as(struct kk_std_core__ctailm_map_fun19419__t_5*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<5916>) -> list<5916> */
  kk_function_t f2 = _self->f2; /* (5915) -> 5917 5916 */
  kk_std_core__list xx2 = _self->xx2; /* list<5915> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x19420 = kk_std_core__mlift17147_op(_accm0, f2, xx2, _b_17872, _ctx); /*list<5916>*/
  return kk_std_core__list_box(_x19420, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_map_fun19422__t_5 {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x2_17255;
};
static kk_std_core__list kk_std_core__ctailm_map_fun19422_5(kk_function_t _fself, kk_std_core__list _ctail_168150, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_fun19422_5(kk_function_t _accm0, kk_box_t x2_17255, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19422__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_fun19422__t_5, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_fun19422_5, kk_context());
  _self->_accm0 = _accm0;
  _self->x2_17255 = x2_17255;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_map_fun19422_5(kk_function_t _fself, kk_std_core__list _ctail_168150, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19422__t_5* _self = kk_function_as(struct kk_std_core__ctailm_map_fun19422__t_5*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<5916>) -> list<5916> */
  kk_box_t x2_17255 = _self->x2_17255; /* 5916 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x2_17255);}, {}, _ctx)
  kk_std_core__list _x19423 = kk_std_core__new_Cons(kk_reuse_null, x2_17255, _ctail_168150, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19423, _ctx));
}

kk_std_core__list kk_std_core__ctailm_map_5(kk_std_core__list xs0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con19416 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con19416->head;
    kk_std_core__list xx2 = _con19416->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_box_t x2_17255;
    kk_function_t _x19417 = kk_function_dup(f2); /*(5915) -> 5917 5916*/
    x2_17255 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19417, (_x19417, x1, _ctx)); /*5916*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x2_17255, _ctx);
      kk_box_t _x19418 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_map_fun19419_5(_accm0, f2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19418, _ctx);
    }
    { // tailcall
      kk_function_t _x19421 = kk_std_core__new_ctailm_map_fun19422_5(_accm0, x2_17255, _ctx); /*(list<5916>) -> list<5916>*/
      xs0 = xx2;
      _accm0 = _x19421;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Apply a function `f`  to each element of the input list in sequence.


// lift anonymous function
struct kk_std_core_map_fun19425__t_5 {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_map_fun19425_5(kk_function_t _fself, kk_std_core__list _ctail_16814, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19425_5(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19425_5, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_map_fun19425_5(kk_function_t _fself, kk_std_core__list _ctail_16814, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16814;
}

kk_std_core__list kk_std_core_map_5(kk_std_core__list xs1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */ 
  bool _match_19137 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19137) {
    kk_std_core_types__ctail _x19424 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_map_5(xs1, f3, _x19424, _ctx);
  }
  {
    return kk_std_core__ctailm_map_5(xs1, f3, kk_std_core_new_map_fun19425_5(_ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16743_list_2(kk_integer_t low, kk_integer_t high, kk_std_core__list acc, kk_context_t* _ctx) { /* (low : int, high : int, acc : list<int>) -> list<int> */ 
  kk__tailcall: ;
  bool _match_19136;
  kk_integer_t _x19426 = kk_integer_dup(low); /*int*/
  kk_integer_t _x19427 = kk_integer_dup(high); /*int*/
  _match_19136 = kk_integer_gt(_x19426,_x19427,kk_context()); /*bool*/
  if (_match_19136) {
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low, _ctx);
    return acc;
  }
  { // tailcall
    kk_integer_t _x19428;
    kk_integer_t _x19429 = kk_integer_dup(high); /*int*/
    _x19428 = kk_integer_sub(_x19429,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_std_core__list _x19430 = kk_std_core__new_Cons(kk_reuse_null, kk_integer_box(high), acc, _ctx); /*list<61>*/
    high = _x19428;
    acc = _x19430;
    goto kk__tailcall;
  }
}
 
// Create a list of characters from `lo`  to `hi`  (inclusive).


// lift anonymous function
struct kk_std_core_list_fun19431__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_list_fun19431_2(kk_function_t _fself, kk_box_t _b_17881, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_list_fun19431_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_list_fun19431_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_list_fun19431_2(kk_function_t _fself, kk_box_t _b_17881, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_char_t _x19432;
  kk_integer_t _x19433 = kk_integer_unbox(_b_17881); /*int*/
  _x19432 = kk_integer_clamp32(_x19433,kk_context()); /*char*/
  return kk_char_box(_x19432, _ctx);
}

kk_std_core__list kk_std_core_list_2(kk_char_t lo, kk_char_t hi, kk_context_t* _ctx) { /* (lo : char, hi : char) -> total list<char> */ 
  kk_integer_t lo0_16601 = kk_integer_from_int(lo,kk_context()); /*int*/;
  kk_integer_t hi0_16602 = kk_integer_from_int(hi,kk_context()); /*int*/;
  kk_std_core__list _b_17882_17879 = kk_std_core__lift16743_list_2(lo0_16601, hi0_16602, kk_std_core__new_Nil(_ctx), _ctx); /*list<int>*/;
  return kk_std_core_map_5(_b_17882_17879, kk_std_core_new_list_fun19431_2(_ctx), _ctx);
}
 
// Convert a string to a list of characters

kk_std_core__list kk_std_core_list_4(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> total list<char> */ 
  return kk_string_to_list(s,kk_context());
}


// lift anonymous function
struct kk_std_core_map_fun19435__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19435(kk_function_t _fself, kk_box_t _b_17885, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19435(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19435, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19435(kk_function_t _fself, kk_box_t _b_17885, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _x19436 = kk_std_core_types__new_Just(_b_17885, _ctx); /*maybe<105>*/
  return kk_std_core_types__maybe_box(_x19436, _ctx);
}

kk_std_core_types__maybe kk_std_core_map(kk_std_core_types__maybe m, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (m : maybe<a>, f : (a) -> e b) -> e maybe<b> */ 
  if (kk_std_core_types__is_Nothing(m)) {
    kk_function_drop(f, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t x = m._cons.Just.value;
    kk_box_t x0_17258 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x, _ctx)); /*5096*/;
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17258, _ctx);
      kk_box_t _x19434 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19435(_ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19434, _ctx);
    }
    {
      return kk_std_core_types__new_Just(x0_17258, _ctx);
    }
  }
}
 
// Map over the `Right` component of an `:either` type.


// lift anonymous function
struct kk_std_core_map_fun19438__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19438_1(kk_function_t _fself, kk_box_t _b_17889, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19438_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19438_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19438_1(kk_function_t _fself, kk_box_t _b_17889, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__either _x19439 = kk_std_core_types__new_Right(_b_17889, _ctx); /*either<72,73>*/
  return kk_std_core_types__either_box(_x19439, _ctx);
}

kk_std_core_types__either kk_std_core_map_1(kk_std_core_types__either e, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,c,e> (e : either<a,b>, f : (b) -> e c) -> e either<a,c> */ 
  if (kk_std_core_types__is_Right(e)) {
    kk_box_t x = e._cons.Right.right;
    kk_box_t x0_17262 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x, _ctx)); /*5134*/;
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17262, _ctx);
      kk_box_t _x19437 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19438_1(_ctx), _ctx); /*3860*/
      return kk_std_core_types__either_unbox(_x19437, _ctx);
    }
    {
      return kk_std_core_types__new_Right(x0_17262, _ctx);
    }
  }
  {
    kk_box_t x00 = e._cons.Left.left;
    kk_function_drop(f, _ctx);
    return kk_std_core_types__new_Left(x00, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17151_map_fun19442__t_2 {
  struct kk_function_s _base;
  kk_box_t _y_16918;
};
static kk_box_t kk_std_core__mlift17151_map_fun19442_2(kk_function_t _fself, kk_box_t _b_17893, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17151_map_fun19442_2(kk_box_t _y_16918, kk_context_t* _ctx) {
  struct kk_std_core__mlift17151_map_fun19442__t_2* _self = kk_function_alloc_as(struct kk_std_core__mlift17151_map_fun19442__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17151_map_fun19442_2, kk_context());
  _self->_y_16918 = _y_16918;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17151_map_fun19442_2(kk_function_t _fself, kk_box_t _b_17893, kk_context_t* _ctx) {
  struct kk_std_core__mlift17151_map_fun19442__t_2* _self = kk_function_as(struct kk_std_core__mlift17151_map_fun19442__t_2*, _fself);
  kk_box_t _y_16918 = _self->_y_16918; /* 5279 */
  kk_drop_match(_self, {kk_box_dup(_y_16918);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19443 = kk_std_core_types__new_dash__lp__comma__rp_(_y_16918, _b_17893, _ctx); /*(6, 7)*/
  return kk_std_core_types__tuple2__box(_x19443, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core__mlift17151_map_2(kk_function_t f, kk_std_core_types__tuple2_ t, kk_box_t _y_16918, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a), b) -> e (b, b) */ 
  kk_box_t x_17266;
  kk_box_t _x19440;
  {
    kk_box_t _x = t.snd;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(t, _ctx);
    _x19440 = _x; /*5278*/
  }
  x_17266 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19440, _ctx)); /*5279*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17266, _ctx);
    kk_box_t _x19441 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17151_map_fun19442_2(_y_16918, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple2__unbox(_x19441, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__rp_(_y_16918, x_17266, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19447__t_2 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple2_ t;
};
static kk_box_t kk_std_core_map_fun19447_2(kk_function_t _fself, kk_box_t _b_17897, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19447_2(kk_function_t f, kk_std_core_types__tuple2_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19447__t_2* _self = kk_function_alloc_as(struct kk_std_core_map_fun19447__t_2, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19447_2, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19447_2(kk_function_t _fself, kk_box_t _b_17897, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19447__t_2* _self = kk_function_as(struct kk_std_core_map_fun19447__t_2*, _fself);
  kk_function_t f = _self->f; /* (5278) -> 5280 5279 */
  kk_std_core_types__tuple2_ t = _self->t; /* (5278, 5278) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple2__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19448 = kk_std_core__mlift17151_map_2(f, t, _b_17897, _ctx); /*(5279, 5279)*/
  return kk_std_core_types__tuple2__box(_x19448, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19451__t_2 {
  struct kk_function_s _base;
  kk_box_t x_17271;
};
static kk_box_t kk_std_core_map_fun19451_2(kk_function_t _fself, kk_box_t _b_17899, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19451_2(kk_box_t x_17271, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19451__t_2* _self = kk_function_alloc_as(struct kk_std_core_map_fun19451__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19451_2, kk_context());
  _self->x_17271 = x_17271;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19451_2(kk_function_t _fself, kk_box_t _b_17899, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19451__t_2* _self = kk_function_as(struct kk_std_core_map_fun19451__t_2*, _fself);
  kk_box_t x_17271 = _self->x_17271; /* 5279 */
  kk_drop_match(_self, {kk_box_dup(x_17271);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19452 = kk_std_core_types__new_dash__lp__comma__rp_(x_17271, _b_17899, _ctx); /*(6, 7)*/
  return kk_std_core_types__tuple2__box(_x19452, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_map_2(kk_std_core_types__tuple2_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a), f : (a) -> e b) -> e (b, b) */ 
  kk_box_t x_17271;
  kk_function_t _x19445 = kk_function_dup(f); /*(5278) -> 5280 5279*/
  kk_box_t _x19444;
  {
    kk_box_t _x = t.fst;
    kk_box_dup(_x);
    _x19444 = _x; /*5278*/
  }
  x_17271 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19445, (_x19445, _x19444, _ctx)); /*5279*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17271, _ctx);
    kk_box_t _x19446 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19447_2(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple2__unbox(_x19446, _ctx);
  }
  {
    kk_box_t x0_17275;
    kk_box_t _x19449;
    {
      kk_box_t _x0 = t.snd;
      kk_box_dup(_x0);
      kk_std_core_types__tuple2__drop(t, _ctx);
      _x19449 = _x0; /*5278*/
    }
    x0_17275 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19449, _ctx)); /*5279*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17275, _ctx);
      kk_box_t _x19450 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19451_2(x_17271, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x19450, _ctx);
    }
    {
      return kk_std_core_types__new_dash__lp__comma__rp_(x_17271, x0_17275, _ctx);
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17153_map_fun19455__t_3 {
  struct kk_function_s _base;
  kk_box_t _y_16920;
  kk_box_t _y_16921;
};
static kk_box_t kk_std_core__mlift17153_map_fun19455_3(kk_function_t _fself, kk_box_t _b_17905, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17153_map_fun19455_3(kk_box_t _y_16920, kk_box_t _y_16921, kk_context_t* _ctx) {
  struct kk_std_core__mlift17153_map_fun19455__t_3* _self = kk_function_alloc_as(struct kk_std_core__mlift17153_map_fun19455__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17153_map_fun19455_3, kk_context());
  _self->_y_16920 = _y_16920;
  _self->_y_16921 = _y_16921;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17153_map_fun19455_3(kk_function_t _fself, kk_box_t _b_17905, kk_context_t* _ctx) {
  struct kk_std_core__mlift17153_map_fun19455__t_3* _self = kk_function_as(struct kk_std_core__mlift17153_map_fun19455__t_3*, _fself);
  kk_box_t _y_16920 = _self->_y_16920; /* 5497 */
  kk_box_t _y_16921 = _self->_y_16921; /* 5497 */
  kk_drop_match(_self, {kk_box_dup(_y_16920);kk_box_dup(_y_16921);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19456 = kk_std_core_types__new_dash__lp__comma__comma__rp_(_y_16920, _y_16921, _b_17905, _ctx); /*(13, 14, 15)*/
  return kk_std_core_types__tuple3__box(_x19456, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core__mlift17153_map_3(kk_box_t _y_16920, kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t _y_16921, kk_context_t* _ctx) { /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */ 
  kk_box_t x_17281;
  kk_box_t _x19453;
  {
    kk_box_t _x = t.thd;
    kk_box_dup(_x);
    kk_std_core_types__tuple3__drop(t, _ctx);
    _x19453 = _x; /*5496*/
  }
  x_17281 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19453, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17281, _ctx);
    kk_box_t _x19454 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17153_map_fun19455_3(_y_16920, _y_16921, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19454, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__comma__rp_(_y_16920, _y_16921, x_17281, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17154_map_fun19460__t_3 {
  struct kk_function_s _base;
  kk_box_t _y_16920;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
};
static kk_box_t kk_std_core__mlift17154_map_fun19460_3(kk_function_t _fself, kk_box_t _b_17909, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17154_map_fun19460_3(kk_box_t _y_16920, kk_function_t f, kk_std_core_types__tuple3_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17154_map_fun19460__t_3* _self = kk_function_alloc_as(struct kk_std_core__mlift17154_map_fun19460__t_3, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17154_map_fun19460_3, kk_context());
  _self->_y_16920 = _y_16920;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17154_map_fun19460_3(kk_function_t _fself, kk_box_t _b_17909, kk_context_t* _ctx) {
  struct kk_std_core__mlift17154_map_fun19460__t_3* _self = kk_function_as(struct kk_std_core__mlift17154_map_fun19460__t_3*, _fself);
  kk_box_t _y_16920 = _self->_y_16920; /* 5497 */
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_drop_match(_self, {kk_box_dup(_y_16920);kk_function_dup(f);kk_std_core_types__tuple3__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19461 = kk_std_core__mlift17153_map_3(_y_16920, f, t, _b_17909, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19461, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core__mlift17154_map_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t _y_16920, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */ 
  kk_box_t x_17287;
  kk_function_t _x19458 = kk_function_dup(f); /*(5496) -> 5498 5497*/
  kk_box_t _x19457;
  {
    kk_box_t _x = t.snd;
    kk_box_dup(_x);
    _x19457 = _x; /*5496*/
  }
  x_17287 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19458, (_x19458, _x19457, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17287, _ctx);
    kk_box_t _x19459 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17154_map_fun19460_3(_y_16920, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19459, _ctx);
  }
  {
    return kk_std_core__mlift17153_map_3(_y_16920, f, t, x_17287, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19465__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
};
static kk_box_t kk_std_core_map_fun19465_3(kk_function_t _fself, kk_box_t _b_17913, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19465_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19465__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19465__t_3, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19465_3, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19465_3(kk_function_t _fself, kk_box_t _b_17913, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19465__t_3* _self = kk_function_as(struct kk_std_core_map_fun19465__t_3*, _fself);
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple3__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19466 = kk_std_core__mlift17154_map_3(f, t, _b_17913, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19466, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19470__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
  kk_box_t x_17290;
};
static kk_box_t kk_std_core_map_fun19470_3(kk_function_t _fself, kk_box_t _b_17915, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19470_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t x_17290, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19470__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19470__t_3, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19470_3, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17290 = x_17290;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19470_3(kk_function_t _fself, kk_box_t _b_17915, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19470__t_3* _self = kk_function_as(struct kk_std_core_map_fun19470__t_3*, _fself);
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_box_t x_17290 = _self->x_17290; /* 5497 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple3__dup(t);kk_box_dup(x_17290);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19471 = kk_std_core__mlift17153_map_3(x_17290, f, t, _b_17915, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19471, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19474__t_3 {
  struct kk_function_s _base;
  kk_box_t x_17290;
  kk_box_t x0_17294;
};
static kk_box_t kk_std_core_map_fun19474_3(kk_function_t _fself, kk_box_t _b_17917, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19474_3(kk_box_t x_17290, kk_box_t x0_17294, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19474__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19474__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19474_3, kk_context());
  _self->x_17290 = x_17290;
  _self->x0_17294 = x0_17294;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19474_3(kk_function_t _fself, kk_box_t _b_17917, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19474__t_3* _self = kk_function_as(struct kk_std_core_map_fun19474__t_3*, _fself);
  kk_box_t x_17290 = _self->x_17290; /* 5497 */
  kk_box_t x0_17294 = _self->x0_17294; /* 5497 */
  kk_drop_match(_self, {kk_box_dup(x_17290);kk_box_dup(x0_17294);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19475 = kk_std_core_types__new_dash__lp__comma__comma__rp_(x_17290, x0_17294, _b_17917, _ctx); /*(13, 14, 15)*/
  return kk_std_core_types__tuple3__box(_x19475, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core_map_3(kk_std_core_types__tuple3_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a, a), f : (a) -> e b) -> e (b, b, b) */ 
  kk_box_t x_17290;
  kk_function_t _x19463 = kk_function_dup(f); /*(5496) -> 5498 5497*/
  kk_box_t _x19462;
  {
    kk_box_t _x = t.fst;
    kk_box_dup(_x);
    _x19462 = _x; /*5496*/
  }
  x_17290 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19463, (_x19463, _x19462, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17290, _ctx);
    kk_box_t _x19464 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19465_3(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19464, _ctx);
  }
  {
    kk_box_t x0_17294;
    kk_function_t _x19468 = kk_function_dup(f); /*(5496) -> 5498 5497*/
    kk_box_t _x19467;
    {
      kk_box_t _x0 = t.snd;
      kk_box_dup(_x0);
      _x19467 = _x0; /*5496*/
    }
    x0_17294 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19468, (_x19468, _x19467, _ctx)); /*5497*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17294, _ctx);
      kk_box_t _x19469 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19470_3(f, t, x_17290, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple3__unbox(_x19469, _ctx);
    }
    {
      kk_box_t x1_17298;
      kk_box_t _x19472;
      {
        kk_box_t _x1 = t.thd;
        kk_box_dup(_x1);
        kk_std_core_types__tuple3__drop(t, _ctx);
        _x19472 = _x1; /*5496*/
      }
      x1_17298 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19472, _ctx)); /*5497*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x1_17298, _ctx);
        kk_box_t _x19473 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19474_3(x_17290, x0_17294, _ctx), _ctx); /*3860*/
        return kk_std_core_types__tuple3__unbox(_x19473, _ctx);
      }
      {
        return kk_std_core_types__new_dash__lp__comma__comma__rp_(x_17290, x0_17294, x1_17298, _ctx);
      }
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17156_map_fun19479__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16923;
  kk_box_t _y_16924;
  kk_box_t _y_16925;
};
static kk_box_t kk_std_core__mlift17156_map_fun19479_4(kk_function_t _fself, kk_box_t _b_17925, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17156_map_fun19479_4(kk_box_t _y_16923, kk_box_t _y_16924, kk_box_t _y_16925, kk_context_t* _ctx) {
  struct kk_std_core__mlift17156_map_fun19479__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17156_map_fun19479__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17156_map_fun19479_4, kk_context());
  _self->_y_16923 = _y_16923;
  _self->_y_16924 = _y_16924;
  _self->_y_16925 = _y_16925;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17156_map_fun19479_4(kk_function_t _fself, kk_box_t _b_17925, kk_context_t* _ctx) {
  struct kk_std_core__mlift17156_map_fun19479__t_4* _self = kk_function_as(struct kk_std_core__mlift17156_map_fun19479__t_4*, _fself);
  kk_box_t _y_16923 = _self->_y_16923; /* 5784 */
  kk_box_t _y_16924 = _self->_y_16924; /* 5784 */
  kk_box_t _y_16925 = _self->_y_16925; /* 5784 */
  kk_drop_match(_self, {kk_box_dup(_y_16923);kk_box_dup(_y_16924);kk_box_dup(_y_16925);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19480 = kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, _y_16923, _y_16924, _y_16925, _b_17925, _ctx); /*(22, 23, 24, 25)*/
  return kk_std_core_types__tuple4__box(_x19480, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17156_map_4(kk_box_t _y_16923, kk_box_t _y_16924, kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16925, kk_context_t* _ctx) { /* forall<a,b,e> (b, b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17305;
  kk_box_t _x19476;
  {
    struct kk_std_core_types__Tuple4_* _con19477 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _pat00 = _con19477->fst;
    kk_box_t _pat10 = _con19477->snd;
    kk_box_t _pat2 = _con19477->thd;
    kk_box_t _x = _con19477->field4;
    if (kk_likely(kk_std_core_types__tuple4__is_unique(t))) {
      kk_box_drop(_pat2, _ctx);
      kk_box_drop(_pat10, _ctx);
      kk_box_drop(_pat00, _ctx);
      kk_std_core_types__tuple4__free(t);
    }
    else {
      kk_box_dup(_x);
      kk_std_core_types__tuple4__decref(t, _ctx);
    }
    _x19476 = _x; /*5783*/
  }
  x_17305 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19476, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17305, _ctx);
    kk_box_t _x19478 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17156_map_fun19479_4(_y_16923, _y_16924, _y_16925, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19478, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, _y_16923, _y_16924, _y_16925, x_17305, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17157_map_fun19485__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16923;
  kk_box_t _y_16924;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core__mlift17157_map_fun19485_4(kk_function_t _fself, kk_box_t _b_17929, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17157_map_fun19485_4(kk_box_t _y_16923, kk_box_t _y_16924, kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17157_map_fun19485__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17157_map_fun19485__t_4, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17157_map_fun19485_4, kk_context());
  _self->_y_16923 = _y_16923;
  _self->_y_16924 = _y_16924;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17157_map_fun19485_4(kk_function_t _fself, kk_box_t _b_17929, kk_context_t* _ctx) {
  struct kk_std_core__mlift17157_map_fun19485__t_4* _self = kk_function_as(struct kk_std_core__mlift17157_map_fun19485__t_4*, _fself);
  kk_box_t _y_16923 = _self->_y_16923; /* 5784 */
  kk_box_t _y_16924 = _self->_y_16924; /* 5784 */
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_box_dup(_y_16923);kk_box_dup(_y_16924);kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19486 = kk_std_core__mlift17156_map_4(_y_16923, _y_16924, f, t, _b_17929, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19486, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17157_map_4(kk_box_t _y_16923, kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16924, kk_context_t* _ctx) { /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17312;
  kk_function_t _x19483 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19481;
  {
    struct kk_std_core_types__Tuple4_* _con19482 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19482->thd;
    kk_box_dup(_x);
    _x19481 = _x; /*5783*/
  }
  x_17312 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19483, (_x19483, _x19481, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17312, _ctx);
    kk_box_t _x19484 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17157_map_fun19485_4(_y_16923, _y_16924, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19484, _ctx);
  }
  {
    return kk_std_core__mlift17156_map_4(_y_16923, _y_16924, f, t, x_17312, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17158_map_fun19491__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16923;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core__mlift17158_map_fun19491_4(kk_function_t _fself, kk_box_t _b_17933, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17158_map_fun19491_4(kk_box_t _y_16923, kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17158_map_fun19491__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17158_map_fun19491__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17158_map_fun19491_4, kk_context());
  _self->_y_16923 = _y_16923;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17158_map_fun19491_4(kk_function_t _fself, kk_box_t _b_17933, kk_context_t* _ctx) {
  struct kk_std_core__mlift17158_map_fun19491__t_4* _self = kk_function_as(struct kk_std_core__mlift17158_map_fun19491__t_4*, _fself);
  kk_box_t _y_16923 = _self->_y_16923; /* 5784 */
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_box_dup(_y_16923);kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19492 = kk_std_core__mlift17157_map_4(_y_16923, f, t, _b_17933, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19492, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17158_map_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16923, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17315;
  kk_function_t _x19489 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19487;
  {
    struct kk_std_core_types__Tuple4_* _con19488 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19488->snd;
    kk_box_dup(_x);
    _x19487 = _x; /*5783*/
  }
  x_17315 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19489, (_x19489, _x19487, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17315, _ctx);
    kk_box_t _x19490 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17158_map_fun19491_4(_y_16923, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19490, _ctx);
  }
  {
    return kk_std_core__mlift17157_map_4(_y_16923, f, t, x_17315, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19497__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core_map_fun19497_4(kk_function_t _fself, kk_box_t _b_17937, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19497_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19497__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19497__t_4, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19497_4, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19497_4(kk_function_t _fself, kk_box_t _b_17937, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19497__t_4* _self = kk_function_as(struct kk_std_core_map_fun19497__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19498 = kk_std_core__mlift17158_map_4(f, t, _b_17937, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19498, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19503__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
  kk_box_t x_17318;
};
static kk_box_t kk_std_core_map_fun19503_4(kk_function_t _fself, kk_box_t _b_17939, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19503_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t x_17318, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19503__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19503__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19503_4, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17318 = x_17318;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19503_4(kk_function_t _fself, kk_box_t _b_17939, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19503__t_4* _self = kk_function_as(struct kk_std_core_map_fun19503__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_box_t x_17318 = _self->x_17318; /* 5784 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);kk_box_dup(x_17318);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19504 = kk_std_core__mlift17157_map_4(x_17318, f, t, _b_17939, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19504, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19509__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
  kk_box_t x_17318;
  kk_box_t x0_17322;
};
static kk_box_t kk_std_core_map_fun19509_4(kk_function_t _fself, kk_box_t _b_17941, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19509_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t x_17318, kk_box_t x0_17322, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19509__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19509__t_4, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19509_4, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17318 = x_17318;
  _self->x0_17322 = x0_17322;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19509_4(kk_function_t _fself, kk_box_t _b_17941, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19509__t_4* _self = kk_function_as(struct kk_std_core_map_fun19509__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_box_t x_17318 = _self->x_17318; /* 5784 */
  kk_box_t x0_17322 = _self->x0_17322; /* 5784 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);kk_box_dup(x_17318);kk_box_dup(x0_17322);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19510 = kk_std_core__mlift17156_map_4(x_17318, x0_17322, f, t, _b_17941, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19510, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19514__t_4 {
  struct kk_function_s _base;
  kk_box_t x_17318;
  kk_box_t x0_17322;
  kk_box_t x1_17326;
};
static kk_box_t kk_std_core_map_fun19514_4(kk_function_t _fself, kk_box_t _b_17943, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19514_4(kk_box_t x_17318, kk_box_t x0_17322, kk_box_t x1_17326, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19514__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19514__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19514_4, kk_context());
  _self->x_17318 = x_17318;
  _self->x0_17322 = x0_17322;
  _self->x1_17326 = x1_17326;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19514_4(kk_function_t _fself, kk_box_t _b_17943, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19514__t_4* _self = kk_function_as(struct kk_std_core_map_fun19514__t_4*, _fself);
  kk_box_t x_17318 = _self->x_17318; /* 5784 */
  kk_box_t x0_17322 = _self->x0_17322; /* 5784 */
  kk_box_t x1_17326 = _self->x1_17326; /* 5784 */
  kk_drop_match(_self, {kk_box_dup(x_17318);kk_box_dup(x0_17322);kk_box_dup(x1_17326);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19515 = kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, x_17318, x0_17322, x1_17326, _b_17943, _ctx); /*(22, 23, 24, 25)*/
  return kk_std_core_types__tuple4__box(_x19515, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core_map_4(kk_std_core_types__tuple4_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a, a, a), f : (a) -> e b) -> e (b, b, b, b) */ 
  kk_box_t x_17318;
  kk_function_t _x19495 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19493;
  {
    struct kk_std_core_types__Tuple4_* _con19494 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19494->fst;
    kk_box_dup(_x);
    _x19493 = _x; /*5783*/
  }
  x_17318 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19495, (_x19495, _x19493, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17318, _ctx);
    kk_box_t _x19496 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19497_4(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19496, _ctx);
  }
  {
    kk_box_t x0_17322;
    kk_function_t _x19501 = kk_function_dup(f); /*(5783) -> 5785 5784*/
    kk_box_t _x19499;
    {
      struct kk_std_core_types__Tuple4_* _con19500 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
      kk_box_t _x0 = _con19500->snd;
      kk_box_dup(_x0);
      _x19499 = _x0; /*5783*/
    }
    x0_17322 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19501, (_x19501, _x19499, _ctx)); /*5784*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17322, _ctx);
      kk_box_t _x19502 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19503_4(f, t, x_17318, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple4__unbox(_x19502, _ctx);
    }
    {
      kk_box_t x1_17326;
      kk_function_t _x19507 = kk_function_dup(f); /*(5783) -> 5785 5784*/
      kk_box_t _x19505;
      {
        struct kk_std_core_types__Tuple4_* _con19506 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
        kk_box_t _x1 = _con19506->thd;
        kk_box_dup(_x1);
        _x19505 = _x1; /*5783*/
      }
      x1_17326 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19507, (_x19507, _x19505, _ctx)); /*5784*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x1_17326, _ctx);
        kk_box_t _x19508 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19509_4(f, t, x_17318, x0_17322, _ctx), _ctx); /*3860*/
        return kk_std_core_types__tuple4__unbox(_x19508, _ctx);
      }
      {
        kk_box_t x2_17330;
        kk_box_t _x19511;
        {
          struct kk_std_core_types__Tuple4_* _con19512 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
          kk_box_t _pat06 = _con19512->fst;
          kk_box_t _pat13 = _con19512->snd;
          kk_box_t _pat22 = _con19512->thd;
          kk_box_t _x2 = _con19512->field4;
          if (kk_likely(kk_std_core_types__tuple4__is_unique(t))) {
            kk_box_drop(_pat22, _ctx);
            kk_box_drop(_pat13, _ctx);
            kk_box_drop(_pat06, _ctx);
            kk_std_core_types__tuple4__free(t);
          }
          else {
            kk_box_dup(_x2);
            kk_std_core_types__tuple4__decref(t, _ctx);
          }
          _x19511 = _x2; /*5783*/
        }
        x2_17330 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19511, _ctx)); /*5784*/
        if (kk_yielding(kk_context())) {
          kk_box_drop(x2_17330, _ctx);
          kk_box_t _x19513 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19514_4(x_17318, x0_17322, x1_17326, _ctx), _ctx); /*3860*/
          return kk_std_core_types__tuple4__unbox(_x19513, _ctx);
        }
        {
          return kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, x_17318, x0_17322, x1_17326, x2_17330, _ctx);
        }
      }
    }
  }
}
 
// Apply a function `f` to each character in a string


// lift anonymous function
struct kk_std_core_map_fun19516__t_6 {
  struct kk_function_s _base;
  kk_function_t f;
};
static kk_box_t kk_std_core_map_fun19516_6(kk_function_t _fself, kk_box_t _b_17954, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19516_6(kk_function_t f, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19516__t_6* _self = kk_function_alloc_as(struct kk_std_core_map_fun19516__t_6, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19516_6, kk_context());
  _self->f = f;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19516_6(kk_function_t _fself, kk_box_t _b_17954, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19516__t_6* _self = kk_function_as(struct kk_std_core_map_fun19516__t_6*, _fself);
  kk_function_t f = _self->f; /* (char) -> 6224 char */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  kk_char_t _x19517;
  kk_char_t _x19518 = kk_char_unbox(_b_17954, _ctx); /*char*/
  _x19517 = kk_function_call(kk_char_t, (kk_function_t, kk_char_t, kk_context_t*), f, (f, _x19518, _ctx)); /*char*/
  return kk_char_box(_x19517, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19520__t_6 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19520_6(kk_function_t _fself, kk_box_t _b_17958, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19520_6(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19520_6, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19520_6(kk_function_t _fself, kk_box_t _b_17958, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19521;
  kk_std_core__list _x19522 = kk_std_core__list_unbox(_b_17958, _ctx); /*list<char>*/
  _x19521 = kk_std_core_string_2(_x19522, _ctx); /*string*/
  return kk_string_box(_x19521);
}

kk_string_t kk_std_core_map_6(kk_string_t s, kk_function_t f, kk_context_t* _ctx) { /* forall<e> (s : string, f : (char) -> e char) -> e string */ 
  kk_std_core__list _b_17955_17952 = kk_std_core_list_4(s, _ctx); /*list<char>*/;
  kk_std_core__list x_17338 = kk_std_core_map_5(_b_17955_17952, kk_std_core_new_map_fun19516_6(f, _ctx), _ctx); /*list<char>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17338, _ctx);
    kk_box_t _x19519 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19520_6(_ctx), _ctx); /*3860*/
    return kk_string_unbox(_x19519);
  }
  {
    return kk_std_core_string_2(x_17338, _ctx);
  }
}
 
// Apply a total function `f` to each element in a vector `v`


// lift anonymous function
struct kk_std_core_map_fun19530__t_7 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
  kk_vector_t w;
};
static kk_unit_t kk_std_core_map_fun19530_7(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19530_7(kk_function_t f, kk_vector_t v, kk_vector_t w, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19530__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19530__t_7, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19530_7, kk_context());
  _self->f = f;
  _self->v = v;
  _self->w = w;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_map_fun19532__t_7 {
  struct kk_function_s _base;
  kk_vector_t w;
  kk_ssize_t i;
};
static kk_box_t kk_std_core_map_fun19532_7(kk_function_t _fself, kk_box_t _b_17961, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19532_7(kk_vector_t w, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19532__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19532__t_7, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19532_7, kk_context());
  _self->w = w;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19532_7(kk_function_t _fself, kk_box_t _b_17961, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19532__t_7* _self = kk_function_as(struct kk_std_core_map_fun19532__t_7*, _fself);
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_ssize_t i = _self->i; /* ssize_t */
  kk_drop_match(_self, {kk_vector_dup(w);;}, {}, _ctx)
  kk_unit_t _x19533 = kk_Unit;
  kk_vector_unsafe_assign(w,i,_b_17961,kk_context());
  return kk_unit_box(_x19533);
}
static kk_unit_t kk_std_core_map_fun19530_7(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19530__t_7* _self = kk_function_as(struct kk_std_core_map_fun19530__t_7*, _fself);
  kk_function_t f = _self->f; /* (6272) -> 6274 6273 */
  kk_vector_t v = _self->v; /* vector<6272> */
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);kk_vector_dup(w);}, {}, _ctx)
  kk_box_t x0_17570 = kk_vector_at(v,i,kk_context()); /*6272*/;
  kk_box_t x1_17346 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x0_17570, _ctx)); /*6273*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x1_17346, _ctx);
    kk_box_t _x19531 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19532_7(w, i, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x19531);
  }
  {
    return kk_vector_unsafe_assign(w,i,x1_17346,kk_context());
  }
}


// lift anonymous function
struct kk_std_core_map_fun19535__t_7 {
  struct kk_function_s _base;
  kk_vector_t w;
};
static kk_box_t kk_std_core_map_fun19535_7(kk_function_t _fself, kk_box_t _b_17965, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19535_7(kk_vector_t w, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19535__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19535__t_7, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19535_7, kk_context());
  _self->w = w;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19535_7(kk_function_t _fself, kk_box_t _b_17965, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19535__t_7* _self = kk_function_as(struct kk_std_core_map_fun19535__t_7*, _fself);
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_drop_match(_self, {kk_vector_dup(w);}, {}, _ctx)
  kk_box_drop(_b_17965, _ctx);
  return kk_vector_box(w, _ctx);
}

kk_vector_t kk_std_core_map_7(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (v : vector<a>, f : (a) -> e b) -> e vector<b> */ 
  kk_vector_t w;
  kk_ssize_t _x19523;
  kk_integer_t _x19524;
  kk_ssize_t _x19525;
  kk_vector_t _x19526 = kk_vector_dup(v); /*vector<6272>*/
  _x19525 = kk_vector_len(_x19526,kk_context()); /*ssize_t*/
  _x19524 = kk_integer_from_ssize_t(_x19525,kk_context()); /*int*/
  _x19523 = kk_std_core_ssize__t(_x19524, _ctx); /*ssize_t*/
  w = kk_vector_alloc_uninit(_x19523,NULL,kk_context()); /*vector<6273>*/
  kk_ssize_t start0_17351 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17352;
  kk_ssize_t _x19527;
  kk_vector_t _x19528 = kk_vector_dup(v); /*vector<6272>*/
  _x19527 = kk_vector_len(_x19528,kk_context()); /*ssize_t*/
  end_17352 = kk_std_core_decr_1(_x19527, _ctx); /*ssize_t*/
  kk_unit_t x_17341 = kk_Unit;
  kk_function_t _x19529;
  kk_vector_dup(w);
  _x19529 = kk_std_core_new_map_fun19530_7(f, v, w, _ctx); /*(i : ssize_t) -> 6274 ()*/
  kk_std_core__lift16739_forz(_x19529, end_17352, start0_17351, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19534 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19535_7(w, _ctx), _ctx); /*3860*/
    return kk_vector_unbox(_x19534, _ctx);
  }
  {
    return w;
  }
}
 
// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.

kk_string_t kk_std_core_pad_left(kk_string_t s, kk_integer_t width, kk_std_core_types__optional fill, kk_context_t* _ctx) { /* (s : string, width : int, fill : optional<char>) -> string */ 
  kk_ssize_t w = kk_std_core_ssize__t(width, _ctx); /*ssize_t*/;
  kk_ssize_t n;
  kk_string_t _x19536 = kk_string_dup(s); /*string*/
  n = kk_string_len(_x19536,kk_context()); /*ssize_t*/
  bool _match_19115 = (w <= n); /*bool*/;
  if (_match_19115) {
    kk_std_core_types__optional_drop(fill, _ctx);
    return s;
  }
  {
    kk_string_t _x19537;
    kk_string_t _x19538;
    kk_char_t _x19539;
    if (kk_std_core_types__is_Optional(fill)) {
      kk_box_t _box_x17968 = fill._cons.Optional.value;
      kk_char_t _fill_6932 = kk_char_unbox(_box_x17968, NULL);
      kk_std_core_types__optional_drop(fill, _ctx);
      _x19539 = _fill_6932; /*char*/
      goto _match19540;
    }
    {
      _x19539 = ' '; /*char*/
    }
    _match19540: ;
    _x19538 = kk_std_core_string(_x19539, _ctx); /*string*/
    kk_ssize_t _x19542 = (w - n); /*ssize_t*/
    _x19537 = kk_std_core_repeatz(_x19538, _x19542, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(_x19537, s, _ctx);
  }
}
 
// Show an `:int` as a hexadecimal value.
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).

kk_string_t kk_std_core_show_hex(kk_integer_t i, kk_std_core_types__optional width, kk_std_core_types__optional use_capitals, kk_std_core_types__optional pre, kk_context_t* _ctx) { /* (i : int, width : optional<int>, use-capitals : optional<bool>, pre : optional<string>) -> string */ 
  kk_string_t _x19543;
  bool _match_19114;
  kk_integer_t _x19544 = kk_integer_dup(i); /*int*/
  _match_19114 = kk_integer_lt(_x19544,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19114) {
    kk_define_string_literal(, _s19545, 1, "-")
    _x19543 = kk_string_dup(_s19545); /*string*/
  }
  else {
    _x19543 = kk_string_empty(); /*string*/
  }
  kk_string_t _x19547;
  kk_string_t _x19548;
  if (kk_std_core_types__is_Optional(pre)) {
    kk_box_t _box_x17969 = pre._cons.Optional.value;
    kk_string_t _pre_7040 = kk_string_unbox(_box_x17969);
    kk_string_dup(_pre_7040);
    kk_std_core_types__optional_drop(pre, _ctx);
    _x19548 = _pre_7040; /*string*/
    goto _match19549;
  }
  {
    kk_define_string_literal(, _s19551, 2, "0x")
    _x19548 = kk_string_dup(_s19551); /*string*/
  }
  _match19549: ;
  kk_string_t _x19552;
  kk_string_t _x19553;
  kk_integer_t _x19554 = kk_integer_abs(i,kk_context()); /*int*/
  bool _x19555;
  if (kk_std_core_types__is_Optional(use_capitals)) {
    kk_box_t _box_x17970 = use_capitals._cons.Optional.value;
    bool _use_capitals_7036 = kk_bool_unbox(_box_x17970);
    kk_std_core_types__optional_drop(use_capitals, _ctx);
    _x19555 = _use_capitals_7036; /*bool*/
    goto _match19556;
  }
  {
    _x19555 = true; /*bool*/
  }
  _match19556: ;
  _x19553 = kk_std_core_int_show_hex(_x19554, _x19555, _ctx); /*string*/
  kk_integer_t _x19558;
  if (kk_std_core_types__is_Optional(width)) {
    kk_box_t _box_x17971 = width._cons.Optional.value;
    kk_integer_t _width_7032 = kk_integer_unbox(_box_x17971);
    kk_integer_dup(_width_7032);
    kk_std_core_types__optional_drop(width, _ctx);
    _x19558 = _width_7032; /*int*/
    goto _match19559;
  }
  {
    _x19558 = kk_integer_from_small(1); /*int*/
  }
  _match19559: ;
  kk_std_core_types__optional _x19561 = kk_std_core_types__new_Optional(kk_char_box('0', _ctx), _ctx); /*optional<110>*/
  _x19552 = kk_std_core_pad_left(_x19553, _x19558, _x19561, _ctx); /*string*/
  _x19547 = kk_std_core__lp__plus__plus__1_rp_(_x19548, _x19552, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19543, _x19547, _ctx);
}
 
// Show a character as a string

kk_string_t kk_std_core_show_char(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> string */ 
  bool _match_19096 = (c < (' ')); /*bool*/;
  if (_match_19096) {
    bool _match_19106 = (c == 0x000A); /*bool*/;
    if (_match_19106) {
      kk_define_string_literal(, _s19570, 2, "\\n")
      return kk_string_dup(_s19570);
    }
    {
      bool _match_19107 = (c == 0x000D); /*bool*/;
      if (_match_19107) {
        kk_define_string_literal(, _s19571, 2, "\\r")
        return kk_string_dup(_s19571);
      }
      {
        bool _match_19108 = (c == 0x0009); /*bool*/;
        if (_match_19108) {
          kk_define_string_literal(, _s19572, 2, "\\t")
          return kk_string_dup(_s19572);
        }
        {
          bool _match_19109;
          kk_integer_t _x19573 = kk_integer_from_int(c,kk_context()); /*int*/
          _match_19109 = kk_integer_lte(_x19573,(kk_integer_from_small(255)),kk_context()); /*bool*/
          if (_match_19109) {
            kk_string_t _x19574;
            kk_define_string_literal(, _s19575, 2, "\\x")
            _x19574 = kk_string_dup(_s19575); /*string*/
            kk_string_t _x19576;
            kk_integer_t _arg_7506 = kk_integer_from_int(c,kk_context()); /*int*/;
            kk_std_core_types__optional _x19577 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(2)), _ctx); /*optional<110>*/
            kk_std_core_types__optional _x19578;
            kk_box_t _x19579;
            kk_string_t _x19580 = kk_string_empty(); /*string*/
            _x19579 = kk_string_box(_x19580); /*110*/
            _x19578 = kk_std_core_types__new_Optional(_x19579, _ctx); /*optional<110>*/
            _x19576 = kk_std_core_show_hex(_arg_7506, _x19577, kk_std_core_types__new_None(_ctx), _x19578, _ctx); /*string*/
            return kk_std_core__lp__plus__plus__1_rp_(_x19574, _x19576, _ctx);
          }
          {
            bool _match_19110;
            kk_integer_t _x19582 = kk_integer_from_int(c,kk_context()); /*int*/
            _match_19110 = kk_integer_lte(_x19582,(kk_integer_from_int(65535, _ctx)),kk_context()); /*bool*/
            if (_match_19110) {
              kk_string_t _x19583;
              kk_define_string_literal(, _s19584, 2, "\\u")
              _x19583 = kk_string_dup(_s19584); /*string*/
              kk_string_t _x19585;
              kk_integer_t _arg_7591 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19586 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(4)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19587;
              kk_box_t _x19588;
              kk_string_t _x19589 = kk_string_empty(); /*string*/
              _x19588 = kk_string_box(_x19589); /*110*/
              _x19587 = kk_std_core_types__new_Optional(_x19588, _ctx); /*optional<110>*/
              _x19585 = kk_std_core_show_hex(_arg_7591, _x19586, kk_std_core_types__new_None(_ctx), _x19587, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19583, _x19585, _ctx);
            }
            {
              kk_string_t _x19591;
              kk_define_string_literal(, _s19592, 2, "\\U")
              _x19591 = kk_string_dup(_s19592); /*string*/
              kk_string_t _x19593;
              kk_integer_t _arg_7634 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19594 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(6)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19595;
              kk_box_t _x19596;
              kk_string_t _x19597 = kk_string_empty(); /*string*/
              _x19596 = kk_string_box(_x19597); /*110*/
              _x19595 = kk_std_core_types__new_Optional(_x19596, _ctx); /*optional<110>*/
              _x19593 = kk_std_core_show_hex(_arg_7634, _x19594, kk_std_core_types__new_None(_ctx), _x19595, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19591, _x19593, _ctx);
            }
          }
        }
      }
    }
  }
  {
    bool _match_19097 = (c > ('~')); /*bool*/;
    if (_match_19097) {
      bool _match_19101 = (c == 0x000A); /*bool*/;
      if (_match_19101) {
        kk_define_string_literal(, _s19599, 2, "\\n")
        return kk_string_dup(_s19599);
      }
      {
        bool _match_19102 = (c == 0x000D); /*bool*/;
        if (_match_19102) {
          kk_define_string_literal(, _s19600, 2, "\\r")
          return kk_string_dup(_s19600);
        }
        {
          bool _match_19103 = (c == 0x0009); /*bool*/;
          if (_match_19103) {
            kk_define_string_literal(, _s19601, 2, "\\t")
            return kk_string_dup(_s19601);
          }
          {
            bool _match_19104;
            kk_integer_t _x19602 = kk_integer_from_int(c,kk_context()); /*int*/
            _match_19104 = kk_integer_lte(_x19602,(kk_integer_from_small(255)),kk_context()); /*bool*/
            if (_match_19104) {
              kk_string_t _x19603;
              kk_define_string_literal(, _s19604, 2, "\\x")
              _x19603 = kk_string_dup(_s19604); /*string*/
              kk_string_t _x19605;
              kk_integer_t _arg_75060 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19606 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(2)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19607;
              kk_box_t _x19608;
              kk_string_t _x19609 = kk_string_empty(); /*string*/
              _x19608 = kk_string_box(_x19609); /*110*/
              _x19607 = kk_std_core_types__new_Optional(_x19608, _ctx); /*optional<110>*/
              _x19605 = kk_std_core_show_hex(_arg_75060, _x19606, kk_std_core_types__new_None(_ctx), _x19607, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19603, _x19605, _ctx);
            }
            {
              bool _match_19105;
              kk_integer_t _x19611 = kk_integer_from_int(c,kk_context()); /*int*/
              _match_19105 = kk_integer_lte(_x19611,(kk_integer_from_int(65535, _ctx)),kk_context()); /*bool*/
              if (_match_19105) {
                kk_string_t _x19612;
                kk_define_string_literal(, _s19613, 2, "\\u")
                _x19612 = kk_string_dup(_s19613); /*string*/
                kk_string_t _x19614;
                kk_integer_t _arg_75910 = kk_integer_from_int(c,kk_context()); /*int*/;
                kk_std_core_types__optional _x19615 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(4)), _ctx); /*optional<110>*/
                kk_std_core_types__optional _x19616;
                kk_box_t _x19617;
                kk_string_t _x19618 = kk_string_empty(); /*string*/
                _x19617 = kk_string_box(_x19618); /*110*/
                _x19616 = kk_std_core_types__new_Optional(_x19617, _ctx); /*optional<110>*/
                _x19614 = kk_std_core_show_hex(_arg_75910, _x19615, kk_std_core_types__new_None(_ctx), _x19616, _ctx); /*string*/
                return kk_std_core__lp__plus__plus__1_rp_(_x19612, _x19614, _ctx);
              }
              {
                kk_string_t _x19620;
                kk_define_string_literal(, _s19621, 2, "\\U")
                _x19620 = kk_string_dup(_s19621); /*string*/
                kk_string_t _x19622;
                kk_integer_t _arg_76340 = kk_integer_from_int(c,kk_context()); /*int*/;
                kk_std_core_types__optional _x19623 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(6)), _ctx); /*optional<110>*/
                kk_std_core_types__optional _x19624;
                kk_box_t _x19625;
                kk_string_t _x19626 = kk_string_empty(); /*string*/
                _x19625 = kk_string_box(_x19626); /*110*/
                _x19624 = kk_std_core_types__new_Optional(_x19625, _ctx); /*optional<110>*/
                _x19622 = kk_std_core_show_hex(_arg_76340, _x19623, kk_std_core_types__new_None(_ctx), _x19624, _ctx); /*string*/
                return kk_std_core__lp__plus__plus__1_rp_(_x19620, _x19622, _ctx);
              }
            }
          }
        }
      }
    }
    {
      bool _match_19098 = (c == ('\'')); /*bool*/;
      if (_match_19098) {
        kk_define_string_literal(, _s19628, 2, "\\\'")
        return kk_string_dup(_s19628);
      }
      {
        bool _match_19099 = (c == ('"')); /*bool*/;
        if (_match_19099) {
          kk_define_string_literal(, _s19629, 2, "\\\"")
          return kk_string_dup(_s19629);
        }
        {
          bool _match_19100 = (c == ('\\')); /*bool*/;
          if (_match_19100) {
            kk_define_string_literal(, _s19630, 2, "\\\\")
            return kk_string_dup(_s19630);
          }
          {
            return kk_std_core_string(c, _ctx);
          }
        }
      }
    }
  }
}
 
// Show a `:double` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.

kk_string_t kk_std_core_show_fixed(double d, kk_std_core_types__optional precision, kk_context_t* _ctx) { /* (d : double, precision : optional<int>) -> string */ 
  double dabs = kk_double_abs(d); /*double*/;
  bool _match_19094 = (dabs < (1.0e-15)); /*bool*/;
  if (_match_19094) {
    int32_t _x19635;
    kk_integer_t _x19636;
    if (kk_std_core_types__is_Optional(precision)) {
      kk_box_t _box_x17999 = precision._cons.Optional.value;
      kk_integer_t _precision_7723 = kk_integer_unbox(_box_x17999);
      kk_integer_dup(_precision_7723);
      kk_std_core_types__optional_drop(precision, _ctx);
      _x19636 = _precision_7723; /*int*/
      goto _match19637;
    }
    {
      _x19636 = kk_integer_from_small(-2); /*int*/
    }
    _match19637: ;
    _x19635 = kk_std_core_int32(_x19636, _ctx); /*int32*/
    return kk_std_core_show_expx(d, _x19635, _ctx);
  }
  {
    bool _match_19095 = (dabs > (1.0e21)); /*bool*/;
    if (_match_19095) {
      int32_t _x19639;
      kk_integer_t _x19640;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18000 = precision._cons.Optional.value;
        kk_integer_t _precision_77230 = kk_integer_unbox(_box_x18000);
        kk_integer_dup(_precision_77230);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19640 = _precision_77230; /*int*/
        goto _match19641;
      }
      {
        _x19640 = kk_integer_from_small(-2); /*int*/
      }
      _match19641: ;
      _x19639 = kk_std_core_int32(_x19640, _ctx); /*int32*/
      return kk_std_core_show_expx(d, _x19639, _ctx);
    }
    {
      int32_t _x19643;
      kk_integer_t _x19644;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18001 = precision._cons.Optional.value;
        kk_integer_t _precision_77231 = kk_integer_unbox(_box_x18001);
        kk_integer_dup(_precision_77231);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19644 = _precision_77231; /*int*/
        goto _match19645;
      }
      {
        _x19644 = kk_integer_from_small(-2); /*int*/
      }
      _match19645: ;
      _x19643 = kk_std_core_int32(_x19644, _ctx); /*int32*/
      return kk_std_core_show_fixedx(d, _x19643, _ctx);
    }
  }
}
 
// lifted

kk_string_t kk_std_core__lift16744_show_list(kk_string_t sep, kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (sep : string, ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19647 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18002 = _con19647->head;
    kk_std_core__list yy = _con19647->tail;
    kk_string_t y = kk_string_unbox(_box_x18002);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    kk_string_t acc0_16774;
    kk_string_t _x19649;
    kk_string_t _x19650 = kk_string_dup(sep); /*string*/
    _x19649 = kk_std_core__lp__plus__plus__1_rp_(_x19650, y, _ctx); /*string*/
    acc0_16774 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19649, _ctx); /*string*/
    { // tailcall
      ys = yy;
      acc = acc0_16774;
      goto kk__tailcall;
    }
  }
  {
    kk_string_drop(sep, _ctx);
    return acc;
  }
}
 
// monadic lift

kk_string_t kk_std_core__mlift17161_show_list(kk_std_core__list _y_16930, kk_context_t* _ctx) { /* forall<e> (list<string>) -> e string */ 
  kk_string_t _x19651;
  kk_define_string_literal(, _s19652, 1, "[")
  _x19651 = kk_string_dup(_s19652); /*string*/
  kk_string_t _x19653;
  kk_string_t _x19654;
  if (kk_std_core__is_Nil(_y_16930)) {
    _x19654 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19656 = kk_std_core__as_Cons(_y_16930);
    kk_box_t _box_x18003 = _con19656->head;
    kk_std_core__list xx = _con19656->tail;
    kk_string_t x = kk_string_unbox(_box_x18003);
    if (kk_likely(kk_std_core__list_is_unique(_y_16930))) {
      kk_std_core__list_free(_y_16930);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(_y_16930, _ctx);
    }
    kk_string_t _x19658;
    kk_define_string_literal(, _s19659, 1, ",")
    _x19658 = kk_string_dup(_s19659); /*string*/
    _x19654 = kk_std_core__lift16744_show_list(_x19658, xx, x, _ctx); /*string*/
  }
  kk_string_t _x19660;
  kk_define_string_literal(, _s19661, 1, "]")
  _x19660 = kk_string_dup(_s19661); /*string*/
  _x19653 = kk_std_core__lp__plus__plus__1_rp_(_x19654, _x19660, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19651, _x19653, _ctx);
}
 
// Convert a list to a string


// lift anonymous function
struct kk_std_core_show_list_fun19662__t {
  struct kk_function_s _base;
  kk_function_t show_elem;
};
static kk_box_t kk_std_core_show_list_fun19662(kk_function_t _fself, kk_box_t _b_18006, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_list_fun19662(kk_function_t show_elem, kk_context_t* _ctx) {
  struct kk_std_core_show_list_fun19662__t* _self = kk_function_alloc_as(struct kk_std_core_show_list_fun19662__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_show_list_fun19662, kk_context());
  _self->show_elem = show_elem;
  return &_self->_base;
}

static kk_box_t kk_std_core_show_list_fun19662(kk_function_t _fself, kk_box_t _b_18006, kk_context_t* _ctx) {
  struct kk_std_core_show_list_fun19662__t* _self = kk_function_as(struct kk_std_core_show_list_fun19662__t*, _fself);
  kk_function_t show_elem = _self->show_elem; /* (8268) -> 8269 string */
  kk_drop_match(_self, {kk_function_dup(show_elem);}, {}, _ctx)
  kk_string_t _x19663 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), show_elem, (show_elem, _b_18006, _ctx)); /*string*/
  return kk_string_box(_x19663);
}


// lift anonymous function
struct kk_std_core_show_list_fun19665__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_show_list_fun19665(kk_function_t _fself, kk_box_t _b_18011, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_list_fun19665(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_show_list_fun19665, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_show_list_fun19665(kk_function_t _fself, kk_box_t _b_18011, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19666;
  kk_string_t _x19667;
  kk_define_string_literal(, _s19668, 1, "[")
  _x19667 = kk_string_dup(_s19668); /*string*/
  kk_string_t _x19669;
  kk_string_t _x19670;
  kk_std_core__list _match_19093 = kk_std_core__list_unbox(_b_18011, _ctx); /*list<string>*/;
  if (kk_std_core__is_Nil(_match_19093)) {
    _x19670 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19672 = kk_std_core__as_Cons(_match_19093);
    kk_box_t _box_x18009 = _con19672->head;
    kk_std_core__list xx = _con19672->tail;
    kk_string_t x0 = kk_string_unbox(_box_x18009);
    if (kk_likely(kk_std_core__list_is_unique(_match_19093))) {
      kk_std_core__list_free(_match_19093);
    }
    else {
      kk_string_dup(x0);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(_match_19093, _ctx);
    }
    kk_string_t _x19674;
    kk_define_string_literal(, _s19675, 1, ",")
    _x19674 = kk_string_dup(_s19675); /*string*/
    _x19670 = kk_std_core__lift16744_show_list(_x19674, xx, x0, _ctx); /*string*/
  }
  kk_string_t _x19676;
  kk_define_string_literal(, _s19677, 1, "]")
  _x19676 = kk_string_dup(_s19677); /*string*/
  _x19669 = kk_std_core__lp__plus__plus__1_rp_(_x19670, _x19676, _ctx); /*string*/
  _x19666 = kk_std_core__lp__plus__plus__1_rp_(_x19667, _x19669, _ctx); /*string*/
  return kk_string_box(_x19666);
}

kk_string_t kk_std_core_show_list(kk_std_core__list xs, kk_function_t show_elem, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, show-elem : (a) -> e string) -> e string */ 
  kk_std_core__list x_17356 = kk_std_core_map_5(xs, kk_std_core_new_show_list_fun19662(show_elem, _ctx), _ctx); /*list<string>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17356, _ctx);
    kk_box_t _x19664 = kk_std_core_hnd_yield_extend(kk_std_core_new_show_list_fun19665(_ctx), _ctx); /*3860*/
    return kk_string_unbox(_x19664);
  }
  {
    kk_string_t _x19678;
    kk_define_string_literal(, _s19679, 1, "[")
    _x19678 = kk_string_dup(_s19679); /*string*/
    kk_string_t _x19680;
    kk_string_t _x19681;
    if (kk_std_core__is_Nil(x_17356)) {
      _x19681 = kk_string_empty(); /*string*/
    }
    else {
      struct kk_std_core_Cons* _con19683 = kk_std_core__as_Cons(x_17356);
      kk_box_t _box_x18012 = _con19683->head;
      kk_std_core__list xx0 = _con19683->tail;
      kk_string_t x1 = kk_string_unbox(_box_x18012);
      if (kk_likely(kk_std_core__list_is_unique(x_17356))) {
        kk_std_core__list_free(x_17356);
      }
      else {
        kk_string_dup(x1);
        kk_std_core__list_dup(xx0);
        kk_std_core__list_decref(x_17356, _ctx);
      }
      kk_string_t _x19685;
      kk_define_string_literal(, _s19686, 1, ",")
      _x19685 = kk_string_dup(_s19686); /*string*/
      _x19681 = kk_std_core__lift16744_show_list(_x19685, xx0, x1, _ctx); /*string*/
    }
    kk_string_t _x19687;
    kk_define_string_literal(, _s19688, 1, "]")
    _x19687 = kk_string_dup(_s19688); /*string*/
    _x19680 = kk_std_core__lp__plus__plus__1_rp_(_x19681, _x19687, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(_x19678, _x19680, _ctx);
  }
}
 
// Convert an `:int` to a string

kk_string_t kk_std_core_show(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> string */ 
  return kk_integer_to_string(i,kk_context());
}
 
// Show a `:double` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.

kk_string_t kk_std_core_show_1(double d, kk_std_core_types__optional precision, kk_context_t* _ctx) { /* (d : double, precision : optional<int>) -> string */ 
  double dabs = kk_double_abs(d); /*double*/;
  bool _match_19090 = (dabs >= (1.0e-5)); /*bool*/;
  if (_match_19090) {
    bool _match_19091 = (dabs < (1.0e21)); /*bool*/;
    if (_match_19091) {
      kk_std_core_types__optional _x19689;
      kk_box_t _x19690;
      kk_integer_t _x19691;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18015 = precision._cons.Optional.value;
        kk_integer_t _precision_8278 = kk_integer_unbox(_box_x18015);
        kk_integer_dup(_precision_8278);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19691 = _precision_8278; /*int*/
        goto _match19692;
      }
      {
        _x19691 = kk_integer_from_small(-17); /*int*/
      }
      _match19692: ;
      _x19690 = kk_integer_box(_x19691); /*110*/
      _x19689 = kk_std_core_types__new_Optional(_x19690, _ctx); /*optional<110>*/
      return kk_std_core_show_fixed(d, _x19689, _ctx);
    }
    {
      int32_t _x19694;
      kk_integer_t _x19695;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18017 = precision._cons.Optional.value;
        kk_integer_t _precision_82780 = kk_integer_unbox(_box_x18017);
        kk_integer_dup(_precision_82780);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19695 = _precision_82780; /*int*/
        goto _match19696;
      }
      {
        _x19695 = kk_integer_from_small(-17); /*int*/
      }
      _match19696: ;
      _x19694 = kk_std_core_int32(_x19695, _ctx); /*int32*/
      return kk_std_core_show_expx(d, _x19694, _ctx);
    }
  }
  {
    int32_t _x19698;
    kk_integer_t _x19699;
    if (kk_std_core_types__is_Optional(precision)) {
      kk_box_t _box_x18018 = precision._cons.Optional.value;
      kk_integer_t _precision_82781 = kk_integer_unbox(_box_x18018);
      kk_integer_dup(_precision_82781);
      kk_std_core_types__optional_drop(precision, _ctx);
      _x19699 = _precision_82781; /*int*/
      goto _match19700;
    }
    {
      _x19699 = kk_integer_from_small(-17); /*int*/
    }
    _match19700: ;
    _x19698 = kk_std_core_int32(_x19699, _ctx); /*int32*/
    return kk_std_core_show_expx(d, _x19698, _ctx);
  }
}
 
// lifted

kk_string_t kk_std_core__lift16745_show_3(kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19708 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18020 = _con19708->head;
    kk_std_core__list yy = _con19708->tail;
    kk_string_t y = kk_string_unbox(_box_x18020);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x19710;
      kk_string_t _x19711;
      kk_string_t _x19712 = kk_string_empty(); /*string*/
      _x19711 = kk_std_core__lp__plus__plus__1_rp_(_x19712, y, _ctx); /*string*/
      _x19710 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19711, _ctx); /*string*/
      ys = yy;
      acc = _x19710;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// Show a string as a string literal


// lift anonymous function
struct kk_std_core_show_fun19718__t_3 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_show_fun19718_3(kk_function_t _fself, kk_box_t _b_18023, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_fun19718_3(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_show_fun19718_3, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_show_fun19718_3(kk_function_t _fself, kk_box_t _b_18023, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19719;
  kk_char_t _x19720 = kk_char_unbox(_b_18023, _ctx); /*char*/
  _x19719 = kk_std_core_show_char(_x19720, _ctx); /*string*/
  return kk_string_box(_x19719);
}

kk_string_t kk_std_core_show_3(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_string_t _x19714;
  kk_define_string_literal(, _s19715, 1, "\"")
  _x19714 = kk_string_dup(_s19715); /*string*/
  kk_string_t _x19716;
  kk_string_t _x19717;
  kk_std_core__list _b_18024_18021 = kk_std_core_list_4(s, _ctx); /*list<char>*/;
  kk_std_core__list xs_16615 = kk_std_core_map_5(_b_18024_18021, kk_std_core_new_show_fun19718_3(_ctx), _ctx); /*list<string>*/;
  if (kk_std_core__is_Nil(xs_16615)) {
    _x19717 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19722 = kk_std_core__as_Cons(xs_16615);
    kk_box_t _box_x18026 = _con19722->head;
    kk_std_core__list xx = _con19722->tail;
    kk_string_t x = kk_string_unbox(_box_x18026);
    if (kk_likely(kk_std_core__list_is_unique(xs_16615))) {
      kk_std_core__list_free(xs_16615);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs_16615, _ctx);
    }
    _x19717 = kk_std_core__lift16745_show_3(xx, x, _ctx); /*string*/
  }
  kk_string_t _x19724;
  kk_define_string_literal(, _s19725, 1, "\"")
  _x19724 = kk_string_dup(_s19725); /*string*/
  _x19716 = kk_std_core__lp__plus__plus__1_rp_(_x19717, _x19724, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19714, _x19716, _ctx);
}
extern kk_string_t kk_std_core_show_fun19730_7(kk_function_t _fself, kk_box_t _b_18029, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19731 = kk_string_unbox(_b_18029); /*string*/
  return kk_std_core_show_3(_x19731, _ctx);
}
extern kk_string_t kk_std_core_show_fun19732_8(kk_function_t _fself, kk_box_t _b_18034, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x19733 = kk_integer_unbox(_b_18034); /*int*/
  return kk_std_core_show(_x19733, _ctx);
}
extern kk_string_t kk_std_core_show_fun19734_9(kk_function_t _fself, kk_box_t _b_18039, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x19735 = kk_bool_unbox(_b_18039); /*bool*/
  return kk_std_core_show_4(_x19735, _ctx);
}

kk_unit_t kk_std_core_prints(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_std_core_types__maybe _match_19089;
  kk_box_t _x19744;
  kk_ref_t _x19745 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  _x19744 = kk_ref_get(_x19745,kk_context()); /*184*/
  _match_19089 = kk_std_core_types__maybe_unbox(_x19744, _ctx); /*maybe<(string) -> console ()>*/
  if (kk_std_core_types__is_Nothing(_match_19089)) {
    kk_std_core_xprints(s, _ctx); return kk_Unit;
  }
  {
    kk_box_t _fun_unbox_x18046 = _match_19089._cons.Just.value;
    kk_box_t _x19746;
    kk_function_t _x19747 = kk_function_unbox(_fun_unbox_x18046); /*(18047) -> console 18048*/
    _x19746 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19747, (_x19747, kk_string_box(s), _ctx)); /*18048*/
    kk_unit_unbox(_x19746); return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_core__default_exn_fun19754__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core__default_exn_fun19754(kk_function_t _fself, kk_std_core_hnd__marker _b_18064, kk_std_core_hnd__ev _b_18065, kk_box_t _b_18066, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19754(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__default_exn_fun19754, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_core__default_exn_fun19755__t {
  struct kk_function_s _base;
  kk_box_t _b_18066;
};
static kk_box_t kk_std_core__default_exn_fun19755(kk_function_t _fself, kk_function_t _b_18061, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19755(kk_box_t _b_18066, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19755__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19755__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19755, kk_context());
  _self->_b_18066 = _b_18066;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core__default_exn_fun19757__t {
  struct kk_function_s _base;
  kk_function_t _b_18061;
};
static kk_unit_t kk_std_core__default_exn_fun19757(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18062, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19757(kk_function_t _b_18061, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19757__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19757__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19757, kk_context());
  _self->_b_18061 = _b_18061;
  return &_self->_base;
}

static kk_unit_t kk_std_core__default_exn_fun19757(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18062, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19757__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19757__t*, _fself);
  kk_function_t _b_18061 = _self->_b_18061; /* (std/core/hnd/resume-result<3924,3927>) -> 3926 3927 */
  kk_drop_match(_self, {kk_function_dup(_b_18061);}, {}, _ctx)
  kk_box_t _x19758 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _b_18061, (_b_18061, _b_18062, _ctx)); /*3927*/
  return kk_unit_unbox(_x19758);
}
static kk_box_t kk_std_core__default_exn_fun19755(kk_function_t _fself, kk_function_t _b_18061, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19755__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19755__t*, _fself);
  kk_box_t _b_18066 = _self->_b_18066; /* 51 */
  kk_drop_match(_self, {kk_box_dup(_b_18066);}, {}, _ctx)
  kk_unit_t _x19756 = kk_Unit;
  kk_function_t ___wildcard__585__45_18083 = kk_std_core__new_default_exn_fun19757(_b_18061, _ctx); /*(std/core/hnd/resume-result<9304,()>) -> <console|9302> ()*/;
  kk_function_drop(___wildcard__585__45_18083, _ctx);
  kk_unit_t __ = kk_Unit;
  kk_string_t _x19759;
  kk_define_string_literal(, _s19760, 20, "uncaught exception: ")
  _x19759 = kk_string_dup(_s19760); /*string*/
  kk_std_core_prints(_x19759, _ctx);
  kk_string_t _x19761;
  kk_std_core__exception _match_19087 = kk_std_core__exception_unbox(_b_18066, _ctx); /*exception*/;
  {
    kk_string_t _x = _match_19087.message;
    kk_string_dup(_x);
    kk_std_core__exception_drop(_match_19087, _ctx);
    _x19761 = _x; /*string*/
  }
  kk_std_core_printsln(_x19761, _ctx);
  return kk_unit_box(_x19756);
}
static kk_box_t kk_std_core__default_exn_fun19754(kk_function_t _fself, kk_std_core_hnd__marker _b_18064, kk_std_core_hnd__ev _b_18065, kk_box_t _b_18066, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_hnd__ev_dropn(_b_18065, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18064, kk_std_core__new_default_exn_fun19755(_b_18066, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core__default_exn_fun19762__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core__default_exn_fun19762(kk_function_t _fself, kk_box_t _b_18071, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19762(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__default_exn_fun19762, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core__default_exn_fun19762(kk_function_t _fself, kk_box_t _b_18071, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_18071;
}


// lift anonymous function
struct kk_std_core__default_exn_fun19763__t {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_box_t kk_std_core__default_exn_fun19763(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19763(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19763__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19763__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19763, kk_context());
  _self->action = action;
  return &_self->_base;
}

static kk_box_t kk_std_core__default_exn_fun19763(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19763__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19763__t*, _fself);
  kk_function_t action = _self->action; /* () -> <exn,console|9302> () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t _x19764 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
  return kk_unit_box(_x19764);
}

kk_unit_t kk_std_core__default_exn(kk_function_t action, kk_context_t* _ctx) { /* forall<e> (action : () -> <console,exn|e> ()) -> <console|e> () */ 
  int32_t _b_18072_18067 = ((int32_t)KI32(0)); /*int32*/;
  kk_box_t _x19751;
  kk_std_core__hnd_exn _x19752;
  kk_std_core_hnd__clause1 _x19753 = kk_std_core_hnd__new_Clause1(kk_std_core__new_default_exn_fun19754(_ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19752 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19753, _ctx); /*.hnd-exn<11,12>*/
  _x19751 = kk_std_core__handle_exn(_b_18072_18067, _x19752, kk_std_core__new_default_exn_fun19762(_ctx), kk_std_core__new_default_exn_fun19763(action, _ctx), _ctx); /*1964*/
  kk_unit_unbox(_x19751); return kk_Unit;
}
 
// Get (zero-based) element `n`  of a list. Return a `:maybe` type.

kk_std_core_types__maybe kk_std_core__lp__lb__rb__2_rp_(kk_std_core__list xs, kk_integer_t n, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> maybe<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19765 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19765->head;
    kk_std_core__list xx = _con19765->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool _match_19085;
    kk_integer_t _x19766 = kk_integer_dup(n); /*int*/
    _match_19085 = kk_integer_gt(_x19766,(kk_integer_from_small(0)),kk_context()); /*bool*/
    if (_match_19085) {
      kk_box_drop(x, _ctx);
      { // tailcall
        kk_integer_t _x19767 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        xs = xx;
        n = _x19767;
        goto kk__tailcall;
      }
    }
    {
      kk_std_core__list_drop(xx, _ctx);
      bool _match_19086 = kk_integer_eq(n,(kk_integer_from_small(0)),kk_context()); /*bool*/;
      if (_match_19086) {
        return kk_std_core_types__new_Just(x, _ctx);
      }
      {
        kk_box_drop(x, _ctx);
        return kk_std_core_types__new_Nothing(_ctx);
      }
    }
  }
  {
    kk_integer_drop(n, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// O(1). Return the string slice from the end of `slice` argument
// to the end of the string.

kk_std_core__sslice kk_std_core_after(kk_std_core__sslice slice0, kk_context_t* _ctx) { /* (slice : sslice) -> sslice */ 
  {
    kk_string_t s = slice0.str;
    kk_ssize_t start0 = slice0.start;
    kk_ssize_t len0 = slice0.len;
    kk_string_dup(s);
    kk_std_core__sslice_drop(slice0, _ctx);
    kk_string_t _x19768 = kk_string_dup(s); /*string*/
    kk_ssize_t _x19769 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x19770;
    kk_ssize_t _x19771 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x19772 = (start0 + len0); /*ssize_t*/
    _x19770 = (_x19771 - _x19772); /*ssize_t*/
    return kk_std_core__new_Sslice(_x19768, _x19769, _x19770, _ctx);
  }
}
 
// monadic lift

bool kk_std_core__mlift17162_all(kk_function_t predicate, kk_std_core__list xx, bool _y_16933, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */ 
  if (_y_16933) {
    return kk_std_core_all(xx, predicate, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    return false;
  }
}
 
// Do all elements satisfy a predicate ?


// lift anonymous function
struct kk_std_core_all_fun19776__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_all_fun19776(kk_function_t _fself, kk_box_t _b_18085, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_all_fun19776(kk_function_t predicate0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_all_fun19776__t* _self = kk_function_alloc_as(struct kk_std_core_all_fun19776__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_all_fun19776, kk_context());
  _self->predicate0 = predicate0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_all_fun19776(kk_function_t _fself, kk_box_t _b_18085, kk_context_t* _ctx) {
  struct kk_std_core_all_fun19776__t* _self = kk_function_as(struct kk_std_core_all_fun19776__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (9539) -> 9540 bool */
  kk_std_core__list xx0 = _self->xx0; /* list<9539> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  bool _x19777;
  bool _x19778 = kk_bool_unbox(_b_18085); /*bool*/
  _x19777 = kk_std_core__mlift17162_all(predicate0, xx0, _x19778, _ctx); /*bool*/
  return kk_bool_box(_x19777);
}

bool kk_std_core_all(kk_std_core__list xs, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19773 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19773->head;
    kk_std_core__list xx0 = _con19773->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool x0_17364;
    kk_function_t _x19774 = kk_function_dup(predicate0); /*(9539) -> 9540 bool*/
    x0_17364 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19774, (_x19774, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19775 = kk_std_core_hnd_yield_extend(kk_std_core_new_all_fun19776(predicate0, xx0, _ctx), _ctx); /*3860*/
      return kk_bool_unbox(_x19775);
    }
    if (x0_17364) { // tailcall
                    xs = xx0;
                    goto kk__tailcall;
    }
    {
      kk_function_drop(predicate0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return false;
    }
  }
  {
    kk_function_drop(predicate0, _ctx);
    return true;
  }
}
 
// monadic lift

bool kk_std_core__mlift17163_any(kk_function_t predicate, kk_std_core__list xx, bool _y_16937, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */ 
  if (_y_16937) {
    kk_function_drop(predicate, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    return true;
  }
  {
    return kk_std_core_any(xx, predicate, _ctx);
  }
}
 
// Are there any elements in a list that satisfy a predicate ?


// lift anonymous function
struct kk_std_core_any_fun19782__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_any_fun19782(kk_function_t _fself, kk_box_t _b_18089, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_any_fun19782(kk_function_t predicate0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_any_fun19782__t* _self = kk_function_alloc_as(struct kk_std_core_any_fun19782__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_any_fun19782, kk_context());
  _self->predicate0 = predicate0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_any_fun19782(kk_function_t _fself, kk_box_t _b_18089, kk_context_t* _ctx) {
  struct kk_std_core_any_fun19782__t* _self = kk_function_as(struct kk_std_core_any_fun19782__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (9567) -> 9568 bool */
  kk_std_core__list xx0 = _self->xx0; /* list<9567> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  bool _x19783;
  bool _x19784 = kk_bool_unbox(_b_18089); /*bool*/
  _x19783 = kk_std_core__mlift17163_any(predicate0, xx0, _x19784, _ctx); /*bool*/
  return kk_bool_box(_x19783);
}

bool kk_std_core_any(kk_std_core__list xs, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19779 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19779->head;
    kk_std_core__list xx0 = _con19779->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool x0_17367;
    kk_function_t _x19780 = kk_function_dup(predicate0); /*(9567) -> 9568 bool*/
    x0_17367 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19780, (_x19780, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19781 = kk_std_core_hnd_yield_extend(kk_std_core_new_any_fun19782(predicate0, xx0, _ctx), _ctx); /*3860*/
      return kk_bool_unbox(_x19781);
    }
    if (x0_17367) {
      kk_function_drop(predicate0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return true;
    }
    { // tailcall
      xs = xx0;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(predicate0, _ctx);
    return false;
  }
}
 
// Return the element at position `index` in vector `v`, or `Nothing` if out of bounds

kk_std_core_types__maybe kk_std_core_at(kk_vector_t v, kk_integer_t index, kk_context_t* _ctx) { /* forall<a> (v : vector<a>, index : int) -> maybe<a> */ 
  kk_ssize_t idx = kk_std_core_ssize__t(index, _ctx); /*ssize_t*/;
  bool _match_19082;
  kk_ssize_t _x19785;
  kk_vector_t _x19786 = kk_vector_dup(v); /*vector<9638>*/
  _x19785 = kk_vector_len(_x19786,kk_context()); /*ssize_t*/
  _match_19082 = (_x19785 <= idx); /*bool*/
  if (_match_19082) {
    kk_vector_drop(v, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t _x19787 = kk_vector_at(v,idx,kk_context()); /*223*/
    return kk_std_core_types__new_Just(_x19787, _ctx);
  }
}
 
// O(`n`). The first `n` (default = `1`) characters in a string.

kk_std_core__sslice kk_std_core_first(kk_string_t s, kk_std_core_types__optional n, kk_context_t* _ctx) { /* (s : string, n : optional<int>) -> sslice */ 
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_19081;
  kk_integer_t _x19791;
  if (kk_std_core_types__is_Optional(n)) {
    kk_box_t _box_x18092 = n._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18092);
    kk_integer_dup(_n_9710);
    _x19791 = _n_9710; /*int*/
    goto _match19792;
  }
  {
    _x19791 = kk_integer_from_small(1); /*int*/
  }
  _match19792: ;
  _match_19081 = kk_integer_eq(_x19791,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19081) {
    kk_std_core_types__optional_drop(n, _ctx);
    return slice0;
  }
  {
    kk_integer_t _x19794;
    kk_integer_t _x19795;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18093 = n._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18093);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(n, _ctx);
      _x19795 = _n_97100; /*int*/
      goto _match19796;
    }
    {
      _x19795 = kk_integer_from_small(1); /*int*/
    }
    _match19796: ;
    _x19794 = kk_integer_sub(_x19795,(kk_integer_from_small(1)),kk_context()); /*int*/
    return kk_std_core_extend(slice0, _x19794, _ctx);
  }
}
 
// Convert the first character of a string to uppercase.

kk_string_t kk_std_core_capitalize(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_string_t _x19798;
  kk_string_t _x19799;
  kk_std_core__sslice _x19800;
  kk_std_core__sslice slice0;
  kk_string_t _x19801 = kk_string_dup(s); /*string*/
  slice0 = kk_std_core_first1(_x19801, _ctx); /*sslice*/
  bool _match_19078;
  kk_integer_t _x19802;
  kk_std_core_types__optional _match_19080 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19080)) {
    kk_box_t _box_x18094 = _match_19080._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18094);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_19080, _ctx);
    _x19802 = _n_9710; /*int*/
    goto _match19803;
  }
  {
    _x19802 = kk_integer_from_small(1); /*int*/
  }
  _match19803: ;
  _match_19078 = kk_integer_eq(_x19802,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19078) {
    _x19800 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x19805;
    kk_integer_t _x19806;
    kk_std_core_types__optional _match_19079 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19079)) {
      kk_box_t _box_x18095 = _match_19079._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18095);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_19079, _ctx);
      _x19806 = _n_97100; /*int*/
      goto _match19807;
    }
    {
      _x19806 = kk_integer_from_small(1); /*int*/
    }
    _match19807: ;
    _x19805 = kk_integer_sub(_x19806,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x19800 = kk_std_core_extend(slice0, _x19805, _ctx); /*sslice*/
  }
  _x19799 = kk_std_core_string_3(_x19800, _ctx); /*string*/
  _x19798 = kk_std_core_to_upper(_x19799, _ctx); /*string*/
  kk_string_t _x19809;
  kk_std_core__sslice _x19810;
  kk_std_core__sslice slice2 = kk_std_core_first1(s, _ctx); /*sslice*/;
  kk_std_core__sslice slice1_16624;
  bool _match_19075;
  kk_integer_t _x19811;
  kk_std_core_types__optional _match_19077 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19077)) {
    kk_box_t _box_x18096 = _match_19077._cons.Optional.value;
    kk_integer_t _n_971000 = kk_integer_unbox(_box_x18096);
    kk_integer_dup(_n_971000);
    kk_std_core_types__optional_drop(_match_19077, _ctx);
    _x19811 = _n_971000; /*int*/
    goto _match19812;
  }
  {
    _x19811 = kk_integer_from_small(1); /*int*/
  }
  _match19812: ;
  _match_19075 = kk_integer_eq(_x19811,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19075) {
    slice1_16624 = slice2; /*sslice*/
  }
  else {
    kk_integer_t _x19814;
    kk_integer_t _x19815;
    kk_std_core_types__optional _match_19076 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19076)) {
      kk_box_t _box_x18097 = _match_19076._cons.Optional.value;
      kk_integer_t _n_971001 = kk_integer_unbox(_box_x18097);
      kk_integer_dup(_n_971001);
      kk_std_core_types__optional_drop(_match_19076, _ctx);
      _x19815 = _n_971001; /*int*/
      goto _match19816;
    }
    {
      _x19815 = kk_integer_from_small(1); /*int*/
    }
    _match19816: ;
    _x19814 = kk_integer_sub(_x19815,(kk_integer_from_small(1)),kk_context()); /*int*/
    slice1_16624 = kk_std_core_extend(slice2, _x19814, _ctx); /*sslice*/
  }
  {
    kk_string_t s1 = slice1_16624.str;
    kk_ssize_t start0 = slice1_16624.start;
    kk_ssize_t len0 = slice1_16624.len;
    kk_string_dup(s1);
    kk_std_core__sslice_drop(slice1_16624, _ctx);
    kk_string_t _x19818 = kk_string_dup(s1); /*string*/
    kk_ssize_t _x19819 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x19820;
    kk_ssize_t _x19821 = kk_string_len(s1,kk_context()); /*ssize_t*/
    kk_ssize_t _x19822 = (start0 + len0); /*ssize_t*/
    _x19820 = (_x19821 - _x19822); /*ssize_t*/
    _x19810 = kk_std_core__new_Sslice(_x19818, _x19819, _x19820, _ctx); /*sslice*/
  }
  _x19809 = kk_std_core_string_3(_x19810, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19798, _x19809, _ctx);
}
 
// Catch any exception raised in `action` and handle it.
// Use `on-exn` or `on-exit` when appropiate.


// lift anonymous function
struct kk_std_core_try_fun19825__t {
  struct kk_function_s _base;
  kk_function_t hndl;
};
static kk_box_t kk_std_core_try_fun19825(kk_function_t _fself, kk_std_core_hnd__marker _b_18099, kk_std_core_hnd__ev _b_18100, kk_box_t _b_18101, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19825(kk_function_t hndl, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19825__t* _self = kk_function_alloc_as(struct kk_std_core_try_fun19825__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19825, kk_context());
  _self->hndl = hndl;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19826__t {
  struct kk_function_s _base;
  kk_box_t _b_18101;
  kk_function_t hndl;
};
static kk_box_t kk_std_core_try_fun19826(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19826(kk_box_t _b_18101, kk_function_t hndl, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19826__t* _self = kk_function_alloc_as(struct kk_std_core_try_fun19826__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19826, kk_context());
  _self->_b_18101 = _b_18101;
  _self->hndl = hndl;
  return &_self->_base;
}

static kk_box_t kk_std_core_try_fun19826(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19826__t* _self = kk_function_as(struct kk_std_core_try_fun19826__t*, _fself);
  kk_box_t _b_18101 = _self->_b_18101; /* 51 */
  kk_function_t hndl = _self->hndl; /* (exception) -> 9877 9876 */
  kk_drop_match(_self, {kk_box_dup(_b_18101);kk_function_dup(hndl);}, {}, _ctx)
  kk_function_drop(___wildcard__585__45, _ctx);
  kk_std_core__exception _x19827 = kk_std_core__exception_unbox(_b_18101, _ctx); /*exception*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core__exception, kk_context_t*), hndl, (hndl, _x19827, _ctx));
}
static kk_box_t kk_std_core_try_fun19825(kk_function_t _fself, kk_std_core_hnd__marker _b_18099, kk_std_core_hnd__ev _b_18100, kk_box_t _b_18101, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19825__t* _self = kk_function_as(struct kk_std_core_try_fun19825__t*, _fself);
  kk_function_t hndl = _self->hndl; /* (exception) -> 9877 9876 */
  kk_drop_match(_self, {kk_function_dup(hndl);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_18100, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18099, kk_std_core_new_try_fun19826(_b_18101, hndl, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_fun19828__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19828(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19828(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19828, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19828(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _x;
}

kk_box_t kk_std_core_try(kk_function_t action, kk_function_t hndl, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <exn|e> a, hndl : (exception) -> e a) -> e a */ 
  kk_std_core__hnd_exn _x19823;
  kk_std_core_hnd__clause1 _x19824 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_fun19825(hndl, _ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19823 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19824, _ctx); /*.hnd-exn<11,12>*/
  return kk_std_core__handle_exn(((int32_t)KI32(0)), _x19823, kk_std_core_new_try_fun19828(_ctx), action, _ctx);
}
 
// Transform an exception effect to an  `:error` type.


// lift anonymous function
struct kk_std_core_try_fun19832__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19832_1(kk_function_t _fself, kk_std_core_hnd__marker _b_18111, kk_std_core_hnd__ev _b_18112, kk_box_t _b_18113, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19832_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19832_1, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_core_try_fun19833__t_1 {
  struct kk_function_s _base;
  kk_box_t _b_18113;
};
static kk_box_t kk_std_core_try_fun19833_1(kk_function_t _fself, kk_function_t _b_18108, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19833_1(kk_box_t _b_18113, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19833__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19833__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19833_1, kk_context());
  _self->_b_18113 = _b_18113;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19835__t_1 {
  struct kk_function_s _base;
  kk_function_t _b_18108;
};
static kk_std_core__error kk_std_core_try_fun19835_1(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18109, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19835_1(kk_function_t _b_18108, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19835__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19835__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19835_1, kk_context());
  _self->_b_18108 = _b_18108;
  return &_self->_base;
}

static kk_std_core__error kk_std_core_try_fun19835_1(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18109, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19835__t_1* _self = kk_function_as(struct kk_std_core_try_fun19835__t_1*, _fself);
  kk_function_t _b_18108 = _self->_b_18108; /* (std/core/hnd/resume-result<3924,3927>) -> 3926 3927 */
  kk_drop_match(_self, {kk_function_dup(_b_18108);}, {}, _ctx)
  kk_box_t _x19836 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _b_18108, (_b_18108, _b_18109, _ctx)); /*3927*/
  return kk_std_core__error_unbox(_x19836, _ctx);
}
static kk_box_t kk_std_core_try_fun19833_1(kk_function_t _fself, kk_function_t _b_18108, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19833__t_1* _self = kk_function_as(struct kk_std_core_try_fun19833__t_1*, _fself);
  kk_box_t _b_18113 = _self->_b_18113; /* 51 */
  kk_drop_match(_self, {kk_box_dup(_b_18113);}, {}, _ctx)
  kk_std_core__error _x19834;
  kk_function_t ___wildcard__585__45_18134 = kk_std_core_new_try_fun19835_1(_b_18108, _ctx); /*(std/core/hnd/resume-result<9863,error<9915>>) -> 9916 error<9915>*/;
  kk_function_drop(___wildcard__585__45_18134, _ctx);
  kk_std_core__exception _x19837 = kk_std_core__exception_unbox(_b_18113, _ctx); /*exception*/
  _x19834 = kk_std_core__new_Error(_x19837, _ctx); /*error<30>*/
  return kk_std_core__error_box(_x19834, _ctx);
}
static kk_box_t kk_std_core_try_fun19832_1(kk_function_t _fself, kk_std_core_hnd__marker _b_18111, kk_std_core_hnd__ev _b_18112, kk_box_t _b_18113, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_hnd__ev_dropn(_b_18112, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18111, kk_std_core_new_try_fun19833_1(_b_18113, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_fun19838__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19838_1(kk_function_t _fself, kk_box_t _b_18120, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19838_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19838_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19838_1(kk_function_t _fself, kk_box_t _b_18120, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_18120;
}


// lift anonymous function
struct kk_std_core_try_fun19839__t_1 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_box_t kk_std_core_try_fun19839_1(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19839_1(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19839__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19839__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19839_1, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19842__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19842_1(kk_function_t _fself, kk_box_t _b_18115, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19842_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19842_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19842_1(kk_function_t _fself, kk_box_t _b_18115, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__error _x19843 = kk_std_core__new_Ok(_b_18115, _ctx); /*error<30>*/
  return kk_std_core__error_box(_x19843, _ctx);
}
static kk_box_t kk_std_core_try_fun19839_1(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19839__t_1* _self = kk_function_as(struct kk_std_core_try_fun19839__t_1*, _fself);
  kk_function_t action = _self->action; /* () -> <exn|9916> 9915 */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_std_core__error _x19840;
  kk_box_t x0_17372 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx)); /*9915*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x0_17372, _ctx);
    kk_box_t _x19841 = kk_std_core_hnd_yield_extend(kk_std_core_new_try_fun19842_1(_ctx), _ctx); /*3860*/
    _x19840 = kk_std_core__error_unbox(_x19841, _ctx); /*error<9915>*/
  }
  else {
    _x19840 = kk_std_core__new_Ok(x0_17372, _ctx); /*error<9915>*/
  }
  return kk_std_core__error_box(_x19840, _ctx);
}

kk_std_core__error kk_std_core_try_1(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <exn|e> a) -> e error<a> */ 
  int32_t _b_18121_18116 = ((int32_t)KI32(0)); /*int32*/;
  kk_box_t _x19829;
  kk_std_core__hnd_exn _x19830;
  kk_std_core_hnd__clause1 _x19831 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_fun19832_1(_ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19830 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19831, _ctx); /*.hnd-exn<11,12>*/
  _x19829 = kk_std_core__handle_exn(_b_18121_18116, _x19830, kk_std_core_new_try_fun19838_1(_ctx), kk_std_core_new_try_fun19839_1(action, _ctx), _ctx); /*1964*/
  return kk_std_core__error_unbox(_x19829, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_cdivmod_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> (int, int) */ 
  bool _match_19073;
  kk_integer_t _x19844 = kk_integer_dup(n); /*int*/
  _match_19073 = kk_integer_lte(_x19844,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19073) {
    kk_integer_drop(n, _ctx);
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(i), kk_integer_box(kk_integer_from_small(0)), _ctx);
  }
  {
    kk_integer_t cq;
    kk_integer_t _x19845 = kk_integer_dup(i); /*int*/
    kk_integer_t _x19846 = kk_integer_dup(n); /*int*/
    cq = kk_std_core_cdiv_exp10(_x19845, _x19846, _ctx); /*int*/
    kk_integer_t cr;
    kk_integer_t _x19847;
    kk_integer_t _x19848 = kk_integer_dup(cq); /*int*/
    _x19847 = kk_std_core_mul_exp10(_x19848, n, _ctx); /*int*/
    cr = kk_integer_sub(i,_x19847,kk_context()); /*int*/
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(cq), kk_integer_box(cr), _ctx);
  }
}
 
// Concatenate a list of `:maybe` values

kk_std_core__list kk_std_core__ctail_concat_maybe(kk_std_core__list xs, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<maybe<a>>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19849 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18143 = _con19849->head;
    kk_std_core__list xx = _con19849->tail;
    kk_std_core_types__maybe x = kk_std_core_types__maybe_unbox(_box_x18143, NULL);
    kk_reuse_t _ru_18891 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core_types__maybe_dup(x);
      kk_box_drop(_box_x18143, _ctx);
      _ru_18891 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_std_core_types__maybe_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18891 = kk_reuse_null;
    }
    if (kk_std_core_types__is_Just(x)) {
      kk_box_t y = x._cons.Just.value;
      kk_std_core__list _ctail_16817 = kk_std_core__list_hole(); /*list<10055>*/;
      kk_std_core__list _ctail_16818 = kk_std_core__new_Cons(_ru_18891, y, _ctail_16817, _ctx); /*list<10055>*/;
      { // tailcall
        kk_std_core_types__ctail _x19851;
        kk_box_t* _b_18154_18149 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16818)->tail)); /*cfield<list<10055>>*/;
        _x19851 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16818, _ctx)),_b_18154_18149); /*ctail<0>*/
        xs = xx;
        _acc = _x19851;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18891, _ctx);
      { // tailcall
        xs = xx;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_box_t _x19852 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19852, _ctx);
  }
}
 
// Concatenate a list of `:maybe` values

kk_std_core__list kk_std_core_concat_maybe(kk_std_core__list xs0, kk_context_t* _ctx) { /* forall<a> (xs : list<maybe<a>>) -> list<a> */ 
  kk_std_core_types__ctail _x19853 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_concat_maybe(xs0, _x19853, _ctx);
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17165_op(kk_function_t action, kk_ssize_t end, kk_ssize_t i, kk_std_core_types__maybe _y_16945, kk_context_t* _ctx) { /* forall<a,e> (action : (ssize_t) -> e maybe<a>, end : ssize_t, i : ssize_t, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_16945)) {
    kk_ssize_t i0_16777 = kk_std_core_incr_1(i, _ctx); /*ssize_t*/;
    return kk_std_core__lift16746_for_whilez(action, end, i0_16777, _ctx);
  }
  {
    kk_box_t x = _y_16945._cons.Just.value;
    kk_function_drop(action, _ctx);
    return kk_std_core_types__new_Just(x, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16746_for_whilez_fun19856__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_ssize_t end0;
  kk_ssize_t i0;
};
static kk_box_t kk_std_core__lift16746_for_whilez_fun19856(kk_function_t _fself, kk_box_t _b_18161, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16746_for_whilez_fun19856(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16746_for_whilez_fun19856__t* _self = kk_function_alloc_as(struct kk_std_core__lift16746_for_whilez_fun19856__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16746_for_whilez_fun19856, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16746_for_whilez_fun19856(kk_function_t _fself, kk_box_t _b_18161, kk_context_t* _ctx) {
  struct kk_std_core__lift16746_for_whilez_fun19856__t* _self = kk_function_as(struct kk_std_core__lift16746_for_whilez_fun19856__t*, _fself);
  kk_function_t action0 = _self->action0; /* (ssize_t) -> 10110 maybe<10109> */
  kk_ssize_t end0 = _self->end0; /* ssize_t */
  kk_ssize_t i0 = _self->i0; /* ssize_t */
  kk_drop_match(_self, {kk_function_dup(action0);;;}, {}, _ctx)
  kk_std_core_types__maybe _x19857;
  kk_std_core_types__maybe _x19858 = kk_std_core_types__maybe_unbox(_b_18161, _ctx); /*maybe<10109>*/
  _x19857 = kk_std_core__mlift17165_op(action0, end0, i0, _x19858, _ctx); /*maybe<10109>*/
  return kk_std_core_types__maybe_box(_x19857, _ctx);
}

kk_std_core_types__maybe kk_std_core__lift16746_for_whilez(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) { /* forall<a,e> (action : (ssize_t) -> e maybe<a>, end : ssize_t, i : ssize_t) -> e maybe<a> */ 
  kk__tailcall: ;
  bool _match_19071 = (i0 <= end0); /*bool*/;
  if (_match_19071) {
    kk_std_core_types__maybe x0_17375;
    kk_function_t _x19854 = kk_function_dup(action0); /*(ssize_t) -> 10110 maybe<10109>*/
    x0_17375 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_ssize_t, kk_context_t*), _x19854, (_x19854, i0, _ctx)); /*maybe<10109>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17375, _ctx);
      kk_box_t _x19855 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16746_for_whilez_fun19856(action0, end0, i0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19855, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17375)) {
      kk_ssize_t i0_167770 = kk_std_core_incr_1(i0, _ctx); /*ssize_t*/;
      { // tailcall
        i0 = i0_167770;
        goto kk__tailcall;
      }
    }
    {
      kk_box_t x1 = x0_17375._cons.Just.value;
      kk_function_drop(action0, _ctx);
      return kk_std_core_types__new_Just(x1, _ctx);
    }
  }
  {
    kk_function_drop(action0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17166_foreach_while(kk_function_t action, kk_std_core__list xx, kk_std_core_types__maybe _y_16950, kk_context_t* _ctx) { /* forall<a,b,e> (action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e maybe<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16950)) {
    return kk_std_core_foreach_while(xx, action, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    return _y_16950;
  }
}
 
// Invoke `action` for each element of a list while `action` return `Nothing`


// lift anonymous function
struct kk_std_core_foreach_while_fun19865__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foreach_while_fun19865(kk_function_t _fself, kk_box_t _b_18165, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19865(kk_function_t action0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19865__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19865__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19865, kk_context());
  _self->action0 = action0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_while_fun19865(kk_function_t _fself, kk_box_t _b_18165, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19865__t* _self = kk_function_as(struct kk_std_core_foreach_while_fun19865__t*, _fself);
  kk_function_t action0 = _self->action0; /* (10243) -> 10245 maybe<10244> */
  kk_std_core__list xx0 = _self->xx0; /* list<10243> */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core_types__maybe _x19866;
  kk_std_core_types__maybe _x19867 = kk_std_core_types__maybe_unbox(_b_18165, _ctx); /*maybe<10244>*/
  _x19866 = kk_std_core__mlift17166_foreach_while(action0, xx0, _x19867, _ctx); /*maybe<10244>*/
  return kk_std_core_types__maybe_box(_x19866, _ctx);
}

kk_std_core_types__maybe kk_std_core_foreach_while(kk_std_core__list xs, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e maybe<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    struct kk_std_core_Cons* _con19862 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19862->head;
    kk_std_core__list xx0 = _con19862->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__maybe x0_17378;
    kk_function_t _x19863 = kk_function_dup(action0); /*(10243) -> 10245 maybe<10244>*/
    x0_17378 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x19863, (_x19863, x, _ctx)); /*maybe<10244>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17378, _ctx);
      kk_box_t _x19864 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_while_fun19865(action0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19864, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17378)) { // tailcall
                                                   xs = xx0;
                                                   goto kk__tailcall;
    }
    {
      kk_function_drop(action0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return x0_17378;
    }
  }
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17167_foreach_while_1(kk_function_t action, kk_std_core__sslice rest, kk_std_core_types__maybe _y_16954, kk_context_t* _ctx) { /* forall<a,e> (action : (c : char) -> e maybe<a>, rest : sslice, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_16954)) {
    return kk_std_core_foreach_while_1(rest, action, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__sslice_drop(rest, _ctx);
    return _y_16954;
  }
}
 
// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.


// lift anonymous function
struct kk_std_core_foreach_while_fun19873__t_1 {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__sslice rest0;
};
static kk_box_t kk_std_core_foreach_while_fun19873_1(kk_function_t _fself, kk_box_t _b_18172, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19873_1(kk_function_t action0, kk_std_core__sslice rest0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19873__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19873__t_1, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19873_1, kk_context());
  _self->action0 = action0;
  _self->rest0 = rest0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_while_fun19873_1(kk_function_t _fself, kk_box_t _b_18172, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19873__t_1* _self = kk_function_as(struct kk_std_core_foreach_while_fun19873__t_1*, _fself);
  kk_function_t action0 = _self->action0; /* (c : char) -> 10330 maybe<10329> */
  kk_std_core__sslice rest0 = _self->rest0; /* sslice */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__sslice_dup(rest0);}, {}, _ctx)
  kk_std_core_types__maybe _x19874;
  kk_std_core_types__maybe _x19875 = kk_std_core_types__maybe_unbox(_b_18172, _ctx); /*maybe<10329>*/
  _x19874 = kk_std_core__mlift17167_foreach_while_1(action0, rest0, _x19875, _ctx); /*maybe<10329>*/
  return kk_std_core_types__maybe_box(_x19874, _ctx);
}

kk_std_core_types__maybe kk_std_core_foreach_while_1(kk_std_core__sslice slice0, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,e> (slice : sslice, action : (c : char) -> e maybe<a>) -> e maybe<a> */ 
  kk__tailcall: ;
  kk_std_core_types__maybe _match_19068 = kk_std_core_next(slice0, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Nothing(_match_19068)) {
    kk_function_drop(action0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t _box_x18168 = _match_19068._cons.Just.value;
    kk_std_core_types__tuple2_ _pat1 = kk_std_core_types__tuple2__unbox(_box_x18168, NULL);
    kk_box_t _box_x18169 = _pat1.fst;
    kk_box_t _box_x18170 = _pat1.snd;
    kk_char_t c = kk_char_unbox(_box_x18169, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x18170, NULL);
    kk_std_core__sslice_dup(rest0);
    kk_std_core_types__maybe_drop(_match_19068, _ctx);
    kk_std_core_types__maybe x_17381;
    kk_function_t _x19871 = kk_function_dup(action0); /*(c : char) -> 10330 maybe<10329>*/
    x_17381 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_char_t, kk_context_t*), _x19871, (_x19871, c, _ctx)); /*maybe<10329>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x_17381, _ctx);
      kk_box_t _x19872 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_while_fun19873_1(action0, rest0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19872, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x_17381)) { // tailcall
                                                  slice0 = rest0;
                                                  goto kk__tailcall;
    }
    {
      kk_function_drop(action0, _ctx);
      kk_std_core__sslice_drop(rest0, _ctx);
      return x_17381;
    }
  }
}
 
// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.


// lift anonymous function
struct kk_std_core_foreach_while_fun19882__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_std_core_types__maybe kk_std_core_foreach_while_fun19882_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19882_3(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19882__t_3* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19882__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19882_3, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_foreach_while_fun19882_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19882__t_3* _self = kk_function_as(struct kk_std_core_foreach_while_fun19882__t_3*, _fself);
  kk_function_t f = _self->f; /* (10428) -> 10430 maybe<10429> */
  kk_vector_t v = _self->v; /* vector<10428> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t _x19883 = kk_vector_at(v,i,kk_context()); /*223*/
  return kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19883, _ctx));
}

kk_std_core_types__maybe kk_std_core_foreach_while_3(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (v : vector<a>, f : (a) -> e maybe<b>) -> e maybe<b> */ 
  kk_ssize_t start0_17384 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17385;
  kk_ssize_t _x19880;
  kk_vector_t _x19881 = kk_vector_dup(v); /*vector<10428>*/
  _x19880 = kk_vector_len(_x19881,kk_context()); /*ssize_t*/
  end_17385 = kk_std_core_decr_1(_x19880, _ctx); /*ssize_t*/
  return kk_std_core__lift16746_for_whilez(kk_std_core_new_foreach_while_fun19882_3(f, v, _ctx), end_17385, start0_17384, _ctx);
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17168_foreach(kk_function_t action, kk_std_core__list xx, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<a,e> (action : (a) -> e (), xx : list<a>, wild_ : ()) -> e () */ 
  kk_std_core_foreach(xx, action, _ctx); return kk_Unit;
}
 
// Invoke `action` for each element of a list


// lift anonymous function
struct kk_std_core_foreach_fun19887__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foreach_fun19887(kk_function_t _fself, kk_box_t _b_18176, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19887(kk_function_t action0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19887__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19887__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19887, kk_context());
  _self->action0 = action0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_fun19887(kk_function_t _fself, kk_box_t _b_18176, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19887__t* _self = kk_function_as(struct kk_std_core_foreach_fun19887__t*, _fself);
  kk_function_t action0 = _self->action0; /* (10623) -> 10624 () */
  kk_std_core__list xx0 = _self->xx0; /* list<10623> */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_unit_t _x19888 = kk_Unit;
  kk_unit_t _x19889 = kk_Unit;
  kk_unit_unbox(_b_18176);
  kk_std_core__mlift17168_foreach(action0, xx0, _x19889, _ctx);
  return kk_unit_box(_x19888);
}

kk_unit_t kk_std_core_foreach(kk_std_core__list xs, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, action : (a) -> e ()) -> e () */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action0, _ctx);
    kk_Unit; return kk_Unit;
  }
  {
    struct kk_std_core_Cons* _con19884 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19884->head;
    kk_std_core__list xx0 = _con19884->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_unit_t x0_17387 = kk_Unit;
    kk_function_t _x19885 = kk_function_dup(action0); /*(10623) -> 10624 ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), _x19885, (_x19885, x, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x19886 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19887(action0, xx0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x19886); return kk_Unit;
    }
    { // tailcall
      xs = xx0;
      goto kk__tailcall;
    }
  }
}
 
// Apply a function for each character in a string slice.


// lift anonymous function
struct kk_std_core_foreach_fun19890__t_1 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_std_core_types__maybe kk_std_core_foreach_fun19890_1(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19890_1(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19890__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19890__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19890_1, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_fun19892__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19892_1(kk_function_t _fself, kk_box_t _b_18180, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19892_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19892_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19892_1(kk_function_t _fself, kk_box_t _b_18180, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18180, _ctx);
  return kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx);
}
static kk_std_core_types__maybe kk_std_core_foreach_fun19890_1(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19890__t_1* _self = kk_function_as(struct kk_std_core_foreach_fun19890__t_1*, _fself);
  kk_function_t action = _self->action; /* (c : char) -> 10682 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t x0_17393 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_char_t, kk_context_t*), action, (action, c, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x19891 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19892_1(_ctx), _ctx); /*3860*/
    return kk_std_core_types__maybe_unbox(_x19891, _ctx);
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_core_foreach_fun19894__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19894_1(kk_function_t _fself, kk_box_t _b_18184, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19894_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19894_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19894_1(kk_function_t _fself, kk_box_t _b_18184, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18184, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_std_core_foreach_1(kk_std_core__sslice slice0, kk_function_t action, kk_context_t* _ctx) { /* forall<e> (slice : sslice, action : (c : char) -> e ()) -> e () */ 
  kk_std_core_types__maybe x_17390 = kk_std_core_foreach_while_1(slice0, kk_std_core_new_foreach_fun19890_1(action, _ctx), _ctx); /*maybe<_10669>*/;
  kk_std_core_types__maybe_drop(x_17390, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19893 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19894_1(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x19893); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// Invoke a function for each character in a string


// lift anonymous function
struct kk_std_core_foreach_fun19898__t_2 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_std_core_types__maybe kk_std_core_foreach_fun19898_2(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19898_2(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19898__t_2* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19898__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19898_2, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_fun19900__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19900_2(kk_function_t _fself, kk_box_t _b_18188, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19900_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19900_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19900_2(kk_function_t _fself, kk_box_t _b_18188, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18188, _ctx);
  return kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx);
}
static kk_std_core_types__maybe kk_std_core_foreach_fun19898_2(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19898__t_2* _self = kk_function_as(struct kk_std_core_foreach_fun19898__t_2*, _fself);
  kk_function_t action = _self->action; /* (c : char) -> 10721 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t x0_17400 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_char_t, kk_context_t*), action, (action, c, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x19899 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19900_2(_ctx), _ctx); /*3860*/
    return kk_std_core_types__maybe_unbox(_x19899, _ctx);
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_core_foreach_fun19902__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19902_2(kk_function_t _fself, kk_box_t _b_18192, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19902_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19902_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19902_2(kk_function_t _fself, kk_box_t _b_18192, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18192, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_std_core_foreach_2(kk_string_t s, kk_function_t action, kk_context_t* _ctx) { /* forall<e> (s : string, action : (c : char) -> e ()) -> e () */ 
  kk_std_core__sslice slice0_16629;
  kk_string_t _x19895 = kk_string_dup(s); /*string*/
  kk_ssize_t _x19896 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x19897 = kk_string_len(s,kk_context()); /*ssize_t*/
  slice0_16629 = kk_std_core__new_Sslice(_x19895, _x19896, _x19897, _ctx); /*sslice*/
  kk_std_core_types__maybe x_17397 = kk_std_core_foreach_while_1(slice0_16629, kk_std_core_new_foreach_fun19898_2(action, _ctx), _ctx); /*maybe<_10669>*/;
  kk_std_core_types__maybe_drop(x_17397, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19901 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19902_2(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x19901); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// Invoke a function `f` for each element in a vector `v`


// lift anonymous function
struct kk_std_core_foreach_fun19905__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_fun19905_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19905_3(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19905__t_3* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19905__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19905_3, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_fun19905_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19905__t_3* _self = kk_function_as(struct kk_std_core_foreach_fun19905__t_3*, _fself);
  kk_function_t f = _self->f; /* (10736) -> 10737 () */
  kk_vector_t v = _self->v; /* vector<10736> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t x_17584 = kk_vector_at(v,i,kk_context()); /*10736*/;
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x_17584, _ctx));
}

kk_unit_t kk_std_core_foreach_3(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a) -> e ()) -> e () */ 
  kk_ssize_t start0_17406 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17407;
  kk_ssize_t _x19903;
  kk_vector_t _x19904 = kk_vector_dup(v); /*vector<10736>*/
  _x19903 = kk_vector_len(_x19904,kk_context()); /*ssize_t*/
  end_17407 = kk_std_core_decr_1(_x19903, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_fun19905_3(f, v, _ctx), end_17407, start0_17406, _ctx); return kk_Unit;
}
 
// O(n). Return the number of characters in a string.

kk_integer_t kk_std_core_count_1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> int */ 
  return kk_string_count_int(s,kk_context());
}
 
// O(n). Return the number of characters in a string slice

kk_integer_t kk_std_core_count_2(kk_std_core__sslice slice0, kk_context_t* _ctx) { /* (slice : sslice) -> int */ 
  return kk_slice_count(slice0,kk_context());
}
 
// Count the number of times a predicate is true for each character in a string


// lift anonymous function
struct kk_std_core_count_fun19910__t_3 {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_count_fun19910_3(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_count_fun19910_3(kk_ref_t loc, kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_count_fun19910__t_3* _self = kk_function_alloc_as(struct kk_std_core_count_fun19910__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_count_fun19910_3, kk_context());
  _self->loc = loc;
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_count_fun19910_3(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_count_fun19910__t_3* _self = kk_function_as(struct kk_std_core_count_fun19910__t_3*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<10930,int> */
  kk_function_t pred = _self->pred; /* (char) -> bool */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_function_dup(pred);}, {}, _ctx)
  kk_unit_t __ = kk_Unit;
  bool _match_19062 = kk_function_call(bool, (kk_function_t, kk_char_t, kk_context_t*), pred, (pred, c, _ctx)); /*bool*/;
  if (_match_19062) {
    kk_integer_t _b_18201_18199;
    kk_integer_t _x19911;
    kk_box_t _x19912;
    kk_ref_t _x19913 = kk_ref_dup(loc); /*local-var<10930,int>*/
    _x19912 = (kk_ref_get(_x19913,kk_context())); /*233*/
    _x19911 = kk_integer_unbox(_x19912); /*int*/
    _b_18201_18199 = kk_integer_add(_x19911,(kk_integer_from_small(1)),kk_context()); /*int*/
    (kk_ref_set(loc,(kk_integer_box(_b_18201_18199)),kk_context()));
  }
  else {
    kk_ref_drop(loc, _ctx);
    kk_Unit;
  }
  return kk_std_core_types__new_Nothing(_ctx);
}

kk_integer_t kk_std_core_count_3(kk_string_t s, kk_function_t pred, kk_context_t* _ctx) { /* (s : string, pred : (char) -> bool) -> int */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<10930,int>*/;
  kk_std_core__sslice slice0_16634;
  kk_string_t _x19906 = kk_string_dup(s); /*string*/
  kk_ssize_t _x19907 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x19908 = kk_string_len(s,kk_context()); /*ssize_t*/
  slice0_16634 = kk_std_core__new_Sslice(_x19906, _x19907, _x19908, _ctx); /*sslice*/
  kk_std_core_types__maybe __0;
  kk_function_t _x19909;
  kk_ref_dup(loc);
  _x19909 = kk_std_core_new_count_fun19910_3(loc, pred, _ctx); /*(c : char) -> (local<10930>) (forall<a> maybe<a>)*/
  __0 = kk_std_core_foreach_while_1(slice0_16634, _x19909, _ctx); /*maybe<_10669>*/
  kk_std_core_types__maybe_drop(__0, _ctx);
  kk_integer_t res;
  kk_box_t _x19914;
  kk_ref_t _x19915 = kk_ref_dup(loc); /*local-var<10930,int>*/
  _x19914 = (kk_ref_get(_x19915,kk_context())); /*233*/
  res = kk_integer_unbox(_x19914); /*int*/
  kk_box_t _x19916 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*9897*/
  return kk_integer_unbox(_x19916);
}
 
// Is the integer negative (stricly smaller than zero)

bool kk_std_core_is_neg_2(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> bool */ 
  kk_std_core_types__order x_16723 = kk_int_as_order(kk_integer_signum(i,kk_context()),kk_context()); /*order*/;
  kk_integer_t _x19920;
  if (kk_std_core_types__is_Lt(x_16723)) {
    _x19920 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19921;
  }
  if (kk_std_core_types__is_Eq(x_16723)) {
    _x19920 = kk_integer_from_small(0); /*int*/
    goto _match19921;
  }
  {
    _x19920 = kk_integer_from_small(1); /*int*/
  }
  _match19921: ;
  kk_integer_t _x19922 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_eq(_x19920,_x19922,kk_context());
}

kk_std_core_types__tuple2_ kk_std_core_divmod_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> (int, int) */ 
  kk_std_core_types__tuple2_ _match_19061;
  kk_integer_t _x19923 = kk_integer_dup(n); /*int*/
  _match_19061 = kk_std_core_cdivmod_exp10(i, _x19923, _ctx); /*(int, int)*/
  {
    kk_box_t _box_x18214 = _match_19061.fst;
    kk_box_t _box_x18215 = _match_19061.snd;
    kk_integer_t cq = kk_integer_unbox(_box_x18214);
    kk_integer_t cr = kk_integer_unbox(_box_x18215);
    kk_std_core_types__order x_16725;
    kk_integer_t _x19926 = kk_integer_dup(cr); /*int*/
    x_16725 = kk_int_as_order(kk_integer_signum(_x19926,kk_context()),kk_context()); /*order*/
    bool b_16637;
    kk_integer_t _x19927;
    if (kk_std_core_types__is_Lt(x_16725)) {
      _x19927 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
      goto _match19928;
    }
    if (kk_std_core_types__is_Eq(x_16725)) {
      _x19927 = kk_integer_from_small(0); /*int*/
      goto _match19928;
    }
    {
      _x19927 = kk_integer_from_small(1); /*int*/
    }
    _match19928: ;
    kk_integer_t _x19929 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    b_16637 = kk_integer_eq(_x19927,_x19929,kk_context()); /*bool*/
    if (b_16637) {
      kk_integer_t _b_18220_18216 = kk_integer_sub(cq,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_integer_t _b_18221_18217;
      kk_integer_t _x19930 = kk_std_core_mul_exp10(kk_integer_from_small(1), n, _ctx); /*int*/
      _b_18221_18217 = kk_integer_add(cr,_x19930,kk_context()); /*int*/
      return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(_b_18220_18216), kk_integer_box(_b_18221_18217), _ctx);
    }
    {
      kk_integer_drop(n, _ctx);
      return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(cq), kk_integer_box(cr), _ctx);
    }
  }
}
 
// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)

kk_std_core__list kk_std_core_drop(kk_std_core__list xs, kk_integer_t n, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19931 = kk_std_core__as_Cons(xs);
    kk_box_t _pat0 = _con19931->head;
    kk_std_core__list xx = _con19931->tail;
    kk_integer_t _x19932 = kk_integer_dup(n); /*int*/
    if (kk_integer_gt(_x19932,(kk_integer_from_small(0)),kk_context())) {
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_box_drop(_pat0, _ctx);
        kk_std_core__list_free(xs);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      { // tailcall
        kk_integer_t _x19933 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        xs = xx;
        n = _x19933;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_integer_drop(n, _ctx);
    return xs;
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17173_drop_while(kk_function_t predicate, kk_std_core__list xs, kk_std_core__list xx, bool _y_16971, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xs : list<a>, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16971) {
    kk_std_core__list_drop(xs, _ctx);
    return kk_std_core_drop_while(xx, predicate, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    return xs;
  }
}
 
// Drop all initial elements that satisfy `predicate`


// lift anonymous function
struct kk_std_core_drop_while_fun19937__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xs0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_drop_while_fun19937(kk_function_t _fself, kk_box_t _b_18225, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_drop_while_fun19937(kk_function_t predicate0, kk_std_core__list xs0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_drop_while_fun19937__t* _self = kk_function_alloc_as(struct kk_std_core_drop_while_fun19937__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_drop_while_fun19937, kk_context());
  _self->predicate0 = predicate0;
  _self->xs0 = xs0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_drop_while_fun19937(kk_function_t _fself, kk_box_t _b_18225, kk_context_t* _ctx) {
  struct kk_std_core_drop_while_fun19937__t* _self = kk_function_as(struct kk_std_core_drop_while_fun19937__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (11198) -> 11199 bool */
  kk_std_core__list xs0 = _self->xs0; /* list<11198> */
  kk_std_core__list xx0 = _self->xx0; /* list<11198> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xs0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core__list _x19938;
  bool _x19939 = kk_bool_unbox(_b_18225); /*bool*/
  _x19938 = kk_std_core__mlift17173_drop_while(predicate0, xs0, xx0, _x19939, _ctx); /*list<11198>*/
  return kk_std_core__list_box(_x19938, _ctx);
}

kk_std_core__list kk_std_core_drop_while(kk_std_core__list xs0, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con19934 = kk_std_core__as_Cons(xs0);
    kk_box_t x = _con19934->head;
    kk_std_core__list xx0 = _con19934->tail;
    kk_box_dup(x);
    kk_std_core__list_dup(xx0);
    bool x0_17411;
    kk_function_t _x19935 = kk_function_dup(predicate0); /*(11198) -> 11199 bool*/
    x0_17411 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19935, (_x19935, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19936 = kk_std_core_hnd_yield_extend(kk_std_core_new_drop_while_fun19937(predicate0, xs0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19936, _ctx);
    }
    if (x0_17411) {
      kk_std_core__list_dropn(xs0, ((int32_t)KI32(2)), _ctx);
      { // tailcall
        xs0 = xx0;
        goto kk__tailcall;
      }
    }
    {
      kk_function_drop(predicate0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return xs0;
    }
  }
  {
    kk_function_drop(predicate0, _ctx);
    return kk_std_core__new_Nil(_ctx);
  }
}
 
// An empty slice

kk_std_core__sslice kk_std_core_empty;
 
// Does string `s`  end with `post`?
// If so, returns a slice of `s` from the start up to the `post` string at the end.

kk_std_core_types__maybe kk_std_core_ends_with(kk_string_t s, kk_string_t post, kk_context_t* _ctx) { /* (s : string, post : string) -> maybe<sslice> */ 
  bool _match_19059;
  kk_string_t _x19944 = kk_string_dup(s); /*string*/
  kk_string_t _x19945 = kk_string_dup(post); /*string*/
  _match_19059 = kk_std_core_xends_with(_x19944, _x19945, _ctx); /*bool*/
  if (_match_19059) {
    kk_std_core__sslice _b_18229_18228;
    kk_string_t _x19946 = kk_string_dup(s); /*string*/
    kk_ssize_t _x19947 = ((kk_ssize_t)0); /*ssize_t*/
    kk_ssize_t _x19948;
    kk_ssize_t _x19949 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x19950 = kk_string_len(post,kk_context()); /*ssize_t*/
    _x19948 = (_x19949 - _x19950); /*ssize_t*/
    _b_18229_18228 = kk_std_core__new_Sslice(_x19946, _x19947, _x19948, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18229_18228, _ctx), _ctx);
  }
  {
    kk_string_drop(post, _ctx);
    kk_string_drop(s, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// Throw an exception with a specified message.


// lift anonymous function
struct kk_std_core_throw_fun19953__t {
  struct kk_function_s _base;
  kk_std_core_types__optional info0;
  kk_string_t message0;
};
static kk_box_t kk_std_core_throw_fun19953(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_throw_fun19953(kk_std_core_types__optional info0, kk_string_t message0, kk_context_t* _ctx) {
  struct kk_std_core_throw_fun19953__t* _self = kk_function_alloc_as(struct kk_std_core_throw_fun19953__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_throw_fun19953, kk_context());
  _self->info0 = info0;
  _self->message0 = message0;
  return &_self->_base;
}

static kk_box_t kk_std_core_throw_fun19953(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_throw_fun19953__t* _self = kk_function_as(struct kk_std_core_throw_fun19953__t*, _fself);
  kk_std_core_types__optional info0 = _self->info0; /* optional<exception-info> */
  kk_string_t message0 = _self->message0; /* string */
  kk_drop_match(_self, {kk_std_core_types__optional_dup(info0);kk_string_dup(message0);}, {}, _ctx)
  kk_std_core__exception _x19954;
  kk_std_core__exception_info _x19955;
  if (kk_std_core_types__is_Optional(info0)) {
    kk_box_t _box_x18230 = info0._cons.Optional.value;
    kk_std_core__exception_info _info_11284 = kk_std_core__exception_info_unbox(_box_x18230, NULL);
    kk_std_core__exception_info_dup(_info_11284);
    kk_std_core_types__optional_drop(info0, _ctx);
    _x19955 = _info_11284; /*exception-info*/
    goto _match19956;
  }
  {
    _x19955 = kk_std_core__new_ExnError(_ctx); /*exception-info*/
  }
  _match19956: ;
  _x19954 = kk_std_core__new_Exception(message0, _x19955, _ctx); /*exception*/
  return kk_std_core__exception_box(_x19954, _ctx);
}

kk_box_t kk_std_core_throw(kk_string_t message0, kk_std_core_types__optional info0, kk_context_t* _ctx) { /* forall<a> (message : string, info : optional<exception-info>) -> exn a */ 
  kk_std_core_hnd__ev ev_17414;
  kk_ssize_t _x19951 = ((kk_ssize_t)0); /*ssize_t*/
  ev_17414 = kk_evv_at(_x19951,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
  kk_std_core__exception x_17416;
  kk_box_t _x19952 = kk_std_core_hnd__open_none0(kk_std_core_new_throw_fun19953(info0, message0, _ctx), _ctx); /*3214*/
  x_17416 = kk_std_core__exception_unbox(_x19952, _ctx); /*exception*/
  {
    struct kk_std_core_hnd_Ev* _con19958 = kk_std_core_hnd__as_Ev(ev_17414);
    kk_std_core_hnd__marker m0 = _con19958->marker;
    kk_box_t _box_x18233 = _con19958->hnd;
    kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18233, NULL);
    kk_std_core__hnd_exn_dup(h);
    kk_std_core_hnd__clause1 _match_19058 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
    {
      kk_function_t _fun_unbox_x18237 = _match_19058.clause;
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18237, (_fun_unbox_x18237, m0, ev_17414, kk_std_core__exception_box(x_17416, _ctx), _ctx));
    }
  }
}
 
// Transform an `:error` type back to an `exn` effect.

kk_box_t kk_std_core_throw_1(kk_std_core__error err, kk_context_t* _ctx) { /* forall<a> (err : error<a>) -> exn a */ 
  if (kk_std_core__is_Error(err)) {
    kk_std_core__exception exn0 = err._cons.Error.exception;
    kk_std_core__exception_dup(exn0);
    kk_std_core__error_drop(err, _ctx);
    kk_std_core_hnd__ev ev_17417;
    kk_ssize_t _x19960 = ((kk_ssize_t)0); /*ssize_t*/
    ev_17417 = kk_evv_at(_x19960,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
    {
      struct kk_std_core_hnd_Ev* _con19961 = kk_std_core_hnd__as_Ev(ev_17417);
      kk_std_core_hnd__marker m0 = _con19961->marker;
      kk_box_t _box_x18241 = _con19961->hnd;
      kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18241, NULL);
      kk_std_core__hnd_exn_dup(h);
      kk_std_core_hnd__clause1 _match_19057 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
      {
        kk_function_t _fun_unbox_x18245 = _match_19057.clause;
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18245, (_fun_unbox_x18245, m0, ev_17417, kk_std_core__exception_box(exn0, _ctx), _ctx));
      }
    }
  }
  {
    kk_box_t x0 = err._cons.Ok.result;
    return x0;
  }
}
 
// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.


// lift anonymous function
struct kk_std_core_error_pattern_fun19965__t {
  struct kk_function_s _base;
  kk_string_t definition;
  kk_string_t location;
};
static kk_box_t kk_std_core_error_pattern_fun19965(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_error_pattern_fun19965(kk_string_t definition, kk_string_t location, kk_context_t* _ctx) {
  struct kk_std_core_error_pattern_fun19965__t* _self = kk_function_alloc_as(struct kk_std_core_error_pattern_fun19965__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_error_pattern_fun19965, kk_context());
  _self->definition = definition;
  _self->location = location;
  return &_self->_base;
}

static kk_box_t kk_std_core_error_pattern_fun19965(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_error_pattern_fun19965__t* _self = kk_function_as(struct kk_std_core_error_pattern_fun19965__t*, _fself);
  kk_string_t definition = _self->definition; /* string */
  kk_string_t location = _self->location; /* string */
  kk_drop_match(_self, {kk_string_dup(definition);kk_string_dup(location);}, {}, _ctx)
  kk_string_t _x19966;
  kk_string_t _x19967;
  kk_string_t _x19968;
  bool _match_19056;
  kk_string_t _x19969 = kk_string_dup(definition); /*string*/
  kk_string_t _x19970 = kk_string_empty(); /*string*/
  _match_19056 = kk_string_is_eq(_x19969,_x19970,kk_context()); /*bool*/
  if (_match_19056) {
    kk_string_drop(definition, _ctx);
    _x19968 = kk_string_empty(); /*string*/
  }
  else {
    kk_string_t _x19973;
    kk_define_string_literal(, _s19974, 2, ": ")
    _x19973 = kk_string_dup(_s19974); /*string*/
    _x19968 = kk_std_core__lp__plus__plus__1_rp_(_x19973, definition, _ctx); /*string*/
  }
  kk_string_t _x19975;
  kk_define_string_literal(, _s19976, 23, ": pattern match failure")
  _x19975 = kk_string_dup(_s19976); /*string*/
  _x19967 = kk_std_core__lp__plus__plus__1_rp_(_x19968, _x19975, _ctx); /*string*/
  _x19966 = kk_std_core__lp__plus__plus__1_rp_(location, _x19967, _ctx); /*string*/
  return kk_string_box(_x19966);
}

kk_box_t kk_std_core_error_pattern(kk_string_t location, kk_string_t definition, kk_context_t* _ctx) { /* forall<a> (location : string, definition : string) -> exn a */ 
  kk_string_t message0_16643;
  kk_box_t _x19963;
  kk_function_t _x19964;
  kk_string_dup(definition);
  kk_string_dup(location);
  _x19964 = kk_std_core_new_error_pattern_fun19965(definition, location, _ctx); /*() -> 3215 3214*/
  _x19963 = kk_std_core_hnd__open_none0(_x19964, _ctx); /*3214*/
  message0_16643 = kk_string_unbox(_x19963); /*string*/
  kk_std_core_hnd__ev ev_17420;
  kk_ssize_t _x19977 = ((kk_ssize_t)0); /*ssize_t*/
  ev_17420 = kk_evv_at(_x19977,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
  {
    struct kk_std_core_hnd_Ev* _con19978 = kk_std_core_hnd__as_Ev(ev_17420);
    kk_std_core_hnd__marker m0 = _con19978->marker;
    kk_box_t _box_x18251 = _con19978->hnd;
    kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18251, NULL);
    kk_std_core__hnd_exn_dup(h);
    kk_std_core_hnd__clause1 _match_19055 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
    {
      kk_function_t _fun_unbox_x18255 = _match_19055.clause;
      kk_box_t _x19980;
      kk_std_core__exception _x19981;
      kk_std_core__exception_info _x19982 = kk_std_core__new_ExnPattern(kk_reuse_null, location, definition, _ctx); /*exception-info*/
      _x19981 = kk_std_core__new_Exception(message0_16643, _x19982, _ctx); /*exception*/
      _x19980 = kk_std_core__exception_box(_x19981, _ctx); /*51*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18255, (_fun_unbox_x18255, m0, ev_17420, _x19980, _ctx));
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17174_op(kk_std_core_types__ctail _acc, kk_function_t pred, kk_box_t x, kk_std_core__list xx, bool _y_16980, kk_context_t* _ctx) { /* forall<a,e> (ctail<list<a>>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16980) {
    kk_std_core__list _ctail_16819 = kk_std_core__list_hole(); /*list<11495>*/;
    kk_std_core__list _ctail_16820 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16819, _ctx); /*list<11495>*/;
    kk_std_core_types__ctail _x19983;
    kk_box_t* _b_18267_18264 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16820)->tail)); /*cfield<list<11495>>*/;
    _x19983 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16820, _ctx)),_b_18267_18264); /*ctail<0>*/
    return kk_std_core__ctail_filter(xx, pred, _x19983, _ctx);
  }
  {
    kk_box_drop(x, _ctx);
    return kk_std_core__ctail_filter(xx, pred, _acc, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17175_op_fun19984__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t x0;
};
static kk_std_core__list kk_std_core__mlift17175_op_fun19984(kk_function_t _fself, kk_std_core__list _ctail_16822, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17175_op_fun19984(kk_function_t _accm, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17175_op_fun19984__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17175_op_fun19984__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17175_op_fun19984, kk_context());
  _self->_accm = _accm;
  _self->x0 = x0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17175_op_fun19984(kk_function_t _fself, kk_std_core__list _ctail_16822, kk_context_t* _ctx) {
  struct kk_std_core__mlift17175_op_fun19984__t* _self = kk_function_as(struct kk_std_core__mlift17175_op_fun19984__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11495>) -> list<11495> */
  kk_box_t x0 = _self->x0; /* 11495 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(x0);}, {}, _ctx)
  kk_std_core__list _x19985 = kk_std_core__new_Cons(kk_reuse_null, x0, _ctail_16822, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x19985, _ctx));
}

kk_std_core__list kk_std_core__mlift17175_op(kk_function_t _accm, kk_function_t pred0, kk_box_t x0, kk_std_core__list xx0, bool _y_16985, kk_context_t* _ctx) { /* forall<a,e> ((list<a>) -> list<a>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16985) {
    return kk_std_core__ctailm_filter(xx0, pred0, kk_std_core__new_mlift17175_op_fun19984(_accm, x0, _ctx), _ctx);
  }
  {
    kk_box_drop(x0, _ctx);
    return kk_std_core__ctailm_filter(xx0, pred0, _accm, _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`


// lift anonymous function
struct kk_std_core__ctail_filter_fun19990__t {
  struct kk_function_s _base;
  kk_function_t pred1;
  kk_box_t x1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_filter_fun19990(kk_function_t _fself, kk_box_t _b_18272, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_filter_fun19990(kk_function_t pred1, kk_box_t x1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_fun19990__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_filter_fun19990__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_filter_fun19990, kk_context());
  _self->pred1 = pred1;
  _self->x1 = x1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_filter_fun19990(kk_function_t _fself, kk_box_t _b_18272, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_fun19990__t* _self = kk_function_as(struct kk_std_core__ctail_filter_fun19990__t*, _fself);
  kk_function_t pred1 = _self->pred1; /* (11495) -> 11496 bool */
  kk_box_t x1 = _self->x1; /* 11495 */
  kk_std_core__list xx1 = _self->xx1; /* list<11495> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11495>> */
  kk_drop_match(_self, {kk_function_dup(pred1);kk_box_dup(x1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19991;
  bool _x19992 = kk_bool_unbox(_b_18272); /*bool*/
  _x19991 = kk_std_core__mlift17174_op(_acc0, pred1, x1, xx1, _x19992, _ctx); /*list<11495>*/
  return kk_std_core__list_box(_x19991, _ctx);
}

kk_std_core__list kk_std_core__ctail_filter(kk_std_core__list xs, kk_function_t pred1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, ctail<list<a>>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19986 = kk_std_core__as_Cons(xs);
    kk_box_t x1 = _con19986->head;
    kk_std_core__list xx1 = _con19986->tail;
    kk_reuse_t _ru_18899 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18899 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18899 = kk_reuse_null;
    }
    bool x2_17423;
    kk_function_t _x19988 = kk_function_dup(pred1); /*(11495) -> 11496 bool*/
    kk_box_t _x19987 = kk_box_dup(x1); /*11495*/
    x2_17423 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19988, (_x19988, _x19987, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18899, _ctx);
      kk_box_t _x19989 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_filter_fun19990(pred1, x1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19989, _ctx);
    }
    if (x2_17423) {
      kk_std_core__list _ctail_168190 = kk_std_core__list_hole(); /*list<11495>*/;
      kk_std_core__list _ctail_168200;
      if (kk_likely(_ru_18899!=NULL)) {
        struct kk_std_core_Cons* _con19993 = (struct kk_std_core_Cons*)_ru_18899;
        _con19993->tail = _ctail_168190;
        _ctail_168200 = kk_std_core__base_Cons(_con19993); /*list<11495>*/
      }
      else {
        _ctail_168200 = kk_std_core__new_Cons(kk_reuse_null, x1, _ctail_168190, _ctx); /*list<11495>*/
      }
      { // tailcall
        kk_std_core_types__ctail _x19994;
        kk_box_t* _b_18284_18278 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168200)->tail)); /*cfield<list<11495>>*/;
        _x19994 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168200, _ctx)),_b_18284_18278); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x19994;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18899, _ctx);
      kk_box_drop(x1, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(pred1, _ctx);
    kk_box_t _x19995 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19995, _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`


// lift anonymous function
struct kk_std_core__ctailm_filter_fun20000__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t pred2;
  kk_box_t x3;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_filter_fun20000(kk_function_t _fself, kk_box_t _b_18292, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_fun20000(kk_function_t _accm0, kk_function_t pred2, kk_box_t x3, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20000__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_fun20000__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_fun20000, kk_context());
  _self->_accm0 = _accm0;
  _self->pred2 = pred2;
  _self->x3 = x3;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_filter_fun20000(kk_function_t _fself, kk_box_t _b_18292, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20000__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_fun20000__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11495>) -> list<11495> */
  kk_function_t pred2 = _self->pred2; /* (11495) -> 11496 bool */
  kk_box_t x3 = _self->x3; /* 11495 */
  kk_std_core__list xx2 = _self->xx2; /* list<11495> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(pred2);kk_box_dup(x3);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20001;
  bool _x20002 = kk_bool_unbox(_b_18292); /*bool*/
  _x20001 = kk_std_core__mlift17175_op(_accm0, pred2, x3, xx2, _x20002, _ctx); /*list<11495>*/
  return kk_std_core__list_box(_x20001, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_filter_fun20004__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x3;
};
static kk_std_core__list kk_std_core__ctailm_filter_fun20004(kk_function_t _fself, kk_std_core__list _ctail_168220, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_fun20004(kk_function_t _accm0, kk_box_t x3, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20004__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_fun20004__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_fun20004, kk_context());
  _self->_accm0 = _accm0;
  _self->x3 = x3;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_filter_fun20004(kk_function_t _fself, kk_std_core__list _ctail_168220, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20004__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_fun20004__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11495>) -> list<11495> */
  kk_box_t x3 = _self->x3; /* 11495 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x3);}, {}, _ctx)
  kk_std_core__list _x20005 = kk_std_core__new_Cons(kk_reuse_null, x3, _ctail_168220, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20005, _ctx));
}

kk_std_core__list kk_std_core__ctailm_filter(kk_std_core__list xs0, kk_function_t pred2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con19996 = kk_std_core__as_Cons(xs0);
    kk_box_t x3 = _con19996->head;
    kk_std_core__list xx2 = _con19996->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x3);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    bool x4_17426;
    kk_function_t _x19998 = kk_function_dup(pred2); /*(11495) -> 11496 bool*/
    kk_box_t _x19997 = kk_box_dup(x3); /*11495*/
    x4_17426 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19998, (_x19998, _x19997, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19999 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_filter_fun20000(_accm0, pred2, x3, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19999, _ctx);
    }
    if (x4_17426) { // tailcall
                    kk_function_t _x20003 = kk_std_core__new_ctailm_filter_fun20004(_accm0, x3, _ctx); /*(list<11495>) -> list<11495>*/
                    xs0 = xx2;
                    _accm0 = _x20003;
                    goto kk__tailcall;
    }
    {
      kk_box_drop(x3, _ctx);
      { // tailcall
        xs0 = xx2;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(pred2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`


// lift anonymous function
struct kk_std_core_filter_fun20007__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_filter_fun20007(kk_function_t _fself, kk_std_core__list _ctail_16821, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_filter_fun20007(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_filter_fun20007, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_filter_fun20007(kk_function_t _fself, kk_std_core__list _ctail_16821, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16821;
}

kk_std_core__list kk_std_core_filter(kk_std_core__list xs1, kk_function_t pred3, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e list<a> */ 
  bool _match_19052 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19052) {
    kk_std_core_types__ctail _x20006 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_filter(xs1, pred3, _x20006, _ctx);
  }
  {
    return kk_std_core__ctailm_filter(xs1, pred3, kk_std_core_new_filter_fun20007(_ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17176_op(kk_std_core_types__ctail _acc, kk_function_t pred, kk_std_core__list xx, kk_std_core_types__maybe _y_16993, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16993)) {
    return kk_std_core__ctail_filter_map(xx, pred, _acc, _ctx);
  }
  {
    kk_box_t y = _y_16993._cons.Just.value;
    kk_std_core__list _ctail_16823 = kk_std_core__list_hole(); /*list<11541>*/;
    kk_std_core__list _ctail_16824 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16823, _ctx); /*list<11541>*/;
    kk_std_core_types__ctail _x20008;
    kk_box_t* _b_18303_18300 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16824)->tail)); /*cfield<list<11541>>*/;
    _x20008 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16824, _ctx)),_b_18303_18300); /*ctail<0>*/
    return kk_std_core__ctail_filter_map(xx, pred, _x20008, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17177_op_fun20009__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17177_op_fun20009(kk_function_t _fself, kk_std_core__list _ctail_16826, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17177_op_fun20009(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17177_op_fun20009__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17177_op_fun20009__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17177_op_fun20009, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17177_op_fun20009(kk_function_t _fself, kk_std_core__list _ctail_16826, kk_context_t* _ctx) {
  struct kk_std_core__mlift17177_op_fun20009__t* _self = kk_function_as(struct kk_std_core__mlift17177_op_fun20009__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11541>) -> list<11541> */
  kk_box_t y0 = _self->y0; /* 11541 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20010 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16826, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20010, _ctx));
}

kk_std_core__list kk_std_core__mlift17177_op(kk_function_t _accm, kk_function_t pred0, kk_std_core__list xx0, kk_std_core_types__maybe _y_16998, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16998)) {
    return kk_std_core__ctailm_filter_map(xx0, pred0, _accm, _ctx);
  }
  {
    kk_box_t y0 = _y_16998._cons.Just.value;
    return kk_std_core__ctailm_filter_map(xx0, pred0, kk_std_core__new_mlift17177_op_fun20009(_accm, y0, _ctx), _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core__ctail_filter_map_fun20015__t {
  struct kk_function_s _base;
  kk_function_t pred1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_filter_map_fun20015(kk_function_t _fself, kk_box_t _b_18310, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_filter_map_fun20015(kk_function_t pred1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_map_fun20015__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_filter_map_fun20015__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_filter_map_fun20015, kk_context());
  _self->pred1 = pred1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_filter_map_fun20015(kk_function_t _fself, kk_box_t _b_18310, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_map_fun20015__t* _self = kk_function_as(struct kk_std_core__ctail_filter_map_fun20015__t*, _fself);
  kk_function_t pred1 = _self->pred1; /* (11540) -> 11542 maybe<11541> */
  kk_std_core__list xx1 = _self->xx1; /* list<11540> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11541>> */
  kk_drop_match(_self, {kk_function_dup(pred1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20016;
  kk_std_core_types__maybe _x20017 = kk_std_core_types__maybe_unbox(_b_18310, _ctx); /*maybe<11541>*/
  _x20016 = kk_std_core__mlift17176_op(_acc0, pred1, xx1, _x20017, _ctx); /*list<11541>*/
  return kk_std_core__list_box(_x20016, _ctx);
}

kk_std_core__list kk_std_core__ctail_filter_map(kk_std_core__list xs, kk_function_t pred1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(pred1, _ctx);
    kk_box_t _x20011 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20011, _ctx);
  }
  {
    struct kk_std_core_Cons* _con20012 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20012->head;
    kk_std_core__list xx1 = _con20012->tail;
    kk_reuse_t _ru_18901 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18901 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18901 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17429;
    kk_function_t _x20013 = kk_function_dup(pred1); /*(11540) -> 11542 maybe<11541>*/
    x0_17429 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20013, (_x20013, x, _ctx)); /*maybe<11541>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18901, _ctx);
      kk_std_core_types__maybe_drop(x0_17429, _ctx);
      kk_box_t _x20014 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_filter_map_fun20015(pred1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20014, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17429)) {
      kk_reuse_drop(_ru_18901, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
    {
      kk_box_t y1 = x0_17429._cons.Just.value;
      kk_std_core__list _ctail_168230 = kk_std_core__list_hole(); /*list<11541>*/;
      kk_std_core__list _ctail_168240 = kk_std_core__new_Cons(_ru_18901, y1, _ctail_168230, _ctx); /*list<11541>*/;
      { // tailcall
        kk_std_core_types__ctail _x20018;
        kk_box_t* _b_18322_18316 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168240)->tail)); /*cfield<list<11541>>*/;
        _x20018 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168240, _ctx)),_b_18322_18316); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20018;
        goto kk__tailcall;
      }
    }
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core__ctailm_filter_map_fun20022__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t pred2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_filter_map_fun20022(kk_function_t _fself, kk_box_t _b_18328, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_map_fun20022(kk_function_t _accm0, kk_function_t pred2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20022__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_map_fun20022__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_map_fun20022, kk_context());
  _self->_accm0 = _accm0;
  _self->pred2 = pred2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_filter_map_fun20022(kk_function_t _fself, kk_box_t _b_18328, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20022__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_map_fun20022__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11541>) -> list<11541> */
  kk_function_t pred2 = _self->pred2; /* (11540) -> 11542 maybe<11541> */
  kk_std_core__list xx2 = _self->xx2; /* list<11540> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(pred2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20023;
  kk_std_core_types__maybe _x20024 = kk_std_core_types__maybe_unbox(_b_18328, _ctx); /*maybe<11541>*/
  _x20023 = kk_std_core__mlift17177_op(_accm0, pred2, xx2, _x20024, _ctx); /*list<11541>*/
  return kk_std_core__list_box(_x20023, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_filter_map_fun20026__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_filter_map_fun20026(kk_function_t _fself, kk_std_core__list _ctail_168260, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_map_fun20026(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20026__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_map_fun20026__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_map_fun20026, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_filter_map_fun20026(kk_function_t _fself, kk_std_core__list _ctail_168260, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20026__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_map_fun20026__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11541>) -> list<11541> */
  kk_box_t y2 = _self->y2; /* 11541 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20027 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168260, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20027, _ctx));
}

kk_std_core__list kk_std_core__ctailm_filter_map(kk_std_core__list xs0, kk_function_t pred2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs0)) {
    kk_function_drop(pred2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
  {
    struct kk_std_core_Cons* _con20019 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20019->head;
    kk_std_core__list xx2 = _con20019->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17432;
    kk_function_t _x20020 = kk_function_dup(pred2); /*(11540) -> 11542 maybe<11541>*/
    x2_17432 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20020, (_x20020, x1, _ctx)); /*maybe<11541>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17432, _ctx);
      kk_box_t _x20021 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_filter_map_fun20022(_accm0, pred2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20021, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x2_17432)) { // tailcall
                                                   xs0 = xx2;
                                                   goto kk__tailcall;
    }
    {
      kk_box_t y2 = x2_17432._cons.Just.value;
      { // tailcall
        kk_function_t _x20025 = kk_std_core__new_ctailm_filter_map_fun20026(_accm0, y2, _ctx); /*(list<11541>) -> list<11541>*/
        xs0 = xx2;
        _accm0 = _x20025;
        goto kk__tailcall;
      }
    }
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core_filter_map_fun20029__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_filter_map_fun20029(kk_function_t _fself, kk_std_core__list _ctail_16825, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_filter_map_fun20029(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_filter_map_fun20029, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_filter_map_fun20029(kk_function_t _fself, kk_std_core__list _ctail_16825, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16825;
}

kk_std_core__list kk_std_core_filter_map(kk_std_core__list xs1, kk_function_t pred3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_19049 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19049) {
    kk_std_core_types__ctail _x20028 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_filter_map(xs1, pred3, _x20028, _ctx);
  }
  {
    return kk_std_core__ctailm_filter_map(xs1, pred3, kk_std_core_new_filter_map_fun20029(_ctx), _ctx);
  }
}

bool kk_std_core_is_zero_1(kk_ssize_t i, kk_context_t* _ctx) { /* (i : ssize_t) -> bool */ 
  return (i == 0);
}
 
// Find the first element satisfying some predicate


// lift anonymous function
struct kk_std_core_find_fun20030__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_find_fun20030(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_find_fun20030(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_find_fun20030__t* _self = kk_function_alloc_as(struct kk_std_core_find_fun20030__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_find_fun20030, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_find_fun20030(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_find_fun20030__t* _self = kk_function_as(struct kk_std_core_find_fun20030__t*, _fself);
  kk_function_t pred = _self->pred; /* (11639) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  bool _match_19048;
  kk_box_t _x20031 = kk_box_dup(x); /*11639*/
  _match_19048 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), pred, (pred, _x20031, _ctx)); /*bool*/
  if (_match_19048) {
    return kk_std_core_types__new_Just(x, _ctx);
  }
  {
    kk_box_drop(x, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_core_find(kk_std_core__list xs, kk_function_t pred, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, pred : (a) -> bool) -> maybe<a> */ 
  return kk_std_core_foreach_while(xs, kk_std_core_new_find_fun20030(pred, _ctx), _ctx);
}
 
// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.

kk_std_core_types__maybe kk_std_core_find_1(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> maybe<sslice> */ 
  kk_ssize_t i;
  kk_string_t _x20032 = kk_string_dup(s); /*string*/
  kk_string_t _x20033 = kk_string_dup(sub); /*string*/
  i = kk_string_index_of1(_x20032,_x20033,kk_context()); /*ssize_t*/
  bool _match_19047 = kk_std_core_is_zero_1(i, _ctx); /*bool*/;
  if (_match_19047) {
    kk_string_drop(s, _ctx);
    kk_string_drop(sub, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_std_core__sslice _b_18332_18331;
    kk_ssize_t _x20034 = kk_std_core_decr_1(i, _ctx); /*ssize_t*/
    kk_ssize_t _x20035 = kk_string_len(sub,kk_context()); /*ssize_t*/
    _b_18332_18331 = kk_std_core__new_Sslice(s, _x20034, _x20035, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18332_18331, _ctx), _ctx);
  }
}
 
// Return the last index of substring `sub` in `s` if it occurs.

kk_std_core_types__maybe kk_std_core_find_last(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> maybe<sslice> */ 
  kk_ssize_t i;
  kk_string_t _x20036 = kk_string_dup(s); /*string*/
  kk_string_t _x20037 = kk_string_dup(sub); /*string*/
  i = kk_string_last_index_of1(_x20036,_x20037,kk_context()); /*ssize_t*/
  bool _match_19046 = kk_std_core_is_zero_1(i, _ctx); /*bool*/;
  if (_match_19046) {
    kk_string_drop(s, _ctx);
    kk_string_drop(sub, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_std_core__sslice _b_18334_18333;
    kk_ssize_t _x20038 = kk_std_core_decr_1(i, _ctx); /*ssize_t*/
    kk_ssize_t _x20039 = kk_string_len(sub,kk_context()); /*ssize_t*/
    _b_18334_18333 = kk_std_core__new_Sslice(s, _x20038, _x20039, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18334_18333, _ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17178_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_std_core_types__maybe _y_17006, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17006)) {
    kk_box_t y = _y_17006._cons.Just.value;
    kk_std_core__list _ctail_16827 = kk_std_core__list_hole(); /*list<11845>*/;
    kk_std_core__list _ctail_16828 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16827, _ctx); /*list<11845>*/;
    kk_std_core_types__ctail _x20040;
    kk_box_t* _b_18343_18340 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16828)->tail)); /*cfield<list<11845>>*/;
    _x20040 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16828, _ctx)),_b_18343_18340); /*ctail<0>*/
    return kk_std_core__ctail_flatmap_maybe(xx, f, _x20040, _ctx);
  }
  {
    return kk_std_core__ctail_flatmap_maybe(xx, f, _acc, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17179_op_fun20041__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17179_op_fun20041(kk_function_t _fself, kk_std_core__list _ctail_16830, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17179_op_fun20041(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17179_op_fun20041__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17179_op_fun20041__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17179_op_fun20041, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17179_op_fun20041(kk_function_t _fself, kk_std_core__list _ctail_16830, kk_context_t* _ctx) {
  struct kk_std_core__mlift17179_op_fun20041__t* _self = kk_function_as(struct kk_std_core__mlift17179_op_fun20041__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11845>) -> list<11845> */
  kk_box_t y0 = _self->y0; /* 11845 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20042 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16830, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20042, _ctx));
}

kk_std_core__list kk_std_core__mlift17179_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_std_core_types__maybe _y_17011, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17011)) {
    kk_box_t y0 = _y_17011._cons.Just.value;
    return kk_std_core__ctailm_flatmap_maybe(xx0, f0, kk_std_core__new_mlift17179_op_fun20041(_accm, y0, _ctx), _ctx);
  }
  {
    return kk_std_core__ctailm_flatmap_maybe(xx0, f0, _accm, _ctx);
  }
}
 
// Concatenate the `Just` result elements from applying a function to all elements.


// lift anonymous function
struct kk_std_core__ctail_flatmap_maybe_fun20046__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_flatmap_maybe_fun20046(kk_function_t _fself, kk_box_t _b_18348, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_flatmap_maybe_fun20046(kk_function_t f1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_flatmap_maybe_fun20046__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_flatmap_maybe_fun20046__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_flatmap_maybe_fun20046, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_flatmap_maybe_fun20046(kk_function_t _fself, kk_box_t _b_18348, kk_context_t* _ctx) {
  struct kk_std_core__ctail_flatmap_maybe_fun20046__t* _self = kk_function_as(struct kk_std_core__ctail_flatmap_maybe_fun20046__t*, _fself);
  kk_function_t f1 = _self->f1; /* (11844) -> 11846 maybe<11845> */
  kk_std_core__list xx1 = _self->xx1; /* list<11844> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11845>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20047;
  kk_std_core_types__maybe _x20048 = kk_std_core_types__maybe_unbox(_b_18348, _ctx); /*maybe<11845>*/
  _x20047 = kk_std_core__mlift17178_op(_acc0, f1, xx1, _x20048, _ctx); /*list<11845>*/
  return kk_std_core__list_box(_x20047, _ctx);
}

kk_std_core__list kk_std_core__ctail_flatmap_maybe(kk_std_core__list xs, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20043 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20043->head;
    kk_std_core__list xx1 = _con20043->tail;
    kk_reuse_t _ru_18903 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18903 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18903 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17435;
    kk_function_t _x20044 = kk_function_dup(f1); /*(11844) -> 11846 maybe<11845>*/
    x0_17435 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20044, (_x20044, x, _ctx)); /*maybe<11845>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18903, _ctx);
      kk_std_core_types__maybe_drop(x0_17435, _ctx);
      kk_box_t _x20045 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_flatmap_maybe_fun20046(f1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20045, _ctx);
    }
    if (kk_std_core_types__is_Just(x0_17435)) {
      kk_box_t y1 = x0_17435._cons.Just.value;
      kk_std_core__list _ctail_168270 = kk_std_core__list_hole(); /*list<11845>*/;
      kk_std_core__list _ctail_168280 = kk_std_core__new_Cons(_ru_18903, y1, _ctail_168270, _ctx); /*list<11845>*/;
      { // tailcall
        kk_std_core_types__ctail _x20049;
        kk_box_t* _b_18360_18354 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168280)->tail)); /*cfield<list<11845>>*/;
        _x20049 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168280, _ctx)),_b_18360_18354); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20049;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18903, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x20050 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20050, _ctx);
  }
}
 
// Concatenate the `Just` result elements from applying a function to all elements.


// lift anonymous function
struct kk_std_core__ctailm_flatmap_maybe_fun20054__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_flatmap_maybe_fun20054(kk_function_t _fself, kk_box_t _b_18368, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_flatmap_maybe_fun20054(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20054__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_flatmap_maybe_fun20054__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_flatmap_maybe_fun20054, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_flatmap_maybe_fun20054(kk_function_t _fself, kk_box_t _b_18368, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20054__t* _self = kk_function_as(struct kk_std_core__ctailm_flatmap_maybe_fun20054__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11845>) -> list<11845> */
  kk_function_t f2 = _self->f2; /* (11844) -> 11846 maybe<11845> */
  kk_std_core__list xx2 = _self->xx2; /* list<11844> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20055;
  kk_std_core_types__maybe _x20056 = kk_std_core_types__maybe_unbox(_b_18368, _ctx); /*maybe<11845>*/
  _x20055 = kk_std_core__mlift17179_op(_accm0, f2, xx2, _x20056, _ctx); /*list<11845>*/
  return kk_std_core__list_box(_x20055, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_flatmap_maybe_fun20058__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_flatmap_maybe_fun20058(kk_function_t _fself, kk_std_core__list _ctail_168300, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_flatmap_maybe_fun20058(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20058__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_flatmap_maybe_fun20058__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_flatmap_maybe_fun20058, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_flatmap_maybe_fun20058(kk_function_t _fself, kk_std_core__list _ctail_168300, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20058__t* _self = kk_function_as(struct kk_std_core__ctailm_flatmap_maybe_fun20058__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11845>) -> list<11845> */
  kk_box_t y2 = _self->y2; /* 11845 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20059 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168300, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20059, _ctx));
}

kk_std_core__list kk_std_core__ctailm_flatmap_maybe(kk_std_core__list xs0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20051 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20051->head;
    kk_std_core__list xx2 = _con20051->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17438;
    kk_function_t _x20052 = kk_function_dup(f2); /*(11844) -> 11846 maybe<11845>*/
    x2_17438 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20052, (_x20052, x1, _ctx)); /*maybe<11845>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17438, _ctx);
      kk_box_t _x20053 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_flatmap_maybe_fun20054(_accm0, f2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20053, _ctx);
    }
    if (kk_std_core_types__is_Just(x2_17438)) {
      kk_box_t y2 = x2_17438._cons.Just.value;
      { // tailcall
        kk_function_t _x20057 = kk_std_core__new_ctailm_flatmap_maybe_fun20058(_accm0, y2, _ctx); /*(list<11845>) -> list<11845>*/
        xs0 = xx2;
        _accm0 = _x20057;
        goto kk__tailcall;
      }
    }
    { // tailcall
      xs0 = xx2;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Concatenate the `Just` result elements from applying a function to all elements.


// lift anonymous function
struct kk_std_core_flatmap_maybe_fun20061__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_flatmap_maybe_fun20061(kk_function_t _fself, kk_std_core__list _ctail_16829, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_flatmap_maybe_fun20061(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_flatmap_maybe_fun20061, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_flatmap_maybe_fun20061(kk_function_t _fself, kk_std_core__list _ctail_16829, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16829;
}

kk_std_core__list kk_std_core_flatmap_maybe(kk_std_core__list xs1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_19043 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19043) {
    kk_std_core_types__ctail _x20060 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_flatmap_maybe(xs1, f3, _x20060, _ctx);
  }
  {
    return kk_std_core__ctailm_flatmap_maybe(xs1, f3, kk_std_core_new_flatmap_maybe_fun20061(_ctx), _ctx);
  }
}

kk_box_t kk_std_core_fold_int(kk_integer_t start0, kk_integer_t end, kk_box_t init0, kk_function_t f, kk_context_t* _ctx) { /* forall<a> (start : int, end : int, init : a, f : (int, a) -> a) -> a */ 
  kk__tailcall: ;
  bool _match_19042;
  kk_integer_t _x20062 = kk_integer_dup(start0); /*int*/
  kk_integer_t _x20063 = kk_integer_dup(end); /*int*/
  _match_19042 = kk_integer_gte(_x20062,_x20063,kk_context()); /*bool*/
  if (_match_19042) {
    kk_integer_drop(end, _ctx);
    kk_function_drop(f, _ctx);
    kk_integer_drop(start0, _ctx);
    return init0;
  }
  {
    kk_box_t x;
    kk_function_t _x20065 = kk_function_dup(f); /*(int, 11903) -> 11903*/
    kk_integer_t _x20064 = kk_integer_dup(start0); /*int*/
    x = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), _x20065, (_x20065, _x20064, init0, _ctx)); /*11903*/
    { // tailcall
      kk_integer_t _x20066 = kk_integer_add(start0,(kk_integer_from_small(1)),kk_context()); /*int*/
      start0 = _x20066;
      init0 = x;
      goto kk__tailcall;
    }
  }
}
 
// monadic lift

kk_box_t kk_std_core__mlift17180_foldl(kk_function_t f, kk_std_core__list xx, kk_box_t _y_17019, kk_context_t* _ctx) { /* forall<a,e,b> (f : (a, b) -> e a, xx : list<b>, a) -> e a */ 
  return kk_std_core_foldl(xx, _y_17019, f, _ctx);
}
 
// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`


// lift anonymous function
struct kk_std_core_foldl_fun20069__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foldl_fun20069(kk_function_t _fself, kk_box_t _y_170190, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldl_fun20069(kk_function_t f0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foldl_fun20069__t* _self = kk_function_alloc_as(struct kk_std_core_foldl_fun20069__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foldl_fun20069, kk_context());
  _self->f0 = f0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foldl_fun20069(kk_function_t _fself, kk_box_t _y_170190, kk_context_t* _ctx) {
  struct kk_std_core_foldl_fun20069__t* _self = kk_function_as(struct kk_std_core_foldl_fun20069__t*, _fself);
  kk_function_t f0 = _self->f0; /* (11906, 11911) -> 11908 11906 */
  kk_std_core__list xx0 = _self->xx0; /* list<11911> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  return kk_std_core__mlift17180_foldl(f0, xx0, _y_170190, _ctx);
}

kk_box_t kk_std_core_foldl(kk_std_core__list xs, kk_box_t z, kk_function_t f0, kk_context_t* _ctx) { /* forall<a,b,e> (list<a>, b, (b, a) -> e b) -> e b */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20067 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20067->head;
    kk_std_core__list xx0 = _con20067->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t x0_17441;
    kk_function_t _x20068 = kk_function_dup(f0); /*(11906, 11911) -> 11908 11906*/
    x0_17441 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20068, (_x20068, z, x, _ctx)); /*11906*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17441, _ctx);
      return kk_std_core_hnd_yield_extend(kk_std_core_new_foldl_fun20069(f0, xx0, _ctx), _ctx);
    }
    { // tailcall
      xs = xx0;
      z = x0_17441;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f0, _ctx);
    return z;
  }
}


// lift anonymous function
struct kk_std_core_foldl1_fun20073__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foldl1_fun20073(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldl1_fun20073(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foldl1_fun20073, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foldl1_fun20073(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x20074;
  kk_define_string_literal(, _s20075, 33, "unexpected Nil in std/core/foldl1")
  _x20074 = kk_string_dup(_s20075); /*string*/
  return kk_std_core_throw(_x20074, kk_std_core_types__new_None(_ctx), _ctx);
}

kk_box_t kk_std_core_foldl1(kk_std_core__list xs, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20070 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20070->head;
    kk_std_core__list xx = _con20070->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core_foldl(xx, x, f, _ctx);
  }
  {
    kk_function_drop(f, _ctx);
    kk_ssize_t _x20071;
    kk_std_core_hnd__htag _x20072 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
    _x20071 = kk_std_core_hnd__evv_index(_x20072, _ctx); /*std/core/hnd/ev-index*/
    return kk_std_core_hnd__open_at0(_x20071, kk_std_core_new_foldl1_fun20073(_ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16747_reverse(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20076 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con20076->head;
    kk_std_core__list xx = _con20076->tail;
    kk_reuse_t _ru_18907 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18907 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18907 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20077;
      if (kk_likely(_ru_18907!=NULL)) {
        struct kk_std_core_Cons* _con20078 = (struct kk_std_core_Cons*)_ru_18907;
        _con20078->tail = acc;
        _x20077 = kk_std_core__base_Cons(_con20078); /*list<61>*/
      }
      else {
        _x20077 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x20077;
      ys = xx;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
extern kk_box_t kk_std_core_foldr_fun20080(kk_function_t _fself, kk_box_t x, kk_box_t y, kk_context_t* _ctx) {
  struct kk_std_core_foldr_fun20080__t* _self = kk_function_as(struct kk_std_core_foldr_fun20080__t*, _fself);
  kk_function_t f = _self->f; /* (11997, 11993) -> 11995 11993 */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), f, (f, y, x, _ctx));
}


// lift anonymous function
struct kk_std_core_foldr1_fun20084__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foldr1_fun20084(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldr1_fun20084(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foldr1_fun20084, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foldr1_fun20084(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x20085;
  kk_define_string_literal(, _s20086, 33, "unexpected Nil in std/core/foldl1")
  _x20085 = kk_string_dup(_s20086); /*string*/
  return kk_std_core_throw(_x20085, kk_std_core_types__new_None(_ctx), _ctx);
}

kk_box_t kk_std_core_foldr1(kk_std_core__list xs, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */ 
  kk_std_core__list xs0_16649 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), xs, _ctx); /*list<12031>*/;
  if (kk_std_core__is_Cons(xs0_16649)) {
    struct kk_std_core_Cons* _con20081 = kk_std_core__as_Cons(xs0_16649);
    kk_box_t x = _con20081->head;
    kk_std_core__list xx = _con20081->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0_16649))) {
      kk_std_core__list_free(xs0_16649);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs0_16649, _ctx);
    }
    return kk_std_core_foldl(xx, x, f, _ctx);
  }
  {
    kk_function_drop(f, _ctx);
    kk_ssize_t _x20082;
    kk_std_core_hnd__htag _x20083 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
    _x20082 = kk_std_core_hnd__evv_index(_x20083, _ctx); /*std/core/hnd/ev-index*/
    return kk_std_core_hnd__open_at0(_x20082, kk_std_core_new_foldr1_fun20084(_ctx), _ctx);
  }
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17181_op(kk_function_t action, kk_integer_t end, kk_integer_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : (int) -> e (), end : int, i : int, wild_ : ()) -> e () */ 
  kk_integer_t i0_16778 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__lift16748_for(action, end, i0_16778, _ctx); return kk_Unit;
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16748_for_fun20092__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_integer_t end0;
  kk_integer_t i0;
};
static kk_box_t kk_std_core__lift16748_for_fun20092(kk_function_t _fself, kk_box_t _b_18372, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16748_for_fun20092(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16748_for_fun20092__t* _self = kk_function_alloc_as(struct kk_std_core__lift16748_for_fun20092__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16748_for_fun20092, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16748_for_fun20092(kk_function_t _fself, kk_box_t _b_18372, kk_context_t* _ctx) {
  struct kk_std_core__lift16748_for_fun20092__t* _self = kk_function_as(struct kk_std_core__lift16748_for_fun20092__t*, _fself);
  kk_function_t action0 = _self->action0; /* (int) -> 12068 () */
  kk_integer_t end0 = _self->end0; /* int */
  kk_integer_t i0 = _self->i0; /* int */
  kk_drop_match(_self, {kk_function_dup(action0);kk_integer_dup(end0);kk_integer_dup(i0);}, {}, _ctx)
  kk_unit_t _x20093 = kk_Unit;
  kk_unit_t _x20094 = kk_Unit;
  kk_unit_unbox(_b_18372);
  kk_std_core__mlift17181_op(action0, end0, i0, _x20094, _ctx);
  return kk_unit_box(_x20093);
}

kk_unit_t kk_std_core__lift16748_for(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) { /* forall<e> (action : (int) -> e (), end : int, i : int) -> e () */ 
  kk__tailcall: ;
  bool _match_19039;
  kk_integer_t _x20087 = kk_integer_dup(i0); /*int*/
  kk_integer_t _x20088 = kk_integer_dup(end0); /*int*/
  _match_19039 = kk_integer_lte(_x20087,_x20088,kk_context()); /*bool*/
  if (_match_19039) {
    kk_unit_t x_17446 = kk_Unit;
    kk_function_t _x20090 = kk_function_dup(action0); /*(int) -> 12068 ()*/
    kk_integer_t _x20089 = kk_integer_dup(i0); /*int*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), _x20090, (_x20090, _x20089, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20091 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16748_for_fun20092(action0, end0, i0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20091); return kk_Unit;
    }
    {
      kk_integer_t i0_167780 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      { // tailcall
        i0 = i0_167780;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(action0, _ctx);
    kk_integer_drop(end0, _ctx);
    kk_integer_drop(i0, _ctx);
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17182_op(kk_function_t action, kk_integer_t end, kk_integer_t i, kk_std_core_types__maybe _y_17036, kk_context_t* _ctx) { /* forall<a,e> (action : (int) -> e maybe<a>, end : int, i : int, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_17036)) {
    kk_integer_t i0_16779 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
    return kk_std_core__lift16749_for_while(action, end, i0_16779, _ctx);
  }
  {
    kk_box_t x = _y_17036._cons.Just.value;
    kk_function_drop(action, _ctx);
    kk_integer_drop(end, _ctx);
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Just(x, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16749_for_while_fun20100__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_integer_t end0;
  kk_integer_t i0;
};
static kk_box_t kk_std_core__lift16749_for_while_fun20100(kk_function_t _fself, kk_box_t _b_18376, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16749_for_while_fun20100(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16749_for_while_fun20100__t* _self = kk_function_alloc_as(struct kk_std_core__lift16749_for_while_fun20100__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16749_for_while_fun20100, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16749_for_while_fun20100(kk_function_t _fself, kk_box_t _b_18376, kk_context_t* _ctx) {
  struct kk_std_core__lift16749_for_while_fun20100__t* _self = kk_function_as(struct kk_std_core__lift16749_for_while_fun20100__t*, _fself);
  kk_function_t action0 = _self->action0; /* (int) -> 12119 maybe<12118> */
  kk_integer_t end0 = _self->end0; /* int */
  kk_integer_t i0 = _self->i0; /* int */
  kk_drop_match(_self, {kk_function_dup(action0);kk_integer_dup(end0);kk_integer_dup(i0);}, {}, _ctx)
  kk_std_core_types__maybe _x20101;
  kk_std_core_types__maybe _x20102 = kk_std_core_types__maybe_unbox(_b_18376, _ctx); /*maybe<12118>*/
  _x20101 = kk_std_core__mlift17182_op(action0, end0, i0, _x20102, _ctx); /*maybe<12118>*/
  return kk_std_core_types__maybe_box(_x20101, _ctx);
}

kk_std_core_types__maybe kk_std_core__lift16749_for_while(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,e> (action : (int) -> e maybe<a>, end : int, i : int) -> e maybe<a> */ 
  kk__tailcall: ;
  bool _match_19037;
  kk_integer_t _x20095 = kk_integer_dup(i0); /*int*/
  kk_integer_t _x20096 = kk_integer_dup(end0); /*int*/
  _match_19037 = kk_integer_lte(_x20095,_x20096,kk_context()); /*bool*/
  if (_match_19037) {
    kk_std_core_types__maybe x0_17449;
    kk_function_t _x20098 = kk_function_dup(action0); /*(int) -> 12119 maybe<12118>*/
    kk_integer_t _x20097 = kk_integer_dup(i0); /*int*/
    x0_17449 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_integer_t, kk_context_t*), _x20098, (_x20098, _x20097, _ctx)); /*maybe<12118>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17449, _ctx);
      kk_box_t _x20099 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16749_for_while_fun20100(action0, end0, i0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x20099, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17449)) {
      kk_integer_t i0_167790 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      { // tailcall
        i0 = i0_167790;
        goto kk__tailcall;
      }
    }
    {
      kk_box_t x1 = x0_17449._cons.Just.value;
      kk_integer_drop(i0, _ctx);
      kk_integer_drop(end0, _ctx);
      kk_function_drop(action0, _ctx);
      return kk_std_core_types__new_Just(x1, _ctx);
    }
  }
  {
    kk_function_drop(action0, _ctx);
    kk_integer_drop(end0, _ctx);
    kk_integer_drop(i0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17184_foreach_indexed_fun20105__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_unit_t kk_std_core__mlift17184_foreach_indexed_fun20105(kk_function_t _fself, kk_integer_t _y_17044, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17184_foreach_indexed_fun20105(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_std_core__mlift17184_foreach_indexed_fun20105__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17184_foreach_indexed_fun20105__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17184_foreach_indexed_fun20105, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_unit_t kk_std_core__mlift17184_foreach_indexed_fun20105(kk_function_t _fself, kk_integer_t _y_17044, kk_context_t* _ctx) {
  struct kk_std_core__mlift17184_foreach_indexed_fun20105__t* _self = kk_function_as(struct kk_std_core__mlift17184_foreach_indexed_fun20105__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _b_18388_18386 = kk_integer_add(_y_17044,(kk_integer_from_small(1)),kk_context()); /*int*/;
  return (kk_ref_set(i,(kk_integer_box(_b_18388_18386)),kk_context()));
}


// lift anonymous function
struct kk_std_core__mlift17184_foreach_indexed_fun20107__t {
  struct kk_function_s _base;
  kk_function_t next0_17453;
};
static kk_box_t kk_std_core__mlift17184_foreach_indexed_fun20107(kk_function_t _fself, kk_box_t _b_18390, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17184_foreach_indexed_fun20107(kk_function_t next0_17453, kk_context_t* _ctx) {
  struct kk_std_core__mlift17184_foreach_indexed_fun20107__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17184_foreach_indexed_fun20107__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17184_foreach_indexed_fun20107, kk_context());
  _self->next0_17453 = next0_17453;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17184_foreach_indexed_fun20107(kk_function_t _fself, kk_box_t _b_18390, kk_context_t* _ctx) {
  struct kk_std_core__mlift17184_foreach_indexed_fun20107__t* _self = kk_function_as(struct kk_std_core__mlift17184_foreach_indexed_fun20107__t*, _fself);
  kk_function_t next0_17453 = _self->next0_17453; /* (int) -> <local<12223>|12230> () */
  kk_drop_match(_self, {kk_function_dup(next0_17453);}, {}, _ctx)
  kk_unit_t _x20108 = kk_Unit;
  kk_integer_t _x20109 = kk_integer_unbox(_b_18390); /*int*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17453, (next0_17453, _x20109, _ctx));
  return kk_unit_box(_x20108);
}

kk_unit_t kk_std_core__mlift17184_foreach_indexed(kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h,e> (i : local-var<h,int>, wild_ : ()) -> <local<h>|e> () */ 
  kk_integer_t x_17452;
  kk_box_t _x20103;
  kk_ref_t _x20104 = kk_ref_dup(i); /*local-var<12223,int>*/
  _x20103 = (kk_ref_get(_x20104,kk_context())); /*233*/
  x_17452 = kk_integer_unbox(_x20103); /*int*/
  kk_function_t next0_17453 = kk_std_core__new_mlift17184_foreach_indexed_fun20105(i, _ctx); /*(int) -> <local<12223>|12230> ()*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_17452, _ctx);
    kk_box_t _x20106 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17184_foreach_indexed_fun20107(next0_17453, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20106); return kk_Unit;
  }
  {
    kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17453, (next0_17453, x_17452, _ctx)); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17185_foreach_indexed_fun20111__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_std_core__mlift17185_foreach_indexed_fun20111(kk_function_t _fself, kk_box_t _b_18393, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17185_foreach_indexed_fun20111(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_std_core__mlift17185_foreach_indexed_fun20111__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17185_foreach_indexed_fun20111__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17185_foreach_indexed_fun20111, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17185_foreach_indexed_fun20111(kk_function_t _fself, kk_box_t _b_18393, kk_context_t* _ctx) {
  struct kk_std_core__mlift17185_foreach_indexed_fun20111__t* _self = kk_function_as(struct kk_std_core__mlift17185_foreach_indexed_fun20111__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_unit_t _x20112 = kk_Unit;
  kk_unit_t _x20113 = kk_Unit;
  kk_unit_unbox(_b_18393);
  kk_std_core__mlift17184_foreach_indexed(i, _x20113, _ctx);
  return kk_unit_box(_x20112);
}

kk_unit_t kk_std_core__mlift17185_foreach_indexed(kk_function_t action, kk_ref_t i, kk_box_t x, kk_integer_t j, kk_context_t* _ctx) { /* forall<h,a,e> (action : (int, a) -> e (), i : local-var<h,int>, x : a, j : int) -> <local<h>|e> () */ 
  kk_unit_t x0_17456 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), action, (action, j, x, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x20110 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17185_foreach_indexed_fun20111(i, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20110); return kk_Unit;
  }
  {
    kk_std_core__mlift17184_foreach_indexed(i, x0_17456, _ctx); return kk_Unit;
  }
}
 
// Invoke `action` for each element of a list, passing also the position of the element.


// lift anonymous function
struct kk_std_core_foreach_indexed_fun20115__t {
  struct kk_function_s _base;
  kk_function_t action;
  kk_ref_t loc;
};
static kk_unit_t kk_std_core_foreach_indexed_fun20115(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20115(kk_function_t action, kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20115__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20115__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20115, kk_context());
  _self->action = action;
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_indexed_fun20119__t {
  struct kk_function_s _base;
  kk_function_t action;
  kk_ref_t loc;
  kk_box_t x;
};
static kk_box_t kk_std_core_foreach_indexed_fun20119(kk_function_t _fself, kk_box_t _b_18401, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20119(kk_function_t action, kk_ref_t loc, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20119__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20119__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20119, kk_context());
  _self->action = action;
  _self->loc = loc;
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_indexed_fun20119(kk_function_t _fself, kk_box_t _b_18401, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20119__t* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20119__t*, _fself);
  kk_function_t action = _self->action; /* (int, 12229) -> 12230 () */
  kk_ref_t loc = _self->loc; /* local-var<12223,int> */
  kk_box_t x = _self->x; /* 12229 */
  kk_drop_match(_self, {kk_function_dup(action);kk_ref_dup(loc);kk_box_dup(x);}, {}, _ctx)
  kk_unit_t _x20120 = kk_Unit;
  kk_integer_t _x20121 = kk_integer_unbox(_b_18401); /*int*/
  kk_std_core__mlift17185_foreach_indexed(action, loc, x, _x20121, _ctx);
  return kk_unit_box(_x20120);
}
static kk_unit_t kk_std_core_foreach_indexed_fun20115(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20115__t* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20115__t*, _fself);
  kk_function_t action = _self->action; /* (int, 12229) -> 12230 () */
  kk_ref_t loc = _self->loc; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_function_dup(action);kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x0_17461;
  kk_box_t _x20116;
  kk_ref_t _x20117 = kk_ref_dup(loc); /*local-var<12223,int>*/
  _x20116 = (kk_ref_get(_x20117,kk_context())); /*233*/
  x0_17461 = kk_integer_unbox(_x20116); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x0_17461, _ctx);
    kk_box_t _x20118 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_indexed_fun20119(action, loc, x, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x20118);
  }
  {
    return kk_std_core__mlift17185_foreach_indexed(action, loc, x, x0_17461, _ctx);
  }
}

kk_unit_t kk_std_core_foreach_indexed(kk_std_core__list xs, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, action : (int, a) -> e ()) -> e () */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<12223,int>*/;
  kk_unit_t res = kk_Unit;
  kk_function_t _x20114;
  kk_ref_dup(loc);
  _x20114 = kk_std_core_new_foreach_indexed_fun20115(action, loc, _ctx); /*(x : 12229) -> <local<12223>|12230> ()*/
  kk_std_core_foreach(xs, _x20114, _ctx);
  kk_box_t _x20122 = kk_std_core_hnd_prompt_local_var(loc, kk_unit_box(res), _ctx); /*9897*/
  kk_unit_unbox(_x20122); return kk_Unit;
}
 
// Invoke a function `f` for each element in a vector `v`


// lift anonymous function
struct kk_std_core_foreach_indexed_fun20125__t_1 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_indexed_fun20125_1(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20125_1(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20125__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20125__t_1, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20125_1, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_indexed_fun20125_1(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20125__t_1* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20125__t_1*, _fself);
  kk_function_t f = _self->f; /* (12259, int) -> 12260 () */
  kk_vector_t v = _self->v; /* vector<12259> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t x_17596 = kk_vector_at(v,i,kk_context()); /*12259*/;
  kk_integer_t _x20126 = kk_integer_from_ssize_t(i,kk_context()); /*int*/
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_integer_t, kk_context_t*), f, (f, x_17596, _x20126, _ctx));
}

kk_unit_t kk_std_core_foreach_indexed_1(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a, int) -> e ()) -> e () */ 
  kk_ssize_t start0_17465 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17466;
  kk_ssize_t _x20123;
  kk_vector_t _x20124 = kk_vector_dup(v); /*vector<12259>*/
  _x20123 = kk_vector_len(_x20124,kk_context()); /*ssize_t*/
  end_17466 = kk_std_core_decr_1(_x20123, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_indexed_fun20125_1(f, v, _ctx), end_17466, start0_17465, _ctx); return kk_Unit;
}
 
// Return the head of list if the list is not empty.

kk_std_core_types__maybe kk_std_core_head_1(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20135 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20135->head;
    kk_std_core__list _pat0 = _con20135->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_drop(_pat0, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core_types__new_Just(x, _ctx);
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// Return the head of list if the list is not empty, or use `default` otherwise

kk_box_t kk_std_core_head_2(kk_std_core__list xs, kk_box_t default0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, default : a) -> a */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20136 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20136->head;
    kk_std_core__list _pat0 = _con20136->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_drop(_pat0, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_drop(default0, _ctx);
    return x;
  }
  {
    return default0;
  }
}
 
// Return the first character of a string as a string (or the empty string)

kk_string_t kk_std_core_head_3(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_std_core__sslice _x20137;
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_19031;
  kk_integer_t _x20138;
  kk_std_core_types__optional _match_19033 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19033)) {
    kk_box_t _box_x18408 = _match_19033._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18408);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_19033, _ctx);
    _x20138 = _n_9710; /*int*/
    goto _match20139;
  }
  {
    _x20138 = kk_integer_from_small(1); /*int*/
  }
  _match20139: ;
  _match_19031 = kk_integer_eq(_x20138,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19031) {
    _x20137 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x20141;
    kk_integer_t _x20142;
    kk_std_core_types__optional _match_19032 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19032)) {
      kk_box_t _box_x18409 = _match_19032._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18409);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_19032, _ctx);
      _x20142 = _n_97100; /*int*/
      goto _match20143;
    }
    {
      _x20142 = kk_integer_from_small(1); /*int*/
    }
    _match20143: ;
    _x20141 = kk_integer_sub(_x20142,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x20137 = kk_std_core_extend(slice0, _x20141, _ctx); /*sslice*/
  }
  return kk_std_core_string_3(_x20137, _ctx);
}
 
// Return the first character of a string (or `Nothing` for the empty string).


// lift anonymous function
struct kk_std_core_head_char_fun20149__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_core_head_char_fun20149(kk_function_t _fself, kk_char_t _b_18410, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_head_char_fun20149(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_head_char_fun20149, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_core_head_char_fun20149(kk_function_t _fself, kk_char_t _b_18410, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_types__new_Just(kk_char_box(_b_18410, _ctx), _ctx);
}

kk_std_core_types__maybe kk_std_core_head_char(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> maybe<char> */ 
  kk_std_core__sslice _x20145;
  kk_string_t _x20146 = kk_string_dup(s); /*string*/
  kk_ssize_t _x20147 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x20148 = kk_string_len(s,kk_context()); /*ssize_t*/
  _x20145 = kk_std_core__new_Sslice(_x20146, _x20147, _x20148, _ctx); /*sslice*/
  return kk_std_core_foreach_while_1(_x20145, kk_std_core_new_head_char_fun20149(_ctx), _ctx);
}

kk_integer_t kk_std_core_index_of_acc(kk_std_core__list xs, kk_function_t pred, kk_integer_t idx, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, pred : (a) -> bool, idx : int) -> int */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_integer_drop(idx, _ctx);
    kk_function_drop(pred, _ctx);
    return kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context());
  }
  {
    struct kk_std_core_Cons* _con20150 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20150->head;
    kk_std_core__list xx = _con20150->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool _match_19030;
    kk_function_t _x20151 = kk_function_dup(pred); /*(12636) -> bool*/
    _match_19030 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20151, (_x20151, x, _ctx)); /*bool*/
    if (_match_19030) {
      kk_function_drop(pred, _ctx);
      kk_std_core__list_drop(xx, _ctx);
      return idx;
    }
    { // tailcall
      kk_integer_t _x20152 = kk_integer_add(idx,(kk_integer_from_small(1)),kk_context()); /*int*/
      xs = xx;
      idx = _x20152;
      goto kk__tailcall;
    }
  }
}
 
// Return the list without its last element.
// Return an empty list for an empty list.

kk_std_core__list kk_std_core__ctail_init(kk_std_core__list xs, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20153 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20153->head;
    kk_std_core__list xx = _con20153->tail;
    if (kk_std_core__is_Cons(xx)) {
      struct kk_std_core_Cons* _con20154 = kk_std_core__as_Cons(xx);
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_reuse_t _ru_18913;
      kk_std_core__list _x20155 = kk_std_core__list_dup(xx); /*list<12673>*/
      _ru_18913 = kk_std_core__list_dropn_reuse(_x20155, ((int32_t)KI32(2)), _ctx); /*reuse*/
      kk_std_core__list _ctail_16831 = kk_std_core__list_hole(); /*list<12673>*/;
      kk_std_core__list _ctail_16832 = kk_std_core__new_Cons(_ru_18913, x, _ctail_16831, _ctx); /*list<12673>*/;
      { // tailcall
        kk_std_core_types__ctail _x20156;
        kk_box_t* _b_18421_18416 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16832)->tail)); /*cfield<list<12673>>*/;
        _x20156 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16832, _ctx)),_b_18421_18416); /*ctail<0>*/
        xs = xx;
        _acc = _x20156;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_std_core__list_drop(xs, _ctx);
    kk_box_t _x20157 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20157, _ctx);
  }
}
 
// Return the list without its last element.
// Return an empty list for an empty list.

kk_std_core__list kk_std_core_init(kk_std_core__list xs0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> list<a> */ 
  kk_std_core_types__ctail _x20158 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_init(xs0, _x20158, _ctx);
}
 
// An invalid slice

kk_std_core__sslice kk_std_core_invalid;
 
// Is the character an ASCII letter is-

bool kk_std_core_is_alpha(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> bool */ 
  bool _match_19024 = (c >= ('a')); /*bool*/;
  if (_match_19024) {
    bool _match_19026 = (c <= ('z')); /*bool*/;
    if (_match_19026) {
      return true;
    }
    {
      bool _match_19027 = (c >= ('A')); /*bool*/;
      if (_match_19027) {
        return (c <= ('Z'));
      }
      {
        return false;
      }
    }
  }
  {
    bool _match_19025 = (c >= ('A')); /*bool*/;
    if (_match_19025) {
      return (c <= ('Z'));
    }
    {
      return false;
    }
  }
}
 
// Is the character an ASCII hexa-decimal digit ?

bool kk_std_core_is_hex_digit(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> bool */ 
  bool _match_19011 = (c >= ('0')); /*bool*/;
  if (_match_19011) {
    bool _match_19016 = (c <= ('9')); /*bool*/;
    if (_match_19016) {
      return true;
    }
    {
      bool _match_19017 = (c >= ('a')); /*bool*/;
      if (_match_19017) {
        bool _match_19019 = (c <= ('f')); /*bool*/;
        if (_match_19019) {
          return true;
        }
        {
          bool _match_19020 = (c >= ('A')); /*bool*/;
          if (_match_19020) {
            return (c <= ('F'));
          }
          {
            return false;
          }
        }
      }
      {
        bool _match_19018 = (c >= ('A')); /*bool*/;
        if (_match_19018) {
          return (c <= ('F'));
        }
        {
          return false;
        }
      }
    }
  }
  {
    bool _match_19012 = (c >= ('a')); /*bool*/;
    if (_match_19012) {
      bool _match_19014 = (c <= ('f')); /*bool*/;
      if (_match_19014) {
        return true;
      }
      {
        bool _match_19015 = (c >= ('A')); /*bool*/;
        if (_match_19015) {
          return (c <= ('F'));
        }
        {
          return false;
        }
      }
    }
    {
      bool _match_19013 = (c >= ('A')); /*bool*/;
      if (_match_19013) {
        return (c <= ('F'));
      }
      {
        return false;
      }
    }
  }
}
 
// Tests if a character is an element of `" \t\n\r"`

bool kk_std_core_is_white(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> bool */ 
  bool _match_19008 = (c == (' ')); /*bool*/;
  if (_match_19008) {
    return true;
  }
  {
    bool _match_19009 = (c == 0x0009); /*bool*/;
    if (_match_19009) {
      return true;
    }
    {
      bool _match_19010 = (c == 0x000A); /*bool*/;
      if (_match_19010) {
        return true;
      }
      {
        return (c == 0x000D);
      }
    }
  }
}
 
// Append `end` to each string in the list `xs` and join them all together.
// `join-end([],end) === ""`
// `join-end(["a","b"],"/") === "a/b/"`

kk_string_t kk_std_core_join_end(kk_std_core__list xs, kk_string_t end, kk_context_t* _ctx) { /* (xs : list<string>, end : string) -> string */ 
  if (kk_std_core__is_Nil(xs)) {
    kk_string_drop(end, _ctx);
    return kk_string_empty();
  }
  {
    kk_string_t _x20169;
    if (kk_std_core__is_Nil(xs)) {
      _x20169 = kk_string_empty(); /*string*/
    }
    else {
      struct kk_std_core_Cons* _con20171 = kk_std_core__as_Cons(xs);
      kk_box_t _box_x18427 = _con20171->head;
      kk_std_core__list xx = _con20171->tail;
      kk_string_t x = kk_string_unbox(_box_x18427);
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_string_t _x20173 = kk_string_dup(end); /*string*/
      _x20169 = kk_std_core__lift16737_joinsep(_x20173, xx, x, _ctx); /*string*/
    }
    return kk_std_core__lp__plus__plus__1_rp_(_x20169, end, _ctx);
  }
}
 
// Return the last element of a list (or `Nothing` for the empty list)

kk_std_core_types__maybe kk_std_core_last(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20174 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20174->head;
    kk_std_core__list _pat0 = _con20174->tail;
    if (kk_std_core__is_Nil(_pat0)) {
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_decref(xs, _ctx);
      }
      return kk_std_core_types__new_Just(x, _ctx);
    }
  }
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20175 = kk_std_core__as_Cons(xs);
    kk_box_t _pat2 = _con20175->head;
    kk_std_core__list xx = _con20175->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_pat2, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    { // tailcall
      xs = xx;
      goto kk__tailcall;
    }
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// Return the last element of a list (or `default` for the empty list)

kk_box_t kk_std_core_last_1(kk_std_core__list xs, kk_box_t default0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, default : a) -> a */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20176 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20176->head;
    kk_std_core__list _pat0 = _con20176->tail;
    if (kk_std_core__is_Nil(_pat0)) {
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_box_drop(default0, _ctx);
      return x;
    }
  }
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20177 = kk_std_core__as_Cons(xs);
    kk_box_t _pat2 = _con20177->head;
    kk_std_core__list xx = _con20177->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_pat2, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    { // tailcall
      xs = xx;
      goto kk__tailcall;
    }
  }
  {
    return default0;
  }
}
 
// O(`n`). The last `n` (default = `1`) characters in a string

kk_std_core__sslice kk_std_core_last_2(kk_string_t s, kk_std_core_types__optional n, kk_context_t* _ctx) { /* (s : string, n : optional<int>) -> sslice */ 
  kk_std_core__sslice slice0 = kk_std_core_last1(s, _ctx); /*sslice*/;
  bool _match_19007;
  kk_integer_t _x20178;
  if (kk_std_core_types__is_Optional(n)) {
    kk_box_t _box_x18428 = n._cons.Optional.value;
    kk_integer_t _n_13267 = kk_integer_unbox(_box_x18428);
    kk_integer_dup(_n_13267);
    _x20178 = _n_13267; /*int*/
    goto _match20179;
  }
  {
    _x20178 = kk_integer_from_small(1); /*int*/
  }
  _match20179: ;
  _match_19007 = kk_integer_eq(_x20178,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19007) {
    kk_std_core_types__optional_drop(n, _ctx);
    return slice0;
  }
  {
    kk_std_core__sslice _x20181;
    kk_integer_t _x20182;
    kk_integer_t _x20183;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18429 = n._cons.Optional.value;
      kk_integer_t _n_132670 = kk_integer_unbox(_box_x18429);
      kk_integer_dup(_n_132670);
      _x20183 = _n_132670; /*int*/
      goto _match20184;
    }
    {
      _x20183 = kk_integer_from_small(1); /*int*/
    }
    _match20184: ;
    _x20182 = kk_integer_sub((kk_integer_from_small(1)),_x20183,kk_context()); /*int*/
    _x20181 = kk_std_core_advance(slice0, _x20182, _ctx); /*sslice*/
    kk_integer_t _x20186;
    kk_integer_t _x20187;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18430 = n._cons.Optional.value;
      kk_integer_t _n_132671 = kk_integer_unbox(_box_x18430);
      kk_integer_dup(_n_132671);
      kk_std_core_types__optional_drop(n, _ctx);
      _x20187 = _n_132671; /*int*/
      goto _match20188;
    }
    {
      _x20187 = kk_integer_from_small(1); /*int*/
    }
    _match20188: ;
    _x20186 = kk_integer_sub(_x20187,(kk_integer_from_small(1)),kk_context()); /*int*/
    return kk_std_core_extend(_x20181, _x20186, _ctx);
  }
}
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)

kk_std_core__list kk_std_core__ctail_take(kk_std_core__list xs, kk_integer_t n, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20190 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20190->head;
    kk_std_core__list xx = _con20190->tail;
    kk_integer_t _x20191 = kk_integer_dup(n); /*int*/
    if (kk_integer_gt(_x20191,(kk_integer_from_small(0)),kk_context())) {
      kk_reuse_t _ru_18919 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        _ru_18919 = (kk_std_core__list_reuse(xs));
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
        _ru_18919 = kk_reuse_null;
      }
      kk_std_core__list _ctail_16833 = kk_std_core__list_hole(); /*list<13391>*/;
      kk_std_core__list _ctail_16834;
      if (kk_likely(_ru_18919!=NULL)) {
        struct kk_std_core_Cons* _con20192 = (struct kk_std_core_Cons*)_ru_18919;
        _con20192->tail = _ctail_16833;
        _ctail_16834 = kk_std_core__base_Cons(_con20192); /*list<13391>*/
      }
      else {
        _ctail_16834 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16833, _ctx); /*list<13391>*/
      }
      { // tailcall
        kk_integer_t _x20193 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        kk_std_core_types__ctail _x20194;
        kk_box_t* _b_18441_18436 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16834)->tail)); /*cfield<list<13391>>*/;
        _x20194 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16834, _ctx)),_b_18441_18436); /*ctail<0>*/
        xs = xx;
        n = _x20193;
        _acc = _x20194;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_integer_drop(n, _ctx);
    kk_std_core__list_drop(xs, _ctx);
    kk_box_t _x20195 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20195, _ctx);
  }
}
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)

kk_std_core__list kk_std_core_take(kk_std_core__list xs0, kk_integer_t n0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> list<a> */ 
  kk_std_core_types__ctail _x20196 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_take(xs0, n0, _x20196, _ctx);
}
 
// split a list at position `n`

kk_std_core_types__tuple2_ kk_std_core_split(kk_std_core__list xs, kk_integer_t n, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> (list<a>, list<a>) */ 
  kk_std_core__list _b_18449_18447;
  kk_std_core__list _x20197 = kk_std_core__list_dup(xs); /*list<13411>*/
  kk_integer_t _x20198 = kk_integer_dup(n); /*int*/
  _b_18449_18447 = kk_std_core_take(_x20197, _x20198, _ctx); /*list<13411>*/
  kk_std_core__list _b_18450_18448 = kk_std_core_drop(xs, n, _ctx); /*list<13411>*/;
  return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18449_18447, _ctx), kk_std_core__list_box(_b_18450_18448, _ctx), _ctx);
}
 
// Lookup the first element satisfying some predicate


// lift anonymous function
struct kk_std_core_lookup_fun20202__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_lookup_fun20202(kk_function_t _fself, kk_box_t _b_18453, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_lookup_fun20202(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_lookup_fun20202__t* _self = kk_function_alloc_as(struct kk_std_core_lookup_fun20202__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_lookup_fun20202, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_lookup_fun20202(kk_function_t _fself, kk_box_t _b_18453, kk_context_t* _ctx) {
  struct kk_std_core_lookup_fun20202__t* _self = kk_function_as(struct kk_std_core_lookup_fun20202__t*, _fself);
  kk_function_t pred = _self->pred; /* (13725) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  bool _match_19004;
  kk_box_t _x20203;
  kk_std_core_types__tuple2_ _match_19006;
  kk_box_t _x20204 = kk_box_dup(_b_18453); /*10243*/
  _match_19006 = kk_std_core_types__tuple2__unbox(_x20204, _ctx); /*(13725, 13726)*/
  {
    kk_box_t _x = _match_19006.fst;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(_match_19006, _ctx);
    _x20203 = _x; /*13725*/
  }
  _match_19004 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), pred, (pred, _x20203, _ctx)); /*bool*/
  if (_match_19004) {
    kk_box_t _x20205;
    kk_std_core_types__tuple2_ _match_19005 = kk_std_core_types__tuple2__unbox(_b_18453, _ctx); /*(13725, 13726)*/;
    {
      kk_box_t _x0 = _match_19005.snd;
      kk_box_dup(_x0);
      kk_std_core_types__tuple2__drop(_match_19005, _ctx);
      _x20205 = _x0; /*13726*/
    }
    return kk_std_core_types__new_Just(_x20205, _ctx);
  }
  {
    kk_box_drop(_b_18453, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_core_lookup(kk_std_core__list xs, kk_function_t pred, kk_context_t* _ctx) { /* forall<a,b> (xs : list<(a, b)>, pred : (a) -> bool) -> maybe<b> */ 
  return kk_std_core_foreach_while(xs, kk_std_core_new_lookup_fun20202(pred, _ctx), _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17186_op(kk_box_t _y_17051, kk_std_core__list _y_17052, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17051, _y_17052, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17187_op_fun20207__t {
  struct kk_function_s _base;
  kk_box_t _y_170510;
};
static kk_box_t kk_std_core__mlift17187_op_fun20207(kk_function_t _fself, kk_box_t _b_18458, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17187_op_fun20207(kk_box_t _y_170510, kk_context_t* _ctx) {
  struct kk_std_core__mlift17187_op_fun20207__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17187_op_fun20207__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17187_op_fun20207, kk_context());
  _self->_y_170510 = _y_170510;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17187_op_fun20207(kk_function_t _fself, kk_box_t _b_18458, kk_context_t* _ctx) {
  struct kk_std_core__mlift17187_op_fun20207__t* _self = kk_function_as(struct kk_std_core__mlift17187_op_fun20207__t*, _fself);
  kk_box_t _y_170510 = _self->_y_170510; /* 13781 */
  kk_drop_match(_self, {kk_box_dup(_y_170510);}, {}, _ctx)
  kk_std_core__list _x20208;
  kk_std_core__list _x20209 = kk_std_core__list_unbox(_b_18458, _ctx); /*list<13781>*/
  _x20208 = kk_std_core__mlift17186_op(_y_170510, _x20209, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20208, _ctx);
}

kk_std_core__list kk_std_core__mlift17187_op(kk_function_t f, kk_integer_t i, kk_std_core__list yy, kk_box_t _y_170510, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a) -> e b, i : int, yy : list<a>, b) -> e list<b> */ 
  kk_integer_t i0_16781 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17470 = kk_std_core__lift16750_map_indexed(f, yy, i0_16781, _ctx); /*list<13781>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17470, _ctx);
    kk_box_t _x20206 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17187_op_fun20207(_y_170510, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20206, _ctx);
  }
  {
    return kk_std_core__mlift17186_op(_y_170510, x_17470, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16750_map_indexed_fun20214__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16750_map_indexed_fun20214(kk_function_t _fself, kk_box_t _b_18462, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16750_map_indexed_fun20214(kk_function_t f0, kk_integer_t i0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20214__t* _self = kk_function_alloc_as(struct kk_std_core__lift16750_map_indexed_fun20214__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16750_map_indexed_fun20214, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16750_map_indexed_fun20214(kk_function_t _fself, kk_box_t _b_18462, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20214__t* _self = kk_function_as(struct kk_std_core__lift16750_map_indexed_fun20214__t*, _fself);
  kk_function_t f0 = _self->f0; /* (idx : int, value : 13780) -> 13782 13781 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list yy0 = _self->yy0; /* list<13780> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20215 = kk_std_core__mlift17187_op(f0, i0, yy0, _b_18462, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20215, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16750_map_indexed_fun20217__t {
  struct kk_function_s _base;
  kk_box_t x0_17472;
};
static kk_box_t kk_std_core__lift16750_map_indexed_fun20217(kk_function_t _fself, kk_box_t _b_18464, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16750_map_indexed_fun20217(kk_box_t x0_17472, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20217__t* _self = kk_function_alloc_as(struct kk_std_core__lift16750_map_indexed_fun20217__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16750_map_indexed_fun20217, kk_context());
  _self->x0_17472 = x0_17472;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16750_map_indexed_fun20217(kk_function_t _fself, kk_box_t _b_18464, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20217__t* _self = kk_function_as(struct kk_std_core__lift16750_map_indexed_fun20217__t*, _fself);
  kk_box_t x0_17472 = _self->x0_17472; /* 13781 */
  kk_drop_match(_self, {kk_box_dup(x0_17472);}, {}, _ctx)
  kk_std_core__list _x20218;
  kk_std_core__list _x20219 = kk_std_core__list_unbox(_b_18464, _ctx); /*list<13781>*/
  _x20218 = kk_std_core__mlift17186_op(x0_17472, _x20219, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20218, _ctx);
}

kk_std_core__list kk_std_core__lift16750_map_indexed(kk_function_t f0, kk_std_core__list ys, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a) -> e b, ys : list<a>, i : int) -> e list<b> */ 
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20210 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con20210->head;
    kk_std_core__list yy0 = _con20210->tail;
    kk_reuse_t _ru_18920 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18920 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18920 = kk_reuse_null;
    }
    kk_box_t x0_17472;
    kk_function_t _x20212 = kk_function_dup(f0); /*(idx : int, value : 13780) -> 13782 13781*/
    kk_integer_t _x20211 = kk_integer_dup(i0); /*int*/
    x0_17472 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), _x20212, (_x20212, _x20211, y, _ctx)); /*13781*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18920, _ctx);
      kk_box_drop(x0_17472, _ctx);
      kk_box_t _x20213 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16750_map_indexed_fun20214(f0, i0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20213, _ctx);
    }
    {
      kk_integer_t i0_167810 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_std_core__list x1_17475 = kk_std_core__lift16750_map_indexed(f0, yy0, i0_167810, _ctx); /*list<13781>*/;
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18920, _ctx);
        kk_std_core__list_drop(x1_17475, _ctx);
        kk_box_t _x20216 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16750_map_indexed_fun20217(x0_17472, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20216, _ctx);
      }
      {
        return kk_std_core__new_Cons(_ru_18920, x0_17472, x1_17475, _ctx);
      }
    }
  }
  {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(i0, _ctx);
    return kk_std_core__new_Nil(_ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17188_op(kk_box_t _y_17055, kk_std_core__list _y_17056, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17055, _y_17056, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17189_op_fun20221__t {
  struct kk_function_s _base;
  kk_box_t _y_170550;
};
static kk_box_t kk_std_core__mlift17189_op_fun20221(kk_function_t _fself, kk_box_t _b_18470, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17189_op_fun20221(kk_box_t _y_170550, kk_context_t* _ctx) {
  struct kk_std_core__mlift17189_op_fun20221__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17189_op_fun20221__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17189_op_fun20221, kk_context());
  _self->_y_170550 = _y_170550;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17189_op_fun20221(kk_function_t _fself, kk_box_t _b_18470, kk_context_t* _ctx) {
  struct kk_std_core__mlift17189_op_fun20221__t* _self = kk_function_as(struct kk_std_core__mlift17189_op_fun20221__t*, _fself);
  kk_box_t _y_170550 = _self->_y_170550; /* 13838 */
  kk_drop_match(_self, {kk_box_dup(_y_170550);}, {}, _ctx)
  kk_std_core__list _x20222;
  kk_std_core__list _x20223 = kk_std_core__list_unbox(_b_18470, _ctx); /*list<13838>*/
  _x20222 = kk_std_core__mlift17188_op(_y_170550, _x20223, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20222, _ctx);
}

kk_std_core__list kk_std_core__mlift17189_op(kk_function_t f, kk_integer_t i, kk_std_core__list yy, kk_box_t _y_170550, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, i : int, yy : list<a>, b) -> e list<b> */ 
  kk_integer_t i0_16783 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17478 = kk_std_core__lift16751_map_indexed_peek(f, yy, i0_16783, _ctx); /*list<13838>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17478, _ctx);
    kk_box_t _x20220 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17189_op_fun20221(_y_170550, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20220, _ctx);
  }
  {
    return kk_std_core__mlift17188_op(_y_170550, x_17478, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16751_map_indexed_peek_fun20229__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20229(kk_function_t _fself, kk_box_t _b_18474, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16751_map_indexed_peek_fun20229(kk_function_t f0, kk_integer_t i0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20229__t* _self = kk_function_alloc_as(struct kk_std_core__lift16751_map_indexed_peek_fun20229__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16751_map_indexed_peek_fun20229, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20229(kk_function_t _fself, kk_box_t _b_18474, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20229__t* _self = kk_function_as(struct kk_std_core__lift16751_map_indexed_peek_fun20229__t*, _fself);
  kk_function_t f0 = _self->f0; /* (idx : int, value : 13837, rest : list<13837>) -> 13839 13838 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list yy0 = _self->yy0; /* list<13837> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20230 = kk_std_core__mlift17189_op(f0, i0, yy0, _b_18474, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20230, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16751_map_indexed_peek_fun20232__t {
  struct kk_function_s _base;
  kk_box_t x0_17480;
};
static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20232(kk_function_t _fself, kk_box_t _b_18476, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16751_map_indexed_peek_fun20232(kk_box_t x0_17480, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20232__t* _self = kk_function_alloc_as(struct kk_std_core__lift16751_map_indexed_peek_fun20232__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16751_map_indexed_peek_fun20232, kk_context());
  _self->x0_17480 = x0_17480;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20232(kk_function_t _fself, kk_box_t _b_18476, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20232__t* _self = kk_function_as(struct kk_std_core__lift16751_map_indexed_peek_fun20232__t*, _fself);
  kk_box_t x0_17480 = _self->x0_17480; /* 13838 */
  kk_drop_match(_self, {kk_box_dup(x0_17480);}, {}, _ctx)
  kk_std_core__list _x20233;
  kk_std_core__list _x20234 = kk_std_core__list_unbox(_b_18476, _ctx); /*list<13838>*/
  _x20233 = kk_std_core__mlift17188_op(x0_17480, _x20234, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20233, _ctx);
}

kk_std_core__list kk_std_core__lift16751_map_indexed_peek(kk_function_t f0, kk_std_core__list ys, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, ys : list<a>, i : int) -> e list<b> */ 
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20224 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con20224->head;
    kk_std_core__list yy0 = _con20224->tail;
    kk_reuse_t _ru_18921 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18921 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18921 = kk_reuse_null;
    }
    kk_box_t x0_17480;
    kk_function_t _x20227 = kk_function_dup(f0); /*(idx : int, value : 13837, rest : list<13837>) -> 13839 13838*/
    kk_integer_t _x20225 = kk_integer_dup(i0); /*int*/
    kk_std_core__list _x20226 = kk_std_core__list_dup(yy0); /*list<13837>*/
    x0_17480 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_std_core__list, kk_context_t*), _x20227, (_x20227, _x20225, y, _x20226, _ctx)); /*13838*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18921, _ctx);
      kk_box_drop(x0_17480, _ctx);
      kk_box_t _x20228 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16751_map_indexed_peek_fun20229(f0, i0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20228, _ctx);
    }
    {
      kk_integer_t i0_167830 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_std_core__list x1_17483 = kk_std_core__lift16751_map_indexed_peek(f0, yy0, i0_167830, _ctx); /*list<13838>*/;
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18921, _ctx);
        kk_std_core__list_drop(x1_17483, _ctx);
        kk_box_t _x20231 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16751_map_indexed_peek_fun20232(x0_17480, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20231, _ctx);
      }
      {
        return kk_std_core__new_Cons(_ru_18921, x0_17480, x1_17483, _ctx);
      }
    }
  }
  {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(i0, _ctx);
    return kk_std_core__new_Nil(_ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17190_op(kk_std_core_types__ctail _acc, kk_function_t action, kk_std_core__list xx, kk_std_core_types__maybe _y_17059, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17059)) {
    kk_box_t y = _y_17059._cons.Just.value;
    kk_std_core__list _ctail_16835 = kk_std_core__list_hole(); /*list<13880>*/;
    kk_std_core__list _ctail_16836 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16835, _ctx); /*list<13880>*/;
    kk_std_core_types__ctail _x20235;
    kk_box_t* _b_18491_18486 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16836)->tail)); /*cfield<list<13880>>*/;
    _x20235 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16836, _ctx)),_b_18491_18486); /*ctail<0>*/
    return kk_std_core__ctail_map_while(xx, action, _x20235, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    kk_box_t _x20236 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20236, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17191_op_fun20237__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17191_op_fun20237(kk_function_t _fself, kk_std_core__list _ctail_16838, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17191_op_fun20237(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17191_op_fun20237__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17191_op_fun20237__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17191_op_fun20237, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17191_op_fun20237(kk_function_t _fself, kk_std_core__list _ctail_16838, kk_context_t* _ctx) {
  struct kk_std_core__mlift17191_op_fun20237__t* _self = kk_function_as(struct kk_std_core__mlift17191_op_fun20237__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<13880>) -> list<13880> */
  kk_box_t y0 = _self->y0; /* 13880 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20238 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16838, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20238, _ctx));
}

kk_std_core__list kk_std_core__mlift17191_op(kk_function_t _accm, kk_function_t action0, kk_std_core__list xx0, kk_std_core_types__maybe _y_17063, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17063)) {
    kk_box_t y0 = _y_17063._cons.Just.value;
    return kk_std_core__ctailm_map_while(xx0, action0, kk_std_core__new_mlift17191_op_fun20237(_accm, y0, _ctx), _ctx);
  }
  {
    kk_function_drop(action0, _ctx);
    kk_std_core__list_drop(xx0, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Invoke `action` on each element of a list while `action` returns `Just`


// lift anonymous function
struct kk_std_core__ctail_map_while_fun20243__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_map_while_fun20243(kk_function_t _fself, kk_box_t _b_18500, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_map_while_fun20243(kk_function_t action1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_while_fun20243__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_map_while_fun20243__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_map_while_fun20243, kk_context());
  _self->action1 = action1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_map_while_fun20243(kk_function_t _fself, kk_box_t _b_18500, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_while_fun20243__t* _self = kk_function_as(struct kk_std_core__ctail_map_while_fun20243__t*, _fself);
  kk_function_t action1 = _self->action1; /* (13879) -> 13881 maybe<13880> */
  kk_std_core__list xx1 = _self->xx1; /* list<13879> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<13880>> */
  kk_drop_match(_self, {kk_function_dup(action1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20244;
  kk_std_core_types__maybe _x20245 = kk_std_core_types__maybe_unbox(_b_18500, _ctx); /*maybe<13880>*/
  _x20244 = kk_std_core__mlift17190_op(_acc0, action1, xx1, _x20245, _ctx); /*list<13880>*/
  return kk_std_core__list_box(_x20244, _ctx);
}

kk_std_core__list kk_std_core__ctail_map_while(kk_std_core__list xs, kk_function_t action1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action1, _ctx);
    kk_box_t _x20239 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20239, _ctx);
  }
  {
    struct kk_std_core_Cons* _con20240 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20240->head;
    kk_std_core__list xx1 = _con20240->tail;
    kk_reuse_t _ru_18922 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18922 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18922 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17486;
    kk_function_t _x20241 = kk_function_dup(action1); /*(13879) -> 13881 maybe<13880>*/
    x0_17486 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20241, (_x20241, x, _ctx)); /*maybe<13880>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18922, _ctx);
      kk_std_core_types__maybe_drop(x0_17486, _ctx);
      kk_box_t _x20242 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_map_while_fun20243(action1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20242, _ctx);
    }
    if (kk_std_core_types__is_Just(x0_17486)) {
      kk_box_t y1 = x0_17486._cons.Just.value;
      kk_std_core__list _ctail_168350 = kk_std_core__list_hole(); /*list<13880>*/;
      kk_std_core__list _ctail_168360 = kk_std_core__new_Cons(_ru_18922, y1, _ctail_168350, _ctx); /*list<13880>*/;
      { // tailcall
        kk_std_core_types__ctail _x20246;
        kk_box_t* _b_18514_18506 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168360)->tail)); /*cfield<list<13880>>*/;
        _x20246 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168360, _ctx)),_b_18514_18506); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20246;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18922, _ctx);
      kk_function_drop(action1, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20247 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20247, _ctx);
    }
  }
}
 
// Invoke `action` on each element of a list while `action` returns `Just`


// lift anonymous function
struct kk_std_core__ctailm_map_while_fun20251__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t action2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_map_while_fun20251(kk_function_t _fself, kk_box_t _b_18522, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_while_fun20251(kk_function_t _accm0, kk_function_t action2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20251__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_while_fun20251__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_while_fun20251, kk_context());
  _self->_accm0 = _accm0;
  _self->action2 = action2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_map_while_fun20251(kk_function_t _fself, kk_box_t _b_18522, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20251__t* _self = kk_function_as(struct kk_std_core__ctailm_map_while_fun20251__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<13880>) -> list<13880> */
  kk_function_t action2 = _self->action2; /* (13879) -> 13881 maybe<13880> */
  kk_std_core__list xx2 = _self->xx2; /* list<13879> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(action2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20252;
  kk_std_core_types__maybe _x20253 = kk_std_core_types__maybe_unbox(_b_18522, _ctx); /*maybe<13880>*/
  _x20252 = kk_std_core__mlift17191_op(_accm0, action2, xx2, _x20253, _ctx); /*list<13880>*/
  return kk_std_core__list_box(_x20252, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_map_while_fun20255__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_map_while_fun20255(kk_function_t _fself, kk_std_core__list _ctail_168380, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_while_fun20255(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20255__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_while_fun20255__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_while_fun20255, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_map_while_fun20255(kk_function_t _fself, kk_std_core__list _ctail_168380, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20255__t* _self = kk_function_as(struct kk_std_core__ctailm_map_while_fun20255__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<13880>) -> list<13880> */
  kk_box_t y2 = _self->y2; /* 13880 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20256 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168380, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20256, _ctx));
}

kk_std_core__list kk_std_core__ctailm_map_while(kk_std_core__list xs0, kk_function_t action2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs0)) {
    kk_function_drop(action2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
  {
    struct kk_std_core_Cons* _con20248 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20248->head;
    kk_std_core__list xx2 = _con20248->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17489;
    kk_function_t _x20249 = kk_function_dup(action2); /*(13879) -> 13881 maybe<13880>*/
    x2_17489 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20249, (_x20249, x1, _ctx)); /*maybe<13880>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17489, _ctx);
      kk_box_t _x20250 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_map_while_fun20251(_accm0, action2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20250, _ctx);
    }
    if (kk_std_core_types__is_Just(x2_17489)) {
      kk_box_t y2 = x2_17489._cons.Just.value;
      { // tailcall
        kk_function_t _x20254 = kk_std_core__new_ctailm_map_while_fun20255(_accm0, y2, _ctx); /*(list<13880>) -> list<13880>*/
        xs0 = xx2;
        _accm0 = _x20254;
        goto kk__tailcall;
      }
    }
    {
      kk_function_drop(action2, _ctx);
      kk_std_core__list_drop(xx2, _ctx);
      return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
    }
  }
}
 
// Invoke `action` on each element of a list while `action` returns `Just`


// lift anonymous function
struct kk_std_core_map_while_fun20258__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_map_while_fun20258(kk_function_t _fself, kk_std_core__list _ctail_16837, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_while_fun20258(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_while_fun20258, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_map_while_fun20258(kk_function_t _fself, kk_std_core__list _ctail_16837, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16837;
}

kk_std_core__list kk_std_core_map_while(kk_std_core__list xs1, kk_function_t action3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_18995 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_18995) {
    kk_std_core_types__ctail _x20257 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_map_while(xs1, action3, _x20257, _ctx);
  }
  {
    return kk_std_core__ctailm_map_while(xs1, action3, kk_std_core_new_map_while_fun20258(_ctx), _ctx);
  }
}
 
// Return the maximum of two integers

kk_integer_t kk_std_core_max(kk_integer_t i, kk_integer_t j, kk_context_t* _ctx) { /* (i : int, j : int) -> int */ 
  bool _match_18994;
  kk_integer_t _x20259 = kk_integer_dup(i); /*int*/
  kk_integer_t _x20260 = kk_integer_dup(j); /*int*/
  _match_18994 = kk_integer_gte(_x20259,_x20260,kk_context()); /*bool*/
  if (_match_18994) {
    kk_integer_drop(j, _ctx);
    return i;
  }
  {
    kk_integer_drop(i, _ctx);
    return j;
  }
}
 
// Returns the largest element of a list of integers (or `default` (=`0`) for the empty list)


// lift anonymous function
struct kk_std_core_maximum_fun20265__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_maximum_fun20265(kk_function_t _fself, kk_box_t _b_18530, kk_box_t _b_18531, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_maximum_fun20265(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_maximum_fun20265, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_maximum_fun20265(kk_function_t _fself, kk_box_t _b_18530, kk_box_t _b_18531, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20266;
  kk_integer_t _x20267 = kk_integer_unbox(_b_18530); /*int*/
  kk_integer_t _x20268 = kk_integer_unbox(_b_18531); /*int*/
  _x20266 = kk_std_core_max(_x20267, _x20268, _ctx); /*int*/
  return kk_integer_box(_x20266);
}

kk_integer_t kk_std_core_maximum(kk_std_core__list xs, kk_std_core_types__optional default0, kk_context_t* _ctx) { /* (xs : list<int>, default : optional<int>) -> int */ 
  if (kk_std_core__is_Nil(xs)) {
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18525 = default0._cons.Optional.value;
      kk_integer_t _default_13929 = kk_integer_unbox(_box_x18525);
      kk_integer_dup(_default_13929);
      kk_std_core_types__optional_drop(default0, _ctx);
      return _default_13929;
    }
    {
      return kk_integer_from_small(0);
    }
  }
  {
    struct kk_std_core_Cons* _con20262 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18526 = _con20262->head;
    kk_std_core__list xx = _con20262->tail;
    kk_integer_t x = kk_integer_unbox(_box_x18526);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_integer_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__optional_drop(default0, _ctx);
    kk_box_t _x20264 = kk_std_core_foldl(xx, kk_integer_box(x), kk_std_core_new_maximum_fun20265(_ctx), _ctx); /*11906*/
    return kk_integer_unbox(_x20264);
  }
}
 
// Returns the largest element of a list of doubles (or `0` for the empty list)


// lift anonymous function
struct kk_std_core_maximum_fun20272__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_maximum_fun20272_1(kk_function_t _fself, kk_box_t _b_18539, kk_box_t _b_18540, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_maximum_fun20272_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_maximum_fun20272_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_maximum_fun20272_1(kk_function_t _fself, kk_box_t _b_18539, kk_box_t _b_18540, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  double _x20273;
  double _x20274 = kk_double_unbox(_b_18539, _ctx); /*double*/
  double _x20275 = kk_double_unbox(_b_18540, _ctx); /*double*/
  _x20273 = kk_std_core_max_1(_x20274, _x20275, _ctx); /*double*/
  return kk_double_box(_x20273, _ctx);
}

double kk_std_core_maximum_1(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<double>) -> double */ 
  if (kk_std_core__is_Nil(xs)) {
    return 0.0;
  }
  {
    struct kk_std_core_Cons* _con20269 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18535 = _con20269->head;
    kk_std_core__list xx = _con20269->tail;
    double x = kk_double_unbox(_box_x18535, NULL);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_box_x18535, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t _x20271 = kk_std_core_foldl(xx, kk_double_box(x, _ctx), kk_std_core_new_maximum_fun20272_1(_ctx), _ctx); /*11906*/
    return kk_double_unbox(_x20271, _ctx);
  }
}
 
// Convert a list to a `:maybe` type, using `Nothing` for an empty list, and otherwise `Just` on the head element.
// Note: this is just `head`.

kk_std_core_types__maybe kk_std_core_maybe_3(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  if (kk_std_core__is_Nil(xs)) {
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    struct kk_std_core_Cons* _con20276 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20276->head;
    kk_std_core__list _pat1 = _con20276->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_drop(_pat1, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core_types__new_Just(x, _ctx);
  }
}
 
// Transform an integer to a maybe type, using `Nothing` for `0`

kk_std_core_types__maybe kk_std_core_maybe_5(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> maybe<int> */ 
  bool _match_18992;
  kk_integer_t _x20277 = kk_integer_dup(i); /*int*/
  _match_18992 = kk_integer_eq(_x20277,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_18992) {
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    return kk_std_core_types__new_Just(kk_integer_box(i), _ctx);
  }
}
 
// Transform a string to a maybe type, using `Nothing` for an empty string

kk_std_core_types__maybe kk_std_core_maybe_6(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> maybe<string> */ 
  bool _match_18991;
  kk_string_t _x20278 = kk_string_dup(s); /*string*/
  kk_string_t _x20279 = kk_string_empty(); /*string*/
  _match_18991 = kk_string_is_eq(_x20278,_x20279,kk_context()); /*bool*/
  if (_match_18991) {
    kk_string_drop(s, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    return kk_std_core_types__new_Just(kk_string_box(s), _ctx);
  }
}
 
// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).

kk_std_core_types__maybe kk_std_core_maybe_8(kk_std_core__null n, kk_context_t* _ctx) { /* forall<a> (n : null<a>) -> maybe<a> */ 
  return (kk_datatype_as_ptr(n) == NULL ? kk_std_core_types__new_Nothing(kk_context()) : kk_std_core_types__new_Just(kk_datatype_box(n),kk_context()));
}
 
// Return the minimum of two integers

kk_integer_t kk_std_core_min(kk_integer_t i, kk_integer_t j, kk_context_t* _ctx) { /* (i : int, j : int) -> int */ 
  bool _match_18990;
  kk_integer_t _x20281 = kk_integer_dup(i); /*int*/
  kk_integer_t _x20282 = kk_integer_dup(j); /*int*/
  _match_18990 = kk_integer_lte(_x20281,_x20282,kk_context()); /*bool*/
  if (_match_18990) {
    kk_integer_drop(j, _ctx);
    return i;
  }
  {
    kk_integer_drop(i, _ctx);
    return j;
  }
}
 
// Returns the smallest element of a list of integers (or `default` (=`0`) for the empty list)


// lift anonymous function
struct kk_std_core_minimum_fun20287__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_minimum_fun20287(kk_function_t _fself, kk_box_t _b_18555, kk_box_t _b_18556, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_minimum_fun20287(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_minimum_fun20287, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_minimum_fun20287(kk_function_t _fself, kk_box_t _b_18555, kk_box_t _b_18556, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20288;
  kk_integer_t _x20289 = kk_integer_unbox(_b_18555); /*int*/
  kk_integer_t _x20290 = kk_integer_unbox(_b_18556); /*int*/
  _x20288 = kk_std_core_min(_x20289, _x20290, _ctx); /*int*/
  return kk_integer_box(_x20288);
}

kk_integer_t kk_std_core_minimum(kk_std_core__list xs, kk_std_core_types__optional default0, kk_context_t* _ctx) { /* (xs : list<int>, default : optional<int>) -> int */ 
  if (kk_std_core__is_Nil(xs)) {
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18550 = default0._cons.Optional.value;
      kk_integer_t _default_14408 = kk_integer_unbox(_box_x18550);
      kk_integer_dup(_default_14408);
      kk_std_core_types__optional_drop(default0, _ctx);
      return _default_14408;
    }
    {
      return kk_integer_from_small(0);
    }
  }
  {
    struct kk_std_core_Cons* _con20284 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18551 = _con20284->head;
    kk_std_core__list xx = _con20284->tail;
    kk_integer_t x = kk_integer_unbox(_box_x18551);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_integer_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__optional_drop(default0, _ctx);
    kk_box_t _x20286 = kk_std_core_foldl(xx, kk_integer_box(x), kk_std_core_new_minimum_fun20287(_ctx), _ctx); /*11906*/
    return kk_integer_unbox(_x20286);
  }
}
 
// Returns the smallest element of a list of doubles (or `0` for the empty list)


// lift anonymous function
struct kk_std_core_minimum_fun20294__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_minimum_fun20294_1(kk_function_t _fself, kk_box_t _b_18564, kk_box_t _b_18565, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_minimum_fun20294_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_minimum_fun20294_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_minimum_fun20294_1(kk_function_t _fself, kk_box_t _b_18564, kk_box_t _b_18565, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  double _x20295;
  double _x20296 = kk_double_unbox(_b_18564, _ctx); /*double*/
  double _x20297 = kk_double_unbox(_b_18565, _ctx); /*double*/
  _x20295 = kk_std_core_min_1(_x20296, _x20297, _ctx); /*double*/
  return kk_double_box(_x20295, _ctx);
}

double kk_std_core_minimum_1(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<double>) -> double */ 
  if (kk_std_core__is_Nil(xs)) {
    return 0.0;
  }
  {
    struct kk_std_core_Cons* _con20291 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18560 = _con20291->head;
    kk_std_core__list xx = _con20291->tail;
    double x = kk_double_unbox(_box_x18560, NULL);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_box_x18560, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t _x20293 = kk_std_core_foldl(xx, kk_double_box(x, _ctx), kk_std_core_new_minimum_fun20294_1(_ctx), _ctx); /*11906*/
    return kk_double_unbox(_x20293, _ctx);
  }
}
 
// Disable tracing completely.

kk_unit_t kk_std_core_notrace(kk_context_t* _ctx) { /* () -> (st<global>) () */ 
  kk_ref_t _x20298 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  kk_ref_set(_x20298,(kk_bool_box(false)),kk_context()); return kk_Unit;
}
 
// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).

kk_std_core__null kk_std_core_null(kk_std_core_types__maybe x, kk_context_t* _ctx) { /* forall<a> (x : maybe<a>) -> null<a> */ 
  return (kk_std_core_types__is_Nothing(x) ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox((x)._cons.Just.value));
}
 
// Cast a integer that is zero to a null

kk_std_core__null kk_std_core_null_1(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> null<int> */ 
  kk_std_core_types__maybe _x20299;
  bool _match_18988;
  kk_integer_t _x20300 = kk_integer_dup(i); /*int*/
  _match_18988 = kk_integer_eq(_x20300,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_18988) {
    kk_integer_drop(i, _ctx);
    _x20299 = kk_std_core_types__new_Nothing(_ctx); /*forall<a> maybe<a>*/
  }
  else {
    _x20299 = kk_std_core_types__new_Just(kk_integer_box(i), _ctx); /*forall<a> maybe<a>*/
  }
  return kk_std_core_null(_x20299, _ctx);
}
 
// Cast an empty string a null

kk_std_core__null kk_std_core_null_2(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> null<string> */ 
  kk_std_core_types__maybe _x20301;
  bool _match_18987;
  kk_string_t _x20302 = kk_string_dup(s); /*string*/
  kk_string_t _x20303 = kk_string_empty(); /*string*/
  _match_18987 = kk_string_is_eq(_x20302,_x20303,kk_context()); /*bool*/
  if (_match_18987) {
    kk_string_drop(s, _ctx);
    _x20301 = kk_std_core_types__new_Nothing(_ctx); /*forall<a> maybe<a>*/
  }
  else {
    _x20301 = kk_std_core_types__new_Just(kk_string_box(s), _ctx); /*forall<a> maybe<a>*/
  }
  return kk_std_core_null(_x20301, _ctx);
}
 
// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.

kk_string_t kk_std_core_pad_right(kk_string_t s, kk_integer_t width, kk_std_core_types__optional fill, kk_context_t* _ctx) { /* (s : string, width : int, fill : optional<char>) -> string */ 
  kk_ssize_t w = kk_std_core_ssize__t(width, _ctx); /*ssize_t*/;
  kk_ssize_t n;
  kk_string_t _x20306 = kk_string_dup(s); /*string*/
  n = kk_string_len(_x20306,kk_context()); /*ssize_t*/
  bool _match_18986 = (w <= n); /*bool*/;
  if (_match_18986) {
    kk_std_core_types__optional_drop(fill, _ctx);
    return s;
  }
  {
    kk_string_t _x20307;
    kk_string_t _x20308;
    kk_char_t _x20309;
    if (kk_std_core_types__is_Optional(fill)) {
      kk_box_t _box_x18579 = fill._cons.Optional.value;
      kk_char_t _fill_14810 = kk_char_unbox(_box_x18579, NULL);
      kk_std_core_types__optional_drop(fill, _ctx);
      _x20309 = _fill_14810; /*char*/
      goto _match20310;
    }
    {
      _x20309 = ' '; /*char*/
    }
    _match20310: ;
    _x20308 = kk_std_core_string(_x20309, _ctx); /*string*/
    kk_ssize_t _x20312 = (w - n); /*ssize_t*/
    _x20307 = kk_std_core_repeatz(_x20308, _x20312, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(s, _x20307, _ctx);
  }
}
 
// Is `pre`  a prefix of `s`? If so, returns a slice
// of `s` following `pre` up to the end of `s`.

kk_std_core_types__maybe kk_std_core_starts_with(kk_string_t s, kk_string_t pre, kk_context_t* _ctx) { /* (s : string, pre : string) -> maybe<sslice> */ 
  bool _match_18985;
  kk_string_t _x20313 = kk_string_dup(s); /*string*/
  kk_string_t _x20314 = kk_string_dup(pre); /*string*/
  _match_18985 = kk_string_starts_with(_x20313,_x20314,kk_context()); /*bool*/
  if (_match_18985) {
    kk_std_core__sslice _b_18581_18580;
    kk_string_t _x20315 = kk_string_dup(s); /*string*/
    kk_ssize_t _x20316;
    kk_string_t _x20317 = kk_string_dup(pre); /*string*/
    _x20316 = kk_string_len(_x20317,kk_context()); /*ssize_t*/
    kk_ssize_t _x20318;
    kk_ssize_t _x20319 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x20320 = kk_string_len(pre,kk_context()); /*ssize_t*/
    _x20318 = (_x20319 - _x20320); /*ssize_t*/
    _b_18581_18580 = kk_std_core__new_Sslice(_x20315, _x20316, _x20318, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18581_18580, _ctx), _ctx);
  }
  {
    kk_string_drop(pre, _ctx);
    kk_string_drop(s, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// Trim off a substring `sub` while `s` starts with that string.

kk_string_t kk_std_core_trim_left_1(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> string */ 
  kk__tailcall: ;
  bool _match_18983;
  kk_string_t _x20321 = kk_string_dup(sub); /*string*/
  kk_string_t _x20322 = kk_string_empty(); /*string*/
  _match_18983 = kk_string_is_eq(_x20321,_x20322,kk_context()); /*bool*/
  if (_match_18983) {
    kk_string_drop(sub, _ctx);
    return s;
  }
  {
    kk_std_core_types__maybe _match_18984;
    kk_string_t _x20324 = kk_string_dup(s); /*string*/
    kk_string_t _x20325 = kk_string_dup(sub); /*string*/
    _match_18984 = kk_std_core_starts_with(_x20324, _x20325, _ctx); /*maybe<sslice>*/
    if (kk_std_core_types__is_Just(_match_18984)) {
      kk_box_t _box_x18582 = _match_18984._cons.Just.value;
      kk_std_core__sslice slice0 = kk_std_core__sslice_unbox(_box_x18582, NULL);
      kk_string_drop(s, _ctx);
      { // tailcall
        kk_string_t _x20327 = kk_std_core_string_3(slice0, _ctx); /*string*/
        s = _x20327;
        goto kk__tailcall;
      }
    }
    {
      kk_string_drop(sub, _ctx);
      return s;
    }
  }
}
 
// Trim off a substring `sub` while `s` ends with that string.

kk_string_t kk_std_core_trim_right_1(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> string */ 
  kk__tailcall: ;
  bool _match_18981;
  kk_string_t _x20328 = kk_string_dup(sub); /*string*/
  kk_string_t _x20329 = kk_string_empty(); /*string*/
  _match_18981 = kk_string_is_eq(_x20328,_x20329,kk_context()); /*bool*/
  if (_match_18981) {
    kk_string_drop(sub, _ctx);
    return s;
  }
  {
    kk_std_core_types__maybe _match_18982;
    kk_string_t _x20331 = kk_string_dup(s); /*string*/
    kk_string_t _x20332 = kk_string_dup(sub); /*string*/
    _match_18982 = kk_std_core_ends_with(_x20331, _x20332, _ctx); /*maybe<sslice>*/
    if (kk_std_core_types__is_Just(_match_18982)) {
      kk_box_t _box_x18583 = _match_18982._cons.Just.value;
      kk_std_core__sslice slice0 = kk_std_core__sslice_unbox(_box_x18583, NULL);
      kk_string_drop(s, _ctx);
      { // tailcall
        kk_string_t _x20334 = kk_std_core_string_3(slice0, _ctx); /*string*/
        s = _x20334;
        goto kk__tailcall;
      }
    }
    {
      kk_string_drop(sub, _ctx);
      return s;
    }
  }
}
 
// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.

kk_integer_t kk_std_core_parse_int_default(kk_string_t s, kk_std_core_types__optional default0, kk_std_core_types__optional hex, kk_context_t* _ctx) { /* (s : string, default : optional<int>, hex : optional<bool>) -> int */ 
  bool _match_18980;
  kk_string_t _x20341 = kk_string_dup(s); /*string*/
  kk_string_t _x20342 = kk_string_empty(); /*string*/
  _match_18980 = kk_string_is_eq(_x20341,_x20342,kk_context()); /*bool*/
  if (_match_18980) {
    kk_std_core_types__optional_drop(hex, _ctx);
    kk_string_drop(s, _ctx);
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18585 = default0._cons.Optional.value;
      kk_integer_t _default_15092 = kk_integer_unbox(_box_x18585);
      kk_integer_dup(_default_15092);
      kk_std_core_types__optional_drop(default0, _ctx);
      return _default_15092;
    }
    {
      return kk_integer_from_small(0);
    }
  }
  {
    kk_std_core_types__maybe m_16684;
    kk_string_t _x20345;
    kk_string_t _x20346 = kk_string_trim_left(s,kk_context()); /*string*/
    _x20345 = kk_string_trim_right(_x20346,kk_context()); /*string*/
    bool _x20347;
    if (kk_std_core_types__is_Optional(hex)) {
      kk_box_t _box_x18586 = hex._cons.Optional.value;
      bool _hex_15096 = kk_bool_unbox(_box_x18586);
      kk_std_core_types__optional_drop(hex, _ctx);
      _x20347 = _hex_15096; /*bool*/
      goto _match20348;
    }
    {
      _x20347 = false; /*bool*/
    }
    _match20348: ;
    m_16684 = kk_std_core_xparse_int(_x20345, _x20347, _ctx); /*maybe<int>*/
    if (kk_std_core_types__is_Nothing(m_16684)) {
      if (kk_std_core_types__is_Optional(default0)) {
        kk_box_t _box_x18587 = default0._cons.Optional.value;
        kk_integer_t _default_150920 = kk_integer_unbox(_box_x18587);
        kk_integer_dup(_default_150920);
        kk_std_core_types__optional_drop(default0, _ctx);
        return _default_150920;
      }
      {
        return kk_integer_from_small(0);
      }
    }
    {
      kk_box_t _box_x18588 = m_16684._cons.Just.value;
      kk_integer_t x = kk_integer_unbox(_box_x18588);
      kk_std_core_types__optional_drop(default0, _ctx);
      return x;
    }
  }
}
 
// monadic lift

kk_std_core_types__tuple2_ kk_std_core__mlift17192_partition_acc(kk_std_core__list acc1, kk_std_core__list acc2, kk_function_t pred, kk_box_t x, kk_std_core__list xx, bool _y_17072, kk_context_t* _ctx) { /* forall<a,e> (acc1 : list<a>, acc2 : list<a>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e (list<a>, list<a>) */ 
  if (_y_17072) {
    kk_std_core__list _x20352 = kk_std_core__new_Cons(kk_reuse_null, x, acc1, _ctx); /*list<61>*/
    return kk_std_core_partition_acc(xx, pred, _x20352, acc2, _ctx);
  }
  {
    kk_std_core__list _x20353 = kk_std_core__new_Cons(kk_reuse_null, x, acc2, _ctx); /*list<61>*/
    return kk_std_core_partition_acc(xx, pred, acc1, _x20353, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_partition_acc_fun20358__t {
  struct kk_function_s _base;
  kk_std_core__list acc10;
  kk_std_core__list acc20;
  kk_function_t pred0;
  kk_box_t x0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_partition_acc_fun20358(kk_function_t _fself, kk_box_t _b_18592, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_partition_acc_fun20358(kk_std_core__list acc10, kk_std_core__list acc20, kk_function_t pred0, kk_box_t x0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_partition_acc_fun20358__t* _self = kk_function_alloc_as(struct kk_std_core_partition_acc_fun20358__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_partition_acc_fun20358, kk_context());
  _self->acc10 = acc10;
  _self->acc20 = acc20;
  _self->pred0 = pred0;
  _self->x0 = x0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_partition_acc_fun20358(kk_function_t _fself, kk_box_t _b_18592, kk_context_t* _ctx) {
  struct kk_std_core_partition_acc_fun20358__t* _self = kk_function_as(struct kk_std_core_partition_acc_fun20358__t*, _fself);
  kk_std_core__list acc10 = _self->acc10; /* list<15214> */
  kk_std_core__list acc20 = _self->acc20; /* list<15214> */
  kk_function_t pred0 = _self->pred0; /* (15214) -> 15215 bool */
  kk_box_t x0 = _self->x0; /* 15214 */
  kk_std_core__list xx0 = _self->xx0; /* list<15214> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc10);kk_std_core__list_dup(acc20);kk_function_dup(pred0);kk_box_dup(x0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x20359;
  bool _x20360 = kk_bool_unbox(_b_18592); /*bool*/
  _x20359 = kk_std_core__mlift17192_partition_acc(acc10, acc20, pred0, x0, xx0, _x20360, _ctx); /*(list<15214>, list<15214>)*/
  return kk_std_core_types__tuple2__box(_x20359, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_partition_acc(kk_std_core__list xs, kk_function_t pred0, kk_std_core__list acc10, kk_std_core__list acc20, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, acc1 : list<a>, acc2 : list<a>) -> e (list<a>, list<a>) */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(pred0, _ctx);
    kk_std_core__list _b_18593_18589 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc10, _ctx); /*list<15214>*/;
    kk_std_core__list _b_18594_18590 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc20, _ctx); /*list<15214>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18593_18589, _ctx), kk_std_core__list_box(_b_18594_18590, _ctx), _ctx);
  }
  {
    struct kk_std_core_Cons* _con20354 = kk_std_core__as_Cons(xs);
    kk_box_t x0 = _con20354->head;
    kk_std_core__list xx0 = _con20354->tail;
    kk_reuse_t _ru_18929 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18929 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18929 = kk_reuse_null;
    }
    bool x1_17494;
    kk_function_t _x20356 = kk_function_dup(pred0); /*(15214) -> 15215 bool*/
    kk_box_t _x20355 = kk_box_dup(x0); /*15214*/
    x1_17494 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20356, (_x20356, _x20355, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18929, _ctx);
      kk_box_t _x20357 = kk_std_core_hnd_yield_extend(kk_std_core_new_partition_acc_fun20358(acc10, acc20, pred0, x0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x20357, _ctx);
    }
    if (x1_17494) { // tailcall
                    kk_std_core__list _x20361;
                    if (kk_likely(_ru_18929!=NULL)) {
                      struct kk_std_core_Cons* _con20362 = (struct kk_std_core_Cons*)_ru_18929;
                      _con20362->tail = acc10;
                      _x20361 = kk_std_core__base_Cons(_con20362); /*list<61>*/
                    }
                    else {
                      _x20361 = kk_std_core__new_Cons(kk_reuse_null, x0, acc10, _ctx); /*list<61>*/
                    }
                    xs = xx0;
                    acc10 = _x20361;
                    goto kk__tailcall;
    }
    { // tailcall
      kk_std_core__list _x20363;
      if (kk_likely(_ru_18929!=NULL)) {
        struct kk_std_core_Cons* _con20364 = (struct kk_std_core_Cons*)_ru_18929;
        _con20364->tail = acc20;
        _x20363 = kk_std_core__base_Cons(_con20364); /*list<61>*/
      }
      else {
        _x20363 = kk_std_core__new_Cons(kk_reuse_null, x0, acc20, _ctx); /*list<61>*/
      }
      xs = xx0;
      acc20 = _x20363;
      goto kk__tailcall;
    }
  }
}
 
// redirect `print` and `println` calls to a specified function.


// lift anonymous function
struct kk_std_core_print_redirect_fun20366__t {
  struct kk_function_s _base;
  kk_function_t print0;
};
static kk_box_t kk_std_core_print_redirect_fun20366(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_print_redirect_fun20366(kk_function_t print0, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20366__t* _self = kk_function_alloc_as(struct kk_std_core_print_redirect_fun20366__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_print_redirect_fun20366, kk_context());
  _self->print0 = print0;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_print_redirect_fun20371__t {
  struct kk_function_s _base;
  kk_function_t print0;
};
static kk_box_t kk_std_core_print_redirect_fun20371(kk_function_t _fself, kk_box_t _b_18600, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_print_redirect_fun20371(kk_function_t print0, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20371__t* _self = kk_function_alloc_as(struct kk_std_core_print_redirect_fun20371__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_print_redirect_fun20371, kk_context());
  _self->print0 = print0;
  return &_self->_base;
}

static kk_box_t kk_std_core_print_redirect_fun20371(kk_function_t _fself, kk_box_t _b_18600, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20371__t* _self = kk_function_as(struct kk_std_core_print_redirect_fun20371__t*, _fself);
  kk_function_t print0 = _self->print0; /* (msg : string) -> console () */
  kk_drop_match(_self, {kk_function_dup(print0);}, {}, _ctx)
  kk_unit_t _x20372 = kk_Unit;
  kk_string_t _x20373 = kk_string_unbox(_b_18600); /*string*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_string_t, kk_context_t*), print0, (print0, _x20373, _ctx));
  return kk_unit_box(_x20372);
}
static kk_box_t kk_std_core_print_redirect_fun20366(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20366__t* _self = kk_function_as(struct kk_std_core_print_redirect_fun20366__t*, _fself);
  kk_function_t print0 = _self->print0; /* (msg : string) -> console () */
  kk_drop_match(_self, {kk_function_dup(print0);}, {}, _ctx)
  kk_unit_t _x20367 = kk_Unit;
  kk_ref_t _x20368 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  kk_box_t _x20369;
  kk_std_core_types__maybe _x20370 = kk_std_core_types__new_Just(kk_function_box(kk_std_core_new_print_redirect_fun20371(print0, _ctx)), _ctx); /*maybe<105>*/
  _x20369 = kk_std_core_types__maybe_box(_x20370, _ctx); /*171*/
  kk_ref_set(_x20368,_x20369,kk_context());
  return kk_unit_box(_x20367);
}

kk_unit_t kk_std_core_print_redirect(kk_function_t print0, kk_context_t* _ctx) { /* (print : (msg : string) -> console ()) -> io () */ 
  kk_box_t _x20365 = kk_std_core_hnd__open_none0(kk_std_core_new_print_redirect_fun20366(print0, _ctx), _ctx); /*3214*/
  kk_unit_unbox(_x20365); return kk_Unit;
}
extern bool kk_std_core_remove_fun20374(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_remove_fun20374__t* _self = kk_function_as(struct kk_std_core_remove_fun20374__t*, _fself);
  kk_function_t pred = _self->pred; /* (15283) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  bool b_16691 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), pred, (pred, x, _ctx)); /*bool*/;
  if (b_16691) {
    return false;
  }
  {
    return true;
  }
}
extern kk_unit_t kk_std_core_repeat_fun20376_1(kk_function_t _fself, kk_integer_t i, kk_context_t* _ctx) {
  struct kk_std_core_repeat_fun20376__t_1* _self = kk_function_as(struct kk_std_core_repeat_fun20376__t_1*, _fself);
  kk_function_t action = _self->action; /* () -> 15302 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_integer_drop(i, _ctx);
  return kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}
 
// Create a list of `n`  repeated elementes `x`

kk_std_core__list kk_std_core__ctail_replicate(kk_box_t x, kk_integer_t n, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (x : a, n : int, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  bool _match_18978;
  kk_integer_t _x20377 = kk_integer_dup(n); /*int*/
  _match_18978 = kk_integer_gt(_x20377,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_18978) {
    kk_std_core__list _ctail_16839 = kk_std_core__list_hole(); /*list<15346>*/;
    kk_std_core__list _ctail_16840;
    kk_box_t _x20378 = kk_box_dup(x); /*15346*/
    _ctail_16840 = kk_std_core__new_Cons(kk_reuse_null, _x20378, _ctail_16839, _ctx); /*list<15346>*/
    { // tailcall
      kk_integer_t _x20379 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
      kk_std_core_types__ctail _x20380;
      kk_box_t* _b_18618_18613 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16840)->tail)); /*cfield<list<15346>>*/;
      _x20380 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16840, _ctx)),_b_18618_18613); /*ctail<0>*/
      n = _x20379;
      _acc = _x20380;
      goto kk__tailcall;
    }
  }
  {
    kk_integer_drop(n, _ctx);
    kk_box_drop(x, _ctx);
    kk_box_t _x20381 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20381, _ctx);
  }
}
 
// Create a list of `n`  repeated elementes `x`

kk_std_core__list kk_std_core_replicate(kk_box_t x0, kk_integer_t n0, kk_context_t* _ctx) { /* forall<a> (x : a, n : int) -> list<a> */ 
  kk_std_core_types__ctail _x20382 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_replicate(x0, n0, _x20382, _ctx);
}
 
// lifted

kk_std_core__list kk_std_core__lift16752_reverse_join(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20383 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con20383->head;
    kk_std_core__list xx = _con20383->tail;
    kk_reuse_t _ru_18930 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18930 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18930 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20384;
      if (kk_likely(_ru_18930!=NULL)) {
        struct kk_std_core_Cons* _con20385 = (struct kk_std_core_Cons*)_ru_18930;
        _con20385->tail = acc;
        _x20384 = kk_std_core__base_Cons(_con20385); /*list<61>*/
      }
      else {
        _x20384 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x20384;
      ys = xx;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// lifted

kk_string_t kk_std_core__lift16753_reverse_join(kk_std_core__list ys0, kk_string_t acc0, kk_context_t* _ctx) { /* (ys0 : list<string>, acc0 : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con20386 = kk_std_core__as_Cons(ys0);
    kk_box_t _box_x18624 = _con20386->head;
    kk_std_core__list yy = _con20386->tail;
    kk_string_t y = kk_string_unbox(_box_x18624);
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys0, _ctx);
    }
    { // tailcall
      kk_string_t _x20388;
      kk_string_t _x20389;
      kk_string_t _x20390 = kk_string_empty(); /*string*/
      _x20389 = kk_std_core__lp__plus__plus__1_rp_(_x20390, y, _ctx); /*string*/
      _x20388 = kk_std_core__lp__plus__plus__1_rp_(acc0, _x20389, _ctx); /*string*/
      ys0 = yy;
      acc0 = _x20388;
      goto kk__tailcall;
    }
  }
  {
    return acc0;
  }
}
 
// Concatenate all strings in a list in reverse order

kk_string_t kk_std_core_reverse_join(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<string>) -> string */ 
  kk_std_core__list xs0_16693 = kk_std_core__lift16752_reverse_join(kk_std_core__new_Nil(_ctx), xs, _ctx); /*list<string>*/;
  if (kk_std_core__is_Nil(xs0_16693)) {
    return kk_string_empty();
  }
  {
    struct kk_std_core_Cons* _con20393 = kk_std_core__as_Cons(xs0_16693);
    kk_box_t _box_x18625 = _con20393->head;
    kk_std_core__list xx0 = _con20393->tail;
    kk_string_t x0 = kk_string_unbox(_box_x18625);
    if (kk_likely(kk_std_core__list_is_unique(xs0_16693))) {
      kk_std_core__list_free(xs0_16693);
    }
    else {
      kk_string_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs0_16693, _ctx);
    }
    return kk_std_core__lift16753_reverse_join(xx0, x0, _ctx);
  }
}

kk_string_t kk_std_core_show_tuple(kk_std_core_types__tuple2_ x, kk_function_t showfst, kk_function_t showsnd, kk_context_t* _ctx) { /* forall<a,b> (x : (a, b), showfst : (a) -> string, showsnd : (b) -> string) -> string */ 
  kk_string_t _x20395;
  kk_define_string_literal(, _s20396, 1, "(")
  _x20395 = kk_string_dup(_s20396); /*string*/
  kk_string_t _x20397;
  kk_string_t _x20398;
  kk_box_t _x20399;
  {
    kk_box_t _x = x.fst;
    kk_box_dup(_x);
    _x20399 = _x; /*15666*/
  }
  _x20398 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), showfst, (showfst, _x20399, _ctx)); /*string*/
  kk_string_t _x20400;
  kk_string_t _x20401;
  kk_define_string_literal(, _s20402, 1, ",")
  _x20401 = kk_string_dup(_s20402); /*string*/
  kk_string_t _x20403;
  kk_string_t _x20404;
  kk_box_t _x20405;
  {
    kk_box_t _x0 = x.snd;
    kk_box_dup(_x0);
    kk_std_core_types__tuple2__drop(x, _ctx);
    _x20405 = _x0; /*15667*/
  }
  _x20404 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), showsnd, (showsnd, _x20405, _ctx)); /*string*/
  kk_string_t _x20406;
  kk_define_string_literal(, _s20407, 1, ")")
  _x20406 = kk_string_dup(_s20407); /*string*/
  _x20403 = kk_std_core__lp__plus__plus__1_rp_(_x20404, _x20406, _ctx); /*string*/
  _x20400 = kk_std_core__lp__plus__plus__1_rp_(_x20401, _x20403, _ctx); /*string*/
  _x20397 = kk_std_core__lp__plus__plus__1_rp_(_x20398, _x20400, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x20395, _x20397, _ctx);
}
 
// monadic lift

kk_std_core_types__tuple2_ kk_std_core__mlift17193_op(kk_std_core__list acc, kk_function_t predicate, kk_box_t y, kk_std_core__list ys, kk_std_core__list yy, bool _y_17080, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, predicate : (a) -> e bool, y : a, ys : list<a>, yy : list<a>, bool) -> e (list<a>, list<a>) */ 
  if (_y_17080) {
    kk_std_core__list_drop(ys, _ctx);
    kk_std_core__list _x20408 = kk_std_core__new_Cons(kk_reuse_null, y, acc, _ctx); /*list<61>*/
    return kk_std_core__lift16754_span(predicate, yy, _x20408, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_box_drop(y, _ctx);
    kk_std_core__list_drop(yy, _ctx);
    kk_std_core__list _b_18628_18626 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc, _ctx); /*list<15725>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18628_18626, _ctx), kk_std_core__list_box(ys, _ctx), _ctx);
  }
}
 
// lifted
// todo: implement TRMC with multiple results to avoid the reverse


// lift anonymous function
struct kk_std_core__lift16754_span_fun20414__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t predicate0;
  kk_box_t y0;
  kk_std_core__list ys0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16754_span_fun20414(kk_function_t _fself, kk_box_t _b_18631, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16754_span_fun20414(kk_std_core__list acc0, kk_function_t predicate0, kk_box_t y0, kk_std_core__list ys0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16754_span_fun20414__t* _self = kk_function_alloc_as(struct kk_std_core__lift16754_span_fun20414__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16754_span_fun20414, kk_context());
  _self->acc0 = acc0;
  _self->predicate0 = predicate0;
  _self->y0 = y0;
  _self->ys0 = ys0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16754_span_fun20414(kk_function_t _fself, kk_box_t _b_18631, kk_context_t* _ctx) {
  struct kk_std_core__lift16754_span_fun20414__t* _self = kk_function_as(struct kk_std_core__lift16754_span_fun20414__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<15725> */
  kk_function_t predicate0 = _self->predicate0; /* (15725) -> 15726 bool */
  kk_box_t y0 = _self->y0; /* 15725 */
  kk_std_core__list ys0 = _self->ys0; /* list<15725> */
  kk_std_core__list yy0 = _self->yy0; /* list<15725> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(predicate0);kk_box_dup(y0);kk_std_core__list_dup(ys0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x20415;
  bool _x20416 = kk_bool_unbox(_b_18631); /*bool*/
  _x20415 = kk_std_core__mlift17193_op(acc0, predicate0, y0, ys0, yy0, _x20416, _ctx); /*(list<15725>, list<15725>)*/
  return kk_std_core_types__tuple2__box(_x20415, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core__lift16754_span(kk_function_t predicate0, kk_std_core__list ys0, kk_std_core__list acc0, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, ys : list<a>, acc : list<a>) -> e (list<a>, list<a>) */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con20409 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con20409->head;
    kk_std_core__list yy0 = _con20409->tail;
    kk_box_dup(y0);
    kk_std_core__list_dup(yy0);
    kk_reuse_t _ru_18933;
    kk_std_core__list _x20410 = kk_std_core__list_dup(ys0); /*list<15725>*/
    _ru_18933 = kk_std_core__list_dropn_reuse(_x20410, ((int32_t)KI32(2)), _ctx); /*reuse*/
    bool x_17501;
    kk_function_t _x20412 = kk_function_dup(predicate0); /*(15725) -> 15726 bool*/
    kk_box_t _x20411 = kk_box_dup(y0); /*15725*/
    x_17501 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20412, (_x20412, _x20411, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18933, _ctx);
      kk_box_t _x20413 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16754_span_fun20414(acc0, predicate0, y0, ys0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x20413, _ctx);
    }
    if (x_17501) {
      kk_std_core__list_dropn(ys0, ((int32_t)KI32(2)), _ctx);
      { // tailcall
        kk_std_core__list _x20417;
        if (kk_likely(_ru_18933!=NULL)) {
          struct kk_std_core_Cons* _con20418 = (struct kk_std_core_Cons*)_ru_18933;
          _con20418->tail = acc0;
          _x20417 = kk_std_core__base_Cons(_con20418); /*list<61>*/
        }
        else {
          _x20417 = kk_std_core__new_Cons(kk_reuse_null, y0, acc0, _ctx); /*list<61>*/
        }
        ys0 = yy0;
        acc0 = _x20417;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18933, _ctx);
      kk_function_drop(predicate0, _ctx);
      kk_box_drop(y0, _ctx);
      kk_std_core__list_drop(yy0, _ctx);
      kk_std_core__list _b_18637_18632 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx); /*list<15725>*/;
      return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18637_18632, _ctx), kk_std_core__list_box(ys0, _ctx), _ctx);
    }
  }
  {
    kk_function_drop(predicate0, _ctx);
    kk_std_core__list _b_18639_18634 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx); /*list<15725>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18639_18634, _ctx), kk_std_core__list_box(ys0, _ctx), _ctx);
  }
}
 
// Return the sum of a list of integers


// lift anonymous function
struct kk_std_core_sum_fun20420__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_sum_fun20420(kk_function_t _fself, kk_box_t _b_18645, kk_box_t _b_18646, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_sum_fun20420(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_sum_fun20420, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_sum_fun20420(kk_function_t _fself, kk_box_t _b_18645, kk_box_t _b_18646, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20421;
  kk_integer_t _x20422 = kk_integer_unbox(_b_18645); /*int*/
  kk_integer_t _x20423 = kk_integer_unbox(_b_18646); /*int*/
  _x20421 = kk_integer_add(_x20422,_x20423,kk_context()); /*int*/
  return kk_integer_box(_x20421);
}

kk_integer_t kk_std_core_sum(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<int>) -> int */ 
  kk_box_t _x20419 = kk_std_core_foldl(xs, kk_integer_box(kk_integer_from_small(0)), kk_std_core_new_sum_fun20420(_ctx), _ctx); /*11906*/
  return kk_integer_unbox(_x20419);
}
 
// Return the tail of list. Returns the empty list if `xs` is empty.

kk_std_core__list kk_std_core_tail_1(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> list<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20424 = kk_std_core__as_Cons(xs);
    kk_box_t _pat0 = _con20424->head;
    kk_std_core__list xx = _con20424->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_pat0, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return xx;
  }
  {
    return kk_std_core__new_Nil(_ctx);
  }
}
 
// Return the tail of a string (or the empty string)

kk_string_t kk_std_core_tail_2(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_std_core__sslice _x20425;
  kk_std_core__sslice slice1 = kk_std_core_first1(s, _ctx); /*sslice*/;
  kk_std_core__sslice slice0_16697;
  bool _match_18974;
  kk_integer_t _x20426;
  kk_std_core_types__optional _match_18976 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_18976)) {
    kk_box_t _box_x18652 = _match_18976._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18652);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_18976, _ctx);
    _x20426 = _n_9710; /*int*/
    goto _match20427;
  }
  {
    _x20426 = kk_integer_from_small(1); /*int*/
  }
  _match20427: ;
  _match_18974 = kk_integer_eq(_x20426,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_18974) {
    slice0_16697 = slice1; /*sslice*/
  }
  else {
    kk_integer_t _x20429;
    kk_integer_t _x20430;
    kk_std_core_types__optional _match_18975 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_18975)) {
      kk_box_t _box_x18653 = _match_18975._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18653);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_18975, _ctx);
      _x20430 = _n_97100; /*int*/
      goto _match20431;
    }
    {
      _x20430 = kk_integer_from_small(1); /*int*/
    }
    _match20431: ;
    _x20429 = kk_integer_sub(_x20430,(kk_integer_from_small(1)),kk_context()); /*int*/
    slice0_16697 = kk_std_core_extend(slice1, _x20429, _ctx); /*sslice*/
  }
  {
    kk_string_t s0 = slice0_16697.str;
    kk_ssize_t start0 = slice0_16697.start;
    kk_ssize_t len0 = slice0_16697.len;
    kk_string_dup(s0);
    kk_std_core__sslice_drop(slice0_16697, _ctx);
    kk_string_t _x20433 = kk_string_dup(s0); /*string*/
    kk_ssize_t _x20434 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x20435;
    kk_ssize_t _x20436 = kk_string_len(s0,kk_context()); /*ssize_t*/
    kk_ssize_t _x20437 = (start0 + len0); /*ssize_t*/
    _x20435 = (_x20436 - _x20437); /*ssize_t*/
    _x20425 = kk_std_core__new_Sslice(_x20433, _x20434, _x20435, _ctx); /*sslice*/
  }
  return kk_std_core_string_3(_x20425, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17194_op(kk_std_core_types__ctail _acc, kk_function_t predicate, kk_box_t x, kk_std_core__list xx, bool _y_17085, kk_context_t* _ctx) { /* forall<a,e> (ctail<list<a>>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_17085) {
    kk_std_core__list _ctail_16841 = kk_std_core__list_hole(); /*list<15836>*/;
    kk_std_core__list _ctail_16842 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16841, _ctx); /*list<15836>*/;
    kk_std_core_types__ctail _x20438;
    kk_box_t* _b_18664_18659 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16842)->tail)); /*cfield<list<15836>>*/;
    _x20438 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16842, _ctx)),_b_18664_18659); /*ctail<0>*/
    return kk_std_core__ctail_take_while(xx, predicate, _x20438, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_box_drop(x, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    kk_box_t _x20439 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20439, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17195_op_fun20440__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t x0;
};
static kk_std_core__list kk_std_core__mlift17195_op_fun20440(kk_function_t _fself, kk_std_core__list _ctail_16844, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17195_op_fun20440(kk_function_t _accm, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17195_op_fun20440__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17195_op_fun20440__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17195_op_fun20440, kk_context());
  _self->_accm = _accm;
  _self->x0 = x0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17195_op_fun20440(kk_function_t _fself, kk_std_core__list _ctail_16844, kk_context_t* _ctx) {
  struct kk_std_core__mlift17195_op_fun20440__t* _self = kk_function_as(struct kk_std_core__mlift17195_op_fun20440__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<15836>) -> list<15836> */
  kk_box_t x0 = _self->x0; /* 15836 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(x0);}, {}, _ctx)
  kk_std_core__list _x20441 = kk_std_core__new_Cons(kk_reuse_null, x0, _ctail_16844, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20441, _ctx));
}

kk_std_core__list kk_std_core__mlift17195_op(kk_function_t _accm, kk_function_t predicate0, kk_box_t x0, kk_std_core__list xx0, bool _y_17089, kk_context_t* _ctx) { /* forall<a,e> ((list<a>) -> list<a>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_17089) {
    return kk_std_core__ctailm_take_while(xx0, predicate0, kk_std_core__new_mlift17195_op_fun20440(_accm, x0, _ctx), _ctx);
  }
  {
    kk_function_drop(predicate0, _ctx);
    kk_box_drop(x0, _ctx);
    kk_std_core__list_drop(xx0, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Keep only those initial elements that satisfy `predicate`


// lift anonymous function
struct kk_std_core__ctail_take_while_fun20446__t {
  struct kk_function_s _base;
  kk_function_t predicate1;
  kk_box_t x1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_take_while_fun20446(kk_function_t _fself, kk_box_t _b_18671, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_take_while_fun20446(kk_function_t predicate1, kk_box_t x1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_take_while_fun20446__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_take_while_fun20446__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_take_while_fun20446, kk_context());
  _self->predicate1 = predicate1;
  _self->x1 = x1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_take_while_fun20446(kk_function_t _fself, kk_box_t _b_18671, kk_context_t* _ctx) {
  struct kk_std_core__ctail_take_while_fun20446__t* _self = kk_function_as(struct kk_std_core__ctail_take_while_fun20446__t*, _fself);
  kk_function_t predicate1 = _self->predicate1; /* (15836) -> 15837 bool */
  kk_box_t x1 = _self->x1; /* 15836 */
  kk_std_core__list xx1 = _self->xx1; /* list<15836> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<15836>> */
  kk_drop_match(_self, {kk_function_dup(predicate1);kk_box_dup(x1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20447;
  bool _x20448 = kk_bool_unbox(_b_18671); /*bool*/
  _x20447 = kk_std_core__mlift17194_op(_acc0, predicate1, x1, xx1, _x20448, _ctx); /*list<15836>*/
  return kk_std_core__list_box(_x20447, _ctx);
}

kk_std_core__list kk_std_core__ctail_take_while(kk_std_core__list xs, kk_function_t predicate1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, ctail<list<a>>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20442 = kk_std_core__as_Cons(xs);
    kk_box_t x1 = _con20442->head;
    kk_std_core__list xx1 = _con20442->tail;
    kk_reuse_t _ru_18935 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18935 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18935 = kk_reuse_null;
    }
    bool x2_17506;
    kk_function_t _x20444 = kk_function_dup(predicate1); /*(15836) -> 15837 bool*/
    kk_box_t _x20443 = kk_box_dup(x1); /*15836*/
    x2_17506 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20444, (_x20444, _x20443, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18935, _ctx);
      kk_box_t _x20445 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_take_while_fun20446(predicate1, x1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20445, _ctx);
    }
    if (x2_17506) {
      kk_std_core__list _ctail_168410 = kk_std_core__list_hole(); /*list<15836>*/;
      kk_std_core__list _ctail_168420;
      if (kk_likely(_ru_18935!=NULL)) {
        struct kk_std_core_Cons* _con20449 = (struct kk_std_core_Cons*)_ru_18935;
        _con20449->tail = _ctail_168410;
        _ctail_168420 = kk_std_core__base_Cons(_con20449); /*list<15836>*/
      }
      else {
        _ctail_168420 = kk_std_core__new_Cons(kk_reuse_null, x1, _ctail_168410, _ctx); /*list<15836>*/
      }
      { // tailcall
        kk_std_core_types__ctail _x20450;
        kk_box_t* _b_18685_18677 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168420)->tail)); /*cfield<list<15836>>*/;
        _x20450 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168420, _ctx)),_b_18685_18677); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20450;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18935, _ctx);
      kk_function_drop(predicate1, _ctx);
      kk_box_drop(x1, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20451 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20451, _ctx);
    }
  }
  {
    kk_function_drop(predicate1, _ctx);
    kk_box_t _x20452 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20452, _ctx);
  }
}
 
// Keep only those initial elements that satisfy `predicate`


// lift anonymous function
struct kk_std_core__ctailm_take_while_fun20457__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t predicate2;
  kk_box_t x3;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_take_while_fun20457(kk_function_t _fself, kk_box_t _b_18695, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_take_while_fun20457(kk_function_t _accm0, kk_function_t predicate2, kk_box_t x3, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20457__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_take_while_fun20457__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_take_while_fun20457, kk_context());
  _self->_accm0 = _accm0;
  _self->predicate2 = predicate2;
  _self->x3 = x3;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_take_while_fun20457(kk_function_t _fself, kk_box_t _b_18695, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20457__t* _self = kk_function_as(struct kk_std_core__ctailm_take_while_fun20457__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<15836>) -> list<15836> */
  kk_function_t predicate2 = _self->predicate2; /* (15836) -> 15837 bool */
  kk_box_t x3 = _self->x3; /* 15836 */
  kk_std_core__list xx2 = _self->xx2; /* list<15836> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(predicate2);kk_box_dup(x3);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20458;
  bool _x20459 = kk_bool_unbox(_b_18695); /*bool*/
  _x20458 = kk_std_core__mlift17195_op(_accm0, predicate2, x3, xx2, _x20459, _ctx); /*list<15836>*/
  return kk_std_core__list_box(_x20458, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_take_while_fun20461__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x3;
};
static kk_std_core__list kk_std_core__ctailm_take_while_fun20461(kk_function_t _fself, kk_std_core__list _ctail_168440, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_take_while_fun20461(kk_function_t _accm0, kk_box_t x3, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20461__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_take_while_fun20461__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_take_while_fun20461, kk_context());
  _self->_accm0 = _accm0;
  _self->x3 = x3;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_take_while_fun20461(kk_function_t _fself, kk_std_core__list _ctail_168440, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20461__t* _self = kk_function_as(struct kk_std_core__ctailm_take_while_fun20461__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<15836>) -> list<15836> */
  kk_box_t x3 = _self->x3; /* 15836 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x3);}, {}, _ctx)
  kk_std_core__list _x20462 = kk_std_core__new_Cons(kk_reuse_null, x3, _ctail_168440, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20462, _ctx));
}

kk_std_core__list kk_std_core__ctailm_take_while(kk_std_core__list xs0, kk_function_t predicate2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20453 = kk_std_core__as_Cons(xs0);
    kk_box_t x3 = _con20453->head;
    kk_std_core__list xx2 = _con20453->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x3);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    bool x4_17509;
    kk_function_t _x20455 = kk_function_dup(predicate2); /*(15836) -> 15837 bool*/
    kk_box_t _x20454 = kk_box_dup(x3); /*15836*/
    x4_17509 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20455, (_x20455, _x20454, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x20456 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_take_while_fun20457(_accm0, predicate2, x3, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20456, _ctx);
    }
    if (x4_17509) { // tailcall
                    kk_function_t _x20460 = kk_std_core__new_ctailm_take_while_fun20461(_accm0, x3, _ctx); /*(list<15836>) -> list<15836>*/
                    xs0 = xx2;
                    _accm0 = _x20460;
                    goto kk__tailcall;
    }
    {
      kk_function_drop(predicate2, _ctx);
      kk_box_drop(x3, _ctx);
      kk_std_core__list_drop(xx2, _ctx);
      return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
    }
  }
  {
    kk_function_drop(predicate2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Keep only those initial elements that satisfy `predicate`


// lift anonymous function
struct kk_std_core_take_while_fun20464__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_take_while_fun20464(kk_function_t _fself, kk_std_core__list _ctail_16843, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_take_while_fun20464(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_take_while_fun20464, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_take_while_fun20464(kk_function_t _fself, kk_std_core__list _ctail_16843, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16843;
}

kk_std_core__list kk_std_core_take_while(kk_std_core__list xs1, kk_function_t predicate3, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */ 
  bool _match_18971 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_18971) {
    kk_std_core_types__ctail _x20463 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_take_while(xs1, predicate3, _x20463, _ctx);
  }
  {
    return kk_std_core__ctailm_take_while(xs1, predicate3, kk_std_core_new_take_while_fun20464(_ctx), _ctx);
  }
}
 
// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.


// lift anonymous function
struct kk_std_core_trace_fun20468__t {
  struct kk_function_s _base;
  kk_string_t message0;
};
static kk_box_t kk_std_core_trace_fun20468(kk_function_t _fself, kk_box_t _b_18701, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_trace_fun20468(kk_string_t message0, kk_context_t* _ctx) {
  struct kk_std_core_trace_fun20468__t* _self = kk_function_alloc_as(struct kk_std_core_trace_fun20468__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_trace_fun20468, kk_context());
  _self->message0 = message0;
  return &_self->_base;
}

static kk_box_t kk_std_core_trace_fun20468(kk_function_t _fself, kk_box_t _b_18701, kk_context_t* _ctx) {
  struct kk_std_core_trace_fun20468__t* _self = kk_function_as(struct kk_std_core_trace_fun20468__t*, _fself);
  kk_string_t message0 = _self->message0; /* string */
  kk_drop_match(_self, {kk_string_dup(message0);}, {}, _ctx)
  kk_unit_t _x20469 = kk_Unit;
  bool _y_18703_17096 = kk_bool_unbox(_b_18701); /*bool*/;
  if (_y_18703_17096) {
    kk_std_core_xtrace(message0, _ctx);
  }
  else {
    kk_string_drop(message0, _ctx);
    kk_Unit;
  }
  return kk_unit_box(_x20469);
}

kk_unit_t kk_std_core_trace(kk_string_t message0, kk_context_t* _ctx) { /* (message : string) -> () */ 
  bool x_17512;
  kk_box_t _x20465;
  kk_ref_t _x20466 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  _x20465 = kk_ref_get(_x20466,kk_context()); /*184*/
  x_17512 = kk_bool_unbox(_x20465); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20467 = kk_std_core_hnd_yield_extend(kk_std_core_new_trace_fun20468(message0, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20467); return kk_Unit;
  }
  if (x_17512) {
    kk_std_core_xtrace(message0, _ctx); return kk_Unit;
  }
  {
    kk_string_drop(message0, _ctx);
    kk_Unit; return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_core_trace_any_fun20473__t {
  struct kk_function_s _base;
  kk_string_t message0;
  kk_box_t x;
};
static kk_box_t kk_std_core_trace_any_fun20473(kk_function_t _fself, kk_box_t _b_18708, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_trace_any_fun20473(kk_string_t message0, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_trace_any_fun20473__t* _self = kk_function_alloc_as(struct kk_std_core_trace_any_fun20473__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_trace_any_fun20473, kk_context());
  _self->message0 = message0;
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_core_trace_any_fun20473(kk_function_t _fself, kk_box_t _b_18708, kk_context_t* _ctx) {
  struct kk_std_core_trace_any_fun20473__t* _self = kk_function_as(struct kk_std_core_trace_any_fun20473__t*, _fself);
  kk_string_t message0 = _self->message0; /* string */
  kk_box_t x = _self->x; /* 15945 */
  kk_drop_match(_self, {kk_string_dup(message0);kk_box_dup(x);}, {}, _ctx)
  kk_unit_t _x20474 = kk_Unit;
  bool _y_18710_17098 = kk_bool_unbox(_b_18708); /*bool*/;
  if (_y_18710_17098) {
    kk_std_core_xtrace_any(message0, x, _ctx);
  }
  else {
    kk_string_drop(message0, _ctx);
    kk_box_drop(x, _ctx);
    kk_Unit;
  }
  return kk_unit_box(_x20474);
}

kk_unit_t kk_std_core_trace_any(kk_string_t message0, kk_box_t x, kk_context_t* _ctx) { /* forall<a> (message : string, x : a) -> () */ 
  bool x0_17516;
  kk_box_t _x20470;
  kk_ref_t _x20471 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  _x20470 = kk_ref_get(_x20471,kk_context()); /*184*/
  x0_17516 = kk_bool_unbox(_x20470); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20472 = kk_std_core_hnd_yield_extend(kk_std_core_new_trace_any_fun20473(message0, x, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20472); return kk_Unit;
  }
  if (x0_17516) {
    kk_std_core_xtrace_any(message0, x, _ctx); return kk_Unit;
  }
  {
    kk_string_drop(message0, _ctx);
    kk_box_drop(x, _ctx);
    kk_Unit; return kk_Unit;
  }
}
 
// Truncate a string to `count` characters.

kk_string_t kk_std_core_truncate(kk_string_t s, kk_integer_t count, kk_context_t* _ctx) { /* (s : string, count : int) -> string */ 
  kk_std_core__sslice _x20475;
  kk_std_core__sslice _x20476;
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_18966;
  kk_integer_t _x20477;
  kk_std_core_types__optional _match_18968 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_18968)) {
    kk_box_t _box_x18712 = _match_18968._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18712);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_18968, _ctx);
    _x20477 = _n_9710; /*int*/
    goto _match20478;
  }
  {
    _x20477 = kk_integer_from_small(1); /*int*/
  }
  _match20478: ;
  _match_18966 = kk_integer_eq(_x20477,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_18966) {
    _x20476 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x20480;
    kk_integer_t _x20481;
    kk_std_core_types__optional _match_18967 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_18967)) {
      kk_box_t _box_x18713 = _match_18967._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18713);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_18967, _ctx);
      _x20481 = _n_97100; /*int*/
      goto _match20482;
    }
    {
      _x20481 = kk_integer_from_small(1); /*int*/
    }
    _match20482: ;
    _x20480 = kk_integer_sub(_x20481,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x20476 = kk_std_core_extend(slice0, _x20480, _ctx); /*sslice*/
  }
  kk_integer_t _x20484 = kk_integer_sub(count,(kk_integer_from_small(1)),kk_context()); /*int*/
  _x20475 = kk_std_core_extend(_x20476, _x20484, _ctx); /*sslice*/
  return kk_std_core_string_3(_x20475, _ctx);
}
 
// Return a default value when an exception is raised


// lift anonymous function
struct kk_std_core_try_default_fun20487__t {
  struct kk_function_s _base;
  kk_box_t value;
};
static kk_box_t kk_std_core_try_default_fun20487(kk_function_t _fself, kk_std_core_hnd__marker _b_18715, kk_std_core_hnd__ev _b_18716, kk_box_t _b_18717, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20487(kk_box_t value, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20487__t* _self = kk_function_alloc_as(struct kk_std_core_try_default_fun20487__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_default_fun20487, kk_context());
  _self->value = value;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_default_fun20488__t {
  struct kk_function_s _base;
  kk_box_t value;
};
static kk_box_t kk_std_core_try_default_fun20488(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20488(kk_box_t value, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20488__t* _self = kk_function_alloc_as(struct kk_std_core_try_default_fun20488__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_default_fun20488, kk_context());
  _self->value = value;
  return &_self->_base;
}

static kk_box_t kk_std_core_try_default_fun20488(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20488__t* _self = kk_function_as(struct kk_std_core_try_default_fun20488__t*, _fself);
  kk_box_t value = _self->value; /* 16021 */
  kk_drop_match(_self, {kk_box_dup(value);}, {}, _ctx)
  kk_function_drop(___wildcard__585__45, _ctx);
  return value;
}
static kk_box_t kk_std_core_try_default_fun20487(kk_function_t _fself, kk_std_core_hnd__marker _b_18715, kk_std_core_hnd__ev _b_18716, kk_box_t _b_18717, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20487__t* _self = kk_function_as(struct kk_std_core_try_default_fun20487__t*, _fself);
  kk_box_t value = _self->value; /* 16021 */
  kk_drop_match(_self, {kk_box_dup(value);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_18716, ((int32_t)KI32(3)), _ctx);
  kk_box_drop(_b_18717, _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18715, kk_std_core_new_try_default_fun20488(value, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_default_fun20489__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_default_fun20489(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20489(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_default_fun20489, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_default_fun20489(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _x;
}

kk_box_t kk_std_core_try_default(kk_box_t value, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (value : a, action : () -> <exn|e> a) -> e a */ 
  kk_std_core__hnd_exn _x20485;
  kk_std_core_hnd__clause1 _x20486 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_default_fun20487(value, _ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x20485 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x20486, _ctx); /*.hnd-exn<11,12>*/
  return kk_std_core__handle_exn(((int32_t)KI32(0)), _x20485, kk_std_core_new_try_default_fun20489(_ctx), action, _ctx);
}
 
// Returns a unique integer (modulo 32-bits).


// lift anonymous function
struct kk_std_core_unique_fun20494__t {
  struct kk_function_s _base;
};
static kk_integer_t kk_std_core_unique_fun20494(kk_function_t _fself, kk_integer_t u, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_unique_fun20494(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_unique_fun20494, _ctx)
  return kk_function_dup(_fself);
}

static kk_integer_t kk_std_core_unique_fun20494(kk_function_t _fself, kk_integer_t u, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _b_18731_18729;
  kk_integer_t _x20495 = kk_integer_dup(u); /*int*/
  _b_18731_18729 = kk_integer_add(_x20495,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t __ = kk_Unit;
  kk_ref_t _x20496 = kk_ref_dup(kk_std_core_unique_count); /*ref<global,int>*/
  kk_ref_set(_x20496,(kk_integer_box(_b_18731_18729)),kk_context());
  return u;
}


// lift anonymous function
struct kk_std_core_unique_fun20498__t {
  struct kk_function_s _base;
  kk_function_t next0_17523;
};
static kk_box_t kk_std_core_unique_fun20498(kk_function_t _fself, kk_box_t _b_18733, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_unique_fun20498(kk_function_t next0_17523, kk_context_t* _ctx) {
  struct kk_std_core_unique_fun20498__t* _self = kk_function_alloc_as(struct kk_std_core_unique_fun20498__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_unique_fun20498, kk_context());
  _self->next0_17523 = next0_17523;
  return &_self->_base;
}

static kk_box_t kk_std_core_unique_fun20498(kk_function_t _fself, kk_box_t _b_18733, kk_context_t* _ctx) {
  struct kk_std_core_unique_fun20498__t* _self = kk_function_as(struct kk_std_core_unique_fun20498__t*, _fself);
  kk_function_t next0_17523 = _self->next0_17523; /* (int) -> <read<global>,write<global>|_16082> int */
  kk_drop_match(_self, {kk_function_dup(next0_17523);}, {}, _ctx)
  kk_integer_t _x20499;
  kk_integer_t _x20500 = kk_integer_unbox(_b_18733); /*int*/
  _x20499 = kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17523, (next0_17523, _x20500, _ctx)); /*int*/
  return kk_integer_box(_x20499);
}

kk_integer_t kk_std_core_unique(kk_context_t* _ctx) { /* () -> ndet int */ 
  kk_integer_t x_17522;
  kk_box_t _x20492;
  kk_ref_t _x20493 = kk_ref_dup(kk_std_core_unique_count); /*ref<global,int>*/
  _x20492 = kk_ref_get(_x20493,kk_context()); /*184*/
  x_17522 = kk_integer_unbox(_x20492); /*int*/
  kk_function_t next0_17523 = kk_std_core_new_unique_fun20494(_ctx); /*(int) -> <read<global>,write<global>|_16082> int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_17522, _ctx);
    kk_box_t _x20497 = kk_std_core_hnd_yield_extend(kk_std_core_new_unique_fun20498(next0_17523, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x20497);
  }
  {
    return kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17523, (next0_17523, x_17522, _ctx));
  }
}
 
// Get the value of the `Just` constructor or raise an exception


// lift anonymous function
struct kk_std_core_unjust_fun20503__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_unjust_fun20503(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_unjust_fun20503(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_unjust_fun20503, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_unjust_fun20503(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__exception _x20504;
  kk_string_t _x20505;
  kk_define_string_literal(, _s20506, 37, "unexpected Nothing in std/core/unjust")
  _x20505 = kk_string_dup(_s20506); /*string*/
  kk_std_core__exception_info _x20507;
  kk_std_core_types__optional _match_18964 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_18964)) {
    kk_box_t _box_x18735 = _match_18964._cons.Optional.value;
    kk_std_core__exception_info _info_11284 = kk_std_core__exception_info_unbox(_box_x18735, NULL);
    kk_std_core__exception_info_dup(_info_11284);
    kk_std_core_types__optional_drop(_match_18964, _ctx);
    _x20507 = _info_11284; /*exception-info*/
    goto _match20508;
  }
  {
    _x20507 = kk_std_core__new_ExnError(_ctx); /*exception-info*/
  }
  _match20508: ;
  _x20504 = kk_std_core__new_Exception(_x20505, _x20507, _ctx); /*exception*/
  return kk_std_core__exception_box(_x20504, _ctx);
}

kk_box_t kk_std_core_unjust(kk_std_core_types__maybe m, kk_context_t* _ctx) { /* forall<a> (m : maybe<a>) -> exn a */ 
  if (kk_std_core_types__is_Just(m)) {
    kk_box_t x = m._cons.Just.value;
    return x;
  }
  {
    kk_std_core_hnd__ev ev_17525;
    kk_ssize_t _x20501 = ((kk_ssize_t)0); /*ssize_t*/
    ev_17525 = kk_evv_at(_x20501,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
    kk_std_core__exception x0_17527;
    kk_box_t _x20502 = kk_std_core_hnd__open_none0(kk_std_core_new_unjust_fun20503(_ctx), _ctx); /*3214*/
    x0_17527 = kk_std_core__exception_unbox(_x20502, _ctx); /*exception*/
    {
      struct kk_std_core_hnd_Ev* _con20510 = kk_std_core_hnd__as_Ev(ev_17525);
      kk_std_core_hnd__marker m0 = _con20510->marker;
      kk_box_t _box_x18738 = _con20510->hnd;
      kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18738, NULL);
      kk_std_core__hnd_exn_dup(h);
      kk_std_core_hnd__clause1 _match_18963 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
      {
        kk_function_t _fun_unbox_x18742 = _match_18963.clause;
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18742, (_fun_unbox_x18742, m0, ev_17525, kk_std_core__exception_box(x0_17527, _ctx), _ctx));
      }
    }
  }
}
 
// lifted

kk_string_t kk_std_core__lift16755_unlines(kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20512 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18746 = _con20512->head;
    kk_std_core__list yy = _con20512->tail;
    kk_string_t y = kk_string_unbox(_box_x18746);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x20514;
      kk_string_t _x20515;
      kk_string_t _x20516;
      kk_define_string_literal(, _s20517, 1, "\n")
      _x20516 = kk_string_dup(_s20517); /*string*/
      _x20515 = kk_std_core__lp__plus__plus__1_rp_(_x20516, y, _ctx); /*string*/
      _x20514 = kk_std_core__lp__plus__plus__1_rp_(acc, _x20515, _ctx); /*string*/
      ys = yy;
      acc = _x20514;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// Join a list of strings with newlines

kk_string_t kk_std_core_unlines(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<string>) -> string */ 
  if (kk_std_core__is_Nil(xs)) {
    return kk_string_empty();
  }
  {
    struct kk_std_core_Cons* _con20519 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18747 = _con20519->head;
    kk_std_core__list xx = _con20519->tail;
    kk_string_t x = kk_string_unbox(_box_x18747);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    return kk_std_core__lift16755_unlines(xx, x, _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16757_unzip(kk_std_core__list acc, kk_std_core__list ys0, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys0 : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con20521 = kk_std_core__as_Cons(ys0);
    kk_box_t x0 = _con20521->head;
    kk_std_core__list xx0 = _con20521->tail;
    kk_reuse_t _ru_18940 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      _ru_18940 = (kk_std_core__list_reuse(ys0));
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(ys0, _ctx);
      _ru_18940 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20522;
      if (kk_likely(_ru_18940!=NULL)) {
        struct kk_std_core_Cons* _con20523 = (struct kk_std_core_Cons*)_ru_18940;
        _con20523->tail = acc;
        _x20522 = kk_std_core__base_Cons(_con20523); /*list<61>*/
      }
      else {
        _x20522 = kk_std_core__new_Cons(kk_reuse_null, x0, acc, _ctx); /*list<61>*/
      }
      acc = _x20522;
      ys0 = xx0;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16758_unzip(kk_std_core__list acc0, kk_std_core__list ys1, kk_context_t* _ctx) { /* forall<a> (acc0 : list<a>, ys1 : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys1)) {
    struct kk_std_core_Cons* _con20524 = kk_std_core__as_Cons(ys1);
    kk_box_t x1 = _con20524->head;
    kk_std_core__list xx1 = _con20524->tail;
    kk_reuse_t _ru_18941 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys1))) {
      _ru_18941 = (kk_std_core__list_reuse(ys1));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(ys1, _ctx);
      _ru_18941 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20525;
      if (kk_likely(_ru_18941!=NULL)) {
        struct kk_std_core_Cons* _con20526 = (struct kk_std_core_Cons*)_ru_18941;
        _con20526->tail = acc0;
        _x20525 = kk_std_core__base_Cons(_con20526); /*list<61>*/
      }
      else {
        _x20525 = kk_std_core__new_Cons(kk_reuse_null, x1, acc0, _ctx); /*list<61>*/
      }
      acc0 = _x20525;
      ys1 = xx1;
      goto kk__tailcall;
    }
  }
  {
    return acc0;
  }
}
 
// lifted
// todo: implement TRMC for multiple results

kk_std_core_types__tuple2_ kk_std_core__lift16756_unzip(kk_std_core__list ys, kk_std_core__list acc1, kk_std_core__list acc2, kk_context_t* _ctx) { /* forall<a,b> (ys : list<(a, b)>, acc1 : list<a>, acc2 : list<b>) -> (list<a>, list<b>) */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20527 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18748 = _con20527->head;
    kk_std_core__list xx = _con20527->tail;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x18748, NULL);
    kk_box_t x = _pat0.fst;
    kk_box_t y = _pat0.snd;
    kk_reuse_t _ru_18942 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_box_dup(x);
      kk_box_dup(y);
      kk_box_drop(_box_x18748, _ctx);
      _ru_18942 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_box_dup(y);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18942 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20529 = kk_std_core__new_Cons(_ru_18942, x, acc1, _ctx); /*list<61>*/
      kk_std_core__list _x20530 = kk_std_core__new_Cons(kk_reuse_null, y, acc2, _ctx); /*list<61>*/
      ys = xx;
      acc1 = _x20529;
      acc2 = _x20530;
      goto kk__tailcall;
    }
  }
  {
    kk_std_core__list _b_18751_18749 = kk_std_core__lift16757_unzip(kk_std_core__new_Nil(_ctx), acc1, _ctx); /*list<16150>*/;
    kk_std_core__list _b_18752_18750 = kk_std_core__lift16758_unzip(kk_std_core__new_Nil(_ctx), acc2, _ctx); /*list<16151>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18751_18749, _ctx), kk_std_core__list_box(_b_18752_18750, _ctx), _ctx);
  }
}
 
// Convert a string to a vector of characters.

kk_vector_t kk_std_core_vector_1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> vector<char> */ 
  return kk_string_to_chars(s,kk_context());
}
extern kk_box_t kk_std_core_vector_fun20532_2(kk_function_t _fself, kk_ssize_t ___wildcard__2100__30, kk_context_t* _ctx) {
  struct kk_std_core_vector_fun20532__t_2* _self = kk_function_as(struct kk_std_core_vector_fun20532__t_2*, _fself);
  kk_box_t default0 = _self->default0; /* 16227 */
  kk_drop_match(_self, {kk_box_dup(default0);}, {}, _ctx)
  return default0;
}
extern kk_box_t kk_std_core_vector_init_fun20534(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_vector_init_fun20534__t* _self = kk_function_as(struct kk_std_core_vector_init_fun20534__t*, _fself);
  kk_function_t f = _self->f; /* (int) -> 16296 */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  kk_integer_t _x20535 = kk_integer_from_ssize_t(i,kk_context()); /*int*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_context_t*), f, (f, _x20535, _ctx));
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17199_while(kk_function_t action, kk_function_t predicate, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : () -> <div|e> (), predicate : () -> <div|e> bool, wild_ : ()) -> <div|e> () */ 
  kk_std_core_while(predicate, action, _ctx); return kk_Unit;
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17200_while_fun20538__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_function_t predicate0;
};
static kk_box_t kk_std_core__mlift17200_while_fun20538(kk_function_t _fself, kk_box_t _b_18754, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17200_while_fun20538(kk_function_t action0, kk_function_t predicate0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17200_while_fun20538__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17200_while_fun20538__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17200_while_fun20538, kk_context());
  _self->action0 = action0;
  _self->predicate0 = predicate0;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17200_while_fun20538(kk_function_t _fself, kk_box_t _b_18754, kk_context_t* _ctx) {
  struct kk_std_core__mlift17200_while_fun20538__t* _self = kk_function_as(struct kk_std_core__mlift17200_while_fun20538__t*, _fself);
  kk_function_t action0 = _self->action0; /* () -> <div|16317> () */
  kk_function_t predicate0 = _self->predicate0; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action0);kk_function_dup(predicate0);}, {}, _ctx)
  kk_unit_t _x20539 = kk_Unit;
  kk_unit_t _x20540 = kk_Unit;
  kk_unit_unbox(_b_18754);
  kk_std_core__mlift17199_while(action0, predicate0, _x20540, _ctx);
  return kk_unit_box(_x20539);
}

kk_unit_t kk_std_core__mlift17200_while(kk_function_t action0, kk_function_t predicate0, bool _y_17104, kk_context_t* _ctx) { /* forall<e> (action : () -> <div|e> (), predicate : () -> <div|e> bool, bool) -> <div|e> () */ 
  if (_y_17104) {
    kk_unit_t x_17528 = kk_Unit;
    kk_function_t _x20536 = kk_function_dup(action0); /*() -> <div|16317> ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), _x20536, (_x20536, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20537 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17200_while_fun20538(action0, predicate0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20537); return kk_Unit;
    }
    {
      kk_std_core__mlift17199_while(action0, predicate0, x_17528, _ctx); return kk_Unit;
    }
  }
  {
    kk_function_drop(action0, _ctx);
    kk_function_drop(predicate0, _ctx);
    kk_Unit; return kk_Unit;
  }
}
 
// The `while` fun executes `action`  as long as `pred`  is `true`.


// lift anonymous function
struct kk_std_core_while_fun20543__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_function_t predicate1;
};
static kk_box_t kk_std_core_while_fun20543(kk_function_t _fself, kk_box_t _b_18758, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_while_fun20543(kk_function_t action1, kk_function_t predicate1, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20543__t* _self = kk_function_alloc_as(struct kk_std_core_while_fun20543__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_while_fun20543, kk_context());
  _self->action1 = action1;
  _self->predicate1 = predicate1;
  return &_self->_base;
}

static kk_box_t kk_std_core_while_fun20543(kk_function_t _fself, kk_box_t _b_18758, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20543__t* _self = kk_function_as(struct kk_std_core_while_fun20543__t*, _fself);
  kk_function_t action1 = _self->action1; /* () -> <div|16317> () */
  kk_function_t predicate1 = _self->predicate1; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action1);kk_function_dup(predicate1);}, {}, _ctx)
  kk_unit_t _x20544 = kk_Unit;
  bool _x20545 = kk_bool_unbox(_b_18758); /*bool*/
  kk_std_core__mlift17200_while(action1, predicate1, _x20545, _ctx);
  return kk_unit_box(_x20544);
}


// lift anonymous function
struct kk_std_core_while_fun20548__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_function_t predicate1;
};
static kk_box_t kk_std_core_while_fun20548(kk_function_t _fself, kk_box_t _b_18760, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_while_fun20548(kk_function_t action1, kk_function_t predicate1, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20548__t* _self = kk_function_alloc_as(struct kk_std_core_while_fun20548__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_while_fun20548, kk_context());
  _self->action1 = action1;
  _self->predicate1 = predicate1;
  return &_self->_base;
}

static kk_box_t kk_std_core_while_fun20548(kk_function_t _fself, kk_box_t _b_18760, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20548__t* _self = kk_function_as(struct kk_std_core_while_fun20548__t*, _fself);
  kk_function_t action1 = _self->action1; /* () -> <div|16317> () */
  kk_function_t predicate1 = _self->predicate1; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action1);kk_function_dup(predicate1);}, {}, _ctx)
  kk_unit_t _x20549 = kk_Unit;
  kk_unit_t _x20550 = kk_Unit;
  kk_unit_unbox(_b_18760);
  kk_std_core__mlift17199_while(action1, predicate1, _x20550, _ctx);
  return kk_unit_box(_x20549);
}

kk_unit_t kk_std_core_while(kk_function_t predicate1, kk_function_t action1, kk_context_t* _ctx) { /* forall<e> (predicate : () -> <div|e> bool, action : () -> <div|e> ()) -> <div|e> () */ 
  kk__tailcall: ;
  bool x0_17530;
  kk_function_t _x20541 = kk_function_dup(predicate1); /*() -> <div|16317> bool*/
  x0_17530 = kk_function_call(bool, (kk_function_t, kk_context_t*), _x20541, (_x20541, _ctx)); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20542 = kk_std_core_hnd_yield_extend(kk_std_core_new_while_fun20543(action1, predicate1, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20542); return kk_Unit;
  }
  if (x0_17530) {
    kk_unit_t x1_17533 = kk_Unit;
    kk_function_t _x20546 = kk_function_dup(action1); /*() -> <div|16317> ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), _x20546, (_x20546, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20547 = kk_std_core_hnd_yield_extend(kk_std_core_new_while_fun20548(action1, predicate1, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20547); return kk_Unit;
    }
    { // tailcall
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(action1, _ctx);
    kk_function_drop(predicate1, _ctx);
    kk_Unit; return kk_Unit;
  }
}
 
// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.

kk_std_core__list kk_std_core__ctail_zip(kk_std_core__list xs, kk_std_core__list ys, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a,b> (xs : list<a>, ys : list<b>, ctail<list<(a, b)>>) -> list<(a, b)> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20551 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20551->head;
    kk_std_core__list xx = _con20551->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20552 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20552->head;
      kk_std_core__list yy = _con20552->tail;
      kk_reuse_t _ru_18944 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18944 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18944 = kk_reuse_null;
      }
      kk_std_core__list _ctail_16845 = kk_std_core__list_hole(); /*list<(16360, 16361)>*/;
      kk_std_core__list _ctail_16846;
      kk_box_t _x20553;
      kk_std_core_types__tuple2_ _x20554 = kk_std_core_types__new_dash__lp__comma__rp_(x, y, _ctx); /*(6, 7)*/
      _x20553 = kk_std_core_types__tuple2__box(_x20554, _ctx); /*61*/
      _ctail_16846 = kk_std_core__new_Cons(_ru_18944, _x20553, _ctail_16845, _ctx); /*list<(16360, 16361)>*/
      { // tailcall
        kk_std_core_types__ctail _x20555;
        kk_box_t* _b_18781_18774 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16846)->tail)); /*cfield<list<(16360, 16361)>>*/;
        _x20555 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16846, _ctx)),_b_18781_18774); /*ctail<0>*/
        xs = xx;
        ys = yy;
        _acc = _x20555;
        goto kk__tailcall;
      }
    }
    {
      kk_box_drop(x, _ctx);
      kk_std_core__list_drop(xx, _ctx);
      kk_box_t _x20556 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20556, _ctx);
    }
  }
  {
    kk_std_core__list_drop(ys, _ctx);
    kk_box_t _x20557 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20557, _ctx);
  }
}
 
// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.

kk_std_core__list kk_std_core_zip(kk_std_core__list xs0, kk_std_core__list ys0, kk_context_t* _ctx) { /* forall<a,b> (xs : list<a>, ys : list<b>) -> list<(a, b)> */ 
  kk_std_core_types__ctail _x20558 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_zip(xs0, ys0, _x20558, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17201_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _ctail_16847, kk_context_t* _ctx) { /* forall<a,b,c,e> (ctail<list<c>>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  kk_std_core__list _ctail_16848 = kk_std_core__list_hole(); /*list<16408>*/;
  kk_std_core__list _ctail_16849 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16847, _ctail_16848, _ctx); /*list<16408>*/;
  kk_std_core_types__ctail _x20559;
  kk_box_t* _b_18797_18794 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16849)->tail)); /*cfield<list<16408>>*/;
  _x20559 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16849, _ctx)),_b_18797_18794); /*ctail<0>*/
  return kk_std_core__ctail_zipwith(xx, yy, f, _x20559, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17202_op_fun20560__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16852;
};
static kk_std_core__list kk_std_core__mlift17202_op_fun20560(kk_function_t _fself, kk_std_core__list _ctail_16851, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17202_op_fun20560(kk_function_t _accm, kk_box_t _ctail_16852, kk_context_t* _ctx) {
  struct kk_std_core__mlift17202_op_fun20560__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17202_op_fun20560__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17202_op_fun20560, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16852 = _ctail_16852;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17202_op_fun20560(kk_function_t _fself, kk_std_core__list _ctail_16851, kk_context_t* _ctx) {
  struct kk_std_core__mlift17202_op_fun20560__t* _self = kk_function_as(struct kk_std_core__mlift17202_op_fun20560__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<16408>) -> list<16408> */
  kk_box_t _ctail_16852 = _self->_ctail_16852; /* 16408 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16852);}, {}, _ctx)
  kk_std_core__list _x20561 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16852, _ctail_16851, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20561, _ctx));
}

kk_std_core__list kk_std_core__mlift17202_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_std_core__list yy0, kk_box_t _ctail_16852, kk_context_t* _ctx) { /* forall<a,b,c,e> ((list<c>) -> list<c>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  return kk_std_core__ctailm_zipwith(xx0, yy0, f0, kk_std_core__new_mlift17202_op_fun20560(_accm, _ctail_16852, _ctx), _ctx);
}
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.


// lift anonymous function
struct kk_std_core__ctail_zipwith_fun20566__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core__list yy1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_zipwith_fun20566(kk_function_t _fself, kk_box_t _b_18802, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_zipwith_fun20566(kk_function_t f1, kk_std_core__list xx1, kk_std_core__list yy1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_zipwith_fun20566__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_zipwith_fun20566__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_zipwith_fun20566, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->yy1 = yy1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_zipwith_fun20566(kk_function_t _fself, kk_box_t _b_18802, kk_context_t* _ctx) {
  struct kk_std_core__ctail_zipwith_fun20566__t* _self = kk_function_as(struct kk_std_core__ctail_zipwith_fun20566__t*, _fself);
  kk_function_t f1 = _self->f1; /* (16406, 16407) -> 16409 16408 */
  kk_std_core__list xx1 = _self->xx1; /* list<16406> */
  kk_std_core__list yy1 = _self->yy1; /* list<16407> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<16408>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core__list_dup(yy1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20567 = kk_std_core__mlift17201_op(_acc0, f1, xx1, yy1, _b_18802, _ctx); /*list<16408>*/
  return kk_std_core__list_box(_x20567, _ctx);
}

kk_std_core__list kk_std_core__ctail_zipwith(kk_std_core__list xs, kk_std_core__list ys, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, ctail<list<c>>) -> e list<c> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20562 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20562->head;
    kk_std_core__list xx1 = _con20562->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20563 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20563->head;
      kk_std_core__list yy1 = _con20563->tail;
      kk_reuse_t _ru_18946 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18946 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy1);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18946 = kk_reuse_null;
      }
      kk_box_t x0_17536;
      kk_function_t _x20564 = kk_function_dup(f1); /*(16406, 16407) -> 16409 16408*/
      x0_17536 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20564, (_x20564, x, y, _ctx)); /*16408*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18946, _ctx);
        kk_box_drop(x0_17536, _ctx);
        kk_box_t _x20565 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_zipwith_fun20566(f1, xx1, yy1, _acc0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20565, _ctx);
      }
      {
        kk_std_core__list _ctail_168480 = kk_std_core__list_hole(); /*list<16408>*/;
        kk_std_core__list _ctail_168490 = kk_std_core__new_Cons(_ru_18946, x0_17536, _ctail_168480, _ctx); /*list<16408>*/;
        { // tailcall
          kk_std_core_types__ctail _x20568;
          kk_box_t* _b_18816_18808 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168490)->tail)); /*cfield<list<16408>>*/;
          _x20568 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168490, _ctx)),_b_18816_18808); /*ctail<0>*/
          xs = xx1;
          ys = yy1;
          _acc0 = _x20568;
          goto kk__tailcall;
        }
      }
    }
    {
      kk_function_drop(f1, _ctx);
      kk_box_drop(x, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20569 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20569, _ctx);
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_std_core__list_drop(ys, _ctx);
    kk_box_t _x20570 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20570, _ctx);
  }
}
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.


// lift anonymous function
struct kk_std_core__ctailm_zipwith_fun20575__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
  kk_std_core__list yy2;
};
static kk_box_t kk_std_core__ctailm_zipwith_fun20575(kk_function_t _fself, kk_box_t _b_18826, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_zipwith_fun20575(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_std_core__list yy2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20575__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_zipwith_fun20575__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_zipwith_fun20575, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  _self->yy2 = yy2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_zipwith_fun20575(kk_function_t _fself, kk_box_t _b_18826, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20575__t* _self = kk_function_as(struct kk_std_core__ctailm_zipwith_fun20575__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<16408>) -> list<16408> */
  kk_function_t f2 = _self->f2; /* (16406, 16407) -> 16409 16408 */
  kk_std_core__list xx2 = _self->xx2; /* list<16406> */
  kk_std_core__list yy2 = _self->yy2; /* list<16407> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);kk_std_core__list_dup(yy2);}, {}, _ctx)
  kk_std_core__list _x20576 = kk_std_core__mlift17202_op(_accm0, f2, xx2, yy2, _b_18826, _ctx); /*list<16408>*/
  return kk_std_core__list_box(_x20576, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_zipwith_fun20578__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x2_17539;
};
static kk_std_core__list kk_std_core__ctailm_zipwith_fun20578(kk_function_t _fself, kk_std_core__list _ctail_168510, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_zipwith_fun20578(kk_function_t _accm0, kk_box_t x2_17539, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20578__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_zipwith_fun20578__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_zipwith_fun20578, kk_context());
  _self->_accm0 = _accm0;
  _self->x2_17539 = x2_17539;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_zipwith_fun20578(kk_function_t _fself, kk_std_core__list _ctail_168510, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20578__t* _self = kk_function_as(struct kk_std_core__ctailm_zipwith_fun20578__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<16408>) -> list<16408> */
  kk_box_t x2_17539 = _self->x2_17539; /* 16408 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x2_17539);}, {}, _ctx)
  kk_std_core__list _x20579 = kk_std_core__new_Cons(kk_reuse_null, x2_17539, _ctail_168510, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20579, _ctx));
}

kk_std_core__list kk_std_core__ctailm_zipwith(kk_std_core__list xs0, kk_std_core__list ys0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, (list<c>) -> list<c>) -> e list<c> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20571 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20571->head;
    kk_std_core__list xx2 = _con20571->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    if (kk_std_core__is_Cons(ys0)) {
      struct kk_std_core_Cons* _con20572 = kk_std_core__as_Cons(ys0);
      kk_box_t y0 = _con20572->head;
      kk_std_core__list yy2 = _con20572->tail;
      if (kk_likely(kk_std_core__list_is_unique(ys0))) {
        kk_std_core__list_free(ys0);
      }
      else {
        kk_box_dup(y0);
        kk_std_core__list_dup(yy2);
        kk_std_core__list_decref(ys0, _ctx);
      }
      kk_box_t x2_17539;
      kk_function_t _x20573 = kk_function_dup(f2); /*(16406, 16407) -> 16409 16408*/
      x2_17539 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20573, (_x20573, x1, y0, _ctx)); /*16408*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x2_17539, _ctx);
        kk_box_t _x20574 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_zipwith_fun20575(_accm0, f2, xx2, yy2, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20574, _ctx);
      }
      { // tailcall
        kk_function_t _x20577 = kk_std_core__new_ctailm_zipwith_fun20578(_accm0, x2_17539, _ctx); /*(list<16408>) -> list<16408>*/
        xs0 = xx2;
        ys0 = yy2;
        _accm0 = _x20577;
        goto kk__tailcall;
      }
    }
    {
      kk_function_drop(f2, _ctx);
      kk_box_drop(x1, _ctx);
      kk_std_core__list_drop(xx2, _ctx);
      return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
    }
  }
  {
    kk_function_drop(f2, _ctx);
    kk_std_core__list_drop(ys0, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.


// lift anonymous function
struct kk_std_core_zipwith_fun20581__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_zipwith_fun20581(kk_function_t _fself, kk_std_core__list _ctail_16850, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_zipwith_fun20581(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_zipwith_fun20581, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_zipwith_fun20581(kk_function_t _fself, kk_std_core__list _ctail_16850, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16850;
}

kk_std_core__list kk_std_core_zipwith(kk_std_core__list xs1, kk_std_core__list ys1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c) -> e list<c> */ 
  bool _match_18957 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_18957) {
    kk_std_core_types__ctail _x20580 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_zipwith(xs1, ys1, f3, _x20580, _ctx);
  }
  {
    return kk_std_core__ctailm_zipwith(xs1, ys1, f3, kk_std_core_new_zipwith_fun20581(_ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17203_zipwith_acc(kk_std_core__list acc, kk_function_t f, kk_integer_t i, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _y_17119, kk_context_t* _ctx) { /* forall<e,a,b,c> (acc : list<b>, f : (int, a, c) -> e b, i : int, xx : list<a>, yy : list<c>, b) -> e list<b> */ 
  kk_integer_t _x20582 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_std_core__list _x20583 = kk_std_core__new_Cons(kk_reuse_null, _y_17119, acc, _ctx); /*list<61>*/
  return kk_std_core_zipwith_acc(f, _x20582, _x20583, xx, yy, _ctx);
}


// lift anonymous function
struct kk_std_core_zipwith_acc_fun20589__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list xx0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core_zipwith_acc_fun20589(kk_function_t _fself, kk_box_t _b_18830, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_zipwith_acc_fun20589(kk_std_core__list acc0, kk_function_t f0, kk_integer_t i0, kk_std_core__list xx0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core_zipwith_acc_fun20589__t* _self = kk_function_alloc_as(struct kk_std_core_zipwith_acc_fun20589__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_zipwith_acc_fun20589, kk_context());
  _self->acc0 = acc0;
  _self->f0 = f0;
  _self->i0 = i0;
  _self->xx0 = xx0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core_zipwith_acc_fun20589(kk_function_t _fself, kk_box_t _b_18830, kk_context_t* _ctx) {
  struct kk_std_core_zipwith_acc_fun20589__t* _self = kk_function_as(struct kk_std_core_zipwith_acc_fun20589__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<16421> */
  kk_function_t f0 = _self->f0; /* (int, 16419, 16429) -> 16416 16421 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list xx0 = _self->xx0; /* list<16419> */
  kk_std_core__list yy0 = _self->yy0; /* list<16429> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(xx0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20590 = kk_std_core__mlift17203_zipwith_acc(acc0, f0, i0, xx0, yy0, _b_18830, _ctx); /*list<16421>*/
  return kk_std_core__list_box(_x20590, _ctx);
}

kk_std_core__list kk_std_core_zipwith_acc(kk_function_t f0, kk_integer_t i0, kk_std_core__list acc0, kk_std_core__list xs, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a,b,c,e> ((int, a, b) -> e c, int, list<c>, list<a>, list<b>) -> e list<c> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(i0, _ctx);
    kk_std_core__list_drop(ys, _ctx);
    return kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx);
  }
  {
    struct kk_std_core_Cons* _con20584 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20584->head;
    kk_std_core__list xx0 = _con20584->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Nil(ys)) {
      kk_function_drop(f0, _ctx);
      kk_integer_drop(i0, _ctx);
      kk_box_drop(x, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx);
    }
    {
      struct kk_std_core_Cons* _con20585 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20585->head;
      kk_std_core__list yy0 = _con20585->tail;
      kk_reuse_t _ru_18950 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18950 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy0);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18950 = kk_reuse_null;
      }
      kk_box_t x0_17544;
      kk_function_t _x20587 = kk_function_dup(f0); /*(int, 16419, 16429) -> 16416 16421*/
      kk_integer_t _x20586 = kk_integer_dup(i0); /*int*/
      x0_17544 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_box_t, kk_context_t*), _x20587, (_x20587, _x20586, x, y, _ctx)); /*16421*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18950, _ctx);
        kk_box_drop(x0_17544, _ctx);
        kk_box_t _x20588 = kk_std_core_hnd_yield_extend(kk_std_core_new_zipwith_acc_fun20589(acc0, f0, i0, xx0, yy0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20588, _ctx);
      }
      { // tailcall
        kk_integer_t _x20591 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/
        kk_std_core__list _x20592 = kk_std_core__new_Cons(_ru_18950, x0_17544, acc0, _ctx); /*list<61>*/
        i0 = _x20591;
        acc0 = _x20592;
        xs = xx0;
        ys = yy0;
        goto kk__tailcall;
      }
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17204_op(kk_box_t _y_17123, kk_std_core__list _y_17124, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17123, _y_17124, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17205_op_fun20594__t {
  struct kk_function_s _base;
  kk_box_t _y_171230;
};
static kk_box_t kk_std_core__mlift17205_op_fun20594(kk_function_t _fself, kk_box_t _b_18834, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17205_op_fun20594(kk_box_t _y_171230, kk_context_t* _ctx) {
  struct kk_std_core__mlift17205_op_fun20594__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17205_op_fun20594__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17205_op_fun20594, kk_context());
  _self->_y_171230 = _y_171230;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17205_op_fun20594(kk_function_t _fself, kk_box_t _b_18834, kk_context_t* _ctx) {
  struct kk_std_core__mlift17205_op_fun20594__t* _self = kk_function_as(struct kk_std_core__mlift17205_op_fun20594__t*, _fself);
  kk_box_t _y_171230 = _self->_y_171230; /* 16550 */
  kk_drop_match(_self, {kk_box_dup(_y_171230);}, {}, _ctx)
  kk_std_core__list _x20595;
  kk_std_core__list _x20596 = kk_std_core__list_unbox(_b_18834, _ctx); /*list<16550>*/
  _x20595 = kk_std_core__mlift17204_op(_y_171230, _x20596, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20595, _ctx);
}

kk_std_core__list kk_std_core__mlift17205_op(kk_function_t f, kk_integer_t i, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _y_171230, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (int, a, b) -> e c, i : int, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  kk_integer_t i0_16786 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17547 = kk_std_core__lift16759_zipwith_indexed(f, i0_16786, xx, yy, _ctx); /*list<16550>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17547, _ctx);
    kk_box_t _x20593 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17205_op_fun20594(_y_171230, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20593, _ctx);
  }
  {
    return kk_std_core__mlift17204_op(_y_171230, x_17547, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16759_zipwith_indexed_fun20602__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list xx0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20602(kk_function_t _fself, kk_box_t _b_18838, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16759_zipwith_indexed_fun20602(kk_function_t f0, kk_integer_t i0, kk_std_core__list xx0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20602__t* _self = kk_function_alloc_as(struct kk_std_core__lift16759_zipwith_indexed_fun20602__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16759_zipwith_indexed_fun20602, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->xx0 = xx0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20602(kk_function_t _fself, kk_box_t _b_18838, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20602__t* _self = kk_function_as(struct kk_std_core__lift16759_zipwith_indexed_fun20602__t*, _fself);
  kk_function_t f0 = _self->f0; /* (int, 16548, 16549) -> 16551 16550 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list xx0 = _self->xx0; /* list<16548> */
  kk_std_core__list yy0 = _self->yy0; /* list<16549> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(xx0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20603 = kk_std_core__mlift17205_op(f0, i0, xx0, yy0, _b_18838, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20603, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16759_zipwith_indexed_fun20605__t {
  struct kk_function_s _base;
  kk_box_t x1_17549;
};
static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20605(kk_function_t _fself, kk_box_t _b_18840, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16759_zipwith_indexed_fun20605(kk_box_t x1_17549, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20605__t* _self = kk_function_alloc_as(struct kk_std_core__lift16759_zipwith_indexed_fun20605__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16759_zipwith_indexed_fun20605, kk_context());
  _self->x1_17549 = x1_17549;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20605(kk_function_t _fself, kk_box_t _b_18840, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20605__t* _self = kk_function_as(struct kk_std_core__lift16759_zipwith_indexed_fun20605__t*, _fself);
  kk_box_t x1_17549 = _self->x1_17549; /* 16550 */
  kk_drop_match(_self, {kk_box_dup(x1_17549);}, {}, _ctx)
  kk_std_core__list _x20606;
  kk_std_core__list _x20607 = kk_std_core__list_unbox(_b_18840, _ctx); /*list<16550>*/
  _x20606 = kk_std_core__mlift17204_op(x1_17549, _x20607, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20606, _ctx);
}

kk_std_core__list kk_std_core__lift16759_zipwith_indexed(kk_function_t f0, kk_integer_t i0, kk_std_core__list xs, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (int, a, b) -> e c, i : int, xs : list<a>, ys : list<b>) -> e list<c> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20597 = kk_std_core__as_Cons(xs);
    kk_box_t x0 = _con20597->head;
    kk_std_core__list xx0 = _con20597->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20598 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20598->head;
      kk_std_core__list yy0 = _con20598->tail;
      kk_reuse_t _ru_18952 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18952 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy0);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18952 = kk_reuse_null;
      }
      kk_box_t x1_17549;
      kk_function_t _x20600 = kk_function_dup(f0); /*(int, 16548, 16549) -> 16551 16550*/
      kk_integer_t _x20599 = kk_integer_dup(i0); /*int*/
      x1_17549 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_box_t, kk_context_t*), _x20600, (_x20600, _x20599, x0, y, _ctx)); /*16550*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18952, _ctx);
        kk_box_drop(x1_17549, _ctx);
        kk_box_t _x20601 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16759_zipwith_indexed_fun20602(f0, i0, xx0, yy0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20601, _ctx);
      }
      {
        kk_integer_t i0_167860 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
        kk_std_core__list x2_17552 = kk_std_core__lift16759_zipwith_indexed(f0, i0_167860, xx0, yy0, _ctx); /*list<16550>*/;
        if (kk_yielding(kk_context())) {
          kk_reuse_drop(_ru_18952, _ctx);
          kk_std_core__list_drop(x2_17552, _ctx);
          kk_box_t _x20604 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16759_zipwith_indexed_fun20605(x1_17549, _ctx), _ctx); /*3860*/
          return kk_std_core__list_unbox(_x20604, _ctx);
        }
        {
          return kk_std_core__new_Cons(_ru_18952, x1_17549, x2_17552, _ctx);
        }
      }
    }
    {
      kk_function_drop(f0, _ctx);
      kk_integer_drop(i0, _ctx);
      kk_box_drop(x0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return kk_std_core__new_Nil(_ctx);
    }
  }
  {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(i0, _ctx);
    kk_std_core__list_drop(ys, _ctx);
    return kk_std_core__new_Nil(_ctx);
  }
}

// initialization
void kk_std_core__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  kk_std_core_types__init(_ctx);
  kk_std_core_hnd__init(_ctx);
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
  {
    kk_string_t _x19202;
    kk_define_string_literal(, _s19203, 8, "exn.core")
    _x19202 = kk_string_dup(_s19203); /*string*/
    kk_std_core__tag_exn = kk_std_core_hnd__new_Htag(_x19202, _ctx); /*std/core/hnd/htag<.hnd-exn>*/
  }
  {
    kk_std_core_redirect = kk_ref_alloc((kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx)),kk_context()); /*ref<global,maybe<(string) -> console ()>>*/
  }
  {
    kk_std_core_maxListStack = kk_integer_from_small(50); /*int*/
  }
  {
    kk_std_core_trace_enabled = kk_ref_alloc((kk_bool_box(true)),kk_context()); /*ref<global,bool>*/
  }
  {
    kk_std_core_unique_count = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*ref<global,int>*/
  }
  {
    kk_string_t _x19940 = kk_string_empty(); /*string*/
    kk_ssize_t _x19942 = ((kk_ssize_t)0); /*ssize_t*/
    kk_ssize_t _x19943 = ((kk_ssize_t)0); /*ssize_t*/
    kk_std_core_empty = kk_std_core__new_Sslice(_x19940, _x19942, _x19943, _ctx); /*sslice*/
  }
  {
    kk_string_t _x20159 = kk_string_empty(); /*string*/
    kk_ssize_t _x20161 = ((kk_ssize_t)-1); /*ssize_t*/
    kk_ssize_t _x20162 = ((kk_ssize_t)0); /*ssize_t*/
    kk_std_core_invalid = kk_std_core__new_Sslice(_x20159, _x20161, _x20162, _ctx); /*sslice*/
  }
}

// termination
void kk_std_core__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_core__sslice_drop(kk_std_core_invalid, _ctx);
  kk_std_core__sslice_drop(kk_std_core_empty, _ctx);
  kk_ref_drop(kk_std_core_unique_count, _ctx);
  kk_ref_drop(kk_std_core_trace_enabled, _ctx);
  kk_integer_drop(kk_std_core_maxListStack, _ctx);
  kk_ref_drop(kk_std_core_redirect, _ctx);
  kk_std_core_hnd__htag_drop(kk_std_core__tag_exn, _ctx);
  kk_std_core_hnd__done(_ctx);
  kk_std_core_types__done(_ctx);
}
