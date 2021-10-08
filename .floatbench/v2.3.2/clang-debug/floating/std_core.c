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
  kk_string_t _x19202;
  if (kk_std_core_types__is_Optional(message0)) {
    kk_box_t _box_x17638 = message0._cons.Optional.value;
    kk_string_t _message_1683 = kk_string_unbox(_box_x17638);
    kk_string_dup(_message_1683);
    kk_std_core_types__optional_drop(message0, _ctx);
    _x19202 = _message_1683; /*string*/
    goto _match19203;
  }
  {
    kk_string_t _x = _this.message;
    kk_string_dup(_x);
    _x19202 = _x; /*string*/
  }
  _match19203: ;
  kk_std_core__exception_info _x19205;
  if (kk_std_core_types__is_Optional(info0)) {
    kk_box_t _box_x17639 = info0._cons.Optional.value;
    kk_std_core__exception_info _info_1689 = kk_std_core__exception_info_unbox(_box_x17639, NULL);
    kk_std_core__exception_info_dup(_info_1689);
    kk_std_core_types__optional_drop(info0, _ctx);
    kk_std_core__exception_drop(_this, _ctx);
    _x19205 = _info_1689; /*exception-info*/
    goto _match19206;
  }
  {
    kk_std_core__exception_info _x0 = _this.info;
    kk_std_core__exception_info_dup(_x0);
    kk_std_core__exception_drop(_this, _ctx);
    _x19205 = _x0; /*exception-info*/
  }
  _match19206: ;
  return kk_std_core__new_Exception(_x19202, _x19205, _ctx);
}

kk_std_core__sslice kk_std_core__copy_2(kk_std_core__sslice _this, kk_std_core_types__optional str0, kk_std_core_types__optional start0, kk_std_core_types__optional len0, kk_context_t* _ctx) { /* (sslice, str : optional<string>, start : optional<ssize_t>, len : optional<ssize_t>) -> sslice */ 
  kk_string_t _x19212;
  if (kk_std_core_types__is_Optional(str0)) {
    kk_box_t _box_x17641 = str0._cons.Optional.value;
    kk_string_t _str_1821 = kk_string_unbox(_box_x17641);
    kk_string_dup(_str_1821);
    kk_std_core_types__optional_drop(str0, _ctx);
    _x19212 = _str_1821; /*string*/
    goto _match19213;
  }
  {
    kk_string_t _x = _this.str;
    kk_string_dup(_x);
    _x19212 = _x; /*string*/
  }
  _match19213: ;
  kk_ssize_t _x19215;
  if (kk_std_core_types__is_Optional(start0)) {
    kk_box_t _box_x17642 = start0._cons.Optional.value;
    kk_ssize_t _start_1827 = kk_ssize_unbox(_box_x17642, NULL);
    kk_std_core_types__optional_drop(start0, _ctx);
    _x19215 = _start_1827; /*ssize_t*/
    goto _match19216;
  }
  {
    kk_ssize_t _x0 = _this.start;
    _x19215 = _x0; /*ssize_t*/
  }
  _match19216: ;
  kk_ssize_t _x19218;
  if (kk_std_core_types__is_Optional(len0)) {
    kk_box_t _box_x17643 = len0._cons.Optional.value;
    kk_ssize_t _len_1833 = kk_ssize_unbox(_box_x17643, NULL);
    kk_std_core_types__optional_drop(len0, _ctx);
    kk_std_core__sslice_drop(_this, _ctx);
    _x19218 = _len_1833; /*ssize_t*/
    goto _match19219;
  }
  {
    kk_ssize_t _x1 = _this.len;
    kk_std_core__sslice_drop(_this, _ctx);
    _x19218 = _x1; /*ssize_t*/
  }
  _match19219: ;
  return kk_std_core__new_Sslice(_x19212, _x19215, _x19218, _ctx);
}

kk_std_core__stream kk_std_core__copy_3(kk_std_core__stream _this, kk_std_core_types__optional head0, kk_std_core_types__optional tail0, kk_context_t* _ctx) { /* forall<a> (stream<a>, head : optional<a>, tail : optional<stream<a>>) -> stream<a> */ 
  kk_box_t _x19223;
  if (kk_std_core_types__is_Optional(head0)) {
    kk_box_t _head_1879 = head0._cons.Optional.value;
    _x19223 = _head_1879; /*1896*/
  }
  else {
    struct kk_std_core_Next* _con19224 = kk_std_core__as_Next(_this);
    kk_box_t _x = _con19224->head;
    kk_box_dup(_x);
    _x19223 = _x; /*1896*/
  }
  kk_std_core__stream _x19225;
  if (kk_std_core_types__is_Optional(tail0)) {
    kk_box_t _box_x17644 = tail0._cons.Optional.value;
    kk_std_core__stream _tail_1886 = kk_std_core__stream_unbox(_box_x17644, NULL);
    kk_std_core__stream_dup(_tail_1886);
    kk_std_core_types__optional_drop(tail0, _ctx);
    kk_std_core__stream_drop(_this, _ctx);
    _x19225 = _tail_1886; /*stream<1896>*/
    goto _match19226;
  }
  {
    struct kk_std_core_Next* _con19228 = kk_std_core__as_Next(_this);
    kk_box_t _pat01 = _con19228->head;
    kk_std_core__stream _x0 = _con19228->tail;
    if (kk_likely(kk_std_core__stream_is_unique(_this))) {
      kk_box_drop(_pat01, _ctx);
      kk_std_core__stream_free(_this);
    }
    else {
      kk_std_core__stream_dup(_x0);
      kk_std_core__stream_decref(_this, _ctx);
    }
    _x19225 = _x0; /*stream<1896>*/
  }
  _match19226: ;
  return kk_std_core__new_Next(kk_reuse_null, _x19223, _x19225, _ctx);
}
 
// runtime tag for the `:exn` effect

kk_std_core_hnd__htag kk_std_core__tag_exn;
 
// handler for the `:exn` effect

kk_box_t kk_std_core__handle_exn(int32_t cfc, kk_std_core__hnd_exn hnd, kk_function_t ret, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e,b> (cfc : int32, hnd : .hnd-exn<e,b>, ret : (res : a) -> e b, action : () -> <exn|e> a) -> e b */ 
  kk_std_core_hnd__htag _x19232 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
  return kk_std_core_hnd__hhandle(_x19232, cfc, kk_std_core__hnd_exn_box(hnd, _ctx), ret, action, _ctx);
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
  kk_std_core__list _x19235;
  if (kk_std_core_types__is_Optional(tail0)) {
    kk_box_t _box_x17658 = tail0._cons.Optional.value;
    kk_std_core__list _tail_2090 = kk_std_core__list_unbox(_box_x17658, NULL);
    kk_std_core__list_dup(_tail_2090);
    kk_std_core_types__optional_drop(tail0, _ctx);
    _x19235 = _tail_2090; /*list<2100>*/
    goto _match19236;
  }
  {
    _x19235 = kk_std_core__new_Nil(_ctx); /*list<2100>*/
  }
  _match19236: ;
  return kk_vector_to_list(v,_x19235,kk_context());
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
  kk_integer_t _x19238;
  if (kk_std_core_types__is_Optional(upto)) {
    kk_box_t _box_x17659 = upto._cons.Optional.value;
    kk_integer_t _upto_2194 = kk_integer_unbox(_box_x17659);
    kk_integer_dup(_upto_2194);
    kk_std_core_types__optional_drop(upto, _ctx);
    _x19238 = _upto_2194; /*int*/
    goto _match19239;
  }
  {
    _x19238 = kk_integer_from_small(-1); /*int*/
  }
  _match19239: ;
  return kk_slice_common_prefix(s,t,_x19238,kk_context());
}
 
// lifted

kk_std_core__list kk_std_core__ctail_lift16732_concat(kk_std_core__list ys, kk_std_core__list zss, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, zss : list<list<a>>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19241 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19241->head;
    kk_std_core__list yy = _con19241->tail;
    kk_reuse_t _ru_18879 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18879 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18879 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16789 = kk_std_core__list_hole(); /*list<2242>*/;
    kk_std_core__list _ctail_16790;
    if (kk_likely(_ru_18879!=NULL)) {
      struct kk_std_core_Cons* _con19242 = (struct kk_std_core_Cons*)_ru_18879;
      _con19242->tail = _ctail_16789;
      _ctail_16790 = kk_std_core__base_Cons(_con19242); /*list<2242>*/
    }
    else {
      _ctail_16790 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16789, _ctx); /*list<2242>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19243;
      kk_box_t* _b_17671_17665 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16790)->tail)); /*cfield<list<2242>>*/;
      _x19243 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16790, _ctx)),_b_17671_17665); /*ctail<0>*/
      ys = yy;
      _acc = _x19243;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zss)) {
    struct kk_std_core_Cons* _con19244 = kk_std_core__as_Cons(zss);
    kk_box_t _box_x17666 = _con19244->head;
    kk_std_core__list zzs = _con19244->tail;
    kk_std_core__list zs = kk_std_core__list_unbox(_box_x17666, NULL);
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
    kk_box_t _x19246 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19246, _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16732_concat(kk_std_core__list ys0, kk_std_core__list zss0, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, zss : list<list<a>>) -> list<a> */ 
  kk_std_core_types__ctail _x19247 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_lift16732_concat(ys0, zss0, _x19247, _ctx);
}
extern kk_box_t kk_std_core_const_fun19248_1(kk_function_t _fself, kk_box_t ___wildcard__122__7, kk_context_t* _ctx) {
  struct kk_std_core_const_fun19248__t_1* _self = kk_function_as(struct kk_std_core_const_fun19248__t_1*, _fself);
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

kk_std_core__list kk_std_core__mlift17129_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list zz, kk_std_core__list ys1_16762, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e list<b>, zz : list<a>, ys1.16762 : list<b>) -> e list<b> */ 
  return kk_std_core__ctail_lift16733_flatmap(f, ys1_16762, zz, _acc, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17130_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list zz0, kk_std_core__list ys1_167620, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e list<b>, zz : list<a>, ys1.16762 : list<b>) -> e list<b> */ 
  return kk_std_core__ctailm_lift16733_flatmap(f0, ys1_167620, zz0, _accm, _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctail_lift16733_flatmap_fun19255__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list zz1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_lift16733_flatmap_fun19255(kk_function_t _fself, kk_box_t _b_17686, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_lift16733_flatmap_fun19255(kk_function_t f1, kk_std_core__list zz1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16733_flatmap_fun19255__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_lift16733_flatmap_fun19255__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_lift16733_flatmap_fun19255, kk_context());
  _self->f1 = f1;
  _self->zz1 = zz1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_lift16733_flatmap_fun19255(kk_function_t _fself, kk_box_t _b_17686, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16733_flatmap_fun19255__t* _self = kk_function_as(struct kk_std_core__ctail_lift16733_flatmap_fun19255__t*, _fself);
  kk_function_t f1 = _self->f1; /* (2419) -> 2421 list<2420> */
  kk_std_core__list zz1 = _self->zz1; /* list<2419> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<2420>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(zz1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19256;
  kk_std_core__list _x19257 = kk_std_core__list_unbox(_b_17686, _ctx); /*list<2420>*/
  _x19256 = kk_std_core__mlift17129_op(_acc0, f1, zz1, _x19257, _ctx); /*list<2420>*/
  return kk_std_core__list_box(_x19256, _ctx);
}

kk_std_core__list kk_std_core__ctail_lift16733_flatmap(kk_function_t f1, kk_std_core__list ys, kk_std_core__list zs, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19249 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19249->head;
    kk_std_core__list yy = _con19249->tail;
    kk_reuse_t _ru_18881 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18881 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18881 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16791 = kk_std_core__list_hole(); /*list<2420>*/;
    kk_std_core__list _ctail_16792;
    if (kk_likely(_ru_18881!=NULL)) {
      struct kk_std_core_Cons* _con19250 = (struct kk_std_core_Cons*)_ru_18881;
      _con19250->tail = _ctail_16791;
      _ctail_16792 = kk_std_core__base_Cons(_con19250); /*list<2420>*/
    }
    else {
      _ctail_16792 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16791, _ctx); /*list<2420>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19251;
      kk_box_t* _b_17691_17684 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16792)->tail)); /*cfield<list<2420>>*/;
      _x19251 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_16792, _ctx)),_b_17691_17684); /*ctail<0>*/
      ys = yy;
      _acc0 = _x19251;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zs)) {
    struct kk_std_core_Cons* _con19252 = kk_std_core__as_Cons(zs);
    kk_box_t z = _con19252->head;
    kk_std_core__list zz1 = _con19252->tail;
    if (kk_likely(kk_std_core__list_is_unique(zs))) {
      kk_std_core__list_free(zs);
    }
    else {
      kk_box_dup(z);
      kk_std_core__list_dup(zz1);
      kk_std_core__list_decref(zs, _ctx);
    }
    kk_std_core__list x_17201;
    kk_function_t _x19253 = kk_function_dup(f1); /*(2419) -> 2421 list<2420>*/
    x_17201 = kk_function_call(kk_std_core__list, (kk_function_t, kk_box_t, kk_context_t*), _x19253, (_x19253, z, _ctx)); /*list<2420>*/
    if (kk_yielding(kk_context())) {
      kk_std_core__list_drop(x_17201, _ctx);
      kk_box_t _x19254 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_lift16733_flatmap_fun19255(f1, zz1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19254, _ctx);
    }
    { // tailcall
      ys = x_17201;
      zs = zz1;
      goto kk__tailcall;
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19258 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19258, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctailm_lift16733_flatmap_fun19261__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__ctailm_lift16733_flatmap_fun19261(kk_function_t _fself, kk_std_core__list _ctail_16794, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16733_flatmap_fun19261(kk_function_t _accm0, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19261__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19261__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16733_flatmap_fun19261, kk_context());
  _self->_accm0 = _accm0;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_lift16733_flatmap_fun19261(kk_function_t _fself, kk_std_core__list _ctail_16794, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19261__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19261__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2420>) -> list<2420> */
  kk_box_t y0 = _self->y0; /* 2420 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x19262 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16794, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19262, _ctx));
}


// lift anonymous function
struct kk_std_core__ctailm_lift16733_flatmap_fun19266__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list zz2;
};
static kk_box_t kk_std_core__ctailm_lift16733_flatmap_fun19266(kk_function_t _fself, kk_box_t _b_17700, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16733_flatmap_fun19266(kk_function_t _accm0, kk_function_t f2, kk_std_core__list zz2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19266__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19266__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16733_flatmap_fun19266, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->zz2 = zz2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_lift16733_flatmap_fun19266(kk_function_t _fself, kk_box_t _b_17700, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16733_flatmap_fun19266__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16733_flatmap_fun19266__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2420>) -> list<2420> */
  kk_function_t f2 = _self->f2; /* (2419) -> 2421 list<2420> */
  kk_std_core__list zz2 = _self->zz2; /* list<2419> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(zz2);}, {}, _ctx)
  kk_std_core__list _x19267;
  kk_std_core__list _x19268 = kk_std_core__list_unbox(_b_17700, _ctx); /*list<2420>*/
  _x19267 = kk_std_core__mlift17130_op(_accm0, f2, zz2, _x19268, _ctx); /*list<2420>*/
  return kk_std_core__list_box(_x19267, _ctx);
}

kk_std_core__list kk_std_core__ctailm_lift16733_flatmap(kk_function_t f2, kk_std_core__list ys0, kk_std_core__list zs0, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con19259 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con19259->head;
    kk_std_core__list yy0 = _con19259->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_box_dup(y0);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys0, _ctx);
    }
    { // tailcall
      kk_function_t _x19260 = kk_std_core__new_ctailm_lift16733_flatmap_fun19261(_accm0, y0, _ctx); /*(list<2420>) -> list<2420>*/
      ys0 = yy0;
      _accm0 = _x19260;
      goto kk__tailcall;
    }
  }
  if (kk_std_core__is_Cons(zs0)) {
    struct kk_std_core_Cons* _con19263 = kk_std_core__as_Cons(zs0);
    kk_box_t z0 = _con19263->head;
    kk_std_core__list zz2 = _con19263->tail;
    if (kk_likely(kk_std_core__list_is_unique(zs0))) {
      kk_std_core__list_free(zs0);
    }
    else {
      kk_box_dup(z0);
      kk_std_core__list_dup(zz2);
      kk_std_core__list_decref(zs0, _ctx);
    }
    kk_std_core__list x0_17204;
    kk_function_t _x19264 = kk_function_dup(f2); /*(2419) -> 2421 list<2420>*/
    x0_17204 = kk_function_call(kk_std_core__list, (kk_function_t, kk_box_t, kk_context_t*), _x19264, (_x19264, z0, _ctx)); /*list<2420>*/
    if (kk_yielding(kk_context())) {
      kk_std_core__list_drop(x0_17204, _ctx);
      kk_box_t _x19265 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_lift16733_flatmap_fun19266(_accm0, f2, zz2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19265, _ctx);
    }
    { // tailcall
      ys0 = x0_17204;
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
struct kk_std_core__lift16733_flatmap_fun19270__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core__lift16733_flatmap_fun19270(kk_function_t _fself, kk_std_core__list _ctail_16793, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16733_flatmap_fun19270(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__lift16733_flatmap_fun19270, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core__lift16733_flatmap_fun19270(kk_function_t _fself, kk_std_core__list _ctail_16793, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16793;
}

kk_std_core__list kk_std_core__lift16733_flatmap(kk_function_t f3, kk_std_core__list ys1, kk_std_core__list zs1, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>) -> e list<b> */ 
  bool _match_19190 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19190) {
    kk_std_core_types__ctail _x19269 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_lift16733_flatmap(f3, ys1, zs1, _x19269, _ctx);
  }
  {
    return kk_std_core__ctailm_lift16733_flatmap(f3, ys1, zs1, kk_std_core__new_lift16733_flatmap_fun19270(_ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16734_reverse_append(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19271 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con19271->head;
    kk_std_core__list xx = _con19271->tail;
    kk_reuse_t _ru_18885 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18885 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18885 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x19272;
      if (kk_likely(_ru_18885!=NULL)) {
        struct kk_std_core_Cons* _con19273 = (struct kk_std_core_Cons*)_ru_18885;
        _con19273->tail = acc;
        _x19272 = kk_std_core__base_Cons(_con19273); /*list<61>*/
      }
      else {
        _x19272 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x19272;
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
struct kk_std_core__mlift17132_force_fun19277__t {
  struct kk_function_s _base;
  kk_ref_t r;
};
static kk_box_t kk_std_core__mlift17132_force_fun19277(kk_function_t _fself, kk_box_t x0, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17132_force_fun19277(kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core__mlift17132_force_fun19277__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17132_force_fun19277__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17132_force_fun19277, kk_context());
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17132_force_fun19277(kk_function_t _fself, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17132_force_fun19277__t* _self = kk_function_as(struct kk_std_core__mlift17132_force_fun19277__t*, _fself);
  kk_ref_t r = _self->r; /* ref<global,either<() -> 2556 2555,2555>> */
  kk_drop_match(_self, {kk_ref_dup(r);}, {}, _ctx)
  kk_unit_t __ = kk_Unit;
  kk_box_t _x19278;
  kk_std_core_types__either _x19279;
  kk_box_t _x19280 = kk_box_dup(x0); /*2555*/
  _x19279 = kk_std_core_types__new_Right(_x19280, _ctx); /*either<72,73>*/
  _x19278 = kk_std_core_types__either_box(_x19279, _ctx); /*171*/
  kk_ref_set(r,_x19278,kk_context());
  return x0;
}

kk_box_t kk_std_core__mlift17132_force(kk_ref_t r, kk_function_t _y_16865, kk_context_t* _ctx) { /* forall<a,e> (r : ref<global,either<() -> e a,a>>, () -> <st<global>,div|e> a) -> <alloc<global>,div,read<global>,write<global>|e> a */ 
  kk_box_t x_17207 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), _y_16865, (_y_16865, _ctx)); /*2555*/;
  kk_function_t next0_17208 = kk_std_core__new_mlift17132_force_fun19277(r, _ctx); /*(2555) -> <st<global>,div|2556> 2555*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17207, _ctx);
    return kk_std_core_hnd_yield_extend(next0_17208, _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), next0_17208, (next0_17208, x_17207, _ctx));
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17133_force_fun19281__t {
  struct kk_function_s _base;
  kk_ref_t r;
};
static kk_box_t kk_std_core__mlift17133_force_fun19281(kk_function_t _fself, kk_box_t _b_17715, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17133_force_fun19281(kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core__mlift17133_force_fun19281__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17133_force_fun19281__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17133_force_fun19281, kk_context());
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17133_force_fun19281(kk_function_t _fself, kk_box_t _b_17715, kk_context_t* _ctx) {
  struct kk_std_core__mlift17133_force_fun19281__t* _self = kk_function_as(struct kk_std_core__mlift17133_force_fun19281__t*, _fself);
  kk_ref_t r = _self->r; /* ref<global,either<() -> 2556 2555,2555>> */
  kk_drop_match(_self, {kk_ref_dup(r);}, {}, _ctx)
  kk_function_t _x19282 = kk_function_unbox(_b_17715); /*() -> <st<global>,div|2556> 17716*/
  return kk_std_core__mlift17132_force(r, _x19282, _ctx);
}

kk_box_t kk_std_core__mlift17133_force(kk_ref_t r, kk_std_core_types__either _y_16863, kk_context_t* _ctx) { /* forall<a,e> (r : ref<global,either<() -> e a,a>>, either<() -> e a,a>) -> <read<global>,div,alloc<global>,write<global>|e> a */ 
  if (kk_std_core_types__is_Right(_y_16863)) {
    kk_box_t x = _y_16863._cons.Right.right;
    kk_ref_drop(r, _ctx);
    return x;
  }
  {
    kk_box_t _fun_unbox_x17712 = _y_16863._cons.Left.left;
    if (kk_yielding(kk_context())) {
      kk_box_drop(_fun_unbox_x17712, _ctx);
      return kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17133_force_fun19281(r, _ctx), _ctx);
    }
    {
      kk_function_t _x19283 = kk_function_unbox(_fun_unbox_x17712); /*() -> 2556 17713*/
      return kk_std_core__mlift17132_force(r, _x19283, _ctx);
    }
  }
}
 
// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.


// lift anonymous function
struct kk_std_core_force_fun19286__t {
  struct kk_function_s _base;
  kk_std_core__delayed delayed;
};
static kk_box_t kk_std_core_force_fun19286(kk_function_t _fself, kk_box_t _b_17722, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_force_fun19286(kk_std_core__delayed delayed, kk_context_t* _ctx) {
  struct kk_std_core_force_fun19286__t* _self = kk_function_alloc_as(struct kk_std_core_force_fun19286__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_force_fun19286, kk_context());
  _self->delayed = delayed;
  return &_self->_base;
}

static kk_box_t kk_std_core_force_fun19286(kk_function_t _fself, kk_box_t _b_17722, kk_context_t* _ctx) {
  struct kk_std_core_force_fun19286__t* _self = kk_function_as(struct kk_std_core_force_fun19286__t*, _fself);
  kk_std_core__delayed delayed = _self->delayed; /* delayed<2556,2555> */
  kk_drop_match(_self, {kk_std_core__delayed_dup(delayed);}, {}, _ctx)
  kk_std_core_types__either _y_17724_16863 = kk_std_core_types__either_unbox(_b_17722, _ctx); /*either<() -> 2556 2555,2555>*/;
  kk_ref_t _x19287;
  {
    kk_ref_t _x0 = delayed.dref;
    _x19287 = _x0; /*ref<global,either<() -> 2556 2555,2555>>*/
  }
  return kk_std_core__mlift17133_force(_x19287, _y_17724_16863, _ctx);
}

kk_box_t kk_std_core_force(kk_std_core__delayed delayed, kk_context_t* _ctx) { /* forall<a,e> (delayed : delayed<e,a>) -> e a */ 
  kk_std_core_types__either x_17216;
  kk_box_t _x19284;
  kk_ref_t _x19285;
  {
    kk_ref_t _x = delayed.dref;
    kk_ref_dup(_x);
    _x19285 = _x; /*ref<global,either<() -> 2556 2555,2555>>*/
  }
  _x19284 = kk_ref_get(_x19285,kk_context()); /*184*/
  x_17216 = kk_std_core_types__either_unbox(_x19284, _ctx); /*either<() -> 2556 2555,2555>*/
  if (kk_yielding(kk_context())) {
    kk_std_core_types__either_drop(x_17216, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_core_new_force_fun19286(delayed, _ctx), _ctx);
  }
  {
    kk_std_core_types__either _y_17725_16863 = x_17216; /*either<() -> 2556 2555,2555>*/;
    kk_ref_t _x19288;
    {
      kk_ref_t _x0 = delayed.dref;
      _x19288 = _x0; /*ref<global,either<() -> 2556 2555,2555>>*/
    }
    return kk_std_core__mlift17133_force(_x19288, _y_17725_16863, _ctx);
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
    struct kk_std_core_Cons* _con19289 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19289->head;
    kk_std_core__list yy = _con19289->tail;
    kk_reuse_t _ru_18886 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18886 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18886 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16795;
    kk_std_core__list _ru_19193 = kk_std_core__list_hole(); /*list<2587>*/;
    if (kk_likely(_ru_18886!=NULL)) {
      struct kk_std_core_Cons* _con19290 = (struct kk_std_core_Cons*)_ru_18886;
      _con19290->tail = _ru_19193;
      _ctail_16795 = kk_std_core__base_Cons(_con19290); /*list<2587>*/
    }
    else {
      _ctail_16795 = kk_std_core__new_Cons(kk_reuse_null, y, _ru_19193, _ctx); /*list<2587>*/
    }
    { // tailcall
      kk_box_t _x19291 = kk_box_dup(s); /*2587*/
      kk_std_core_types__ctail _x19292;
      kk_box_t* _b_17736_17731 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16795)->tail)); /*cfield<list<2587>>*/;
      kk_box_t _x19293;
      kk_std_core__list _x19294 = kk_std_core__new_Cons(kk_reuse_null, s, _ctail_16795, _ctx); /*list<61>*/
      _x19293 = kk_std_core__list_box(_x19294, _ctx); /*0*/
      _x19292 = kk_ctail_link(_acc,_x19293,_b_17736_17731); /*ctail<0>*/
      ys = yy;
      s = _x19291;
      _acc = _x19292;
      goto kk__tailcall;
    }
  }
  {
    kk_box_drop(s, _ctx);
    kk_box_t _x19295 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19295, _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16735_intersperse(kk_std_core__list ys0, kk_box_t s0, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, s : a) -> list<a> */ 
  kk_std_core_types__ctail _x19296 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_lift16735_intersperse(ys0, s0, _x19296, _ctx);
}
 
// Insert a separator `sep`  between all elements of a list `xs` .

kk_std_core__list kk_std_core_intersperse(kk_std_core__list xs, kk_box_t sep, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, sep : a) -> list<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19297 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19297->head;
    kk_std_core__list xx = _con19297->tail;
    kk_reuse_t _ru_18887 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18887 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18887 = kk_reuse_null;
    }
    kk_std_core__list _ru_19194 = kk_std_core__lift16735_intersperse(xx, sep, _ctx); /*list<2587>*/;
    if (kk_likely(_ru_18887!=NULL)) {
      struct kk_std_core_Cons* _con19298 = (struct kk_std_core_Cons*)_ru_18887;
      _con19298->tail = _ru_19194;
      return kk_std_core__base_Cons(_con19298);
    }
    {
      return kk_std_core__new_Cons(kk_reuse_null, x, _ru_19194, _ctx);
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

kk_std_core__list kk_std_core__mlift17134_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list yy, kk_box_t _ctail_16797, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */ 
  kk_std_core__list _ctail_16798 = kk_std_core__list_hole(); /*list<2728>*/;
  kk_std_core__list _ctail_16799 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16797, _ctail_16798, _ctx); /*list<2728>*/;
  kk_std_core_types__ctail _x19299;
  kk_box_t* _b_17750_17747 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16799)->tail)); /*cfield<list<2728>>*/;
  _x19299 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16799, _ctx)),_b_17750_17747); /*ctail<0>*/
  return kk_std_core__ctail_lift16736_map_peek(f, yy, _x19299, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17135_op_fun19300__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16802;
};
static kk_std_core__list kk_std_core__mlift17135_op_fun19300(kk_function_t _fself, kk_std_core__list _ctail_16801, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17135_op_fun19300(kk_function_t _accm, kk_box_t _ctail_16802, kk_context_t* _ctx) {
  struct kk_std_core__mlift17135_op_fun19300__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17135_op_fun19300__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17135_op_fun19300, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16802 = _ctail_16802;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17135_op_fun19300(kk_function_t _fself, kk_std_core__list _ctail_16801, kk_context_t* _ctx) {
  struct kk_std_core__mlift17135_op_fun19300__t* _self = kk_function_as(struct kk_std_core__mlift17135_op_fun19300__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<2728>) -> list<2728> */
  kk_box_t _ctail_16802 = _self->_ctail_16802; /* 2728 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16802);}, {}, _ctx)
  kk_std_core__list _x19301 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16802, _ctail_16801, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x19301, _ctx));
}

kk_std_core__list kk_std_core__mlift17135_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list yy0, kk_box_t _ctail_16802, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */ 
  return kk_std_core__ctailm_lift16736_map_peek(f0, yy0, kk_std_core__new_mlift17135_op_fun19300(_accm, _ctail_16802, _ctx), _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctail_lift16736_map_peek_fun19306__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list yy1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_lift16736_map_peek_fun19306(kk_function_t _fself, kk_box_t _b_17755, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_lift16736_map_peek_fun19306(kk_function_t f1, kk_std_core__list yy1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16736_map_peek_fun19306__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_lift16736_map_peek_fun19306__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_lift16736_map_peek_fun19306, kk_context());
  _self->f1 = f1;
  _self->yy1 = yy1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_lift16736_map_peek_fun19306(kk_function_t _fself, kk_box_t _b_17755, kk_context_t* _ctx) {
  struct kk_std_core__ctail_lift16736_map_peek_fun19306__t* _self = kk_function_as(struct kk_std_core__ctail_lift16736_map_peek_fun19306__t*, _fself);
  kk_function_t f1 = _self->f1; /* (value : 2727, rest : list<2727>) -> 2729 2728 */
  kk_std_core__list yy1 = _self->yy1; /* list<2727> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<2728>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(yy1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19307 = kk_std_core__mlift17134_op(_acc0, f1, yy1, _b_17755, _ctx); /*list<2728>*/
  return kk_std_core__list_box(_x19307, _ctx);
}

kk_std_core__list kk_std_core__ctail_lift16736_map_peek(kk_function_t f1, kk_std_core__list ys, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19302 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con19302->head;
    kk_std_core__list yy1 = _con19302->tail;
    kk_reuse_t _ru_18888 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18888 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy1);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18888 = kk_reuse_null;
    }
    kk_box_t x_17218;
    kk_function_t _x19304 = kk_function_dup(f1); /*(value : 2727, rest : list<2727>) -> 2729 2728*/
    kk_std_core__list _x19303 = kk_std_core__list_dup(yy1); /*list<2727>*/
    x_17218 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_std_core__list, kk_context_t*), _x19304, (_x19304, y, _x19303, _ctx)); /*2728*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18888, _ctx);
      kk_box_drop(x_17218, _ctx);
      kk_box_t _x19305 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_lift16736_map_peek_fun19306(f1, yy1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19305, _ctx);
    }
    {
      kk_std_core__list _ctail_167980 = kk_std_core__list_hole(); /*list<2728>*/;
      kk_std_core__list _ctail_167990 = kk_std_core__new_Cons(_ru_18888, x_17218, _ctail_167980, _ctx); /*list<2728>*/;
      { // tailcall
        kk_std_core_types__ctail _x19308;
        kk_box_t* _b_17767_17761 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_167990)->tail)); /*cfield<list<2728>>*/;
        _x19308 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_167990, _ctx)),_b_17767_17761); /*ctail<0>*/
        ys = yy1;
        _acc0 = _x19308;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19309 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19309, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__ctailm_lift16736_map_peek_fun19314__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list yy2;
};
static kk_box_t kk_std_core__ctailm_lift16736_map_peek_fun19314(kk_function_t _fself, kk_box_t _b_17775, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16736_map_peek_fun19314(kk_function_t _accm0, kk_function_t f2, kk_std_core__list yy2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19314__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19314__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16736_map_peek_fun19314, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->yy2 = yy2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_lift16736_map_peek_fun19314(kk_function_t _fself, kk_box_t _b_17775, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19314__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19314__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2728>) -> list<2728> */
  kk_function_t f2 = _self->f2; /* (value : 2727, rest : list<2727>) -> 2729 2728 */
  kk_std_core__list yy2 = _self->yy2; /* list<2727> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(yy2);}, {}, _ctx)
  kk_std_core__list _x19315 = kk_std_core__mlift17135_op(_accm0, f2, yy2, _b_17775, _ctx); /*list<2728>*/
  return kk_std_core__list_box(_x19315, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_lift16736_map_peek_fun19317__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x0_17221;
};
static kk_std_core__list kk_std_core__ctailm_lift16736_map_peek_fun19317(kk_function_t _fself, kk_std_core__list _ctail_168010, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_lift16736_map_peek_fun19317(kk_function_t _accm0, kk_box_t x0_17221, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19317__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19317__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_lift16736_map_peek_fun19317, kk_context());
  _self->_accm0 = _accm0;
  _self->x0_17221 = x0_17221;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_lift16736_map_peek_fun19317(kk_function_t _fself, kk_std_core__list _ctail_168010, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_lift16736_map_peek_fun19317__t* _self = kk_function_as(struct kk_std_core__ctailm_lift16736_map_peek_fun19317__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<2728>) -> list<2728> */
  kk_box_t x0_17221 = _self->x0_17221; /* 2728 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x0_17221);}, {}, _ctx)
  kk_std_core__list _x19318 = kk_std_core__new_Cons(kk_reuse_null, x0_17221, _ctail_168010, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19318, _ctx));
}

kk_std_core__list kk_std_core__ctailm_lift16736_map_peek(kk_function_t f2, kk_std_core__list ys0, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con19310 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con19310->head;
    kk_std_core__list yy2 = _con19310->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_box_dup(y0);
      kk_std_core__list_dup(yy2);
      kk_std_core__list_decref(ys0, _ctx);
    }
    kk_box_t x0_17221;
    kk_function_t _x19312 = kk_function_dup(f2); /*(value : 2727, rest : list<2727>) -> 2729 2728*/
    kk_std_core__list _x19311 = kk_std_core__list_dup(yy2); /*list<2727>*/
    x0_17221 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_std_core__list, kk_context_t*), _x19312, (_x19312, y0, _x19311, _ctx)); /*2728*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17221, _ctx);
      kk_box_t _x19313 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_lift16736_map_peek_fun19314(_accm0, f2, yy2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19313, _ctx);
    }
    { // tailcall
      kk_function_t _x19316 = kk_std_core__new_ctailm_lift16736_map_peek_fun19317(_accm0, x0_17221, _ctx); /*(list<2728>) -> list<2728>*/
      ys0 = yy2;
      _accm0 = _x19316;
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
struct kk_std_core__lift16736_map_peek_fun19320__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core__lift16736_map_peek_fun19320(kk_function_t _fself, kk_std_core__list _ctail_16800, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16736_map_peek_fun19320(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__lift16736_map_peek_fun19320, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core__lift16736_map_peek_fun19320(kk_function_t _fself, kk_std_core__list _ctail_16800, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16800;
}

kk_std_core__list kk_std_core__lift16736_map_peek(kk_function_t f3, kk_std_core__list ys1, kk_context_t* _ctx) { /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>) -> e list<b> */ 
  bool _match_19184 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19184) {
    kk_std_core_types__ctail _x19319 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_lift16736_map_peek(f3, ys1, _x19319, _ctx);
  }
  {
    return kk_std_core__ctailm_lift16736_map_peek(f3, ys1, kk_std_core__new_lift16736_map_peek_fun19320(_ctx), _ctx);
  }
}

kk_integer_t kk_std_core_maxListStack;

kk_ref_t kk_std_core_trace_enabled;
 
// Compose two funs `f` and `g`.


// lift anonymous function
struct kk_std_core_o_fun19322__t {
  struct kk_function_s _base;
  kk_function_t f;
  kk_function_t g;
};
static kk_box_t kk_std_core_o_fun19322(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_o_fun19322(kk_function_t f, kk_function_t g, kk_context_t* _ctx) {
  struct kk_std_core_o_fun19322__t* _self = kk_function_alloc_as(struct kk_std_core_o_fun19322__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_o_fun19322, kk_context());
  _self->f = f;
  _self->g = g;
  return &_self->_base;
}

static kk_box_t kk_std_core_o_fun19322(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_o_fun19322__t* _self = kk_function_as(struct kk_std_core_o_fun19322__t*, _fself);
  kk_function_t f = _self->f; /* (2771) -> 2768 2769 */
  kk_function_t g = _self->g; /* (2767) -> 2768 2771 */
  kk_drop_match(_self, {kk_function_dup(f);kk_function_dup(g);}, {}, _ctx)
  kk_box_t x0_17224 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), g, (g, x, _ctx)); /*2771*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x0_17224, _ctx);
    return kk_std_core_hnd_yield_extend(f, _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x0_17224, _ctx));
  }
}

kk_function_t kk_std_core_o(kk_function_t f, kk_function_t g, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (a) -> e b, g : (c) -> e a) -> ((x : c) -> e b) */ 
  return kk_std_core_new_o_fun19322(f, g, _ctx);
}
 
// monadic lift

kk_box_t kk_std_core__mlift17136_once(kk_function_t calc, kk_ref_t r, kk_std_core_types__maybe _y_16882, kk_context_t* _ctx) { /* forall<_h,_e,a> (calc : () -> a, r : ref<_h,maybe<a>>, maybe<a>) -> <read<_h>,write<_h>,div|_e> a */ 
  if (kk_std_core_types__is_Just(_y_16882)) {
    kk_box_t x = _y_16882._cons.Just.value;
    kk_function_drop(calc, _ctx);
    kk_ref_drop(r, _ctx);
    return x;
  }
  {
    kk_box_t x0 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), calc, (calc, _ctx)); /*2892*/;
    kk_unit_t __ = kk_Unit;
    kk_box_t _x19324;
    kk_std_core_types__maybe _x19325;
    kk_box_t _x19326 = kk_box_dup(x0); /*2892*/
    _x19325 = kk_std_core_types__new_Just(_x19326, _ctx); /*maybe<105>*/
    _x19324 = kk_std_core_types__maybe_box(_x19325, _ctx); /*171*/
    kk_ref_set(r,_x19324,kk_context());
    return x0;
  }
}
 
// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.


// lift anonymous function
struct kk_std_core_once_fun19327__t {
  struct kk_function_s _base;
  kk_function_t calc;
  kk_ref_t r;
};
static kk_box_t kk_std_core_once_fun19327(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19327(kk_function_t calc, kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19327__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19327__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19327, kk_context());
  _self->calc = calc;
  _self->r = r;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_once_fun19330__t {
  struct kk_function_s _base;
  kk_function_t calc;
  kk_ref_t r;
};
static kk_box_t kk_std_core_once_fun19330(kk_function_t _fself, kk_std_core_types__maybe _y_16882, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19330(kk_function_t calc, kk_ref_t r, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19330__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19330__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19330, kk_context());
  _self->calc = calc;
  _self->r = r;
  return &_self->_base;
}

static kk_box_t kk_std_core_once_fun19330(kk_function_t _fself, kk_std_core_types__maybe _y_16882, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19330__t* _self = kk_function_as(struct kk_std_core_once_fun19330__t*, _fself);
  kk_function_t calc = _self->calc; /* () -> 2892 */
  kk_ref_t r = _self->r; /* ref<_2800,maybe<2892>> */
  kk_drop_match(_self, {kk_function_dup(calc);kk_ref_dup(r);}, {}, _ctx)
  if (kk_std_core_types__is_Just(_y_16882)) {
    kk_box_t x0 = _y_16882._cons.Just.value;
    kk_function_drop(calc, _ctx);
    kk_ref_drop(r, _ctx);
    return x0;
  }
  {
    kk_box_t x00 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), calc, (calc, _ctx)); /*2892*/;
    kk_unit_t __ = kk_Unit;
    kk_box_t _x19331;
    kk_std_core_types__maybe _x19332;
    kk_box_t _x19333 = kk_box_dup(x00); /*2892*/
    _x19332 = kk_std_core_types__new_Just(_x19333, _ctx); /*maybe<105>*/
    _x19331 = kk_std_core_types__maybe_box(_x19332, _ctx); /*171*/
    kk_ref_set(r,_x19331,kk_context());
    return x00;
  }
}


// lift anonymous function
struct kk_std_core_once_fun19334__t {
  struct kk_function_s _base;
  kk_function_t next0_17229;
};
static kk_box_t kk_std_core_once_fun19334(kk_function_t _fself, kk_box_t _b_17794, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_once_fun19334(kk_function_t next0_17229, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19334__t* _self = kk_function_alloc_as(struct kk_std_core_once_fun19334__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_once_fun19334, kk_context());
  _self->next0_17229 = next0_17229;
  return &_self->_base;
}

static kk_box_t kk_std_core_once_fun19334(kk_function_t _fself, kk_box_t _b_17794, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19334__t* _self = kk_function_as(struct kk_std_core_once_fun19334__t*, _fself);
  kk_function_t next0_17229 = _self->next0_17229; /* (maybe<2892>) -> <read<_2800>,write<_2800>,div|_2884> 2892 */
  kk_drop_match(_self, {kk_function_dup(next0_17229);}, {}, _ctx)
  kk_std_core_types__maybe _x19335 = kk_std_core_types__maybe_unbox(_b_17794, _ctx); /*maybe<2892>*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_types__maybe, kk_context_t*), next0_17229, (next0_17229, _x19335, _ctx));
}
static kk_box_t kk_std_core_once_fun19327(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_once_fun19327__t* _self = kk_function_as(struct kk_std_core_once_fun19327__t*, _fself);
  kk_function_t calc = _self->calc; /* () -> 2892 */
  kk_ref_t r = _self->r; /* ref<_2800,maybe<2892>> */
  kk_drop_match(_self, {kk_function_dup(calc);kk_ref_dup(r);}, {}, _ctx)
  kk_std_core_types__maybe x_17228;
  kk_box_t _x19328;
  kk_ref_t _x19329 = kk_ref_dup(r); /*ref<_2800,maybe<2892>>*/
  _x19328 = kk_ref_get(_x19329,kk_context()); /*184*/
  x_17228 = kk_std_core_types__maybe_unbox(_x19328, _ctx); /*maybe<2892>*/
  kk_function_t next0_17229 = kk_std_core_new_once_fun19330(calc, r, _ctx); /*(maybe<2892>) -> <read<_2800>,write<_2800>,div|_2884> 2892*/;
  if (kk_yielding(kk_context())) {
    kk_std_core_types__maybe_drop(x_17228, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_core_new_once_fun19334(next0_17229, _ctx), _ctx);
  }
  {
    return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_types__maybe, kk_context_t*), next0_17229, (next0_17229, x_17228, _ctx));
  }
}

kk_function_t kk_std_core_once(kk_function_t calc, kk_context_t* _ctx) { /* forall<a> (calc : () -> a) -> (() -> a) */ 
  kk_ref_t r = kk_ref_alloc((kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx)),kk_context()); /*ref<_2800,maybe<2892>>*/;
  return kk_std_core_new_once_fun19327(calc, r, _ctx);
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
  kk_integer_t _x19339;
  if (kk_std_core_types__is_Lt(x)) {
    _x19339 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19340;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19339 = kk_integer_from_small(0); /*int*/
    goto _match19340;
  }
  {
    _x19339 = kk_integer_from_small(1); /*int*/
  }
  _match19340: ;
  kk_integer_t _x19341;
  if (kk_std_core_types__is_Lt(y)) {
    _x19341 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19342;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19341 = kk_integer_from_small(0); /*int*/
    goto _match19342;
  }
  {
    _x19341 = kk_integer_from_small(1); /*int*/
  }
  _match19342: ;
  return kk_integer_neq(_x19339,_x19341,kk_context());
}
 
// Append two lists.

kk_std_core__list kk_std_core__ctail_append(kk_std_core__list xs, kk_std_core__list ys, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ys : list<a>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19346 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19346->head;
    kk_std_core__list xx = _con19346->tail;
    kk_reuse_t _ru_18890 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18890 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18890 = kk_reuse_null;
    }
    kk_std_core__list _ctail_16803 = kk_std_core__list_hole(); /*list<3284>*/;
    kk_std_core__list _ctail_16804;
    if (kk_likely(_ru_18890!=NULL)) {
      struct kk_std_core_Cons* _con19347 = (struct kk_std_core_Cons*)_ru_18890;
      _con19347->tail = _ctail_16803;
      _ctail_16804 = kk_std_core__base_Cons(_con19347); /*list<3284>*/
    }
    else {
      _ctail_16804 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16803, _ctx); /*list<3284>*/
    }
    { // tailcall
      kk_std_core_types__ctail _x19348;
      kk_box_t* _b_17808_17803 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16804)->tail)); /*cfield<list<3284>>*/;
      _x19348 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16804, _ctx)),_b_17808_17803); /*ctail<0>*/
      xs = xx;
      _acc = _x19348;
      goto kk__tailcall;
    }
  }
  {
    kk_box_t _x19349 = kk_ctail_resolve(_acc,(kk_std_core__list_box(ys, _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19349, _ctx);
  }
}
 
// Append two lists.

kk_std_core__list kk_std_core_append(kk_std_core__list xs0, kk_std_core__list ys0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ys : list<a>) -> list<a> */ 
  kk_std_core_types__ctail _x19350 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_append(xs0, ys0, _x19350, _ctx);
}
 
// Append two strings.

kk_string_t kk_std_core__lp__plus__plus__1_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> string */ 
  return kk_string_cat(x,y,kk_context());
}

kk_unit_t kk_std_core_printsln(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_std_core_types__maybe _match_19181;
  kk_box_t _x19351;
  kk_ref_t _x19352 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  _x19351 = kk_ref_get(_x19352,kk_context()); /*184*/
  _match_19181 = kk_std_core_types__maybe_unbox(_x19351, _ctx); /*maybe<(string) -> console ()>*/
  if (kk_std_core_types__is_Nothing(_match_19181)) {
    kk_std_core_xprintsln(s, _ctx); return kk_Unit;
  }
  {
    kk_box_t _fun_unbox_x17818 = _match_19181._cons.Just.value;
    kk_string_t _b_17821;
    kk_string_t _x19353;
    kk_define_string_literal(, _s19354, 1, "\n")
    _x19353 = kk_string_dup(_s19354); /*string*/
    _b_17821 = kk_std_core__lp__plus__plus__1_rp_(s, _x19353, _ctx); /*string*/
    kk_box_t _x19355;
    kk_function_t _x19356 = kk_function_unbox(_fun_unbox_x17818); /*(17819) -> console 17820*/
    _x19355 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19356, (_x19356, kk_string_box(_b_17821), _ctx)); /*17820*/
    kk_unit_unbox(_x19355); return kk_Unit;
  }
}

bool kk_std_core__lp__eq__eq__4_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19357;
  if (kk_std_core_types__is_Lt(x)) {
    _x19357 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19358;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19357 = kk_integer_from_small(0); /*int*/
    goto _match19358;
  }
  {
    _x19357 = kk_integer_from_small(1); /*int*/
  }
  _match19358: ;
  kk_integer_t _x19359;
  if (kk_std_core_types__is_Lt(y)) {
    _x19359 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19360;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19359 = kk_integer_from_small(0); /*int*/
    goto _match19360;
  }
  {
    _x19359 = kk_integer_from_small(1); /*int*/
  }
  _match19360: ;
  return kk_integer_eq(_x19357,_x19359,kk_context());
}

bool kk_std_core__lp__lt__5_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19361;
  if (kk_std_core_types__is_Lt(x)) {
    _x19361 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19362;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19361 = kk_integer_from_small(0); /*int*/
    goto _match19362;
  }
  {
    _x19361 = kk_integer_from_small(1); /*int*/
  }
  _match19362: ;
  kk_integer_t _x19363;
  if (kk_std_core_types__is_Lt(y)) {
    _x19363 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19364;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19363 = kk_integer_from_small(0); /*int*/
    goto _match19364;
  }
  {
    _x19363 = kk_integer_from_small(1); /*int*/
  }
  _match19364: ;
  return kk_integer_lt(_x19361,_x19363,kk_context());
}

kk_std_core_types__order kk_std_core_order(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> order */ 
  bool _match_19179;
  kk_integer_t _x19365 = kk_integer_dup(i); /*int*/
  _match_19179 = kk_integer_lt(_x19365,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19179) {
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Lt(_ctx);
  }
  {
    bool _match_19180 = kk_integer_gt(i,(kk_integer_from_small(0)),kk_context()); /*bool*/;
    if (_match_19180) {
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
  bool _match_19177;
  kk_integer_t _x19366 = kk_integer_dup(i_16577); /*int*/
  _match_19177 = kk_integer_lt(_x19366,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19177) {
    kk_integer_drop(i_16577, _ctx);
    return kk_std_core_types__new_Lt(_ctx);
  }
  {
    bool _match_19178 = kk_integer_gt(i_16577,(kk_integer_from_small(0)),kk_context()); /*bool*/;
    if (_match_19178) {
      return kk_std_core_types__new_Gt(_ctx);
    }
    {
      return kk_std_core_types__new_Eq(_ctx);
    }
  }
}

bool kk_std_core__lp__lt__7_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> bool */ 
  kk_std_core_types__order x0_16713 = kk_std_core_compare_4(x, y, _ctx); /*order*/;
  kk_integer_t _x19367;
  if (kk_std_core_types__is_Lt(x0_16713)) {
    _x19367 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19368;
  }
  if (kk_std_core_types__is_Eq(x0_16713)) {
    _x19367 = kk_integer_from_small(0); /*int*/
    goto _match19368;
  }
  {
    _x19367 = kk_integer_from_small(1); /*int*/
  }
  _match19368: ;
  kk_integer_t _x19369 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_eq(_x19367,_x19369,kk_context());
}

bool kk_std_core__lp__gt__3_rp_(kk_std_core_types__order x, kk_std_core_types__order y, kk_context_t* _ctx) { /* (x : order, y : order) -> bool */ 
  kk_integer_t _x19370;
  if (kk_std_core_types__is_Lt(x)) {
    _x19370 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19371;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19370 = kk_integer_from_small(0); /*int*/
    goto _match19371;
  }
  {
    _x19370 = kk_integer_from_small(1); /*int*/
  }
  _match19371: ;
  kk_integer_t _x19372;
  if (kk_std_core_types__is_Lt(y)) {
    _x19372 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19373;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19372 = kk_integer_from_small(0); /*int*/
    goto _match19373;
  }
  {
    _x19372 = kk_integer_from_small(1); /*int*/
  }
  _match19373: ;
  return kk_integer_gt(_x19370,_x19372,kk_context());
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
  kk_integer_t _x19376;
  if (kk_std_core_types__is_Lt(x)) {
    _x19376 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19377;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19376 = kk_integer_from_small(0); /*int*/
    goto _match19377;
  }
  {
    _x19376 = kk_integer_from_small(1); /*int*/
  }
  _match19377: ;
  kk_integer_t _x19378;
  if (kk_std_core_types__is_Lt(y)) {
    _x19378 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19379;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19378 = kk_integer_from_small(0); /*int*/
    goto _match19379;
  }
  {
    _x19378 = kk_integer_from_small(1); /*int*/
  }
  _match19379: ;
  return kk_integer_gte(_x19376,_x19378,kk_context());
}

bool kk_std_core__lp__gt__eq__6_rp_(kk_string_t x, kk_string_t y, kk_context_t* _ctx) { /* (x : string, y : string) -> bool */ 
  kk_std_core_types__order x0_16717 = kk_std_core_compare_4(x, y, _ctx); /*order*/;
  kk_integer_t _x19380;
  if (kk_std_core_types__is_Lt(x0_16717)) {
    _x19380 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19381;
  }
  if (kk_std_core_types__is_Eq(x0_16717)) {
    _x19380 = kk_integer_from_small(0); /*int*/
    goto _match19381;
  }
  {
    _x19380 = kk_integer_from_small(1); /*int*/
  }
  _match19381: ;
  kk_integer_t _x19382 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_gt(_x19380,_x19382,kk_context());
}
 
// lifted

kk_string_t kk_std_core__lift16737_joinsep(kk_string_t sep, kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (sep : string, ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19383 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x17823 = _con19383->head;
    kk_std_core__list yy = _con19383->tail;
    kk_string_t y = kk_string_unbox(_box_x17823);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    kk_string_t acc0_16766;
    kk_string_t _x19385;
    kk_string_t _x19386 = kk_string_dup(sep); /*string*/
    _x19385 = kk_std_core__lp__plus__plus__1_rp_(_x19386, y, _ctx); /*string*/
    acc0_16766 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19385, _ctx); /*string*/
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
    struct kk_std_core_Cons* _con19388 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17824 = _con19388->head;
    kk_std_core__list xx = _con19388->tail;
    kk_string_t x = kk_string_unbox(_box_x17824);
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
    struct kk_std_core_Cons* _con19390 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x17825 = _con19390->head;
    kk_std_core__list yy = _con19390->tail;
    kk_string_t y = kk_string_unbox(_box_x17825);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x19392;
      kk_string_t _x19393;
      kk_string_t _x19394 = kk_string_empty(); /*string*/
      _x19393 = kk_std_core__lp__plus__plus__1_rp_(_x19394, y, _ctx); /*string*/
      _x19392 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19393, _ctx); /*string*/
      ys = yy;
      acc = _x19392;
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
    struct kk_std_core_Cons* _con19397 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17826 = _con19397->head;
    kk_std_core__list xx = _con19397->tail;
    kk_string_t x = kk_string_unbox(_box_x17826);
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
    struct kk_std_core_Cons* _con19400 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x17827 = _con19400->head;
    kk_std_core__list xx = _con19400->tail;
    kk_string_t x = kk_string_unbox(_box_x17827);
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
  kk_integer_t _x19402;
  if (kk_std_core_types__is_Lt(x)) {
    _x19402 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19403;
  }
  if (kk_std_core_types__is_Eq(x)) {
    _x19402 = kk_integer_from_small(0); /*int*/
    goto _match19403;
  }
  {
    _x19402 = kk_integer_from_small(1); /*int*/
  }
  _match19403: ;
  kk_integer_t _x19404;
  if (kk_std_core_types__is_Lt(y)) {
    _x19404 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19405;
  }
  if (kk_std_core_types__is_Eq(y)) {
    _x19404 = kk_integer_from_small(0); /*int*/
    goto _match19405;
  }
  {
    _x19404 = kk_integer_from_small(1); /*int*/
  }
  _match19405: ;
  return kk_integer_lte(_x19402,_x19404,kk_context());
}

kk_ssize_t kk_std_core_incr_1(kk_ssize_t i, kk_context_t* _ctx) { /* (i : ssize_t) -> ssize_t */ 
  return (i + 1);
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17137_op(kk_function_t action, kk_ssize_t end, kk_ssize_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : (ssize_t) -> e (), end : ssize_t, i : ssize_t, wild_ : ()) -> e () */ 
  kk_ssize_t i0_16769 = kk_std_core_incr_1(i, _ctx); /*ssize_t*/;
  kk_std_core__lift16739_forz(action, end, i0_16769, _ctx); return kk_Unit;
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16739_forz_fun19410__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_ssize_t end0;
  kk_ssize_t i0;
};
static kk_box_t kk_std_core__lift16739_forz_fun19410(kk_function_t _fself, kk_box_t _b_17829, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16739_forz_fun19410(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16739_forz_fun19410__t* _self = kk_function_alloc_as(struct kk_std_core__lift16739_forz_fun19410__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16739_forz_fun19410, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16739_forz_fun19410(kk_function_t _fself, kk_box_t _b_17829, kk_context_t* _ctx) {
  struct kk_std_core__lift16739_forz_fun19410__t* _self = kk_function_as(struct kk_std_core__lift16739_forz_fun19410__t*, _fself);
  kk_function_t action0 = _self->action0; /* (ssize_t) -> 4408 () */
  kk_ssize_t end0 = _self->end0; /* ssize_t */
  kk_ssize_t i0 = _self->i0; /* ssize_t */
  kk_drop_match(_self, {kk_function_dup(action0);;;}, {}, _ctx)
  kk_unit_t _x19411 = kk_Unit;
  kk_unit_t _x19412 = kk_Unit;
  kk_unit_unbox(_b_17829);
  kk_std_core__mlift17137_op(action0, end0, i0, _x19412, _ctx);
  return kk_unit_box(_x19411);
}

kk_unit_t kk_std_core__lift16739_forz(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) { /* forall<e> (action : (ssize_t) -> e (), end : ssize_t, i : ssize_t) -> e () */ 
  kk__tailcall: ;
  bool _match_19171 = (i0 <= end0); /*bool*/;
  if (_match_19171) {
    kk_unit_t x_17237 = kk_Unit;
    kk_function_t _x19408 = kk_function_dup(action0); /*(ssize_t) -> 4408 ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_ssize_t, kk_context_t*), _x19408, (_x19408, i0, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x19409 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16739_forz_fun19410(action0, end0, i0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x19409); return kk_Unit;
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
struct kk_std_core_foreach_indexedz_fun19415__t {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_indexedz_fun19415(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexedz_fun19415(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexedz_fun19415__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexedz_fun19415__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexedz_fun19415, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_indexedz_fun19415(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexedz_fun19415__t* _self = kk_function_as(struct kk_std_core_foreach_indexedz_fun19415__t*, _fself);
  kk_function_t f = _self->f; /* (4442, ssize_t) -> 4443 () */
  kk_vector_t v = _self->v; /* vector<4442> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t _x19416 = kk_vector_at(v,i,kk_context()); /*223*/
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_ssize_t, kk_context_t*), f, (f, _x19416, i, _ctx));
}

kk_unit_t kk_std_core_foreach_indexedz(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a, ssize_t) -> e ()) -> e () */ 
  kk_ssize_t start0_17240 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17241;
  kk_ssize_t _x19413;
  kk_vector_t _x19414 = kk_vector_dup(v); /*vector<4442>*/
  _x19413 = kk_vector_len(_x19414,kk_context()); /*ssize_t*/
  end_17241 = kk_std_core_decr_1(_x19413, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_indexedz_fun19415(f, v, _ctx), end_17241, start0_17240, _ctx); return kk_Unit;
}
 
// lifted

kk_integer_t kk_std_core__lift16740_length_1(kk_std_core__list ys, kk_integer_t acc, kk_context_t* _ctx) { /* forall<a> (ys : list<a>, acc : int) -> int */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19417 = kk_std_core__as_Cons(ys);
    kk_box_t _pat0 = _con19417->head;
    kk_std_core__list yy = _con19417->tail;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_box_drop(_pat0, _ctx);
      kk_std_core__list_free(ys);
    }
    else {
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_integer_t _x19418 = kk_integer_add(acc,(kk_integer_from_small(1)),kk_context()); /*int*/
      ys = yy;
      acc = _x19418;
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
  bool _match_19170;
  kk_integer_t _x19420 = kk_integer_dup(low); /*int*/
  kk_integer_t _x19421 = kk_integer_dup(high); /*int*/
  _match_19170 = kk_integer_gt(_x19420,_x19421,kk_context()); /*bool*/
  if (_match_19170) {
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low, _ctx);
    return acc;
  }
  { // tailcall
    kk_integer_t _x19422;
    kk_integer_t _x19423 = kk_integer_dup(high); /*int*/
    _x19422 = kk_integer_sub(_x19423,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_std_core__list _x19424 = kk_std_core__new_Cons(kk_reuse_null, kk_integer_box(high), acc, _ctx); /*list<61>*/
    high = _x19422;
    acc = _x19424;
    goto kk__tailcall;
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17138_op(kk_std_core__list acc, kk_function_t f, kk_integer_t high0_16771, kk_integer_t low, kk_box_t _y_16890, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, f : (int) -> e a, high0.16771 : int, low : int, a) -> e list<a> */ 
  kk_std_core__list _x19425 = kk_std_core__new_Cons(kk_reuse_null, _y_16890, acc, _ctx); /*list<61>*/
  return kk_std_core__lift16742_list_1(f, low, high0_16771, _x19425, _ctx);
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16742_list_fun19431__t_1 {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t f0;
  kk_integer_t high0_167710;
  kk_integer_t low0;
};
static kk_box_t kk_std_core__lift16742_list_fun19431_1(kk_function_t _fself, kk_box_t _b_17837, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16742_list_fun19431_1(kk_std_core__list acc0, kk_function_t f0, kk_integer_t high0_167710, kk_integer_t low0, kk_context_t* _ctx) {
  struct kk_std_core__lift16742_list_fun19431__t_1* _self = kk_function_alloc_as(struct kk_std_core__lift16742_list_fun19431__t_1, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16742_list_fun19431_1, kk_context());
  _self->acc0 = acc0;
  _self->f0 = f0;
  _self->high0_167710 = high0_167710;
  _self->low0 = low0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16742_list_fun19431_1(kk_function_t _fself, kk_box_t _b_17837, kk_context_t* _ctx) {
  struct kk_std_core__lift16742_list_fun19431__t_1* _self = kk_function_as(struct kk_std_core__lift16742_list_fun19431__t_1*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<4816> */
  kk_function_t f0 = _self->f0; /* (int) -> 4817 4816 */
  kk_integer_t high0_167710 = _self->high0_167710; /* int */
  kk_integer_t low0 = _self->low0; /* int */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(f0);kk_integer_dup(high0_167710);kk_integer_dup(low0);}, {}, _ctx)
  kk_std_core__list _x19432 = kk_std_core__mlift17138_op(acc0, f0, high0_167710, low0, _b_17837, _ctx); /*list<4816>*/
  return kk_std_core__list_box(_x19432, _ctx);
}

kk_std_core__list kk_std_core__lift16742_list_1(kk_function_t f0, kk_integer_t low0, kk_integer_t high, kk_std_core__list acc0, kk_context_t* _ctx) { /* forall<a,e> (f : (int) -> e a, low : int, high : int, acc : list<a>) -> e list<a> */ 
  kk__tailcall: ;
  bool _match_19168;
  kk_integer_t _x19426 = kk_integer_dup(low0); /*int*/
  kk_integer_t _x19427 = kk_integer_dup(high); /*int*/
  _match_19168 = kk_integer_gt(_x19426,_x19427,kk_context()); /*bool*/
  if (_match_19168) {
    kk_function_drop(f0, _ctx);
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low0, _ctx);
    return acc0;
  }
  {
    kk_integer_t high0_167710;
    kk_integer_t _x19428 = kk_integer_dup(high); /*int*/
    high0_167710 = kk_integer_sub(_x19428,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_box_t x_17243;
    kk_function_t _x19429 = kk_function_dup(f0); /*(int) -> 4817 4816*/
    x_17243 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_context_t*), _x19429, (_x19429, high, _ctx)); /*4816*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x_17243, _ctx);
      kk_box_t _x19430 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16742_list_fun19431_1(acc0, f0, high0_167710, low0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19430, _ctx);
    }
    { // tailcall
      kk_std_core__list _x19433 = kk_std_core__new_Cons(kk_reuse_null, x_17243, acc0, _ctx); /*list<61>*/
      high = high0_167710;
      acc0 = _x19433;
      goto kk__tailcall;
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17139_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_box_t _ctail_16805, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */ 
  kk_std_core__list _ctail_16806 = kk_std_core__list_hole(); /*list<5916>*/;
  kk_std_core__list _ctail_16807 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16805, _ctail_16806, _ctx); /*list<5916>*/;
  kk_std_core_types__ctail _x19434;
  kk_box_t* _b_17848_17845 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16807)->tail)); /*cfield<list<5916>>*/;
  _x19434 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16807, _ctx)),_b_17848_17845); /*ctail<0>*/
  return kk_std_core__ctail_map_5(xx, f, _x19434, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17140_op_fun19435__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16810;
};
static kk_std_core__list kk_std_core__mlift17140_op_fun19435(kk_function_t _fself, kk_std_core__list _ctail_16809, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17140_op_fun19435(kk_function_t _accm, kk_box_t _ctail_16810, kk_context_t* _ctx) {
  struct kk_std_core__mlift17140_op_fun19435__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17140_op_fun19435__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17140_op_fun19435, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16810 = _ctail_16810;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17140_op_fun19435(kk_function_t _fself, kk_std_core__list _ctail_16809, kk_context_t* _ctx) {
  struct kk_std_core__mlift17140_op_fun19435__t* _self = kk_function_as(struct kk_std_core__mlift17140_op_fun19435__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<5916>) -> list<5916> */
  kk_box_t _ctail_16810 = _self->_ctail_16810; /* 5916 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16810);}, {}, _ctx)
  kk_std_core__list _x19436 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16810, _ctail_16809, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x19436, _ctx));
}

kk_std_core__list kk_std_core__mlift17140_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_box_t _ctail_16810, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */ 
  return kk_std_core__ctailm_map_5(xx0, f0, kk_std_core__new_mlift17140_op_fun19435(_accm, _ctail_16810, _ctx), _ctx);
}
 
// Apply a function `f`  to each element of the input list in sequence.


// lift anonymous function
struct kk_std_core__ctail_map_fun19440__t_5 {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_map_fun19440_5(kk_function_t _fself, kk_box_t _b_17853, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_map_fun19440_5(kk_function_t f1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_fun19440__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctail_map_fun19440__t_5, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_map_fun19440_5, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_map_fun19440_5(kk_function_t _fself, kk_box_t _b_17853, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_fun19440__t_5* _self = kk_function_as(struct kk_std_core__ctail_map_fun19440__t_5*, _fself);
  kk_function_t f1 = _self->f1; /* (5915) -> 5917 5916 */
  kk_std_core__list xx1 = _self->xx1; /* list<5915> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<5916>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x19441 = kk_std_core__mlift17139_op(_acc0, f1, xx1, _b_17853, _ctx); /*list<5916>*/
  return kk_std_core__list_box(_x19441, _ctx);
}

kk_std_core__list kk_std_core__ctail_map_5(kk_std_core__list xs, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19437 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19437->head;
    kk_std_core__list xx1 = _con19437->tail;
    kk_reuse_t _ru_18897 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18897 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18897 = kk_reuse_null;
    }
    kk_box_t x0_17246;
    kk_function_t _x19438 = kk_function_dup(f1); /*(5915) -> 5917 5916*/
    x0_17246 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19438, (_x19438, x, _ctx)); /*5916*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18897, _ctx);
      kk_box_drop(x0_17246, _ctx);
      kk_box_t _x19439 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_map_fun19440_5(f1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19439, _ctx);
    }
    {
      kk_std_core__list _ctail_168060 = kk_std_core__list_hole(); /*list<5916>*/;
      kk_std_core__list _ctail_168070 = kk_std_core__new_Cons(_ru_18897, x0_17246, _ctail_168060, _ctx); /*list<5916>*/;
      { // tailcall
        kk_std_core_types__ctail _x19442;
        kk_box_t* _b_17865_17859 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168070)->tail)); /*cfield<list<5916>>*/;
        _x19442 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168070, _ctx)),_b_17865_17859); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x19442;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x19443 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19443, _ctx);
  }
}
 
// Apply a function `f`  to each element of the input list in sequence.


// lift anonymous function
struct kk_std_core__ctailm_map_fun19447__t_5 {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_map_fun19447_5(kk_function_t _fself, kk_box_t _b_17873, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_fun19447_5(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19447__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_fun19447__t_5, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_fun19447_5, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_map_fun19447_5(kk_function_t _fself, kk_box_t _b_17873, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19447__t_5* _self = kk_function_as(struct kk_std_core__ctailm_map_fun19447__t_5*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<5916>) -> list<5916> */
  kk_function_t f2 = _self->f2; /* (5915) -> 5917 5916 */
  kk_std_core__list xx2 = _self->xx2; /* list<5915> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x19448 = kk_std_core__mlift17140_op(_accm0, f2, xx2, _b_17873, _ctx); /*list<5916>*/
  return kk_std_core__list_box(_x19448, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_map_fun19450__t_5 {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x2_17249;
};
static kk_std_core__list kk_std_core__ctailm_map_fun19450_5(kk_function_t _fself, kk_std_core__list _ctail_168090, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_fun19450_5(kk_function_t _accm0, kk_box_t x2_17249, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19450__t_5* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_fun19450__t_5, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_fun19450_5, kk_context());
  _self->_accm0 = _accm0;
  _self->x2_17249 = x2_17249;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_map_fun19450_5(kk_function_t _fself, kk_std_core__list _ctail_168090, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_fun19450__t_5* _self = kk_function_as(struct kk_std_core__ctailm_map_fun19450__t_5*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<5916>) -> list<5916> */
  kk_box_t x2_17249 = _self->x2_17249; /* 5916 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x2_17249);}, {}, _ctx)
  kk_std_core__list _x19451 = kk_std_core__new_Cons(kk_reuse_null, x2_17249, _ctail_168090, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x19451, _ctx));
}

kk_std_core__list kk_std_core__ctailm_map_5(kk_std_core__list xs0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con19444 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con19444->head;
    kk_std_core__list xx2 = _con19444->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_box_t x2_17249;
    kk_function_t _x19445 = kk_function_dup(f2); /*(5915) -> 5917 5916*/
    x2_17249 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19445, (_x19445, x1, _ctx)); /*5916*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x2_17249, _ctx);
      kk_box_t _x19446 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_map_fun19447_5(_accm0, f2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19446, _ctx);
    }
    { // tailcall
      kk_function_t _x19449 = kk_std_core__new_ctailm_map_fun19450_5(_accm0, x2_17249, _ctx); /*(list<5916>) -> list<5916>*/
      xs0 = xx2;
      _accm0 = _x19449;
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
struct kk_std_core_map_fun19453__t_5 {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_map_fun19453_5(kk_function_t _fself, kk_std_core__list _ctail_16808, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19453_5(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19453_5, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_map_fun19453_5(kk_function_t _fself, kk_std_core__list _ctail_16808, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16808;
}

kk_std_core__list kk_std_core_map_5(kk_std_core__list xs1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */ 
  bool _match_19165 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19165) {
    kk_std_core_types__ctail _x19452 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_map_5(xs1, f3, _x19452, _ctx);
  }
  {
    return kk_std_core__ctailm_map_5(xs1, f3, kk_std_core_new_map_fun19453_5(_ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16743_list_2(kk_integer_t low, kk_integer_t high, kk_std_core__list acc, kk_context_t* _ctx) { /* (low : int, high : int, acc : list<int>) -> list<int> */ 
  kk__tailcall: ;
  bool _match_19164;
  kk_integer_t _x19454 = kk_integer_dup(low); /*int*/
  kk_integer_t _x19455 = kk_integer_dup(high); /*int*/
  _match_19164 = kk_integer_gt(_x19454,_x19455,kk_context()); /*bool*/
  if (_match_19164) {
    kk_integer_drop(high, _ctx);
    kk_integer_drop(low, _ctx);
    return acc;
  }
  { // tailcall
    kk_integer_t _x19456;
    kk_integer_t _x19457 = kk_integer_dup(high); /*int*/
    _x19456 = kk_integer_sub(_x19457,(kk_integer_from_small(1)),kk_context()); /*int*/
    kk_std_core__list _x19458 = kk_std_core__new_Cons(kk_reuse_null, kk_integer_box(high), acc, _ctx); /*list<61>*/
    high = _x19456;
    acc = _x19458;
    goto kk__tailcall;
  }
}
 
// Create a list of characters from `lo`  to `hi`  (inclusive).


// lift anonymous function
struct kk_std_core_list_fun19459__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_list_fun19459_2(kk_function_t _fself, kk_box_t _b_17882, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_list_fun19459_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_list_fun19459_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_list_fun19459_2(kk_function_t _fself, kk_box_t _b_17882, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_char_t _x19460;
  kk_integer_t _x19461 = kk_integer_unbox(_b_17882); /*int*/
  _x19460 = kk_integer_clamp32(_x19461,kk_context()); /*char*/
  return kk_char_box(_x19460, _ctx);
}

kk_std_core__list kk_std_core_list_2(kk_char_t lo, kk_char_t hi, kk_context_t* _ctx) { /* (lo : char, hi : char) -> total list<char> */ 
  kk_integer_t lo0_16601 = kk_integer_from_int(lo,kk_context()); /*int*/;
  kk_integer_t hi0_16602 = kk_integer_from_int(hi,kk_context()); /*int*/;
  kk_std_core__list _b_17883_17880 = kk_std_core__lift16743_list_2(lo0_16601, hi0_16602, kk_std_core__new_Nil(_ctx), _ctx); /*list<int>*/;
  return kk_std_core_map_5(_b_17883_17880, kk_std_core_new_list_fun19459_2(_ctx), _ctx);
}
 
// Convert a string to a list of characters

kk_std_core__list kk_std_core_list_4(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> total list<char> */ 
  return kk_string_to_list(s,kk_context());
}


// lift anonymous function
struct kk_std_core_map_fun19463__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19463(kk_function_t _fself, kk_box_t _b_17886, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19463(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19463, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19463(kk_function_t _fself, kk_box_t _b_17886, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__maybe _x19464 = kk_std_core_types__new_Just(_b_17886, _ctx); /*maybe<105>*/
  return kk_std_core_types__maybe_box(_x19464, _ctx);
}

kk_std_core_types__maybe kk_std_core_map(kk_std_core_types__maybe m, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (m : maybe<a>, f : (a) -> e b) -> e maybe<b> */ 
  if (kk_std_core_types__is_Nothing(m)) {
    kk_function_drop(f, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t x = m._cons.Just.value;
    kk_box_t x0_17252 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x, _ctx)); /*5096*/;
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17252, _ctx);
      kk_box_t _x19462 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19463(_ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19462, _ctx);
    }
    {
      return kk_std_core_types__new_Just(x0_17252, _ctx);
    }
  }
}
 
// Map over the `Right` component of an `:either` type.


// lift anonymous function
struct kk_std_core_map_fun19466__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19466_1(kk_function_t _fself, kk_box_t _b_17890, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19466_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19466_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19466_1(kk_function_t _fself, kk_box_t _b_17890, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_types__either _x19467 = kk_std_core_types__new_Right(_b_17890, _ctx); /*either<72,73>*/
  return kk_std_core_types__either_box(_x19467, _ctx);
}

kk_std_core_types__either kk_std_core_map_1(kk_std_core_types__either e, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,c,e> (e : either<a,b>, f : (b) -> e c) -> e either<a,c> */ 
  if (kk_std_core_types__is_Right(e)) {
    kk_box_t x = e._cons.Right.right;
    kk_box_t x0_17256 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x, _ctx)); /*5134*/;
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17256, _ctx);
      kk_box_t _x19465 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19466_1(_ctx), _ctx); /*3860*/
      return kk_std_core_types__either_unbox(_x19465, _ctx);
    }
    {
      return kk_std_core_types__new_Right(x0_17256, _ctx);
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
struct kk_std_core__mlift17144_map_fun19470__t_2 {
  struct kk_function_s _base;
  kk_box_t _y_16907;
};
static kk_box_t kk_std_core__mlift17144_map_fun19470_2(kk_function_t _fself, kk_box_t _b_17894, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17144_map_fun19470_2(kk_box_t _y_16907, kk_context_t* _ctx) {
  struct kk_std_core__mlift17144_map_fun19470__t_2* _self = kk_function_alloc_as(struct kk_std_core__mlift17144_map_fun19470__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17144_map_fun19470_2, kk_context());
  _self->_y_16907 = _y_16907;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17144_map_fun19470_2(kk_function_t _fself, kk_box_t _b_17894, kk_context_t* _ctx) {
  struct kk_std_core__mlift17144_map_fun19470__t_2* _self = kk_function_as(struct kk_std_core__mlift17144_map_fun19470__t_2*, _fself);
  kk_box_t _y_16907 = _self->_y_16907; /* 5279 */
  kk_drop_match(_self, {kk_box_dup(_y_16907);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19471 = kk_std_core_types__new_dash__lp__comma__rp_(_y_16907, _b_17894, _ctx); /*(6, 7)*/
  return kk_std_core_types__tuple2__box(_x19471, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core__mlift17144_map_2(kk_function_t f, kk_std_core_types__tuple2_ t, kk_box_t _y_16907, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a), b) -> e (b, b) */ 
  kk_box_t x_17260;
  kk_box_t _x19468;
  {
    kk_box_t _x = t.snd;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(t, _ctx);
    _x19468 = _x; /*5278*/
  }
  x_17260 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19468, _ctx)); /*5279*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17260, _ctx);
    kk_box_t _x19469 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17144_map_fun19470_2(_y_16907, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple2__unbox(_x19469, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__rp_(_y_16907, x_17260, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19475__t_2 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple2_ t;
};
static kk_box_t kk_std_core_map_fun19475_2(kk_function_t _fself, kk_box_t _b_17898, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19475_2(kk_function_t f, kk_std_core_types__tuple2_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19475__t_2* _self = kk_function_alloc_as(struct kk_std_core_map_fun19475__t_2, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19475_2, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19475_2(kk_function_t _fself, kk_box_t _b_17898, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19475__t_2* _self = kk_function_as(struct kk_std_core_map_fun19475__t_2*, _fself);
  kk_function_t f = _self->f; /* (5278) -> 5280 5279 */
  kk_std_core_types__tuple2_ t = _self->t; /* (5278, 5278) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple2__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19476 = kk_std_core__mlift17144_map_2(f, t, _b_17898, _ctx); /*(5279, 5279)*/
  return kk_std_core_types__tuple2__box(_x19476, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19479__t_2 {
  struct kk_function_s _base;
  kk_box_t x_17265;
};
static kk_box_t kk_std_core_map_fun19479_2(kk_function_t _fself, kk_box_t _b_17900, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19479_2(kk_box_t x_17265, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19479__t_2* _self = kk_function_alloc_as(struct kk_std_core_map_fun19479__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19479_2, kk_context());
  _self->x_17265 = x_17265;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19479_2(kk_function_t _fself, kk_box_t _b_17900, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19479__t_2* _self = kk_function_as(struct kk_std_core_map_fun19479__t_2*, _fself);
  kk_box_t x_17265 = _self->x_17265; /* 5279 */
  kk_drop_match(_self, {kk_box_dup(x_17265);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x19480 = kk_std_core_types__new_dash__lp__comma__rp_(x_17265, _b_17900, _ctx); /*(6, 7)*/
  return kk_std_core_types__tuple2__box(_x19480, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_map_2(kk_std_core_types__tuple2_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a), f : (a) -> e b) -> e (b, b) */ 
  kk_box_t x_17265;
  kk_function_t _x19473 = kk_function_dup(f); /*(5278) -> 5280 5279*/
  kk_box_t _x19472;
  {
    kk_box_t _x = t.fst;
    kk_box_dup(_x);
    _x19472 = _x; /*5278*/
  }
  x_17265 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19473, (_x19473, _x19472, _ctx)); /*5279*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17265, _ctx);
    kk_box_t _x19474 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19475_2(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple2__unbox(_x19474, _ctx);
  }
  {
    kk_box_t x0_17269;
    kk_box_t _x19477;
    {
      kk_box_t _x0 = t.snd;
      kk_box_dup(_x0);
      kk_std_core_types__tuple2__drop(t, _ctx);
      _x19477 = _x0; /*5278*/
    }
    x0_17269 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19477, _ctx)); /*5279*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17269, _ctx);
      kk_box_t _x19478 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19479_2(x_17265, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x19478, _ctx);
    }
    {
      return kk_std_core_types__new_dash__lp__comma__rp_(x_17265, x0_17269, _ctx);
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17146_map_fun19483__t_3 {
  struct kk_function_s _base;
  kk_box_t _y_16909;
  kk_box_t _y_16910;
};
static kk_box_t kk_std_core__mlift17146_map_fun19483_3(kk_function_t _fself, kk_box_t _b_17906, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17146_map_fun19483_3(kk_box_t _y_16909, kk_box_t _y_16910, kk_context_t* _ctx) {
  struct kk_std_core__mlift17146_map_fun19483__t_3* _self = kk_function_alloc_as(struct kk_std_core__mlift17146_map_fun19483__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17146_map_fun19483_3, kk_context());
  _self->_y_16909 = _y_16909;
  _self->_y_16910 = _y_16910;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17146_map_fun19483_3(kk_function_t _fself, kk_box_t _b_17906, kk_context_t* _ctx) {
  struct kk_std_core__mlift17146_map_fun19483__t_3* _self = kk_function_as(struct kk_std_core__mlift17146_map_fun19483__t_3*, _fself);
  kk_box_t _y_16909 = _self->_y_16909; /* 5497 */
  kk_box_t _y_16910 = _self->_y_16910; /* 5497 */
  kk_drop_match(_self, {kk_box_dup(_y_16909);kk_box_dup(_y_16910);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19484 = kk_std_core_types__new_dash__lp__comma__comma__rp_(_y_16909, _y_16910, _b_17906, _ctx); /*(13, 14, 15)*/
  return kk_std_core_types__tuple3__box(_x19484, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core__mlift17146_map_3(kk_box_t _y_16909, kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t _y_16910, kk_context_t* _ctx) { /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */ 
  kk_box_t x_17275;
  kk_box_t _x19481;
  {
    kk_box_t _x = t.thd;
    kk_box_dup(_x);
    kk_std_core_types__tuple3__drop(t, _ctx);
    _x19481 = _x; /*5496*/
  }
  x_17275 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19481, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17275, _ctx);
    kk_box_t _x19482 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17146_map_fun19483_3(_y_16909, _y_16910, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19482, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__comma__rp_(_y_16909, _y_16910, x_17275, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17147_map_fun19488__t_3 {
  struct kk_function_s _base;
  kk_box_t _y_16909;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
};
static kk_box_t kk_std_core__mlift17147_map_fun19488_3(kk_function_t _fself, kk_box_t _b_17910, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17147_map_fun19488_3(kk_box_t _y_16909, kk_function_t f, kk_std_core_types__tuple3_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17147_map_fun19488__t_3* _self = kk_function_alloc_as(struct kk_std_core__mlift17147_map_fun19488__t_3, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17147_map_fun19488_3, kk_context());
  _self->_y_16909 = _y_16909;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17147_map_fun19488_3(kk_function_t _fself, kk_box_t _b_17910, kk_context_t* _ctx) {
  struct kk_std_core__mlift17147_map_fun19488__t_3* _self = kk_function_as(struct kk_std_core__mlift17147_map_fun19488__t_3*, _fself);
  kk_box_t _y_16909 = _self->_y_16909; /* 5497 */
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_drop_match(_self, {kk_box_dup(_y_16909);kk_function_dup(f);kk_std_core_types__tuple3__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19489 = kk_std_core__mlift17146_map_3(_y_16909, f, t, _b_17910, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19489, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core__mlift17147_map_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t _y_16909, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a), b) -> e (b, b, b) */ 
  kk_box_t x_17281;
  kk_function_t _x19486 = kk_function_dup(f); /*(5496) -> 5498 5497*/
  kk_box_t _x19485;
  {
    kk_box_t _x = t.snd;
    kk_box_dup(_x);
    _x19485 = _x; /*5496*/
  }
  x_17281 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19486, (_x19486, _x19485, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17281, _ctx);
    kk_box_t _x19487 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17147_map_fun19488_3(_y_16909, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19487, _ctx);
  }
  {
    return kk_std_core__mlift17146_map_3(_y_16909, f, t, x_17281, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19493__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
};
static kk_box_t kk_std_core_map_fun19493_3(kk_function_t _fself, kk_box_t _b_17914, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19493_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19493__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19493__t_3, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19493_3, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19493_3(kk_function_t _fself, kk_box_t _b_17914, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19493__t_3* _self = kk_function_as(struct kk_std_core_map_fun19493__t_3*, _fself);
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple3__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19494 = kk_std_core__mlift17147_map_3(f, t, _b_17914, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19494, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19498__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple3_ t;
  kk_box_t x_17284;
};
static kk_box_t kk_std_core_map_fun19498_3(kk_function_t _fself, kk_box_t _b_17916, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19498_3(kk_function_t f, kk_std_core_types__tuple3_ t, kk_box_t x_17284, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19498__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19498__t_3, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19498_3, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17284 = x_17284;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19498_3(kk_function_t _fself, kk_box_t _b_17916, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19498__t_3* _self = kk_function_as(struct kk_std_core_map_fun19498__t_3*, _fself);
  kk_function_t f = _self->f; /* (5496) -> 5498 5497 */
  kk_std_core_types__tuple3_ t = _self->t; /* (5496, 5496, 5496) */
  kk_box_t x_17284 = _self->x_17284; /* 5497 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple3__dup(t);kk_box_dup(x_17284);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19499 = kk_std_core__mlift17146_map_3(x_17284, f, t, _b_17916, _ctx); /*(5497, 5497, 5497)*/
  return kk_std_core_types__tuple3__box(_x19499, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19502__t_3 {
  struct kk_function_s _base;
  kk_box_t x_17284;
  kk_box_t x0_17288;
};
static kk_box_t kk_std_core_map_fun19502_3(kk_function_t _fself, kk_box_t _b_17918, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19502_3(kk_box_t x_17284, kk_box_t x0_17288, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19502__t_3* _self = kk_function_alloc_as(struct kk_std_core_map_fun19502__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19502_3, kk_context());
  _self->x_17284 = x_17284;
  _self->x0_17288 = x0_17288;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19502_3(kk_function_t _fself, kk_box_t _b_17918, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19502__t_3* _self = kk_function_as(struct kk_std_core_map_fun19502__t_3*, _fself);
  kk_box_t x_17284 = _self->x_17284; /* 5497 */
  kk_box_t x0_17288 = _self->x0_17288; /* 5497 */
  kk_drop_match(_self, {kk_box_dup(x_17284);kk_box_dup(x0_17288);}, {}, _ctx)
  kk_std_core_types__tuple3_ _x19503 = kk_std_core_types__new_dash__lp__comma__comma__rp_(x_17284, x0_17288, _b_17918, _ctx); /*(13, 14, 15)*/
  return kk_std_core_types__tuple3__box(_x19503, _ctx);
}

kk_std_core_types__tuple3_ kk_std_core_map_3(kk_std_core_types__tuple3_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a, a), f : (a) -> e b) -> e (b, b, b) */ 
  kk_box_t x_17284;
  kk_function_t _x19491 = kk_function_dup(f); /*(5496) -> 5498 5497*/
  kk_box_t _x19490;
  {
    kk_box_t _x = t.fst;
    kk_box_dup(_x);
    _x19490 = _x; /*5496*/
  }
  x_17284 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19491, (_x19491, _x19490, _ctx)); /*5497*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17284, _ctx);
    kk_box_t _x19492 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19493_3(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple3__unbox(_x19492, _ctx);
  }
  {
    kk_box_t x0_17288;
    kk_function_t _x19496 = kk_function_dup(f); /*(5496) -> 5498 5497*/
    kk_box_t _x19495;
    {
      kk_box_t _x0 = t.snd;
      kk_box_dup(_x0);
      _x19495 = _x0; /*5496*/
    }
    x0_17288 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19496, (_x19496, _x19495, _ctx)); /*5497*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17288, _ctx);
      kk_box_t _x19497 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19498_3(f, t, x_17284, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple3__unbox(_x19497, _ctx);
    }
    {
      kk_box_t x1_17292;
      kk_box_t _x19500;
      {
        kk_box_t _x1 = t.thd;
        kk_box_dup(_x1);
        kk_std_core_types__tuple3__drop(t, _ctx);
        _x19500 = _x1; /*5496*/
      }
      x1_17292 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19500, _ctx)); /*5497*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x1_17292, _ctx);
        kk_box_t _x19501 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19502_3(x_17284, x0_17288, _ctx), _ctx); /*3860*/
        return kk_std_core_types__tuple3__unbox(_x19501, _ctx);
      }
      {
        return kk_std_core_types__new_dash__lp__comma__comma__rp_(x_17284, x0_17288, x1_17292, _ctx);
      }
    }
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17149_map_fun19507__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16912;
  kk_box_t _y_16913;
  kk_box_t _y_16914;
};
static kk_box_t kk_std_core__mlift17149_map_fun19507_4(kk_function_t _fself, kk_box_t _b_17926, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17149_map_fun19507_4(kk_box_t _y_16912, kk_box_t _y_16913, kk_box_t _y_16914, kk_context_t* _ctx) {
  struct kk_std_core__mlift17149_map_fun19507__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17149_map_fun19507__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17149_map_fun19507_4, kk_context());
  _self->_y_16912 = _y_16912;
  _self->_y_16913 = _y_16913;
  _self->_y_16914 = _y_16914;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17149_map_fun19507_4(kk_function_t _fself, kk_box_t _b_17926, kk_context_t* _ctx) {
  struct kk_std_core__mlift17149_map_fun19507__t_4* _self = kk_function_as(struct kk_std_core__mlift17149_map_fun19507__t_4*, _fself);
  kk_box_t _y_16912 = _self->_y_16912; /* 5784 */
  kk_box_t _y_16913 = _self->_y_16913; /* 5784 */
  kk_box_t _y_16914 = _self->_y_16914; /* 5784 */
  kk_drop_match(_self, {kk_box_dup(_y_16912);kk_box_dup(_y_16913);kk_box_dup(_y_16914);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19508 = kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, _y_16912, _y_16913, _y_16914, _b_17926, _ctx); /*(22, 23, 24, 25)*/
  return kk_std_core_types__tuple4__box(_x19508, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17149_map_4(kk_box_t _y_16912, kk_box_t _y_16913, kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16914, kk_context_t* _ctx) { /* forall<a,b,e> (b, b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17299;
  kk_box_t _x19504;
  {
    struct kk_std_core_types__Tuple4_* _con19505 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _pat00 = _con19505->fst;
    kk_box_t _pat10 = _con19505->snd;
    kk_box_t _pat2 = _con19505->thd;
    kk_box_t _x = _con19505->field4;
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
    _x19504 = _x; /*5783*/
  }
  x_17299 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19504, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17299, _ctx);
    kk_box_t _x19506 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17149_map_fun19507_4(_y_16912, _y_16913, _y_16914, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19506, _ctx);
  }
  {
    return kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, _y_16912, _y_16913, _y_16914, x_17299, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17150_map_fun19513__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16912;
  kk_box_t _y_16913;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core__mlift17150_map_fun19513_4(kk_function_t _fself, kk_box_t _b_17930, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17150_map_fun19513_4(kk_box_t _y_16912, kk_box_t _y_16913, kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17150_map_fun19513__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17150_map_fun19513__t_4, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17150_map_fun19513_4, kk_context());
  _self->_y_16912 = _y_16912;
  _self->_y_16913 = _y_16913;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17150_map_fun19513_4(kk_function_t _fself, kk_box_t _b_17930, kk_context_t* _ctx) {
  struct kk_std_core__mlift17150_map_fun19513__t_4* _self = kk_function_as(struct kk_std_core__mlift17150_map_fun19513__t_4*, _fself);
  kk_box_t _y_16912 = _self->_y_16912; /* 5784 */
  kk_box_t _y_16913 = _self->_y_16913; /* 5784 */
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_box_dup(_y_16912);kk_box_dup(_y_16913);kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19514 = kk_std_core__mlift17149_map_4(_y_16912, _y_16913, f, t, _b_17930, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19514, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17150_map_4(kk_box_t _y_16912, kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16913, kk_context_t* _ctx) { /* forall<a,b,e> (b, f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17306;
  kk_function_t _x19511 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19509;
  {
    struct kk_std_core_types__Tuple4_* _con19510 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19510->thd;
    kk_box_dup(_x);
    _x19509 = _x; /*5783*/
  }
  x_17306 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19511, (_x19511, _x19509, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17306, _ctx);
    kk_box_t _x19512 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17150_map_fun19513_4(_y_16912, _y_16913, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19512, _ctx);
  }
  {
    return kk_std_core__mlift17149_map_4(_y_16912, _y_16913, f, t, x_17306, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17151_map_fun19519__t_4 {
  struct kk_function_s _base;
  kk_box_t _y_16912;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core__mlift17151_map_fun19519_4(kk_function_t _fself, kk_box_t _b_17934, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17151_map_fun19519_4(kk_box_t _y_16912, kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core__mlift17151_map_fun19519__t_4* _self = kk_function_alloc_as(struct kk_std_core__mlift17151_map_fun19519__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17151_map_fun19519_4, kk_context());
  _self->_y_16912 = _y_16912;
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17151_map_fun19519_4(kk_function_t _fself, kk_box_t _b_17934, kk_context_t* _ctx) {
  struct kk_std_core__mlift17151_map_fun19519__t_4* _self = kk_function_as(struct kk_std_core__mlift17151_map_fun19519__t_4*, _fself);
  kk_box_t _y_16912 = _self->_y_16912; /* 5784 */
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_box_dup(_y_16912);kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19520 = kk_std_core__mlift17150_map_4(_y_16912, f, t, _b_17934, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19520, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core__mlift17151_map_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t _y_16912, kk_context_t* _ctx) { /* forall<a,b,e> (f : (a) -> e b, t : (a, a, a, a), b) -> e (b, b, b, b) */ 
  kk_box_t x_17309;
  kk_function_t _x19517 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19515;
  {
    struct kk_std_core_types__Tuple4_* _con19516 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19516->snd;
    kk_box_dup(_x);
    _x19515 = _x; /*5783*/
  }
  x_17309 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19517, (_x19517, _x19515, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17309, _ctx);
    kk_box_t _x19518 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17151_map_fun19519_4(_y_16912, f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19518, _ctx);
  }
  {
    return kk_std_core__mlift17150_map_4(_y_16912, f, t, x_17309, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_map_fun19525__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
};
static kk_box_t kk_std_core_map_fun19525_4(kk_function_t _fself, kk_box_t _b_17938, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19525_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19525__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19525__t_4, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19525_4, kk_context());
  _self->f = f;
  _self->t = t;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19525_4(kk_function_t _fself, kk_box_t _b_17938, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19525__t_4* _self = kk_function_as(struct kk_std_core_map_fun19525__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19526 = kk_std_core__mlift17151_map_4(f, t, _b_17938, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19526, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19531__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
  kk_box_t x_17312;
};
static kk_box_t kk_std_core_map_fun19531_4(kk_function_t _fself, kk_box_t _b_17940, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19531_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t x_17312, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19531__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19531__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19531_4, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17312 = x_17312;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19531_4(kk_function_t _fself, kk_box_t _b_17940, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19531__t_4* _self = kk_function_as(struct kk_std_core_map_fun19531__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_box_t x_17312 = _self->x_17312; /* 5784 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);kk_box_dup(x_17312);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19532 = kk_std_core__mlift17150_map_4(x_17312, f, t, _b_17940, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19532, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19537__t_4 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_std_core_types__tuple4_ t;
  kk_box_t x_17312;
  kk_box_t x0_17316;
};
static kk_box_t kk_std_core_map_fun19537_4(kk_function_t _fself, kk_box_t _b_17942, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19537_4(kk_function_t f, kk_std_core_types__tuple4_ t, kk_box_t x_17312, kk_box_t x0_17316, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19537__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19537__t_4, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19537_4, kk_context());
  _self->f = f;
  _self->t = t;
  _self->x_17312 = x_17312;
  _self->x0_17316 = x0_17316;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19537_4(kk_function_t _fself, kk_box_t _b_17942, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19537__t_4* _self = kk_function_as(struct kk_std_core_map_fun19537__t_4*, _fself);
  kk_function_t f = _self->f; /* (5783) -> 5785 5784 */
  kk_std_core_types__tuple4_ t = _self->t; /* (5783, 5783, 5783, 5783) */
  kk_box_t x_17312 = _self->x_17312; /* 5784 */
  kk_box_t x0_17316 = _self->x0_17316; /* 5784 */
  kk_drop_match(_self, {kk_function_dup(f);kk_std_core_types__tuple4__dup(t);kk_box_dup(x_17312);kk_box_dup(x0_17316);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19538 = kk_std_core__mlift17149_map_4(x_17312, x0_17316, f, t, _b_17942, _ctx); /*(5784, 5784, 5784, 5784)*/
  return kk_std_core_types__tuple4__box(_x19538, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19542__t_4 {
  struct kk_function_s _base;
  kk_box_t x_17312;
  kk_box_t x0_17316;
  kk_box_t x1_17320;
};
static kk_box_t kk_std_core_map_fun19542_4(kk_function_t _fself, kk_box_t _b_17944, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19542_4(kk_box_t x_17312, kk_box_t x0_17316, kk_box_t x1_17320, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19542__t_4* _self = kk_function_alloc_as(struct kk_std_core_map_fun19542__t_4, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19542_4, kk_context());
  _self->x_17312 = x_17312;
  _self->x0_17316 = x0_17316;
  _self->x1_17320 = x1_17320;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19542_4(kk_function_t _fself, kk_box_t _b_17944, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19542__t_4* _self = kk_function_as(struct kk_std_core_map_fun19542__t_4*, _fself);
  kk_box_t x_17312 = _self->x_17312; /* 5784 */
  kk_box_t x0_17316 = _self->x0_17316; /* 5784 */
  kk_box_t x1_17320 = _self->x1_17320; /* 5784 */
  kk_drop_match(_self, {kk_box_dup(x_17312);kk_box_dup(x0_17316);kk_box_dup(x1_17320);}, {}, _ctx)
  kk_std_core_types__tuple4_ _x19543 = kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, x_17312, x0_17316, x1_17320, _b_17944, _ctx); /*(22, 23, 24, 25)*/
  return kk_std_core_types__tuple4__box(_x19543, _ctx);
}

kk_std_core_types__tuple4_ kk_std_core_map_4(kk_std_core_types__tuple4_ t, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (t : (a, a, a, a), f : (a) -> e b) -> e (b, b, b, b) */ 
  kk_box_t x_17312;
  kk_function_t _x19523 = kk_function_dup(f); /*(5783) -> 5785 5784*/
  kk_box_t _x19521;
  {
    struct kk_std_core_types__Tuple4_* _con19522 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
    kk_box_t _x = _con19522->fst;
    kk_box_dup(_x);
    _x19521 = _x; /*5783*/
  }
  x_17312 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19523, (_x19523, _x19521, _ctx)); /*5784*/
  if (kk_yielding(kk_context())) {
    kk_box_drop(x_17312, _ctx);
    kk_box_t _x19524 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19525_4(f, t, _ctx), _ctx); /*3860*/
    return kk_std_core_types__tuple4__unbox(_x19524, _ctx);
  }
  {
    kk_box_t x0_17316;
    kk_function_t _x19529 = kk_function_dup(f); /*(5783) -> 5785 5784*/
    kk_box_t _x19527;
    {
      struct kk_std_core_types__Tuple4_* _con19528 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
      kk_box_t _x0 = _con19528->snd;
      kk_box_dup(_x0);
      _x19527 = _x0; /*5783*/
    }
    x0_17316 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19529, (_x19529, _x19527, _ctx)); /*5784*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17316, _ctx);
      kk_box_t _x19530 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19531_4(f, t, x_17312, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple4__unbox(_x19530, _ctx);
    }
    {
      kk_box_t x1_17320;
      kk_function_t _x19535 = kk_function_dup(f); /*(5783) -> 5785 5784*/
      kk_box_t _x19533;
      {
        struct kk_std_core_types__Tuple4_* _con19534 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
        kk_box_t _x1 = _con19534->thd;
        kk_box_dup(_x1);
        _x19533 = _x1; /*5783*/
      }
      x1_17320 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19535, (_x19535, _x19533, _ctx)); /*5784*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x1_17320, _ctx);
        kk_box_t _x19536 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19537_4(f, t, x_17312, x0_17316, _ctx), _ctx); /*3860*/
        return kk_std_core_types__tuple4__unbox(_x19536, _ctx);
      }
      {
        kk_box_t x2_17324;
        kk_box_t _x19539;
        {
          struct kk_std_core_types__Tuple4_* _con19540 = kk_std_core_types__as_dash__lp__comma__comma__comma__rp_(t);
          kk_box_t _pat06 = _con19540->fst;
          kk_box_t _pat13 = _con19540->snd;
          kk_box_t _pat22 = _con19540->thd;
          kk_box_t _x2 = _con19540->field4;
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
          _x19539 = _x2; /*5783*/
        }
        x2_17324 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19539, _ctx)); /*5784*/
        if (kk_yielding(kk_context())) {
          kk_box_drop(x2_17324, _ctx);
          kk_box_t _x19541 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19542_4(x_17312, x0_17316, x1_17320, _ctx), _ctx); /*3860*/
          return kk_std_core_types__tuple4__unbox(_x19541, _ctx);
        }
        {
          return kk_std_core_types__new_dash__lp__comma__comma__comma__rp_(kk_reuse_null, x_17312, x0_17316, x1_17320, x2_17324, _ctx);
        }
      }
    }
  }
}
 
// Apply a function `f` to each character in a string


// lift anonymous function
struct kk_std_core_map_fun19544__t_6 {
  struct kk_function_s _base;
  kk_function_t f;
};
static kk_box_t kk_std_core_map_fun19544_6(kk_function_t _fself, kk_box_t _b_17955, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19544_6(kk_function_t f, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19544__t_6* _self = kk_function_alloc_as(struct kk_std_core_map_fun19544__t_6, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19544_6, kk_context());
  _self->f = f;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19544_6(kk_function_t _fself, kk_box_t _b_17955, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19544__t_6* _self = kk_function_as(struct kk_std_core_map_fun19544__t_6*, _fself);
  kk_function_t f = _self->f; /* (char) -> 6224 char */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  kk_char_t _x19545;
  kk_char_t _x19546 = kk_char_unbox(_b_17955, _ctx); /*char*/
  _x19545 = kk_function_call(kk_char_t, (kk_function_t, kk_char_t, kk_context_t*), f, (f, _x19546, _ctx)); /*char*/
  return kk_char_box(_x19545, _ctx);
}


// lift anonymous function
struct kk_std_core_map_fun19548__t_6 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_map_fun19548_6(kk_function_t _fself, kk_box_t _b_17959, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19548_6(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_fun19548_6, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_map_fun19548_6(kk_function_t _fself, kk_box_t _b_17959, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19549;
  kk_std_core__list _x19550 = kk_std_core__list_unbox(_b_17959, _ctx); /*list<char>*/
  _x19549 = kk_std_core_string_2(_x19550, _ctx); /*string*/
  return kk_string_box(_x19549);
}

kk_string_t kk_std_core_map_6(kk_string_t s, kk_function_t f, kk_context_t* _ctx) { /* forall<e> (s : string, f : (char) -> e char) -> e string */ 
  kk_std_core__list _b_17956_17953 = kk_std_core_list_4(s, _ctx); /*list<char>*/;
  kk_std_core__list x_17332 = kk_std_core_map_5(_b_17956_17953, kk_std_core_new_map_fun19544_6(f, _ctx), _ctx); /*list<char>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17332, _ctx);
    kk_box_t _x19547 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19548_6(_ctx), _ctx); /*3860*/
    return kk_string_unbox(_x19547);
  }
  {
    return kk_std_core_string_2(x_17332, _ctx);
  }
}
 
// Apply a total function `f` to each element in a vector `v`


// lift anonymous function
struct kk_std_core_map_fun19558__t_7 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
  kk_vector_t w;
};
static kk_unit_t kk_std_core_map_fun19558_7(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19558_7(kk_function_t f, kk_vector_t v, kk_vector_t w, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19558__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19558__t_7, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19558_7, kk_context());
  _self->f = f;
  _self->v = v;
  _self->w = w;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_map_fun19560__t_7 {
  struct kk_function_s _base;
  kk_vector_t w;
  kk_ssize_t i;
};
static kk_box_t kk_std_core_map_fun19560_7(kk_function_t _fself, kk_box_t _b_17962, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19560_7(kk_vector_t w, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19560__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19560__t_7, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19560_7, kk_context());
  _self->w = w;
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19560_7(kk_function_t _fself, kk_box_t _b_17962, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19560__t_7* _self = kk_function_as(struct kk_std_core_map_fun19560__t_7*, _fself);
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_ssize_t i = _self->i; /* ssize_t */
  kk_drop_match(_self, {kk_vector_dup(w);;}, {}, _ctx)
  kk_unit_t _x19561 = kk_Unit;
  kk_vector_unsafe_assign(w,i,_b_17962,kk_context());
  return kk_unit_box(_x19561);
}
static kk_unit_t kk_std_core_map_fun19558_7(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19558__t_7* _self = kk_function_as(struct kk_std_core_map_fun19558__t_7*, _fself);
  kk_function_t f = _self->f; /* (6272) -> 6274 6273 */
  kk_vector_t v = _self->v; /* vector<6272> */
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);kk_vector_dup(w);}, {}, _ctx)
  kk_box_t x0_17570 = kk_vector_at(v,i,kk_context()); /*6272*/;
  kk_box_t x1_17340 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x0_17570, _ctx)); /*6273*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x1_17340, _ctx);
    kk_box_t _x19559 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19560_7(w, i, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x19559);
  }
  {
    return kk_vector_unsafe_assign(w,i,x1_17340,kk_context());
  }
}


// lift anonymous function
struct kk_std_core_map_fun19563__t_7 {
  struct kk_function_s _base;
  kk_vector_t w;
};
static kk_box_t kk_std_core_map_fun19563_7(kk_function_t _fself, kk_box_t _b_17966, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_fun19563_7(kk_vector_t w, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19563__t_7* _self = kk_function_alloc_as(struct kk_std_core_map_fun19563__t_7, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_map_fun19563_7, kk_context());
  _self->w = w;
  return &_self->_base;
}

static kk_box_t kk_std_core_map_fun19563_7(kk_function_t _fself, kk_box_t _b_17966, kk_context_t* _ctx) {
  struct kk_std_core_map_fun19563__t_7* _self = kk_function_as(struct kk_std_core_map_fun19563__t_7*, _fself);
  kk_vector_t w = _self->w; /* vector<6273> */
  kk_drop_match(_self, {kk_vector_dup(w);}, {}, _ctx)
  kk_box_drop(_b_17966, _ctx);
  return kk_vector_box(w, _ctx);
}

kk_vector_t kk_std_core_map_7(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (v : vector<a>, f : (a) -> e b) -> e vector<b> */ 
  kk_vector_t w;
  kk_ssize_t _x19551;
  kk_integer_t _x19552;
  kk_ssize_t _x19553;
  kk_vector_t _x19554 = kk_vector_dup(v); /*vector<6272>*/
  _x19553 = kk_vector_len(_x19554,kk_context()); /*ssize_t*/
  _x19552 = kk_integer_from_ssize_t(_x19553,kk_context()); /*int*/
  _x19551 = kk_std_core_ssize__t(_x19552, _ctx); /*ssize_t*/
  w = kk_vector_alloc_uninit(_x19551,NULL,kk_context()); /*vector<6273>*/
  kk_ssize_t start0_17345 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17346;
  kk_ssize_t _x19555;
  kk_vector_t _x19556 = kk_vector_dup(v); /*vector<6272>*/
  _x19555 = kk_vector_len(_x19556,kk_context()); /*ssize_t*/
  end_17346 = kk_std_core_decr_1(_x19555, _ctx); /*ssize_t*/
  kk_unit_t x_17335 = kk_Unit;
  kk_function_t _x19557;
  kk_vector_dup(w);
  _x19557 = kk_std_core_new_map_fun19558_7(f, v, w, _ctx); /*(i : ssize_t) -> 6274 ()*/
  kk_std_core__lift16739_forz(_x19557, end_17346, start0_17345, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19562 = kk_std_core_hnd_yield_extend(kk_std_core_new_map_fun19563_7(w, _ctx), _ctx); /*3860*/
    return kk_vector_unbox(_x19562, _ctx);
  }
  {
    return w;
  }
}
 
// Right-align a string to width `width`  using `fill`  (default is a space) to fill from the left.

kk_string_t kk_std_core_pad_left(kk_string_t s, kk_integer_t width, kk_std_core_types__optional fill, kk_context_t* _ctx) { /* (s : string, width : int, fill : optional<char>) -> string */ 
  kk_ssize_t w = kk_std_core_ssize__t(width, _ctx); /*ssize_t*/;
  kk_ssize_t n;
  kk_string_t _x19564 = kk_string_dup(s); /*string*/
  n = kk_string_len(_x19564,kk_context()); /*ssize_t*/
  bool _match_19143 = (w <= n); /*bool*/;
  if (_match_19143) {
    kk_std_core_types__optional_drop(fill, _ctx);
    return s;
  }
  {
    kk_string_t _x19565;
    kk_string_t _x19566;
    kk_char_t _x19567;
    if (kk_std_core_types__is_Optional(fill)) {
      kk_box_t _box_x17969 = fill._cons.Optional.value;
      kk_char_t _fill_6932 = kk_char_unbox(_box_x17969, NULL);
      kk_std_core_types__optional_drop(fill, _ctx);
      _x19567 = _fill_6932; /*char*/
      goto _match19568;
    }
    {
      _x19567 = ' '; /*char*/
    }
    _match19568: ;
    _x19566 = kk_std_core_string(_x19567, _ctx); /*string*/
    kk_ssize_t _x19570 = (w - n); /*ssize_t*/
    _x19565 = kk_std_core_repeatz(_x19566, _x19570, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(_x19565, s, _ctx);
  }
}
 
// Show an `:int` as a hexadecimal value.
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).

kk_string_t kk_std_core_show_hex(kk_integer_t i, kk_std_core_types__optional width, kk_std_core_types__optional use_capitals, kk_std_core_types__optional pre, kk_context_t* _ctx) { /* (i : int, width : optional<int>, use-capitals : optional<bool>, pre : optional<string>) -> string */ 
  kk_string_t _x19571;
  bool _match_19142;
  kk_integer_t _x19572 = kk_integer_dup(i); /*int*/
  _match_19142 = kk_integer_lt(_x19572,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19142) {
    kk_define_string_literal(, _s19573, 1, "-")
    _x19571 = kk_string_dup(_s19573); /*string*/
  }
  else {
    _x19571 = kk_string_empty(); /*string*/
  }
  kk_string_t _x19575;
  kk_string_t _x19576;
  if (kk_std_core_types__is_Optional(pre)) {
    kk_box_t _box_x17970 = pre._cons.Optional.value;
    kk_string_t _pre_7040 = kk_string_unbox(_box_x17970);
    kk_string_dup(_pre_7040);
    kk_std_core_types__optional_drop(pre, _ctx);
    _x19576 = _pre_7040; /*string*/
    goto _match19577;
  }
  {
    kk_define_string_literal(, _s19579, 2, "0x")
    _x19576 = kk_string_dup(_s19579); /*string*/
  }
  _match19577: ;
  kk_string_t _x19580;
  kk_string_t _x19581;
  kk_integer_t _x19582 = kk_integer_abs(i,kk_context()); /*int*/
  bool _x19583;
  if (kk_std_core_types__is_Optional(use_capitals)) {
    kk_box_t _box_x17971 = use_capitals._cons.Optional.value;
    bool _use_capitals_7036 = kk_bool_unbox(_box_x17971);
    kk_std_core_types__optional_drop(use_capitals, _ctx);
    _x19583 = _use_capitals_7036; /*bool*/
    goto _match19584;
  }
  {
    _x19583 = true; /*bool*/
  }
  _match19584: ;
  _x19581 = kk_std_core_int_show_hex(_x19582, _x19583, _ctx); /*string*/
  kk_integer_t _x19586;
  if (kk_std_core_types__is_Optional(width)) {
    kk_box_t _box_x17972 = width._cons.Optional.value;
    kk_integer_t _width_7032 = kk_integer_unbox(_box_x17972);
    kk_integer_dup(_width_7032);
    kk_std_core_types__optional_drop(width, _ctx);
    _x19586 = _width_7032; /*int*/
    goto _match19587;
  }
  {
    _x19586 = kk_integer_from_small(1); /*int*/
  }
  _match19587: ;
  kk_std_core_types__optional _x19589 = kk_std_core_types__new_Optional(kk_char_box('0', _ctx), _ctx); /*optional<110>*/
  _x19580 = kk_std_core_pad_left(_x19581, _x19586, _x19589, _ctx); /*string*/
  _x19575 = kk_std_core__lp__plus__plus__1_rp_(_x19576, _x19580, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19571, _x19575, _ctx);
}
 
// Show a character as a string

kk_string_t kk_std_core_show_char(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> string */ 
  bool _match_19124 = (c < (' ')); /*bool*/;
  if (_match_19124) {
    bool _match_19134 = (c == 0x000A); /*bool*/;
    if (_match_19134) {
      kk_define_string_literal(, _s19598, 2, "\\n")
      return kk_string_dup(_s19598);
    }
    {
      bool _match_19135 = (c == 0x000D); /*bool*/;
      if (_match_19135) {
        kk_define_string_literal(, _s19599, 2, "\\r")
        return kk_string_dup(_s19599);
      }
      {
        bool _match_19136 = (c == 0x0009); /*bool*/;
        if (_match_19136) {
          kk_define_string_literal(, _s19600, 2, "\\t")
          return kk_string_dup(_s19600);
        }
        {
          bool _match_19137;
          kk_integer_t _x19601 = kk_integer_from_int(c,kk_context()); /*int*/
          _match_19137 = kk_integer_lte(_x19601,(kk_integer_from_small(255)),kk_context()); /*bool*/
          if (_match_19137) {
            kk_string_t _x19602;
            kk_define_string_literal(, _s19603, 2, "\\x")
            _x19602 = kk_string_dup(_s19603); /*string*/
            kk_string_t _x19604;
            kk_integer_t _arg_7506 = kk_integer_from_int(c,kk_context()); /*int*/;
            kk_std_core_types__optional _x19605 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(2)), _ctx); /*optional<110>*/
            kk_std_core_types__optional _x19606;
            kk_box_t _x19607;
            kk_string_t _x19608 = kk_string_empty(); /*string*/
            _x19607 = kk_string_box(_x19608); /*110*/
            _x19606 = kk_std_core_types__new_Optional(_x19607, _ctx); /*optional<110>*/
            _x19604 = kk_std_core_show_hex(_arg_7506, _x19605, kk_std_core_types__new_None(_ctx), _x19606, _ctx); /*string*/
            return kk_std_core__lp__plus__plus__1_rp_(_x19602, _x19604, _ctx);
          }
          {
            bool _match_19138;
            kk_integer_t _x19610 = kk_integer_from_int(c,kk_context()); /*int*/
            _match_19138 = kk_integer_lte(_x19610,(kk_integer_from_int(65535, _ctx)),kk_context()); /*bool*/
            if (_match_19138) {
              kk_string_t _x19611;
              kk_define_string_literal(, _s19612, 2, "\\u")
              _x19611 = kk_string_dup(_s19612); /*string*/
              kk_string_t _x19613;
              kk_integer_t _arg_7591 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19614 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(4)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19615;
              kk_box_t _x19616;
              kk_string_t _x19617 = kk_string_empty(); /*string*/
              _x19616 = kk_string_box(_x19617); /*110*/
              _x19615 = kk_std_core_types__new_Optional(_x19616, _ctx); /*optional<110>*/
              _x19613 = kk_std_core_show_hex(_arg_7591, _x19614, kk_std_core_types__new_None(_ctx), _x19615, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19611, _x19613, _ctx);
            }
            {
              kk_string_t _x19619;
              kk_define_string_literal(, _s19620, 2, "\\U")
              _x19619 = kk_string_dup(_s19620); /*string*/
              kk_string_t _x19621;
              kk_integer_t _arg_7634 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19622 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(6)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19623;
              kk_box_t _x19624;
              kk_string_t _x19625 = kk_string_empty(); /*string*/
              _x19624 = kk_string_box(_x19625); /*110*/
              _x19623 = kk_std_core_types__new_Optional(_x19624, _ctx); /*optional<110>*/
              _x19621 = kk_std_core_show_hex(_arg_7634, _x19622, kk_std_core_types__new_None(_ctx), _x19623, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19619, _x19621, _ctx);
            }
          }
        }
      }
    }
  }
  {
    bool _match_19125 = (c > ('~')); /*bool*/;
    if (_match_19125) {
      bool _match_19129 = (c == 0x000A); /*bool*/;
      if (_match_19129) {
        kk_define_string_literal(, _s19627, 2, "\\n")
        return kk_string_dup(_s19627);
      }
      {
        bool _match_19130 = (c == 0x000D); /*bool*/;
        if (_match_19130) {
          kk_define_string_literal(, _s19628, 2, "\\r")
          return kk_string_dup(_s19628);
        }
        {
          bool _match_19131 = (c == 0x0009); /*bool*/;
          if (_match_19131) {
            kk_define_string_literal(, _s19629, 2, "\\t")
            return kk_string_dup(_s19629);
          }
          {
            bool _match_19132;
            kk_integer_t _x19630 = kk_integer_from_int(c,kk_context()); /*int*/
            _match_19132 = kk_integer_lte(_x19630,(kk_integer_from_small(255)),kk_context()); /*bool*/
            if (_match_19132) {
              kk_string_t _x19631;
              kk_define_string_literal(, _s19632, 2, "\\x")
              _x19631 = kk_string_dup(_s19632); /*string*/
              kk_string_t _x19633;
              kk_integer_t _arg_75060 = kk_integer_from_int(c,kk_context()); /*int*/;
              kk_std_core_types__optional _x19634 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(2)), _ctx); /*optional<110>*/
              kk_std_core_types__optional _x19635;
              kk_box_t _x19636;
              kk_string_t _x19637 = kk_string_empty(); /*string*/
              _x19636 = kk_string_box(_x19637); /*110*/
              _x19635 = kk_std_core_types__new_Optional(_x19636, _ctx); /*optional<110>*/
              _x19633 = kk_std_core_show_hex(_arg_75060, _x19634, kk_std_core_types__new_None(_ctx), _x19635, _ctx); /*string*/
              return kk_std_core__lp__plus__plus__1_rp_(_x19631, _x19633, _ctx);
            }
            {
              bool _match_19133;
              kk_integer_t _x19639 = kk_integer_from_int(c,kk_context()); /*int*/
              _match_19133 = kk_integer_lte(_x19639,(kk_integer_from_int(65535, _ctx)),kk_context()); /*bool*/
              if (_match_19133) {
                kk_string_t _x19640;
                kk_define_string_literal(, _s19641, 2, "\\u")
                _x19640 = kk_string_dup(_s19641); /*string*/
                kk_string_t _x19642;
                kk_integer_t _arg_75910 = kk_integer_from_int(c,kk_context()); /*int*/;
                kk_std_core_types__optional _x19643 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(4)), _ctx); /*optional<110>*/
                kk_std_core_types__optional _x19644;
                kk_box_t _x19645;
                kk_string_t _x19646 = kk_string_empty(); /*string*/
                _x19645 = kk_string_box(_x19646); /*110*/
                _x19644 = kk_std_core_types__new_Optional(_x19645, _ctx); /*optional<110>*/
                _x19642 = kk_std_core_show_hex(_arg_75910, _x19643, kk_std_core_types__new_None(_ctx), _x19644, _ctx); /*string*/
                return kk_std_core__lp__plus__plus__1_rp_(_x19640, _x19642, _ctx);
              }
              {
                kk_string_t _x19648;
                kk_define_string_literal(, _s19649, 2, "\\U")
                _x19648 = kk_string_dup(_s19649); /*string*/
                kk_string_t _x19650;
                kk_integer_t _arg_76340 = kk_integer_from_int(c,kk_context()); /*int*/;
                kk_std_core_types__optional _x19651 = kk_std_core_types__new_Optional(kk_integer_box(kk_integer_from_small(6)), _ctx); /*optional<110>*/
                kk_std_core_types__optional _x19652;
                kk_box_t _x19653;
                kk_string_t _x19654 = kk_string_empty(); /*string*/
                _x19653 = kk_string_box(_x19654); /*110*/
                _x19652 = kk_std_core_types__new_Optional(_x19653, _ctx); /*optional<110>*/
                _x19650 = kk_std_core_show_hex(_arg_76340, _x19651, kk_std_core_types__new_None(_ctx), _x19652, _ctx); /*string*/
                return kk_std_core__lp__plus__plus__1_rp_(_x19648, _x19650, _ctx);
              }
            }
          }
        }
      }
    }
    {
      bool _match_19126 = (c == ('\'')); /*bool*/;
      if (_match_19126) {
        kk_define_string_literal(, _s19656, 2, "\\\'")
        return kk_string_dup(_s19656);
      }
      {
        bool _match_19127 = (c == ('"')); /*bool*/;
        if (_match_19127) {
          kk_define_string_literal(, _s19657, 2, "\\\"")
          return kk_string_dup(_s19657);
        }
        {
          bool _match_19128 = (c == ('\\')); /*bool*/;
          if (_match_19128) {
            kk_define_string_literal(, _s19658, 2, "\\\\")
            return kk_string_dup(_s19658);
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
  bool _match_19122 = (dabs < (1.0e-15)); /*bool*/;
  if (_match_19122) {
    int32_t _x19663;
    kk_integer_t _x19664;
    if (kk_std_core_types__is_Optional(precision)) {
      kk_box_t _box_x18000 = precision._cons.Optional.value;
      kk_integer_t _precision_7723 = kk_integer_unbox(_box_x18000);
      kk_integer_dup(_precision_7723);
      kk_std_core_types__optional_drop(precision, _ctx);
      _x19664 = _precision_7723; /*int*/
      goto _match19665;
    }
    {
      _x19664 = kk_integer_from_small(-2); /*int*/
    }
    _match19665: ;
    _x19663 = kk_std_core_int32(_x19664, _ctx); /*int32*/
    return kk_std_core_show_expx(d, _x19663, _ctx);
  }
  {
    bool _match_19123 = (dabs > (1.0e21)); /*bool*/;
    if (_match_19123) {
      int32_t _x19667;
      kk_integer_t _x19668;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18001 = precision._cons.Optional.value;
        kk_integer_t _precision_77230 = kk_integer_unbox(_box_x18001);
        kk_integer_dup(_precision_77230);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19668 = _precision_77230; /*int*/
        goto _match19669;
      }
      {
        _x19668 = kk_integer_from_small(-2); /*int*/
      }
      _match19669: ;
      _x19667 = kk_std_core_int32(_x19668, _ctx); /*int32*/
      return kk_std_core_show_expx(d, _x19667, _ctx);
    }
    {
      int32_t _x19671;
      kk_integer_t _x19672;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18002 = precision._cons.Optional.value;
        kk_integer_t _precision_77231 = kk_integer_unbox(_box_x18002);
        kk_integer_dup(_precision_77231);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19672 = _precision_77231; /*int*/
        goto _match19673;
      }
      {
        _x19672 = kk_integer_from_small(-2); /*int*/
      }
      _match19673: ;
      _x19671 = kk_std_core_int32(_x19672, _ctx); /*int32*/
      return kk_std_core_show_fixedx(d, _x19671, _ctx);
    }
  }
}
 
// lifted

kk_string_t kk_std_core__lift16744_show_list(kk_string_t sep, kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (sep : string, ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19675 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18003 = _con19675->head;
    kk_std_core__list yy = _con19675->tail;
    kk_string_t y = kk_string_unbox(_box_x18003);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    kk_string_t acc0_16774;
    kk_string_t _x19677;
    kk_string_t _x19678 = kk_string_dup(sep); /*string*/
    _x19677 = kk_std_core__lp__plus__plus__1_rp_(_x19678, y, _ctx); /*string*/
    acc0_16774 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19677, _ctx); /*string*/
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

kk_string_t kk_std_core__mlift17154_show_list(kk_std_core__list _y_16919, kk_context_t* _ctx) { /* forall<e> (list<string>) -> e string */ 
  kk_string_t _x19679;
  kk_define_string_literal(, _s19680, 1, "[")
  _x19679 = kk_string_dup(_s19680); /*string*/
  kk_string_t _x19681;
  kk_string_t _x19682;
  if (kk_std_core__is_Nil(_y_16919)) {
    _x19682 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19684 = kk_std_core__as_Cons(_y_16919);
    kk_box_t _box_x18004 = _con19684->head;
    kk_std_core__list xx = _con19684->tail;
    kk_string_t x = kk_string_unbox(_box_x18004);
    if (kk_likely(kk_std_core__list_is_unique(_y_16919))) {
      kk_std_core__list_free(_y_16919);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(_y_16919, _ctx);
    }
    kk_string_t _x19686;
    kk_define_string_literal(, _s19687, 1, ",")
    _x19686 = kk_string_dup(_s19687); /*string*/
    _x19682 = kk_std_core__lift16744_show_list(_x19686, xx, x, _ctx); /*string*/
  }
  kk_string_t _x19688;
  kk_define_string_literal(, _s19689, 1, "]")
  _x19688 = kk_string_dup(_s19689); /*string*/
  _x19681 = kk_std_core__lp__plus__plus__1_rp_(_x19682, _x19688, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19679, _x19681, _ctx);
}
 
// Convert a list to a string


// lift anonymous function
struct kk_std_core_show_list_fun19690__t {
  struct kk_function_s _base;
  kk_function_t show_elem;
};
static kk_box_t kk_std_core_show_list_fun19690(kk_function_t _fself, kk_box_t _b_18007, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_list_fun19690(kk_function_t show_elem, kk_context_t* _ctx) {
  struct kk_std_core_show_list_fun19690__t* _self = kk_function_alloc_as(struct kk_std_core_show_list_fun19690__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_show_list_fun19690, kk_context());
  _self->show_elem = show_elem;
  return &_self->_base;
}

static kk_box_t kk_std_core_show_list_fun19690(kk_function_t _fself, kk_box_t _b_18007, kk_context_t* _ctx) {
  struct kk_std_core_show_list_fun19690__t* _self = kk_function_as(struct kk_std_core_show_list_fun19690__t*, _fself);
  kk_function_t show_elem = _self->show_elem; /* (8268) -> 8269 string */
  kk_drop_match(_self, {kk_function_dup(show_elem);}, {}, _ctx)
  kk_string_t _x19691 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), show_elem, (show_elem, _b_18007, _ctx)); /*string*/
  return kk_string_box(_x19691);
}


// lift anonymous function
struct kk_std_core_show_list_fun19693__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_show_list_fun19693(kk_function_t _fself, kk_box_t _b_18012, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_list_fun19693(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_show_list_fun19693, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_show_list_fun19693(kk_function_t _fself, kk_box_t _b_18012, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19694;
  kk_string_t _x19695;
  kk_define_string_literal(, _s19696, 1, "[")
  _x19695 = kk_string_dup(_s19696); /*string*/
  kk_string_t _x19697;
  kk_string_t _x19698;
  kk_std_core__list _match_19121 = kk_std_core__list_unbox(_b_18012, _ctx); /*list<string>*/;
  if (kk_std_core__is_Nil(_match_19121)) {
    _x19698 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19700 = kk_std_core__as_Cons(_match_19121);
    kk_box_t _box_x18010 = _con19700->head;
    kk_std_core__list xx = _con19700->tail;
    kk_string_t x0 = kk_string_unbox(_box_x18010);
    if (kk_likely(kk_std_core__list_is_unique(_match_19121))) {
      kk_std_core__list_free(_match_19121);
    }
    else {
      kk_string_dup(x0);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(_match_19121, _ctx);
    }
    kk_string_t _x19702;
    kk_define_string_literal(, _s19703, 1, ",")
    _x19702 = kk_string_dup(_s19703); /*string*/
    _x19698 = kk_std_core__lift16744_show_list(_x19702, xx, x0, _ctx); /*string*/
  }
  kk_string_t _x19704;
  kk_define_string_literal(, _s19705, 1, "]")
  _x19704 = kk_string_dup(_s19705); /*string*/
  _x19697 = kk_std_core__lp__plus__plus__1_rp_(_x19698, _x19704, _ctx); /*string*/
  _x19694 = kk_std_core__lp__plus__plus__1_rp_(_x19695, _x19697, _ctx); /*string*/
  return kk_string_box(_x19694);
}

kk_string_t kk_std_core_show_list(kk_std_core__list xs, kk_function_t show_elem, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, show-elem : (a) -> e string) -> e string */ 
  kk_std_core__list x_17350 = kk_std_core_map_5(xs, kk_std_core_new_show_list_fun19690(show_elem, _ctx), _ctx); /*list<string>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17350, _ctx);
    kk_box_t _x19692 = kk_std_core_hnd_yield_extend(kk_std_core_new_show_list_fun19693(_ctx), _ctx); /*3860*/
    return kk_string_unbox(_x19692);
  }
  {
    kk_string_t _x19706;
    kk_define_string_literal(, _s19707, 1, "[")
    _x19706 = kk_string_dup(_s19707); /*string*/
    kk_string_t _x19708;
    kk_string_t _x19709;
    if (kk_std_core__is_Nil(x_17350)) {
      _x19709 = kk_string_empty(); /*string*/
    }
    else {
      struct kk_std_core_Cons* _con19711 = kk_std_core__as_Cons(x_17350);
      kk_box_t _box_x18013 = _con19711->head;
      kk_std_core__list xx0 = _con19711->tail;
      kk_string_t x1 = kk_string_unbox(_box_x18013);
      if (kk_likely(kk_std_core__list_is_unique(x_17350))) {
        kk_std_core__list_free(x_17350);
      }
      else {
        kk_string_dup(x1);
        kk_std_core__list_dup(xx0);
        kk_std_core__list_decref(x_17350, _ctx);
      }
      kk_string_t _x19713;
      kk_define_string_literal(, _s19714, 1, ",")
      _x19713 = kk_string_dup(_s19714); /*string*/
      _x19709 = kk_std_core__lift16744_show_list(_x19713, xx0, x1, _ctx); /*string*/
    }
    kk_string_t _x19715;
    kk_define_string_literal(, _s19716, 1, "]")
    _x19715 = kk_string_dup(_s19716); /*string*/
    _x19708 = kk_std_core__lp__plus__plus__1_rp_(_x19709, _x19715, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(_x19706, _x19708, _ctx);
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
  bool _match_19118 = (dabs >= (1.0e-5)); /*bool*/;
  if (_match_19118) {
    bool _match_19119 = (dabs < (1.0e21)); /*bool*/;
    if (_match_19119) {
      kk_std_core_types__optional _x19717;
      kk_box_t _x19718;
      kk_integer_t _x19719;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18016 = precision._cons.Optional.value;
        kk_integer_t _precision_8278 = kk_integer_unbox(_box_x18016);
        kk_integer_dup(_precision_8278);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19719 = _precision_8278; /*int*/
        goto _match19720;
      }
      {
        _x19719 = kk_integer_from_small(-17); /*int*/
      }
      _match19720: ;
      _x19718 = kk_integer_box(_x19719); /*110*/
      _x19717 = kk_std_core_types__new_Optional(_x19718, _ctx); /*optional<110>*/
      return kk_std_core_show_fixed(d, _x19717, _ctx);
    }
    {
      int32_t _x19722;
      kk_integer_t _x19723;
      if (kk_std_core_types__is_Optional(precision)) {
        kk_box_t _box_x18018 = precision._cons.Optional.value;
        kk_integer_t _precision_82780 = kk_integer_unbox(_box_x18018);
        kk_integer_dup(_precision_82780);
        kk_std_core_types__optional_drop(precision, _ctx);
        _x19723 = _precision_82780; /*int*/
        goto _match19724;
      }
      {
        _x19723 = kk_integer_from_small(-17); /*int*/
      }
      _match19724: ;
      _x19722 = kk_std_core_int32(_x19723, _ctx); /*int32*/
      return kk_std_core_show_expx(d, _x19722, _ctx);
    }
  }
  {
    int32_t _x19726;
    kk_integer_t _x19727;
    if (kk_std_core_types__is_Optional(precision)) {
      kk_box_t _box_x18019 = precision._cons.Optional.value;
      kk_integer_t _precision_82781 = kk_integer_unbox(_box_x18019);
      kk_integer_dup(_precision_82781);
      kk_std_core_types__optional_drop(precision, _ctx);
      _x19727 = _precision_82781; /*int*/
      goto _match19728;
    }
    {
      _x19727 = kk_integer_from_small(-17); /*int*/
    }
    _match19728: ;
    _x19726 = kk_std_core_int32(_x19727, _ctx); /*int32*/
    return kk_std_core_show_expx(d, _x19726, _ctx);
  }
}
 
// lifted

kk_string_t kk_std_core__lift16745_show_3(kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con19736 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18021 = _con19736->head;
    kk_std_core__list yy = _con19736->tail;
    kk_string_t y = kk_string_unbox(_box_x18021);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x19738;
      kk_string_t _x19739;
      kk_string_t _x19740 = kk_string_empty(); /*string*/
      _x19739 = kk_std_core__lp__plus__plus__1_rp_(_x19740, y, _ctx); /*string*/
      _x19738 = kk_std_core__lp__plus__plus__1_rp_(acc, _x19739, _ctx); /*string*/
      ys = yy;
      acc = _x19738;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
 
// Show a string as a string literal


// lift anonymous function
struct kk_std_core_show_fun19746__t_3 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_show_fun19746_3(kk_function_t _fself, kk_box_t _b_18024, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_show_fun19746_3(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_show_fun19746_3, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_show_fun19746_3(kk_function_t _fself, kk_box_t _b_18024, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19747;
  kk_char_t _x19748 = kk_char_unbox(_b_18024, _ctx); /*char*/
  _x19747 = kk_std_core_show_char(_x19748, _ctx); /*string*/
  return kk_string_box(_x19747);
}

kk_string_t kk_std_core_show_3(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_string_t _x19742;
  kk_define_string_literal(, _s19743, 1, "\"")
  _x19742 = kk_string_dup(_s19743); /*string*/
  kk_string_t _x19744;
  kk_string_t _x19745;
  kk_std_core__list _b_18025_18022 = kk_std_core_list_4(s, _ctx); /*list<char>*/;
  kk_std_core__list xs_16615 = kk_std_core_map_5(_b_18025_18022, kk_std_core_new_show_fun19746_3(_ctx), _ctx); /*list<string>*/;
  if (kk_std_core__is_Nil(xs_16615)) {
    _x19745 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con19750 = kk_std_core__as_Cons(xs_16615);
    kk_box_t _box_x18027 = _con19750->head;
    kk_std_core__list xx = _con19750->tail;
    kk_string_t x = kk_string_unbox(_box_x18027);
    if (kk_likely(kk_std_core__list_is_unique(xs_16615))) {
      kk_std_core__list_free(xs_16615);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs_16615, _ctx);
    }
    _x19745 = kk_std_core__lift16745_show_3(xx, x, _ctx); /*string*/
  }
  kk_string_t _x19752;
  kk_define_string_literal(, _s19753, 1, "\"")
  _x19752 = kk_string_dup(_s19753); /*string*/
  _x19744 = kk_std_core__lp__plus__plus__1_rp_(_x19745, _x19752, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19742, _x19744, _ctx);
}
extern kk_string_t kk_std_core_show_fun19758_7(kk_function_t _fself, kk_box_t _b_18030, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x19759 = kk_string_unbox(_b_18030); /*string*/
  return kk_std_core_show_3(_x19759, _ctx);
}
extern kk_string_t kk_std_core_show_fun19760_8(kk_function_t _fself, kk_box_t _b_18035, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x19761 = kk_integer_unbox(_b_18035); /*int*/
  return kk_std_core_show(_x19761, _ctx);
}
extern kk_string_t kk_std_core_show_fun19762_9(kk_function_t _fself, kk_box_t _b_18040, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x19763 = kk_bool_unbox(_b_18040); /*bool*/
  return kk_std_core_show_4(_x19763, _ctx);
}

kk_unit_t kk_std_core_prints(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> console () */ 
  kk_std_core_types__maybe _match_19117;
  kk_box_t _x19772;
  kk_ref_t _x19773 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  _x19772 = kk_ref_get(_x19773,kk_context()); /*184*/
  _match_19117 = kk_std_core_types__maybe_unbox(_x19772, _ctx); /*maybe<(string) -> console ()>*/
  if (kk_std_core_types__is_Nothing(_match_19117)) {
    kk_std_core_xprints(s, _ctx); return kk_Unit;
  }
  {
    kk_box_t _fun_unbox_x18047 = _match_19117._cons.Just.value;
    kk_box_t _x19774;
    kk_function_t _x19775 = kk_function_unbox(_fun_unbox_x18047); /*(18048) -> console 18049*/
    _x19774 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), _x19775, (_x19775, kk_string_box(s), _ctx)); /*18049*/
    kk_unit_unbox(_x19774); return kk_Unit;
  }
}


// lift anonymous function
struct kk_std_core__default_exn_fun19782__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core__default_exn_fun19782(kk_function_t _fself, kk_std_core_hnd__marker _b_18065, kk_std_core_hnd__ev _b_18066, kk_box_t _b_18067, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19782(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__default_exn_fun19782, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_core__default_exn_fun19783__t {
  struct kk_function_s _base;
  kk_box_t _b_18067;
};
static kk_box_t kk_std_core__default_exn_fun19783(kk_function_t _fself, kk_function_t _b_18062, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19783(kk_box_t _b_18067, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19783__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19783__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19783, kk_context());
  _self->_b_18067 = _b_18067;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core__default_exn_fun19785__t {
  struct kk_function_s _base;
  kk_function_t _b_18062;
};
static kk_unit_t kk_std_core__default_exn_fun19785(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18063, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19785(kk_function_t _b_18062, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19785__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19785__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19785, kk_context());
  _self->_b_18062 = _b_18062;
  return &_self->_base;
}

static kk_unit_t kk_std_core__default_exn_fun19785(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18063, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19785__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19785__t*, _fself);
  kk_function_t _b_18062 = _self->_b_18062; /* (std/core/hnd/resume-result<3924,3927>) -> 3926 3927 */
  kk_drop_match(_self, {kk_function_dup(_b_18062);}, {}, _ctx)
  kk_box_t _x19786 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _b_18062, (_b_18062, _b_18063, _ctx)); /*3927*/
  return kk_unit_unbox(_x19786);
}
static kk_box_t kk_std_core__default_exn_fun19783(kk_function_t _fself, kk_function_t _b_18062, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19783__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19783__t*, _fself);
  kk_box_t _b_18067 = _self->_b_18067; /* 51 */
  kk_drop_match(_self, {kk_box_dup(_b_18067);}, {}, _ctx)
  kk_unit_t _x19784 = kk_Unit;
  kk_function_t ___wildcard__585__45_18084 = kk_std_core__new_default_exn_fun19785(_b_18062, _ctx); /*(std/core/hnd/resume-result<9304,()>) -> <console|9302> ()*/;
  kk_function_drop(___wildcard__585__45_18084, _ctx);
  kk_unit_t __ = kk_Unit;
  kk_string_t _x19787;
  kk_define_string_literal(, _s19788, 20, "uncaught exception: ")
  _x19787 = kk_string_dup(_s19788); /*string*/
  kk_std_core_prints(_x19787, _ctx);
  kk_string_t _x19789;
  kk_std_core__exception _match_19115 = kk_std_core__exception_unbox(_b_18067, _ctx); /*exception*/;
  {
    kk_string_t _x = _match_19115.message;
    kk_string_dup(_x);
    kk_std_core__exception_drop(_match_19115, _ctx);
    _x19789 = _x; /*string*/
  }
  kk_std_core_printsln(_x19789, _ctx);
  return kk_unit_box(_x19784);
}
static kk_box_t kk_std_core__default_exn_fun19782(kk_function_t _fself, kk_std_core_hnd__marker _b_18065, kk_std_core_hnd__ev _b_18066, kk_box_t _b_18067, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_hnd__ev_dropn(_b_18066, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18065, kk_std_core__new_default_exn_fun19783(_b_18067, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core__default_exn_fun19790__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core__default_exn_fun19790(kk_function_t _fself, kk_box_t _b_18072, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19790(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core__default_exn_fun19790, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core__default_exn_fun19790(kk_function_t _fself, kk_box_t _b_18072, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_18072;
}


// lift anonymous function
struct kk_std_core__default_exn_fun19791__t {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_box_t kk_std_core__default_exn_fun19791(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_default_exn_fun19791(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19791__t* _self = kk_function_alloc_as(struct kk_std_core__default_exn_fun19791__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__default_exn_fun19791, kk_context());
  _self->action = action;
  return &_self->_base;
}

static kk_box_t kk_std_core__default_exn_fun19791(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core__default_exn_fun19791__t* _self = kk_function_as(struct kk_std_core__default_exn_fun19791__t*, _fself);
  kk_function_t action = _self->action; /* () -> <exn,console|9302> () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t _x19792 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
  return kk_unit_box(_x19792);
}

kk_unit_t kk_std_core__default_exn(kk_function_t action, kk_context_t* _ctx) { /* forall<e> (action : () -> <console,exn|e> ()) -> <console|e> () */ 
  int32_t _b_18073_18068 = ((int32_t)KI32(0)); /*int32*/;
  kk_box_t _x19779;
  kk_std_core__hnd_exn _x19780;
  kk_std_core_hnd__clause1 _x19781 = kk_std_core_hnd__new_Clause1(kk_std_core__new_default_exn_fun19782(_ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19780 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19781, _ctx); /*.hnd-exn<11,12>*/
  _x19779 = kk_std_core__handle_exn(_b_18073_18068, _x19780, kk_std_core__new_default_exn_fun19790(_ctx), kk_std_core__new_default_exn_fun19791(action, _ctx), _ctx); /*1964*/
  kk_unit_unbox(_x19779); return kk_Unit;
}
 
// Get (zero-based) element `n`  of a list. Return a `:maybe` type.

kk_std_core_types__maybe kk_std_core__lp__lb__rb__2_rp_(kk_std_core__list xs, kk_integer_t n, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> maybe<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19793 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19793->head;
    kk_std_core__list xx = _con19793->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool _match_19113;
    kk_integer_t _x19794 = kk_integer_dup(n); /*int*/
    _match_19113 = kk_integer_gt(_x19794,(kk_integer_from_small(0)),kk_context()); /*bool*/
    if (_match_19113) {
      kk_box_drop(x, _ctx);
      { // tailcall
        kk_integer_t _x19795 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        xs = xx;
        n = _x19795;
        goto kk__tailcall;
      }
    }
    {
      kk_std_core__list_drop(xx, _ctx);
      bool _match_19114 = kk_integer_eq(n,(kk_integer_from_small(0)),kk_context()); /*bool*/;
      if (_match_19114) {
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
    kk_string_t _x19796 = kk_string_dup(s); /*string*/
    kk_ssize_t _x19797 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x19798;
    kk_ssize_t _x19799 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x19800 = (start0 + len0); /*ssize_t*/
    _x19798 = (_x19799 - _x19800); /*ssize_t*/
    return kk_std_core__new_Sslice(_x19796, _x19797, _x19798, _ctx);
  }
}
 
// monadic lift

bool kk_std_core__mlift17155_all(kk_function_t predicate, kk_std_core__list xx, bool _y_16922, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */ 
  if (_y_16922) {
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
struct kk_std_core_all_fun19804__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_all_fun19804(kk_function_t _fself, kk_box_t _b_18086, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_all_fun19804(kk_function_t predicate0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_all_fun19804__t* _self = kk_function_alloc_as(struct kk_std_core_all_fun19804__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_all_fun19804, kk_context());
  _self->predicate0 = predicate0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_all_fun19804(kk_function_t _fself, kk_box_t _b_18086, kk_context_t* _ctx) {
  struct kk_std_core_all_fun19804__t* _self = kk_function_as(struct kk_std_core_all_fun19804__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (9539) -> 9540 bool */
  kk_std_core__list xx0 = _self->xx0; /* list<9539> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  bool _x19805;
  bool _x19806 = kk_bool_unbox(_b_18086); /*bool*/
  _x19805 = kk_std_core__mlift17155_all(predicate0, xx0, _x19806, _ctx); /*bool*/
  return kk_bool_box(_x19805);
}

bool kk_std_core_all(kk_std_core__list xs, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19801 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19801->head;
    kk_std_core__list xx0 = _con19801->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool x0_17358;
    kk_function_t _x19802 = kk_function_dup(predicate0); /*(9539) -> 9540 bool*/
    x0_17358 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19802, (_x19802, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19803 = kk_std_core_hnd_yield_extend(kk_std_core_new_all_fun19804(predicate0, xx0, _ctx), _ctx); /*3860*/
      return kk_bool_unbox(_x19803);
    }
    if (x0_17358) { // tailcall
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

bool kk_std_core__mlift17156_any(kk_function_t predicate, kk_std_core__list xx, bool _y_16926, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */ 
  if (_y_16926) {
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
struct kk_std_core_any_fun19810__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_any_fun19810(kk_function_t _fself, kk_box_t _b_18090, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_any_fun19810(kk_function_t predicate0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_any_fun19810__t* _self = kk_function_alloc_as(struct kk_std_core_any_fun19810__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_any_fun19810, kk_context());
  _self->predicate0 = predicate0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_any_fun19810(kk_function_t _fself, kk_box_t _b_18090, kk_context_t* _ctx) {
  struct kk_std_core_any_fun19810__t* _self = kk_function_as(struct kk_std_core_any_fun19810__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (9567) -> 9568 bool */
  kk_std_core__list xx0 = _self->xx0; /* list<9567> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  bool _x19811;
  bool _x19812 = kk_bool_unbox(_b_18090); /*bool*/
  _x19811 = kk_std_core__mlift17156_any(predicate0, xx0, _x19812, _ctx); /*bool*/
  return kk_bool_box(_x19811);
}

bool kk_std_core_any(kk_std_core__list xs, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19807 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19807->head;
    kk_std_core__list xx0 = _con19807->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool x0_17361;
    kk_function_t _x19808 = kk_function_dup(predicate0); /*(9567) -> 9568 bool*/
    x0_17361 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19808, (_x19808, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19809 = kk_std_core_hnd_yield_extend(kk_std_core_new_any_fun19810(predicate0, xx0, _ctx), _ctx); /*3860*/
      return kk_bool_unbox(_x19809);
    }
    if (x0_17361) {
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
  bool _match_19110;
  kk_ssize_t _x19813;
  kk_vector_t _x19814 = kk_vector_dup(v); /*vector<9638>*/
  _x19813 = kk_vector_len(_x19814,kk_context()); /*ssize_t*/
  _match_19110 = (_x19813 <= idx); /*bool*/
  if (_match_19110) {
    kk_vector_drop(v, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t _x19815 = kk_vector_at(v,idx,kk_context()); /*223*/
    return kk_std_core_types__new_Just(_x19815, _ctx);
  }
}
 
// O(`n`). The first `n` (default = `1`) characters in a string.

kk_std_core__sslice kk_std_core_first(kk_string_t s, kk_std_core_types__optional n, kk_context_t* _ctx) { /* (s : string, n : optional<int>) -> sslice */ 
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_19109;
  kk_integer_t _x19819;
  if (kk_std_core_types__is_Optional(n)) {
    kk_box_t _box_x18093 = n._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18093);
    kk_integer_dup(_n_9710);
    _x19819 = _n_9710; /*int*/
    goto _match19820;
  }
  {
    _x19819 = kk_integer_from_small(1); /*int*/
  }
  _match19820: ;
  _match_19109 = kk_integer_eq(_x19819,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19109) {
    kk_std_core_types__optional_drop(n, _ctx);
    return slice0;
  }
  {
    kk_integer_t _x19822;
    kk_integer_t _x19823;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18094 = n._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18094);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(n, _ctx);
      _x19823 = _n_97100; /*int*/
      goto _match19824;
    }
    {
      _x19823 = kk_integer_from_small(1); /*int*/
    }
    _match19824: ;
    _x19822 = kk_integer_sub(_x19823,(kk_integer_from_small(1)),kk_context()); /*int*/
    return kk_std_core_extend(slice0, _x19822, _ctx);
  }
}
 
// Convert the first character of a string to uppercase.

kk_string_t kk_std_core_capitalize(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> string */ 
  kk_string_t _x19826;
  kk_string_t _x19827;
  kk_std_core__sslice _x19828;
  kk_std_core__sslice slice0;
  kk_string_t _x19829 = kk_string_dup(s); /*string*/
  slice0 = kk_std_core_first1(_x19829, _ctx); /*sslice*/
  bool _match_19106;
  kk_integer_t _x19830;
  kk_std_core_types__optional _match_19108 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19108)) {
    kk_box_t _box_x18095 = _match_19108._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18095);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_19108, _ctx);
    _x19830 = _n_9710; /*int*/
    goto _match19831;
  }
  {
    _x19830 = kk_integer_from_small(1); /*int*/
  }
  _match19831: ;
  _match_19106 = kk_integer_eq(_x19830,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19106) {
    _x19828 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x19833;
    kk_integer_t _x19834;
    kk_std_core_types__optional _match_19107 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19107)) {
      kk_box_t _box_x18096 = _match_19107._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18096);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_19107, _ctx);
      _x19834 = _n_97100; /*int*/
      goto _match19835;
    }
    {
      _x19834 = kk_integer_from_small(1); /*int*/
    }
    _match19835: ;
    _x19833 = kk_integer_sub(_x19834,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x19828 = kk_std_core_extend(slice0, _x19833, _ctx); /*sslice*/
  }
  _x19827 = kk_std_core_string_3(_x19828, _ctx); /*string*/
  _x19826 = kk_std_core_to_upper(_x19827, _ctx); /*string*/
  kk_string_t _x19837;
  kk_std_core__sslice _x19838;
  kk_std_core__sslice slice2 = kk_std_core_first1(s, _ctx); /*sslice*/;
  kk_std_core__sslice slice1_16624;
  bool _match_19103;
  kk_integer_t _x19839;
  kk_std_core_types__optional _match_19105 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19105)) {
    kk_box_t _box_x18097 = _match_19105._cons.Optional.value;
    kk_integer_t _n_971000 = kk_integer_unbox(_box_x18097);
    kk_integer_dup(_n_971000);
    kk_std_core_types__optional_drop(_match_19105, _ctx);
    _x19839 = _n_971000; /*int*/
    goto _match19840;
  }
  {
    _x19839 = kk_integer_from_small(1); /*int*/
  }
  _match19840: ;
  _match_19103 = kk_integer_eq(_x19839,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19103) {
    slice1_16624 = slice2; /*sslice*/
  }
  else {
    kk_integer_t _x19842;
    kk_integer_t _x19843;
    kk_std_core_types__optional _match_19104 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19104)) {
      kk_box_t _box_x18098 = _match_19104._cons.Optional.value;
      kk_integer_t _n_971001 = kk_integer_unbox(_box_x18098);
      kk_integer_dup(_n_971001);
      kk_std_core_types__optional_drop(_match_19104, _ctx);
      _x19843 = _n_971001; /*int*/
      goto _match19844;
    }
    {
      _x19843 = kk_integer_from_small(1); /*int*/
    }
    _match19844: ;
    _x19842 = kk_integer_sub(_x19843,(kk_integer_from_small(1)),kk_context()); /*int*/
    slice1_16624 = kk_std_core_extend(slice2, _x19842, _ctx); /*sslice*/
  }
  {
    kk_string_t s1 = slice1_16624.str;
    kk_ssize_t start0 = slice1_16624.start;
    kk_ssize_t len0 = slice1_16624.len;
    kk_string_dup(s1);
    kk_std_core__sslice_drop(slice1_16624, _ctx);
    kk_string_t _x19846 = kk_string_dup(s1); /*string*/
    kk_ssize_t _x19847 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x19848;
    kk_ssize_t _x19849 = kk_string_len(s1,kk_context()); /*ssize_t*/
    kk_ssize_t _x19850 = (start0 + len0); /*ssize_t*/
    _x19848 = (_x19849 - _x19850); /*ssize_t*/
    _x19838 = kk_std_core__new_Sslice(_x19846, _x19847, _x19848, _ctx); /*sslice*/
  }
  _x19837 = kk_std_core_string_3(_x19838, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x19826, _x19837, _ctx);
}
 
// Catch any exception raised in `action` and handle it.
// Use `on-exn` or `on-exit` when appropiate.


// lift anonymous function
struct kk_std_core_try_fun19853__t {
  struct kk_function_s _base;
  kk_function_t hndl;
};
static kk_box_t kk_std_core_try_fun19853(kk_function_t _fself, kk_std_core_hnd__marker _b_18100, kk_std_core_hnd__ev _b_18101, kk_box_t _b_18102, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19853(kk_function_t hndl, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19853__t* _self = kk_function_alloc_as(struct kk_std_core_try_fun19853__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19853, kk_context());
  _self->hndl = hndl;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19854__t {
  struct kk_function_s _base;
  kk_box_t _b_18102;
  kk_function_t hndl;
};
static kk_box_t kk_std_core_try_fun19854(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19854(kk_box_t _b_18102, kk_function_t hndl, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19854__t* _self = kk_function_alloc_as(struct kk_std_core_try_fun19854__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19854, kk_context());
  _self->_b_18102 = _b_18102;
  _self->hndl = hndl;
  return &_self->_base;
}

static kk_box_t kk_std_core_try_fun19854(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19854__t* _self = kk_function_as(struct kk_std_core_try_fun19854__t*, _fself);
  kk_box_t _b_18102 = _self->_b_18102; /* 51 */
  kk_function_t hndl = _self->hndl; /* (exception) -> 9877 9876 */
  kk_drop_match(_self, {kk_box_dup(_b_18102);kk_function_dup(hndl);}, {}, _ctx)
  kk_function_drop(___wildcard__585__45, _ctx);
  kk_std_core__exception _x19855 = kk_std_core__exception_unbox(_b_18102, _ctx); /*exception*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_std_core__exception, kk_context_t*), hndl, (hndl, _x19855, _ctx));
}
static kk_box_t kk_std_core_try_fun19853(kk_function_t _fself, kk_std_core_hnd__marker _b_18100, kk_std_core_hnd__ev _b_18101, kk_box_t _b_18102, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19853__t* _self = kk_function_as(struct kk_std_core_try_fun19853__t*, _fself);
  kk_function_t hndl = _self->hndl; /* (exception) -> 9877 9876 */
  kk_drop_match(_self, {kk_function_dup(hndl);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_18101, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18100, kk_std_core_new_try_fun19854(_b_18102, hndl, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_fun19856__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19856(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19856(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19856, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19856(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _x;
}

kk_box_t kk_std_core_try(kk_function_t action, kk_function_t hndl, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <exn|e> a, hndl : (exception) -> e a) -> e a */ 
  kk_std_core__hnd_exn _x19851;
  kk_std_core_hnd__clause1 _x19852 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_fun19853(hndl, _ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19851 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19852, _ctx); /*.hnd-exn<11,12>*/
  return kk_std_core__handle_exn(((int32_t)KI32(0)), _x19851, kk_std_core_new_try_fun19856(_ctx), action, _ctx);
}
 
// Transform an exception effect to an  `:error` type.


// lift anonymous function
struct kk_std_core_try_fun19860__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19860_1(kk_function_t _fself, kk_std_core_hnd__marker _b_18112, kk_std_core_hnd__ev _b_18113, kk_box_t _b_18114, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19860_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19860_1, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_core_try_fun19861__t_1 {
  struct kk_function_s _base;
  kk_box_t _b_18114;
};
static kk_box_t kk_std_core_try_fun19861_1(kk_function_t _fself, kk_function_t _b_18109, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19861_1(kk_box_t _b_18114, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19861__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19861__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19861_1, kk_context());
  _self->_b_18114 = _b_18114;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19863__t_1 {
  struct kk_function_s _base;
  kk_function_t _b_18109;
};
static kk_std_core__error kk_std_core_try_fun19863_1(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18110, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19863_1(kk_function_t _b_18109, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19863__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19863__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19863_1, kk_context());
  _self->_b_18109 = _b_18109;
  return &_self->_base;
}

static kk_std_core__error kk_std_core_try_fun19863_1(kk_function_t _fself, kk_std_core_hnd__resume_result _b_18110, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19863__t_1* _self = kk_function_as(struct kk_std_core_try_fun19863__t_1*, _fself);
  kk_function_t _b_18109 = _self->_b_18109; /* (std/core/hnd/resume-result<3924,3927>) -> 3926 3927 */
  kk_drop_match(_self, {kk_function_dup(_b_18109);}, {}, _ctx)
  kk_box_t _x19864 = kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__resume_result, kk_context_t*), _b_18109, (_b_18109, _b_18110, _ctx)); /*3927*/
  return kk_std_core__error_unbox(_x19864, _ctx);
}
static kk_box_t kk_std_core_try_fun19861_1(kk_function_t _fself, kk_function_t _b_18109, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19861__t_1* _self = kk_function_as(struct kk_std_core_try_fun19861__t_1*, _fself);
  kk_box_t _b_18114 = _self->_b_18114; /* 51 */
  kk_drop_match(_self, {kk_box_dup(_b_18114);}, {}, _ctx)
  kk_std_core__error _x19862;
  kk_function_t ___wildcard__585__45_18135 = kk_std_core_new_try_fun19863_1(_b_18109, _ctx); /*(std/core/hnd/resume-result<9863,error<9915>>) -> 9916 error<9915>*/;
  kk_function_drop(___wildcard__585__45_18135, _ctx);
  kk_std_core__exception _x19865 = kk_std_core__exception_unbox(_b_18114, _ctx); /*exception*/
  _x19862 = kk_std_core__new_Error(_x19865, _ctx); /*error<30>*/
  return kk_std_core__error_box(_x19862, _ctx);
}
static kk_box_t kk_std_core_try_fun19860_1(kk_function_t _fself, kk_std_core_hnd__marker _b_18112, kk_std_core_hnd__ev _b_18113, kk_box_t _b_18114, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core_hnd__ev_dropn(_b_18113, ((int32_t)KI32(3)), _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18112, kk_std_core_new_try_fun19861_1(_b_18114, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_fun19866__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19866_1(kk_function_t _fself, kk_box_t _b_18121, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19866_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19866_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19866_1(kk_function_t _fself, kk_box_t _b_18121, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _b_18121;
}


// lift anonymous function
struct kk_std_core_try_fun19867__t_1 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_box_t kk_std_core_try_fun19867_1(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19867_1(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19867__t_1* _self = kk_function_alloc_as(struct kk_std_core_try_fun19867__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_fun19867_1, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_fun19870__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_fun19870_1(kk_function_t _fself, kk_box_t _b_18116, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_fun19870_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_fun19870_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_fun19870_1(kk_function_t _fself, kk_box_t _b_18116, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__error _x19871 = kk_std_core__new_Ok(_b_18116, _ctx); /*error<30>*/
  return kk_std_core__error_box(_x19871, _ctx);
}
static kk_box_t kk_std_core_try_fun19867_1(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_core_try_fun19867__t_1* _self = kk_function_as(struct kk_std_core_try_fun19867__t_1*, _fself);
  kk_function_t action = _self->action; /* () -> <exn|9916> 9915 */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_std_core__error _x19868;
  kk_box_t x0_17366 = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), action, (action, _ctx)); /*9915*/;
  if (kk_yielding(kk_context())) {
    kk_box_drop(x0_17366, _ctx);
    kk_box_t _x19869 = kk_std_core_hnd_yield_extend(kk_std_core_new_try_fun19870_1(_ctx), _ctx); /*3860*/
    _x19868 = kk_std_core__error_unbox(_x19869, _ctx); /*error<9915>*/
  }
  else {
    _x19868 = kk_std_core__new_Ok(x0_17366, _ctx); /*error<9915>*/
  }
  return kk_std_core__error_box(_x19868, _ctx);
}

kk_std_core__error kk_std_core_try_1(kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (action : () -> <exn|e> a) -> e error<a> */ 
  int32_t _b_18122_18117 = ((int32_t)KI32(0)); /*int32*/;
  kk_box_t _x19857;
  kk_std_core__hnd_exn _x19858;
  kk_std_core_hnd__clause1 _x19859 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_fun19860_1(_ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x19858 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x19859, _ctx); /*.hnd-exn<11,12>*/
  _x19857 = kk_std_core__handle_exn(_b_18122_18117, _x19858, kk_std_core_new_try_fun19866_1(_ctx), kk_std_core_new_try_fun19867_1(action, _ctx), _ctx); /*1964*/
  return kk_std_core__error_unbox(_x19857, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_cdivmod_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> (int, int) */ 
  bool _match_19101;
  kk_integer_t _x19872 = kk_integer_dup(n); /*int*/
  _match_19101 = kk_integer_lte(_x19872,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19101) {
    kk_integer_drop(n, _ctx);
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(i), kk_integer_box(kk_integer_from_small(0)), _ctx);
  }
  {
    kk_integer_t cq;
    kk_integer_t _x19873 = kk_integer_dup(i); /*int*/
    kk_integer_t _x19874 = kk_integer_dup(n); /*int*/
    cq = kk_std_core_cdiv_exp10(_x19873, _x19874, _ctx); /*int*/
    kk_integer_t cr;
    kk_integer_t _x19875;
    kk_integer_t _x19876 = kk_integer_dup(cq); /*int*/
    _x19875 = kk_std_core_mul_exp10(_x19876, n, _ctx); /*int*/
    cr = kk_integer_sub(i,_x19875,kk_context()); /*int*/
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(cq), kk_integer_box(cr), _ctx);
  }
}
 
// Concatenate a list of `:maybe` values

kk_std_core__list kk_std_core__ctail_concat_maybe(kk_std_core__list xs, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<maybe<a>>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con19877 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18144 = _con19877->head;
    kk_std_core__list xx = _con19877->tail;
    kk_std_core_types__maybe x = kk_std_core_types__maybe_unbox(_box_x18144, NULL);
    kk_reuse_t _ru_18916 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core_types__maybe_dup(x);
      kk_box_drop(_box_x18144, _ctx);
      _ru_18916 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_std_core_types__maybe_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18916 = kk_reuse_null;
    }
    if (kk_std_core_types__is_Just(x)) {
      kk_box_t y = x._cons.Just.value;
      kk_std_core__list _ctail_16811 = kk_std_core__list_hole(); /*list<10055>*/;
      kk_std_core__list _ctail_16812 = kk_std_core__new_Cons(_ru_18916, y, _ctail_16811, _ctx); /*list<10055>*/;
      { // tailcall
        kk_std_core_types__ctail _x19879;
        kk_box_t* _b_18155_18150 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16812)->tail)); /*cfield<list<10055>>*/;
        _x19879 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16812, _ctx)),_b_18155_18150); /*ctail<0>*/
        xs = xx;
        _acc = _x19879;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18916, _ctx);
      { // tailcall
        xs = xx;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_box_t _x19880 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x19880, _ctx);
  }
}
 
// Concatenate a list of `:maybe` values

kk_std_core__list kk_std_core_concat_maybe(kk_std_core__list xs0, kk_context_t* _ctx) { /* forall<a> (xs : list<maybe<a>>) -> list<a> */ 
  kk_std_core_types__ctail _x19881 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_concat_maybe(xs0, _x19881, _ctx);
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17158_op(kk_function_t action, kk_ssize_t end, kk_ssize_t i, kk_std_core_types__maybe _y_16934, kk_context_t* _ctx) { /* forall<a,e> (action : (ssize_t) -> e maybe<a>, end : ssize_t, i : ssize_t, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_16934)) {
    kk_ssize_t i0_16777 = kk_std_core_incr_1(i, _ctx); /*ssize_t*/;
    return kk_std_core__lift16746_for_whilez(action, end, i0_16777, _ctx);
  }
  {
    kk_box_t x = _y_16934._cons.Just.value;
    kk_function_drop(action, _ctx);
    return kk_std_core_types__new_Just(x, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16746_for_whilez_fun19884__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_ssize_t end0;
  kk_ssize_t i0;
};
static kk_box_t kk_std_core__lift16746_for_whilez_fun19884(kk_function_t _fself, kk_box_t _b_18162, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16746_for_whilez_fun19884(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16746_for_whilez_fun19884__t* _self = kk_function_alloc_as(struct kk_std_core__lift16746_for_whilez_fun19884__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16746_for_whilez_fun19884, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16746_for_whilez_fun19884(kk_function_t _fself, kk_box_t _b_18162, kk_context_t* _ctx) {
  struct kk_std_core__lift16746_for_whilez_fun19884__t* _self = kk_function_as(struct kk_std_core__lift16746_for_whilez_fun19884__t*, _fself);
  kk_function_t action0 = _self->action0; /* (ssize_t) -> 10110 maybe<10109> */
  kk_ssize_t end0 = _self->end0; /* ssize_t */
  kk_ssize_t i0 = _self->i0; /* ssize_t */
  kk_drop_match(_self, {kk_function_dup(action0);;;}, {}, _ctx)
  kk_std_core_types__maybe _x19885;
  kk_std_core_types__maybe _x19886 = kk_std_core_types__maybe_unbox(_b_18162, _ctx); /*maybe<10109>*/
  _x19885 = kk_std_core__mlift17158_op(action0, end0, i0, _x19886, _ctx); /*maybe<10109>*/
  return kk_std_core_types__maybe_box(_x19885, _ctx);
}

kk_std_core_types__maybe kk_std_core__lift16746_for_whilez(kk_function_t action0, kk_ssize_t end0, kk_ssize_t i0, kk_context_t* _ctx) { /* forall<a,e> (action : (ssize_t) -> e maybe<a>, end : ssize_t, i : ssize_t) -> e maybe<a> */ 
  kk__tailcall: ;
  bool _match_19099 = (i0 <= end0); /*bool*/;
  if (_match_19099) {
    kk_std_core_types__maybe x0_17369;
    kk_function_t _x19882 = kk_function_dup(action0); /*(ssize_t) -> 10110 maybe<10109>*/
    x0_17369 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_ssize_t, kk_context_t*), _x19882, (_x19882, i0, _ctx)); /*maybe<10109>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17369, _ctx);
      kk_box_t _x19883 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16746_for_whilez_fun19884(action0, end0, i0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19883, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17369)) {
      kk_ssize_t i0_167770 = kk_std_core_incr_1(i0, _ctx); /*ssize_t*/;
      { // tailcall
        i0 = i0_167770;
        goto kk__tailcall;
      }
    }
    {
      kk_box_t x1 = x0_17369._cons.Just.value;
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

kk_std_core_types__maybe kk_std_core__mlift17159_foreach_while(kk_function_t action, kk_std_core__list xx, kk_std_core_types__maybe _y_16939, kk_context_t* _ctx) { /* forall<a,b,e> (action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e maybe<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16939)) {
    return kk_std_core_foreach_while(xx, action, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    return _y_16939;
  }
}
 
// Invoke `action` for each element of a list while `action` return `Nothing`


// lift anonymous function
struct kk_std_core_foreach_while_fun19893__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foreach_while_fun19893(kk_function_t _fself, kk_box_t _b_18166, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19893(kk_function_t action0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19893__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19893__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19893, kk_context());
  _self->action0 = action0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_while_fun19893(kk_function_t _fself, kk_box_t _b_18166, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19893__t* _self = kk_function_as(struct kk_std_core_foreach_while_fun19893__t*, _fself);
  kk_function_t action0 = _self->action0; /* (10243) -> 10245 maybe<10244> */
  kk_std_core__list xx0 = _self->xx0; /* list<10243> */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core_types__maybe _x19894;
  kk_std_core_types__maybe _x19895 = kk_std_core_types__maybe_unbox(_b_18166, _ctx); /*maybe<10244>*/
  _x19894 = kk_std_core__mlift17159_foreach_while(action0, xx0, _x19895, _ctx); /*maybe<10244>*/
  return kk_std_core_types__maybe_box(_x19894, _ctx);
}

kk_std_core_types__maybe kk_std_core_foreach_while(kk_std_core__list xs, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e maybe<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    struct kk_std_core_Cons* _con19890 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19890->head;
    kk_std_core__list xx0 = _con19890->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__maybe x0_17372;
    kk_function_t _x19891 = kk_function_dup(action0); /*(10243) -> 10245 maybe<10244>*/
    x0_17372 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x19891, (_x19891, x, _ctx)); /*maybe<10244>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17372, _ctx);
      kk_box_t _x19892 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_while_fun19893(action0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19892, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17372)) { // tailcall
                                                   xs = xx0;
                                                   goto kk__tailcall;
    }
    {
      kk_function_drop(action0, _ctx);
      kk_std_core__list_drop(xx0, _ctx);
      return x0_17372;
    }
  }
}
 
// monadic lift

kk_std_core_types__maybe kk_std_core__mlift17160_foreach_while_1(kk_function_t action, kk_std_core__sslice rest, kk_std_core_types__maybe _y_16943, kk_context_t* _ctx) { /* forall<a,e> (action : (c : char) -> e maybe<a>, rest : sslice, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_16943)) {
    return kk_std_core_foreach_while_1(rest, action, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__sslice_drop(rest, _ctx);
    return _y_16943;
  }
}
 
// Apply a function for each character in a string slice.
// If `action` returns `Just`, the function returns immediately with that result.


// lift anonymous function
struct kk_std_core_foreach_while_fun19901__t_1 {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__sslice rest0;
};
static kk_box_t kk_std_core_foreach_while_fun19901_1(kk_function_t _fself, kk_box_t _b_18173, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19901_1(kk_function_t action0, kk_std_core__sslice rest0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19901__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19901__t_1, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19901_1, kk_context());
  _self->action0 = action0;
  _self->rest0 = rest0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_while_fun19901_1(kk_function_t _fself, kk_box_t _b_18173, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19901__t_1* _self = kk_function_as(struct kk_std_core_foreach_while_fun19901__t_1*, _fself);
  kk_function_t action0 = _self->action0; /* (c : char) -> 10330 maybe<10329> */
  kk_std_core__sslice rest0 = _self->rest0; /* sslice */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__sslice_dup(rest0);}, {}, _ctx)
  kk_std_core_types__maybe _x19902;
  kk_std_core_types__maybe _x19903 = kk_std_core_types__maybe_unbox(_b_18173, _ctx); /*maybe<10329>*/
  _x19902 = kk_std_core__mlift17160_foreach_while_1(action0, rest0, _x19903, _ctx); /*maybe<10329>*/
  return kk_std_core_types__maybe_box(_x19902, _ctx);
}

kk_std_core_types__maybe kk_std_core_foreach_while_1(kk_std_core__sslice slice0, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,e> (slice : sslice, action : (c : char) -> e maybe<a>) -> e maybe<a> */ 
  kk__tailcall: ;
  kk_std_core_types__maybe _match_19096 = kk_std_core_next(slice0, _ctx); /*maybe<(char, sslice)>*/;
  if (kk_std_core_types__is_Nothing(_match_19096)) {
    kk_function_drop(action0, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_box_t _box_x18169 = _match_19096._cons.Just.value;
    kk_std_core_types__tuple2_ _pat1 = kk_std_core_types__tuple2__unbox(_box_x18169, NULL);
    kk_box_t _box_x18170 = _pat1.fst;
    kk_box_t _box_x18171 = _pat1.snd;
    kk_char_t c = kk_char_unbox(_box_x18170, NULL);
    kk_std_core__sslice rest0 = kk_std_core__sslice_unbox(_box_x18171, NULL);
    kk_std_core__sslice_dup(rest0);
    kk_std_core_types__maybe_drop(_match_19096, _ctx);
    kk_std_core_types__maybe x_17375;
    kk_function_t _x19899 = kk_function_dup(action0); /*(c : char) -> 10330 maybe<10329>*/
    x_17375 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_char_t, kk_context_t*), _x19899, (_x19899, c, _ctx)); /*maybe<10329>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x_17375, _ctx);
      kk_box_t _x19900 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_while_fun19901_1(action0, rest0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x19900, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x_17375)) { // tailcall
                                                  slice0 = rest0;
                                                  goto kk__tailcall;
    }
    {
      kk_function_drop(action0, _ctx);
      kk_std_core__sslice_drop(rest0, _ctx);
      return x_17375;
    }
  }
}
 
// Invoke a function `f` for each element in a vector `v`.
// If `f` returns `Just`, the iteration is stopped early and the result is returned.


// lift anonymous function
struct kk_std_core_foreach_while_fun19910__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_std_core_types__maybe kk_std_core_foreach_while_fun19910_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_while_fun19910_3(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19910__t_3* _self = kk_function_alloc_as(struct kk_std_core_foreach_while_fun19910__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_while_fun19910_3, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_foreach_while_fun19910_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_while_fun19910__t_3* _self = kk_function_as(struct kk_std_core_foreach_while_fun19910__t_3*, _fself);
  kk_function_t f = _self->f; /* (10428) -> 10430 maybe<10429> */
  kk_vector_t v = _self->v; /* vector<10428> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t _x19911 = kk_vector_at(v,i,kk_context()); /*223*/
  return kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), f, (f, _x19911, _ctx));
}

kk_std_core_types__maybe kk_std_core_foreach_while_3(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,b,e> (v : vector<a>, f : (a) -> e maybe<b>) -> e maybe<b> */ 
  kk_ssize_t start0_17378 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17379;
  kk_ssize_t _x19908;
  kk_vector_t _x19909 = kk_vector_dup(v); /*vector<10428>*/
  _x19908 = kk_vector_len(_x19909,kk_context()); /*ssize_t*/
  end_17379 = kk_std_core_decr_1(_x19908, _ctx); /*ssize_t*/
  return kk_std_core__lift16746_for_whilez(kk_std_core_new_foreach_while_fun19910_3(f, v, _ctx), end_17379, start0_17378, _ctx);
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17161_foreach(kk_function_t action, kk_std_core__list xx, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<a,e> (action : (a) -> e (), xx : list<a>, wild_ : ()) -> e () */ 
  kk_std_core_foreach(xx, action, _ctx); return kk_Unit;
}
 
// Invoke `action` for each element of a list


// lift anonymous function
struct kk_std_core_foreach_fun19915__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foreach_fun19915(kk_function_t _fself, kk_box_t _b_18177, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19915(kk_function_t action0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19915__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19915__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19915, kk_context());
  _self->action0 = action0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_fun19915(kk_function_t _fself, kk_box_t _b_18177, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19915__t* _self = kk_function_as(struct kk_std_core_foreach_fun19915__t*, _fself);
  kk_function_t action0 = _self->action0; /* (10623) -> 10624 () */
  kk_std_core__list xx0 = _self->xx0; /* list<10623> */
  kk_drop_match(_self, {kk_function_dup(action0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_unit_t _x19916 = kk_Unit;
  kk_unit_t _x19917 = kk_Unit;
  kk_unit_unbox(_b_18177);
  kk_std_core__mlift17161_foreach(action0, xx0, _x19917, _ctx);
  return kk_unit_box(_x19916);
}

kk_unit_t kk_std_core_foreach(kk_std_core__list xs, kk_function_t action0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, action : (a) -> e ()) -> e () */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action0, _ctx);
    kk_Unit; return kk_Unit;
  }
  {
    struct kk_std_core_Cons* _con19912 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con19912->head;
    kk_std_core__list xx0 = _con19912->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_unit_t x0_17381 = kk_Unit;
    kk_function_t _x19913 = kk_function_dup(action0); /*(10623) -> 10624 ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), _x19913, (_x19913, x, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x19914 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19915(action0, xx0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x19914); return kk_Unit;
    }
    { // tailcall
      xs = xx0;
      goto kk__tailcall;
    }
  }
}
 
// Apply a function for each character in a string slice.


// lift anonymous function
struct kk_std_core_foreach_fun19918__t_1 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_std_core_types__maybe kk_std_core_foreach_fun19918_1(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19918_1(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19918__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19918__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19918_1, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_fun19920__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19920_1(kk_function_t _fself, kk_box_t _b_18181, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19920_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19920_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19920_1(kk_function_t _fself, kk_box_t _b_18181, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18181, _ctx);
  return kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx);
}
static kk_std_core_types__maybe kk_std_core_foreach_fun19918_1(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19918__t_1* _self = kk_function_as(struct kk_std_core_foreach_fun19918__t_1*, _fself);
  kk_function_t action = _self->action; /* (c : char) -> 10682 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t x0_17387 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_char_t, kk_context_t*), action, (action, c, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x19919 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19920_1(_ctx), _ctx); /*3860*/
    return kk_std_core_types__maybe_unbox(_x19919, _ctx);
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_core_foreach_fun19922__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19922_1(kk_function_t _fself, kk_box_t _b_18185, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19922_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19922_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19922_1(kk_function_t _fself, kk_box_t _b_18185, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18185, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_std_core_foreach_1(kk_std_core__sslice slice0, kk_function_t action, kk_context_t* _ctx) { /* forall<e> (slice : sslice, action : (c : char) -> e ()) -> e () */ 
  kk_std_core_types__maybe x_17384 = kk_std_core_foreach_while_1(slice0, kk_std_core_new_foreach_fun19918_1(action, _ctx), _ctx); /*maybe<_10669>*/;
  kk_std_core_types__maybe_drop(x_17384, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19921 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19922_1(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x19921); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// Invoke a function for each character in a string


// lift anonymous function
struct kk_std_core_foreach_fun19926__t_2 {
  struct kk_function_s _base;
  kk_function_t action;
};
static kk_std_core_types__maybe kk_std_core_foreach_fun19926_2(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19926_2(kk_function_t action, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19926__t_2* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19926__t_2, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19926_2, kk_context());
  _self->action = action;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_fun19928__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19928_2(kk_function_t _fself, kk_box_t _b_18189, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19928_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19928_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19928_2(kk_function_t _fself, kk_box_t _b_18189, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18189, _ctx);
  return kk_std_core_types__maybe_box(kk_std_core_types__new_Nothing(_ctx), _ctx);
}
static kk_std_core_types__maybe kk_std_core_foreach_fun19926_2(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19926__t_2* _self = kk_function_as(struct kk_std_core_foreach_fun19926__t_2*, _fself);
  kk_function_t action = _self->action; /* (c : char) -> 10721 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_unit_t x0_17394 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_char_t, kk_context_t*), action, (action, c, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x19927 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19928_2(_ctx), _ctx); /*3860*/
    return kk_std_core_types__maybe_unbox(_x19927, _ctx);
  }
  {
    return kk_std_core_types__new_Nothing(_ctx);
  }
}


// lift anonymous function
struct kk_std_core_foreach_fun19930__t_2 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foreach_fun19930_2(kk_function_t _fself, kk_box_t _b_18193, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19930_2(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foreach_fun19930_2, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foreach_fun19930_2(kk_function_t _fself, kk_box_t _b_18193, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_box_drop(_b_18193, _ctx);
  return kk_unit_box(kk_Unit);
}

kk_unit_t kk_std_core_foreach_2(kk_string_t s, kk_function_t action, kk_context_t* _ctx) { /* forall<e> (s : string, action : (c : char) -> e ()) -> e () */ 
  kk_std_core__sslice slice0_16629;
  kk_string_t _x19923 = kk_string_dup(s); /*string*/
  kk_ssize_t _x19924 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x19925 = kk_string_len(s,kk_context()); /*ssize_t*/
  slice0_16629 = kk_std_core__new_Sslice(_x19923, _x19924, _x19925, _ctx); /*sslice*/
  kk_std_core_types__maybe x_17391 = kk_std_core_foreach_while_1(slice0_16629, kk_std_core_new_foreach_fun19926_2(action, _ctx), _ctx); /*maybe<_10669>*/;
  kk_std_core_types__maybe_drop(x_17391, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x19929 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_fun19930_2(_ctx), _ctx); /*3860*/
    kk_unit_unbox(_x19929); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// Invoke a function `f` for each element in a vector `v`


// lift anonymous function
struct kk_std_core_foreach_fun19933__t_3 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_fun19933_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_fun19933_3(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19933__t_3* _self = kk_function_alloc_as(struct kk_std_core_foreach_fun19933__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_fun19933_3, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_fun19933_3(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_fun19933__t_3* _self = kk_function_as(struct kk_std_core_foreach_fun19933__t_3*, _fself);
  kk_function_t f = _self->f; /* (10736) -> 10737 () */
  kk_vector_t v = _self->v; /* vector<10736> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t x_17584 = kk_vector_at(v,i,kk_context()); /*10736*/;
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x_17584, _ctx));
}

kk_unit_t kk_std_core_foreach_3(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a) -> e ()) -> e () */ 
  kk_ssize_t start0_17400 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17401;
  kk_ssize_t _x19931;
  kk_vector_t _x19932 = kk_vector_dup(v); /*vector<10736>*/
  _x19931 = kk_vector_len(_x19932,kk_context()); /*ssize_t*/
  end_17401 = kk_std_core_decr_1(_x19931, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_fun19933_3(f, v, _ctx), end_17401, start0_17400, _ctx); return kk_Unit;
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
struct kk_std_core_count_fun19938__t_3 {
  struct kk_function_s _base;
  kk_ref_t loc;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_count_fun19938_3(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_count_fun19938_3(kk_ref_t loc, kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_count_fun19938__t_3* _self = kk_function_alloc_as(struct kk_std_core_count_fun19938__t_3, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_count_fun19938_3, kk_context());
  _self->loc = loc;
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_count_fun19938_3(kk_function_t _fself, kk_char_t c, kk_context_t* _ctx) {
  struct kk_std_core_count_fun19938__t_3* _self = kk_function_as(struct kk_std_core_count_fun19938__t_3*, _fself);
  kk_ref_t loc = _self->loc; /* local-var<10930,int> */
  kk_function_t pred = _self->pred; /* (char) -> bool */
  kk_drop_match(_self, {kk_ref_dup(loc);kk_function_dup(pred);}, {}, _ctx)
  kk_unit_t __ = kk_Unit;
  bool _match_19090 = kk_function_call(bool, (kk_function_t, kk_char_t, kk_context_t*), pred, (pred, c, _ctx)); /*bool*/;
  if (_match_19090) {
    kk_integer_t _b_18202_18200;
    kk_integer_t _x19939;
    kk_box_t _x19940;
    kk_ref_t _x19941 = kk_ref_dup(loc); /*local-var<10930,int>*/
    _x19940 = (kk_ref_get(_x19941,kk_context())); /*233*/
    _x19939 = kk_integer_unbox(_x19940); /*int*/
    _b_18202_18200 = kk_integer_add(_x19939,(kk_integer_from_small(1)),kk_context()); /*int*/
    (kk_ref_set(loc,(kk_integer_box(_b_18202_18200)),kk_context()));
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
  kk_string_t _x19934 = kk_string_dup(s); /*string*/
  kk_ssize_t _x19935 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x19936 = kk_string_len(s,kk_context()); /*ssize_t*/
  slice0_16634 = kk_std_core__new_Sslice(_x19934, _x19935, _x19936, _ctx); /*sslice*/
  kk_std_core_types__maybe __0;
  kk_function_t _x19937;
  kk_ref_dup(loc);
  _x19937 = kk_std_core_new_count_fun19938_3(loc, pred, _ctx); /*(c : char) -> (local<10930>) (forall<a> maybe<a>)*/
  __0 = kk_std_core_foreach_while_1(slice0_16634, _x19937, _ctx); /*maybe<_10669>*/
  kk_std_core_types__maybe_drop(__0, _ctx);
  kk_integer_t res;
  kk_box_t _x19942;
  kk_ref_t _x19943 = kk_ref_dup(loc); /*local-var<10930,int>*/
  _x19942 = (kk_ref_get(_x19943,kk_context())); /*233*/
  res = kk_integer_unbox(_x19942); /*int*/
  kk_box_t _x19944 = kk_std_core_hnd_prompt_local_var(loc, kk_integer_box(res), _ctx); /*9897*/
  return kk_integer_unbox(_x19944);
}
 
// Is the integer negative (stricly smaller than zero)

bool kk_std_core_is_neg_2(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> bool */ 
  kk_std_core_types__order x_16723 = kk_int_as_order(kk_integer_signum(i,kk_context()),kk_context()); /*order*/;
  kk_integer_t _x19948;
  if (kk_std_core_types__is_Lt(x_16723)) {
    _x19948 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    goto _match19949;
  }
  if (kk_std_core_types__is_Eq(x_16723)) {
    _x19948 = kk_integer_from_small(0); /*int*/
    goto _match19949;
  }
  {
    _x19948 = kk_integer_from_small(1); /*int*/
  }
  _match19949: ;
  kk_integer_t _x19950 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
  return kk_integer_eq(_x19948,_x19950,kk_context());
}

kk_std_core_types__tuple2_ kk_std_core_divmod_exp10(kk_integer_t i, kk_integer_t n, kk_context_t* _ctx) { /* (i : int, n : int) -> (int, int) */ 
  kk_std_core_types__tuple2_ _match_19089;
  kk_integer_t _x19951 = kk_integer_dup(n); /*int*/
  _match_19089 = kk_std_core_cdivmod_exp10(i, _x19951, _ctx); /*(int, int)*/
  {
    kk_box_t _box_x18215 = _match_19089.fst;
    kk_box_t _box_x18216 = _match_19089.snd;
    kk_integer_t cq = kk_integer_unbox(_box_x18215);
    kk_integer_t cr = kk_integer_unbox(_box_x18216);
    kk_std_core_types__order x_16725;
    kk_integer_t _x19954 = kk_integer_dup(cr); /*int*/
    x_16725 = kk_int_as_order(kk_integer_signum(_x19954,kk_context()),kk_context()); /*order*/
    bool b_16637;
    kk_integer_t _x19955;
    if (kk_std_core_types__is_Lt(x_16725)) {
      _x19955 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
      goto _match19956;
    }
    if (kk_std_core_types__is_Eq(x_16725)) {
      _x19955 = kk_integer_from_small(0); /*int*/
      goto _match19956;
    }
    {
      _x19955 = kk_integer_from_small(1); /*int*/
    }
    _match19956: ;
    kk_integer_t _x19957 = kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context()); /*int*/
    b_16637 = kk_integer_eq(_x19955,_x19957,kk_context()); /*bool*/
    if (b_16637) {
      kk_integer_t _b_18221_18217 = kk_integer_sub(cq,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_integer_t _b_18222_18218;
      kk_integer_t _x19958 = kk_std_core_mul_exp10(kk_integer_from_small(1), n, _ctx); /*int*/
      _b_18222_18218 = kk_integer_add(cr,_x19958,kk_context()); /*int*/
      return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(_b_18221_18217), kk_integer_box(_b_18222_18218), _ctx);
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
    struct kk_std_core_Cons* _con19959 = kk_std_core__as_Cons(xs);
    kk_box_t _pat0 = _con19959->head;
    kk_std_core__list xx = _con19959->tail;
    kk_integer_t _x19960 = kk_integer_dup(n); /*int*/
    if (kk_integer_gt(_x19960,(kk_integer_from_small(0)),kk_context())) {
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_box_drop(_pat0, _ctx);
        kk_std_core__list_free(xs);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      { // tailcall
        kk_integer_t _x19961 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        xs = xx;
        n = _x19961;
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

kk_std_core__list kk_std_core__mlift17166_drop_while(kk_function_t predicate, kk_std_core__list xs, kk_std_core__list xx, bool _y_16959, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, xs : list<a>, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16959) {
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
struct kk_std_core_drop_while_fun19965__t {
  struct kk_function_s _base;
  kk_function_t predicate0;
  kk_std_core__list xs0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_drop_while_fun19965(kk_function_t _fself, kk_box_t _b_18226, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_drop_while_fun19965(kk_function_t predicate0, kk_std_core__list xs0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_drop_while_fun19965__t* _self = kk_function_alloc_as(struct kk_std_core_drop_while_fun19965__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_drop_while_fun19965, kk_context());
  _self->predicate0 = predicate0;
  _self->xs0 = xs0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_drop_while_fun19965(kk_function_t _fself, kk_box_t _b_18226, kk_context_t* _ctx) {
  struct kk_std_core_drop_while_fun19965__t* _self = kk_function_as(struct kk_std_core_drop_while_fun19965__t*, _fself);
  kk_function_t predicate0 = _self->predicate0; /* (11198) -> 11199 bool */
  kk_std_core__list xs0 = _self->xs0; /* list<11198> */
  kk_std_core__list xx0 = _self->xx0; /* list<11198> */
  kk_drop_match(_self, {kk_function_dup(predicate0);kk_std_core__list_dup(xs0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core__list _x19966;
  bool _x19967 = kk_bool_unbox(_b_18226); /*bool*/
  _x19966 = kk_std_core__mlift17166_drop_while(predicate0, xs0, xx0, _x19967, _ctx); /*list<11198>*/
  return kk_std_core__list_box(_x19966, _ctx);
}

kk_std_core__list kk_std_core_drop_while(kk_std_core__list xs0, kk_function_t predicate0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con19962 = kk_std_core__as_Cons(xs0);
    kk_box_t x = _con19962->head;
    kk_std_core__list xx0 = _con19962->tail;
    kk_box_dup(x);
    kk_std_core__list_dup(xx0);
    bool x0_17405;
    kk_function_t _x19963 = kk_function_dup(predicate0); /*(11198) -> 11199 bool*/
    x0_17405 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x19963, (_x19963, x, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x19964 = kk_std_core_hnd_yield_extend(kk_std_core_new_drop_while_fun19965(predicate0, xs0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x19964, _ctx);
    }
    if (x0_17405) {
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
  bool _match_19087;
  kk_string_t _x19972 = kk_string_dup(s); /*string*/
  kk_string_t _x19973 = kk_string_dup(post); /*string*/
  _match_19087 = kk_std_core_xends_with(_x19972, _x19973, _ctx); /*bool*/
  if (_match_19087) {
    kk_std_core__sslice _b_18230_18229;
    kk_string_t _x19974 = kk_string_dup(s); /*string*/
    kk_ssize_t _x19975 = ((kk_ssize_t)0); /*ssize_t*/
    kk_ssize_t _x19976;
    kk_ssize_t _x19977 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x19978 = kk_string_len(post,kk_context()); /*ssize_t*/
    _x19976 = (_x19977 - _x19978); /*ssize_t*/
    _b_18230_18229 = kk_std_core__new_Sslice(_x19974, _x19975, _x19976, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18230_18229, _ctx), _ctx);
  }
  {
    kk_string_drop(post, _ctx);
    kk_string_drop(s, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}
 
// Throw an exception with a specified message.

kk_box_t kk_std_core_throw(kk_string_t message0, kk_std_core_types__optional info0, kk_context_t* _ctx) { /* forall<a> (message : string, info : optional<exception-info>) -> exn a */ 
  kk_std_core_hnd__ev ev_17408;
  kk_ssize_t _x19979 = ((kk_ssize_t)0); /*ssize_t*/
  ev_17408 = kk_evv_at(_x19979,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
  {
    struct kk_std_core_hnd_Ev* _con19980 = kk_std_core_hnd__as_Ev(ev_17408);
    kk_std_core_hnd__marker m0 = _con19980->marker;
    kk_box_t _box_x18231 = _con19980->hnd;
    kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18231, NULL);
    kk_std_core__hnd_exn_dup(h);
    kk_std_core_hnd__clause1 _match_19086 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
    {
      kk_function_t _fun_unbox_x18235 = _match_19086.clause;
      kk_box_t _x19982;
      kk_std_core__exception _x19983;
      kk_std_core__exception_info _x19984;
      if (kk_std_core_types__is_Optional(info0)) {
        kk_box_t _box_x18239 = info0._cons.Optional.value;
        kk_std_core__exception_info _info_11284 = kk_std_core__exception_info_unbox(_box_x18239, NULL);
        kk_std_core__exception_info_dup(_info_11284);
        kk_std_core_types__optional_drop(info0, _ctx);
        _x19984 = _info_11284; /*exception-info*/
        goto _match19985;
      }
      {
        _x19984 = kk_std_core__new_ExnError(_ctx); /*exception-info*/
      }
      _match19985: ;
      _x19983 = kk_std_core__new_Exception(message0, _x19984, _ctx); /*exception*/
      _x19982 = kk_std_core__exception_box(_x19983, _ctx); /*51*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18235, (_fun_unbox_x18235, m0, ev_17408, _x19982, _ctx));
    }
  }
}
 
// Transform an `:error` type back to an `exn` effect.

kk_box_t kk_std_core_throw_1(kk_std_core__error err, kk_context_t* _ctx) { /* forall<a> (err : error<a>) -> exn a */ 
  if (kk_std_core__is_Error(err)) {
    kk_std_core__exception exn0 = err._cons.Error.exception;
    kk_std_core__exception_dup(exn0);
    kk_std_core__error_drop(err, _ctx);
    kk_std_core_hnd__ev ev_17411;
    kk_ssize_t _x19987 = ((kk_ssize_t)0); /*ssize_t*/
    ev_17411 = kk_evv_at(_x19987,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
    {
      struct kk_std_core_hnd_Ev* _con19988 = kk_std_core_hnd__as_Ev(ev_17411);
      kk_std_core_hnd__marker m0 = _con19988->marker;
      kk_box_t _box_x18240 = _con19988->hnd;
      kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18240, NULL);
      kk_std_core__hnd_exn_dup(h);
      kk_std_core_hnd__clause1 _match_19085 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
      {
        kk_function_t _fun_unbox_x18244 = _match_19085.clause;
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18244, (_fun_unbox_x18244, m0, ev_17411, kk_std_core__exception_box(exn0, _ctx), _ctx));
      }
    }
  }
  {
    kk_box_t x0 = err._cons.Ok.result;
    return x0;
  }
}
 
// monadic lift

kk_box_t kk_std_core__mlift17167_error_pattern(kk_string_t definition, kk_string_t location, kk_string_t _c_16966, kk_context_t* _ctx) { /* forall<a> (definition : string, location : string, string) -> a */ 
  kk_string_t message0_16643;
  kk_string_t _x19990 = kk_string_dup(location); /*string*/
  kk_string_t _x19991;
  kk_string_t _x19992;
  kk_define_string_literal(, _s19993, 23, ": pattern match failure")
  _x19992 = kk_string_dup(_s19993); /*string*/
  _x19991 = kk_std_core__lp__plus__plus__1_rp_(_c_16966, _x19992, _ctx); /*string*/
  message0_16643 = kk_std_core__lp__plus__plus__1_rp_(_x19990, _x19991, _ctx); /*string*/
  kk_std_core_hnd__ev ev_17414;
  kk_ssize_t _x19994 = ((kk_ssize_t)0); /*ssize_t*/
  ev_17414 = kk_evv_at(_x19994,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
  {
    struct kk_std_core_hnd_Ev* _con19995 = kk_std_core_hnd__as_Ev(ev_17414);
    kk_std_core_hnd__marker m0 = _con19995->marker;
    kk_box_t _box_x18248 = _con19995->hnd;
    kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18248, NULL);
    kk_std_core__hnd_exn_dup(h);
    kk_std_core_hnd__clause1 _match_19084 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
    {
      kk_function_t _fun_unbox_x18252 = _match_19084.clause;
      kk_box_t _x19997;
      kk_std_core__exception _x19998;
      kk_std_core__exception_info _x19999 = kk_std_core__new_ExnPattern(kk_reuse_null, location, definition, _ctx); /*exception-info*/
      _x19998 = kk_std_core__new_Exception(message0_16643, _x19999, _ctx); /*exception*/
      _x19997 = kk_std_core__exception_box(_x19998, _ctx); /*51*/
      return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18252, (_fun_unbox_x18252, m0, ev_17414, _x19997, _ctx));
    }
  }
}
 
// Raise a pattern match exception. This is function is used internally by the
// compiler to generate error messages on pattern match failures.


// lift anonymous function
struct kk_std_core_error_pattern_fun20007__t {
  struct kk_function_s _base;
  kk_string_t definition;
  kk_string_t location;
};
static kk_box_t kk_std_core_error_pattern_fun20007(kk_function_t _fself, kk_box_t _b_18257, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_error_pattern_fun20007(kk_string_t definition, kk_string_t location, kk_context_t* _ctx) {
  struct kk_std_core_error_pattern_fun20007__t* _self = kk_function_alloc_as(struct kk_std_core_error_pattern_fun20007__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_error_pattern_fun20007, kk_context());
  _self->definition = definition;
  _self->location = location;
  return &_self->_base;
}

static kk_box_t kk_std_core_error_pattern_fun20007(kk_function_t _fself, kk_box_t _b_18257, kk_context_t* _ctx) {
  struct kk_std_core_error_pattern_fun20007__t* _self = kk_function_as(struct kk_std_core_error_pattern_fun20007__t*, _fself);
  kk_string_t definition = _self->definition; /* string */
  kk_string_t location = _self->location; /* string */
  kk_drop_match(_self, {kk_string_dup(definition);kk_string_dup(location);}, {}, _ctx)
  kk_string_t _x20008 = kk_string_unbox(_b_18257); /*string*/
  return kk_std_core__mlift17167_error_pattern(definition, location, _x20008, _ctx);
}

kk_box_t kk_std_core_error_pattern(kk_string_t location, kk_string_t definition, kk_context_t* _ctx) { /* forall<a> (location : string, definition : string) -> exn a */ 
  kk_string_t x_17417;
  bool _match_19083;
  kk_string_t _x20000 = kk_string_dup(definition); /*string*/
  kk_string_t _x20001 = kk_string_empty(); /*string*/
  _match_19083 = kk_string_is_eq(_x20000,_x20001,kk_context()); /*bool*/
  if (_match_19083) {
    x_17417 = kk_string_empty(); /*string*/
  }
  else {
    kk_string_t _x20004;
    kk_define_string_literal(, _s20005, 2, ": ")
    _x20004 = kk_string_dup(_s20005); /*string*/
    kk_string_t _x20006 = kk_string_dup(definition); /*string*/
    x_17417 = kk_std_core__lp__plus__plus__1_rp_(_x20004, _x20006, _ctx); /*string*/
  }
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_17417, _ctx);
    return kk_std_core_hnd_yield_extend(kk_std_core_new_error_pattern_fun20007(definition, location, _ctx), _ctx);
  }
  {
    kk_string_t message0_16643;
    kk_string_t _x20009 = kk_string_dup(location); /*string*/
    kk_string_t _x20010;
    kk_string_t _x20011;
    kk_define_string_literal(, _s20012, 23, ": pattern match failure")
    _x20011 = kk_string_dup(_s20012); /*string*/
    _x20010 = kk_std_core__lp__plus__plus__1_rp_(x_17417, _x20011, _ctx); /*string*/
    message0_16643 = kk_std_core__lp__plus__plus__1_rp_(_x20009, _x20010, _ctx); /*string*/
    kk_std_core_hnd__ev ev_17420;
    kk_ssize_t _x20013 = ((kk_ssize_t)0); /*ssize_t*/
    ev_17420 = kk_evv_at(_x20013,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
    {
      struct kk_std_core_hnd_Ev* _con20014 = kk_std_core_hnd__as_Ev(ev_17420);
      kk_std_core_hnd__marker m0 = _con20014->marker;
      kk_box_t _box_x18258 = _con20014->hnd;
      kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18258, NULL);
      kk_std_core__hnd_exn_dup(h);
      kk_std_core_hnd__clause1 _match_19082 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
      {
        kk_function_t _fun_unbox_x18262 = _match_19082.clause;
        kk_box_t _x20016;
        kk_std_core__exception _x20017;
        kk_std_core__exception_info _x20018 = kk_std_core__new_ExnPattern(kk_reuse_null, location, definition, _ctx); /*exception-info*/
        _x20017 = kk_std_core__new_Exception(message0_16643, _x20018, _ctx); /*exception*/
        _x20016 = kk_std_core__exception_box(_x20017, _ctx); /*51*/
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18262, (_fun_unbox_x18262, m0, ev_17420, _x20016, _ctx));
      }
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17168_op(kk_std_core_types__ctail _acc, kk_function_t pred, kk_box_t x, kk_std_core__list xx, bool _y_16969, kk_context_t* _ctx) { /* forall<a,e> (ctail<list<a>>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16969) {
    kk_std_core__list _ctail_16813 = kk_std_core__list_hole(); /*list<11495>*/;
    kk_std_core__list _ctail_16814 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16813, _ctx); /*list<11495>*/;
    kk_std_core_types__ctail _x20019;
    kk_box_t* _b_18276_18273 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16814)->tail)); /*cfield<list<11495>>*/;
    _x20019 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16814, _ctx)),_b_18276_18273); /*ctail<0>*/
    return kk_std_core__ctail_filter(xx, pred, _x20019, _ctx);
  }
  {
    kk_box_drop(x, _ctx);
    return kk_std_core__ctail_filter(xx, pred, _acc, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17169_op_fun20020__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t x0;
};
static kk_std_core__list kk_std_core__mlift17169_op_fun20020(kk_function_t _fself, kk_std_core__list _ctail_16816, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17169_op_fun20020(kk_function_t _accm, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17169_op_fun20020__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17169_op_fun20020__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17169_op_fun20020, kk_context());
  _self->_accm = _accm;
  _self->x0 = x0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17169_op_fun20020(kk_function_t _fself, kk_std_core__list _ctail_16816, kk_context_t* _ctx) {
  struct kk_std_core__mlift17169_op_fun20020__t* _self = kk_function_as(struct kk_std_core__mlift17169_op_fun20020__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11495>) -> list<11495> */
  kk_box_t x0 = _self->x0; /* 11495 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(x0);}, {}, _ctx)
  kk_std_core__list _x20021 = kk_std_core__new_Cons(kk_reuse_null, x0, _ctail_16816, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20021, _ctx));
}

kk_std_core__list kk_std_core__mlift17169_op(kk_function_t _accm, kk_function_t pred0, kk_box_t x0, kk_std_core__list xx0, bool _y_16974, kk_context_t* _ctx) { /* forall<a,e> ((list<a>) -> list<a>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_16974) {
    return kk_std_core__ctailm_filter(xx0, pred0, kk_std_core__new_mlift17169_op_fun20020(_accm, x0, _ctx), _ctx);
  }
  {
    kk_box_drop(x0, _ctx);
    return kk_std_core__ctailm_filter(xx0, pred0, _accm, _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`


// lift anonymous function
struct kk_std_core__ctail_filter_fun20026__t {
  struct kk_function_s _base;
  kk_function_t pred1;
  kk_box_t x1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_filter_fun20026(kk_function_t _fself, kk_box_t _b_18281, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_filter_fun20026(kk_function_t pred1, kk_box_t x1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_fun20026__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_filter_fun20026__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_filter_fun20026, kk_context());
  _self->pred1 = pred1;
  _self->x1 = x1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_filter_fun20026(kk_function_t _fself, kk_box_t _b_18281, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_fun20026__t* _self = kk_function_as(struct kk_std_core__ctail_filter_fun20026__t*, _fself);
  kk_function_t pred1 = _self->pred1; /* (11495) -> 11496 bool */
  kk_box_t x1 = _self->x1; /* 11495 */
  kk_std_core__list xx1 = _self->xx1; /* list<11495> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11495>> */
  kk_drop_match(_self, {kk_function_dup(pred1);kk_box_dup(x1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20027;
  bool _x20028 = kk_bool_unbox(_b_18281); /*bool*/
  _x20027 = kk_std_core__mlift17168_op(_acc0, pred1, x1, xx1, _x20028, _ctx); /*list<11495>*/
  return kk_std_core__list_box(_x20027, _ctx);
}

kk_std_core__list kk_std_core__ctail_filter(kk_std_core__list xs, kk_function_t pred1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, ctail<list<a>>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20022 = kk_std_core__as_Cons(xs);
    kk_box_t x1 = _con20022->head;
    kk_std_core__list xx1 = _con20022->tail;
    kk_reuse_t _ru_18925 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18925 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18925 = kk_reuse_null;
    }
    bool x2_17423;
    kk_function_t _x20024 = kk_function_dup(pred1); /*(11495) -> 11496 bool*/
    kk_box_t _x20023 = kk_box_dup(x1); /*11495*/
    x2_17423 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20024, (_x20024, _x20023, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18925, _ctx);
      kk_box_t _x20025 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_filter_fun20026(pred1, x1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20025, _ctx);
    }
    if (x2_17423) {
      kk_std_core__list _ctail_168130 = kk_std_core__list_hole(); /*list<11495>*/;
      kk_std_core__list _ctail_168140;
      if (kk_likely(_ru_18925!=NULL)) {
        struct kk_std_core_Cons* _con20029 = (struct kk_std_core_Cons*)_ru_18925;
        _con20029->tail = _ctail_168130;
        _ctail_168140 = kk_std_core__base_Cons(_con20029); /*list<11495>*/
      }
      else {
        _ctail_168140 = kk_std_core__new_Cons(kk_reuse_null, x1, _ctail_168130, _ctx); /*list<11495>*/
      }
      { // tailcall
        kk_std_core_types__ctail _x20030;
        kk_box_t* _b_18293_18287 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168140)->tail)); /*cfield<list<11495>>*/;
        _x20030 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168140, _ctx)),_b_18293_18287); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20030;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18925, _ctx);
      kk_box_drop(x1, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(pred1, _ctx);
    kk_box_t _x20031 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20031, _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`


// lift anonymous function
struct kk_std_core__ctailm_filter_fun20036__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t pred2;
  kk_box_t x3;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_filter_fun20036(kk_function_t _fself, kk_box_t _b_18301, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_fun20036(kk_function_t _accm0, kk_function_t pred2, kk_box_t x3, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20036__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_fun20036__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_fun20036, kk_context());
  _self->_accm0 = _accm0;
  _self->pred2 = pred2;
  _self->x3 = x3;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_filter_fun20036(kk_function_t _fself, kk_box_t _b_18301, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20036__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_fun20036__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11495>) -> list<11495> */
  kk_function_t pred2 = _self->pred2; /* (11495) -> 11496 bool */
  kk_box_t x3 = _self->x3; /* 11495 */
  kk_std_core__list xx2 = _self->xx2; /* list<11495> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(pred2);kk_box_dup(x3);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20037;
  bool _x20038 = kk_bool_unbox(_b_18301); /*bool*/
  _x20037 = kk_std_core__mlift17169_op(_accm0, pred2, x3, xx2, _x20038, _ctx); /*list<11495>*/
  return kk_std_core__list_box(_x20037, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_filter_fun20040__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x3;
};
static kk_std_core__list kk_std_core__ctailm_filter_fun20040(kk_function_t _fself, kk_std_core__list _ctail_168160, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_fun20040(kk_function_t _accm0, kk_box_t x3, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20040__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_fun20040__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_fun20040, kk_context());
  _self->_accm0 = _accm0;
  _self->x3 = x3;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_filter_fun20040(kk_function_t _fself, kk_std_core__list _ctail_168160, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_fun20040__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_fun20040__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11495>) -> list<11495> */
  kk_box_t x3 = _self->x3; /* 11495 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x3);}, {}, _ctx)
  kk_std_core__list _x20041 = kk_std_core__new_Cons(kk_reuse_null, x3, _ctail_168160, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20041, _ctx));
}

kk_std_core__list kk_std_core__ctailm_filter(kk_std_core__list xs0, kk_function_t pred2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20032 = kk_std_core__as_Cons(xs0);
    kk_box_t x3 = _con20032->head;
    kk_std_core__list xx2 = _con20032->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x3);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    bool x4_17426;
    kk_function_t _x20034 = kk_function_dup(pred2); /*(11495) -> 11496 bool*/
    kk_box_t _x20033 = kk_box_dup(x3); /*11495*/
    x4_17426 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20034, (_x20034, _x20033, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x20035 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_filter_fun20036(_accm0, pred2, x3, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20035, _ctx);
    }
    if (x4_17426) { // tailcall
                    kk_function_t _x20039 = kk_std_core__new_ctailm_filter_fun20040(_accm0, x3, _ctx); /*(list<11495>) -> list<11495>*/
                    xs0 = xx2;
                    _accm0 = _x20039;
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
struct kk_std_core_filter_fun20043__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_filter_fun20043(kk_function_t _fself, kk_std_core__list _ctail_16815, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_filter_fun20043(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_filter_fun20043, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_filter_fun20043(kk_function_t _fself, kk_std_core__list _ctail_16815, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16815;
}

kk_std_core__list kk_std_core_filter(kk_std_core__list xs1, kk_function_t pred3, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e list<a> */ 
  bool _match_19078 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19078) {
    kk_std_core_types__ctail _x20042 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_filter(xs1, pred3, _x20042, _ctx);
  }
  {
    return kk_std_core__ctailm_filter(xs1, pred3, kk_std_core_new_filter_fun20043(_ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17170_op(kk_std_core_types__ctail _acc, kk_function_t pred, kk_std_core__list xx, kk_std_core_types__maybe _y_16982, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16982)) {
    return kk_std_core__ctail_filter_map(xx, pred, _acc, _ctx);
  }
  {
    kk_box_t y = _y_16982._cons.Just.value;
    kk_std_core__list _ctail_16817 = kk_std_core__list_hole(); /*list<11541>*/;
    kk_std_core__list _ctail_16818 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16817, _ctx); /*list<11541>*/;
    kk_std_core_types__ctail _x20044;
    kk_box_t* _b_18312_18309 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16818)->tail)); /*cfield<list<11541>>*/;
    _x20044 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16818, _ctx)),_b_18312_18309); /*ctail<0>*/
    return kk_std_core__ctail_filter_map(xx, pred, _x20044, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17171_op_fun20045__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17171_op_fun20045(kk_function_t _fself, kk_std_core__list _ctail_16820, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17171_op_fun20045(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17171_op_fun20045__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17171_op_fun20045__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17171_op_fun20045, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17171_op_fun20045(kk_function_t _fself, kk_std_core__list _ctail_16820, kk_context_t* _ctx) {
  struct kk_std_core__mlift17171_op_fun20045__t* _self = kk_function_as(struct kk_std_core__mlift17171_op_fun20045__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11541>) -> list<11541> */
  kk_box_t y0 = _self->y0; /* 11541 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20046 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16820, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20046, _ctx));
}

kk_std_core__list kk_std_core__mlift17171_op(kk_function_t _accm, kk_function_t pred0, kk_std_core__list xx0, kk_std_core_types__maybe _y_16987, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Nothing(_y_16987)) {
    return kk_std_core__ctailm_filter_map(xx0, pred0, _accm, _ctx);
  }
  {
    kk_box_t y0 = _y_16987._cons.Just.value;
    return kk_std_core__ctailm_filter_map(xx0, pred0, kk_std_core__new_mlift17171_op_fun20045(_accm, y0, _ctx), _ctx);
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core__ctail_filter_map_fun20051__t {
  struct kk_function_s _base;
  kk_function_t pred1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_filter_map_fun20051(kk_function_t _fself, kk_box_t _b_18319, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_filter_map_fun20051(kk_function_t pred1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_map_fun20051__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_filter_map_fun20051__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_filter_map_fun20051, kk_context());
  _self->pred1 = pred1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_filter_map_fun20051(kk_function_t _fself, kk_box_t _b_18319, kk_context_t* _ctx) {
  struct kk_std_core__ctail_filter_map_fun20051__t* _self = kk_function_as(struct kk_std_core__ctail_filter_map_fun20051__t*, _fself);
  kk_function_t pred1 = _self->pred1; /* (11540) -> 11542 maybe<11541> */
  kk_std_core__list xx1 = _self->xx1; /* list<11540> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11541>> */
  kk_drop_match(_self, {kk_function_dup(pred1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20052;
  kk_std_core_types__maybe _x20053 = kk_std_core_types__maybe_unbox(_b_18319, _ctx); /*maybe<11541>*/
  _x20052 = kk_std_core__mlift17170_op(_acc0, pred1, xx1, _x20053, _ctx); /*list<11541>*/
  return kk_std_core__list_box(_x20052, _ctx);
}

kk_std_core__list kk_std_core__ctail_filter_map(kk_std_core__list xs, kk_function_t pred1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(pred1, _ctx);
    kk_box_t _x20047 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20047, _ctx);
  }
  {
    struct kk_std_core_Cons* _con20048 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20048->head;
    kk_std_core__list xx1 = _con20048->tail;
    kk_reuse_t _ru_18927 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18927 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18927 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17429;
    kk_function_t _x20049 = kk_function_dup(pred1); /*(11540) -> 11542 maybe<11541>*/
    x0_17429 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20049, (_x20049, x, _ctx)); /*maybe<11541>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18927, _ctx);
      kk_std_core_types__maybe_drop(x0_17429, _ctx);
      kk_box_t _x20050 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_filter_map_fun20051(pred1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20050, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x0_17429)) {
      kk_reuse_drop(_ru_18927, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
    {
      kk_box_t y1 = x0_17429._cons.Just.value;
      kk_std_core__list _ctail_168170 = kk_std_core__list_hole(); /*list<11541>*/;
      kk_std_core__list _ctail_168180 = kk_std_core__new_Cons(_ru_18927, y1, _ctail_168170, _ctx); /*list<11541>*/;
      { // tailcall
        kk_std_core_types__ctail _x20054;
        kk_box_t* _b_18331_18325 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168180)->tail)); /*cfield<list<11541>>*/;
        _x20054 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168180, _ctx)),_b_18331_18325); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20054;
        goto kk__tailcall;
      }
    }
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core__ctailm_filter_map_fun20058__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t pred2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_filter_map_fun20058(kk_function_t _fself, kk_box_t _b_18337, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_map_fun20058(kk_function_t _accm0, kk_function_t pred2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20058__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_map_fun20058__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_map_fun20058, kk_context());
  _self->_accm0 = _accm0;
  _self->pred2 = pred2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_filter_map_fun20058(kk_function_t _fself, kk_box_t _b_18337, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20058__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_map_fun20058__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11541>) -> list<11541> */
  kk_function_t pred2 = _self->pred2; /* (11540) -> 11542 maybe<11541> */
  kk_std_core__list xx2 = _self->xx2; /* list<11540> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(pred2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20059;
  kk_std_core_types__maybe _x20060 = kk_std_core_types__maybe_unbox(_b_18337, _ctx); /*maybe<11541>*/
  _x20059 = kk_std_core__mlift17171_op(_accm0, pred2, xx2, _x20060, _ctx); /*list<11541>*/
  return kk_std_core__list_box(_x20059, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_filter_map_fun20062__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_filter_map_fun20062(kk_function_t _fself, kk_std_core__list _ctail_168200, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_filter_map_fun20062(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20062__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_filter_map_fun20062__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_filter_map_fun20062, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_filter_map_fun20062(kk_function_t _fself, kk_std_core__list _ctail_168200, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_filter_map_fun20062__t* _self = kk_function_as(struct kk_std_core__ctailm_filter_map_fun20062__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11541>) -> list<11541> */
  kk_box_t y2 = _self->y2; /* 11541 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20063 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168200, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20063, _ctx));
}

kk_std_core__list kk_std_core__ctailm_filter_map(kk_std_core__list xs0, kk_function_t pred2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs0)) {
    kk_function_drop(pred2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
  {
    struct kk_std_core_Cons* _con20055 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20055->head;
    kk_std_core__list xx2 = _con20055->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17432;
    kk_function_t _x20056 = kk_function_dup(pred2); /*(11540) -> 11542 maybe<11541>*/
    x2_17432 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20056, (_x20056, x1, _ctx)); /*maybe<11541>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17432, _ctx);
      kk_box_t _x20057 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_filter_map_fun20058(_accm0, pred2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20057, _ctx);
    }
    if (kk_std_core_types__is_Nothing(x2_17432)) { // tailcall
                                                   xs0 = xx2;
                                                   goto kk__tailcall;
    }
    {
      kk_box_t y2 = x2_17432._cons.Just.value;
      { // tailcall
        kk_function_t _x20061 = kk_std_core__new_ctailm_filter_map_fun20062(_accm0, y2, _ctx); /*(list<11541>) -> list<11541>*/
        xs0 = xx2;
        _accm0 = _x20061;
        goto kk__tailcall;
      }
    }
  }
}
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if (i.odd?) then Nothing else Just(i*i) }) == [4]`


// lift anonymous function
struct kk_std_core_filter_map_fun20065__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_filter_map_fun20065(kk_function_t _fself, kk_std_core__list _ctail_16819, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_filter_map_fun20065(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_filter_map_fun20065, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_filter_map_fun20065(kk_function_t _fself, kk_std_core__list _ctail_16819, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16819;
}

kk_std_core__list kk_std_core_filter_map(kk_std_core__list xs1, kk_function_t pred3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_19075 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19075) {
    kk_std_core_types__ctail _x20064 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_filter_map(xs1, pred3, _x20064, _ctx);
  }
  {
    return kk_std_core__ctailm_filter_map(xs1, pred3, kk_std_core_new_filter_map_fun20065(_ctx), _ctx);
  }
}

bool kk_std_core_is_zero_1(kk_ssize_t i, kk_context_t* _ctx) { /* (i : ssize_t) -> bool */ 
  return (i == 0);
}
 
// Find the first element satisfying some predicate


// lift anonymous function
struct kk_std_core_find_fun20066__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_find_fun20066(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_find_fun20066(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_find_fun20066__t* _self = kk_function_alloc_as(struct kk_std_core_find_fun20066__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_find_fun20066, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_find_fun20066(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_find_fun20066__t* _self = kk_function_as(struct kk_std_core_find_fun20066__t*, _fself);
  kk_function_t pred = _self->pred; /* (11639) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  bool _match_19074;
  kk_box_t _x20067 = kk_box_dup(x); /*11639*/
  _match_19074 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), pred, (pred, _x20067, _ctx)); /*bool*/
  if (_match_19074) {
    return kk_std_core_types__new_Just(x, _ctx);
  }
  {
    kk_box_drop(x, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_core_find(kk_std_core__list xs, kk_function_t pred, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, pred : (a) -> bool) -> maybe<a> */ 
  return kk_std_core_foreach_while(xs, kk_std_core_new_find_fun20066(pred, _ctx), _ctx);
}
 
// O(n). If it occurs, return the position of substring `sub` in `s`, tupled with
// the position just following the substring `sub`.

kk_std_core_types__maybe kk_std_core_find_1(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> maybe<sslice> */ 
  kk_ssize_t i;
  kk_string_t _x20068 = kk_string_dup(s); /*string*/
  kk_string_t _x20069 = kk_string_dup(sub); /*string*/
  i = kk_string_index_of1(_x20068,_x20069,kk_context()); /*ssize_t*/
  bool _match_19073 = kk_std_core_is_zero_1(i, _ctx); /*bool*/;
  if (_match_19073) {
    kk_string_drop(s, _ctx);
    kk_string_drop(sub, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_std_core__sslice _b_18341_18340;
    kk_ssize_t _x20070 = kk_std_core_decr_1(i, _ctx); /*ssize_t*/
    kk_ssize_t _x20071 = kk_string_len(sub,kk_context()); /*ssize_t*/
    _b_18341_18340 = kk_std_core__new_Sslice(s, _x20070, _x20071, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18341_18340, _ctx), _ctx);
  }
}
 
// Return the last index of substring `sub` in `s` if it occurs.

kk_std_core_types__maybe kk_std_core_find_last(kk_string_t s, kk_string_t sub, kk_context_t* _ctx) { /* (s : string, sub : string) -> maybe<sslice> */ 
  kk_ssize_t i;
  kk_string_t _x20072 = kk_string_dup(s); /*string*/
  kk_string_t _x20073 = kk_string_dup(sub); /*string*/
  i = kk_string_last_index_of1(_x20072,_x20073,kk_context()); /*ssize_t*/
  bool _match_19072 = kk_std_core_is_zero_1(i, _ctx); /*bool*/;
  if (_match_19072) {
    kk_string_drop(s, _ctx);
    kk_string_drop(sub, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    kk_std_core__sslice _b_18343_18342;
    kk_ssize_t _x20074 = kk_std_core_decr_1(i, _ctx); /*ssize_t*/
    kk_ssize_t _x20075 = kk_string_len(sub,kk_context()); /*ssize_t*/
    _b_18343_18342 = kk_std_core__new_Sslice(s, _x20074, _x20075, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18343_18342, _ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17172_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_std_core_types__maybe _y_16995, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_16995)) {
    kk_box_t y = _y_16995._cons.Just.value;
    kk_std_core__list _ctail_16821 = kk_std_core__list_hole(); /*list<11845>*/;
    kk_std_core__list _ctail_16822 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16821, _ctx); /*list<11845>*/;
    kk_std_core_types__ctail _x20076;
    kk_box_t* _b_18352_18349 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16822)->tail)); /*cfield<list<11845>>*/;
    _x20076 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16822, _ctx)),_b_18352_18349); /*ctail<0>*/
    return kk_std_core__ctail_flatmap_maybe(xx, f, _x20076, _ctx);
  }
  {
    return kk_std_core__ctail_flatmap_maybe(xx, f, _acc, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17173_op_fun20077__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17173_op_fun20077(kk_function_t _fself, kk_std_core__list _ctail_16824, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17173_op_fun20077(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17173_op_fun20077__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17173_op_fun20077__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17173_op_fun20077, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17173_op_fun20077(kk_function_t _fself, kk_std_core__list _ctail_16824, kk_context_t* _ctx) {
  struct kk_std_core__mlift17173_op_fun20077__t* _self = kk_function_as(struct kk_std_core__mlift17173_op_fun20077__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<11845>) -> list<11845> */
  kk_box_t y0 = _self->y0; /* 11845 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20078 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16824, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20078, _ctx));
}

kk_std_core__list kk_std_core__mlift17173_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_std_core_types__maybe _y_17000, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17000)) {
    kk_box_t y0 = _y_17000._cons.Just.value;
    return kk_std_core__ctailm_flatmap_maybe(xx0, f0, kk_std_core__new_mlift17173_op_fun20077(_accm, y0, _ctx), _ctx);
  }
  {
    return kk_std_core__ctailm_flatmap_maybe(xx0, f0, _accm, _ctx);
  }
}
 
// Concatenate the `Just` result elements from applying a function to all elements.


// lift anonymous function
struct kk_std_core__ctail_flatmap_maybe_fun20082__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_flatmap_maybe_fun20082(kk_function_t _fself, kk_box_t _b_18357, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_flatmap_maybe_fun20082(kk_function_t f1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_flatmap_maybe_fun20082__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_flatmap_maybe_fun20082__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_flatmap_maybe_fun20082, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_flatmap_maybe_fun20082(kk_function_t _fself, kk_box_t _b_18357, kk_context_t* _ctx) {
  struct kk_std_core__ctail_flatmap_maybe_fun20082__t* _self = kk_function_as(struct kk_std_core__ctail_flatmap_maybe_fun20082__t*, _fself);
  kk_function_t f1 = _self->f1; /* (11844) -> 11846 maybe<11845> */
  kk_std_core__list xx1 = _self->xx1; /* list<11844> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<11845>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20083;
  kk_std_core_types__maybe _x20084 = kk_std_core_types__maybe_unbox(_b_18357, _ctx); /*maybe<11845>*/
  _x20083 = kk_std_core__mlift17172_op(_acc0, f1, xx1, _x20084, _ctx); /*list<11845>*/
  return kk_std_core__list_box(_x20083, _ctx);
}

kk_std_core__list kk_std_core__ctail_flatmap_maybe(kk_std_core__list xs, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20079 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20079->head;
    kk_std_core__list xx1 = _con20079->tail;
    kk_reuse_t _ru_18929 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18929 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18929 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17435;
    kk_function_t _x20080 = kk_function_dup(f1); /*(11844) -> 11846 maybe<11845>*/
    x0_17435 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20080, (_x20080, x, _ctx)); /*maybe<11845>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18929, _ctx);
      kk_std_core_types__maybe_drop(x0_17435, _ctx);
      kk_box_t _x20081 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_flatmap_maybe_fun20082(f1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20081, _ctx);
    }
    if (kk_std_core_types__is_Just(x0_17435)) {
      kk_box_t y1 = x0_17435._cons.Just.value;
      kk_std_core__list _ctail_168210 = kk_std_core__list_hole(); /*list<11845>*/;
      kk_std_core__list _ctail_168220 = kk_std_core__new_Cons(_ru_18929, y1, _ctail_168210, _ctx); /*list<11845>*/;
      { // tailcall
        kk_std_core_types__ctail _x20085;
        kk_box_t* _b_18369_18363 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168220)->tail)); /*cfield<list<11845>>*/;
        _x20085 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168220, _ctx)),_b_18369_18363); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20085;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18929, _ctx);
      { // tailcall
        xs = xx1;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_box_t _x20086 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20086, _ctx);
  }
}
 
// Concatenate the `Just` result elements from applying a function to all elements.


// lift anonymous function
struct kk_std_core__ctailm_flatmap_maybe_fun20090__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_flatmap_maybe_fun20090(kk_function_t _fself, kk_box_t _b_18377, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_flatmap_maybe_fun20090(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20090__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_flatmap_maybe_fun20090__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_flatmap_maybe_fun20090, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_flatmap_maybe_fun20090(kk_function_t _fself, kk_box_t _b_18377, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20090__t* _self = kk_function_as(struct kk_std_core__ctailm_flatmap_maybe_fun20090__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11845>) -> list<11845> */
  kk_function_t f2 = _self->f2; /* (11844) -> 11846 maybe<11845> */
  kk_std_core__list xx2 = _self->xx2; /* list<11844> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20091;
  kk_std_core_types__maybe _x20092 = kk_std_core_types__maybe_unbox(_b_18377, _ctx); /*maybe<11845>*/
  _x20091 = kk_std_core__mlift17173_op(_accm0, f2, xx2, _x20092, _ctx); /*list<11845>*/
  return kk_std_core__list_box(_x20091, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_flatmap_maybe_fun20094__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_flatmap_maybe_fun20094(kk_function_t _fself, kk_std_core__list _ctail_168240, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_flatmap_maybe_fun20094(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20094__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_flatmap_maybe_fun20094__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_flatmap_maybe_fun20094, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_flatmap_maybe_fun20094(kk_function_t _fself, kk_std_core__list _ctail_168240, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_flatmap_maybe_fun20094__t* _self = kk_function_as(struct kk_std_core__ctailm_flatmap_maybe_fun20094__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<11845>) -> list<11845> */
  kk_box_t y2 = _self->y2; /* 11845 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20095 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168240, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20095, _ctx));
}

kk_std_core__list kk_std_core__ctailm_flatmap_maybe(kk_std_core__list xs0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20087 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20087->head;
    kk_std_core__list xx2 = _con20087->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17438;
    kk_function_t _x20088 = kk_function_dup(f2); /*(11844) -> 11846 maybe<11845>*/
    x2_17438 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20088, (_x20088, x1, _ctx)); /*maybe<11845>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17438, _ctx);
      kk_box_t _x20089 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_flatmap_maybe_fun20090(_accm0, f2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20089, _ctx);
    }
    if (kk_std_core_types__is_Just(x2_17438)) {
      kk_box_t y2 = x2_17438._cons.Just.value;
      { // tailcall
        kk_function_t _x20093 = kk_std_core__new_ctailm_flatmap_maybe_fun20094(_accm0, y2, _ctx); /*(list<11845>) -> list<11845>*/
        xs0 = xx2;
        _accm0 = _x20093;
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
struct kk_std_core_flatmap_maybe_fun20097__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_flatmap_maybe_fun20097(kk_function_t _fself, kk_std_core__list _ctail_16823, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_flatmap_maybe_fun20097(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_flatmap_maybe_fun20097, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_flatmap_maybe_fun20097(kk_function_t _fself, kk_std_core__list _ctail_16823, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16823;
}

kk_std_core__list kk_std_core_flatmap_maybe(kk_std_core__list xs1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_19069 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19069) {
    kk_std_core_types__ctail _x20096 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_flatmap_maybe(xs1, f3, _x20096, _ctx);
  }
  {
    return kk_std_core__ctailm_flatmap_maybe(xs1, f3, kk_std_core_new_flatmap_maybe_fun20097(_ctx), _ctx);
  }
}

kk_box_t kk_std_core_fold_int(kk_integer_t start0, kk_integer_t end, kk_box_t init0, kk_function_t f, kk_context_t* _ctx) { /* forall<a> (start : int, end : int, init : a, f : (int, a) -> a) -> a */ 
  kk__tailcall: ;
  bool _match_19068;
  kk_integer_t _x20098 = kk_integer_dup(start0); /*int*/
  kk_integer_t _x20099 = kk_integer_dup(end); /*int*/
  _match_19068 = kk_integer_gte(_x20098,_x20099,kk_context()); /*bool*/
  if (_match_19068) {
    kk_integer_drop(end, _ctx);
    kk_function_drop(f, _ctx);
    kk_integer_drop(start0, _ctx);
    return init0;
  }
  {
    kk_box_t x;
    kk_function_t _x20101 = kk_function_dup(f); /*(int, 11903) -> 11903*/
    kk_integer_t _x20100 = kk_integer_dup(start0); /*int*/
    x = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), _x20101, (_x20101, _x20100, init0, _ctx)); /*11903*/
    { // tailcall
      kk_integer_t _x20102 = kk_integer_add(start0,(kk_integer_from_small(1)),kk_context()); /*int*/
      start0 = _x20102;
      init0 = x;
      goto kk__tailcall;
    }
  }
}
 
// monadic lift

kk_box_t kk_std_core__mlift17174_foldl(kk_function_t f, kk_std_core__list xx, kk_box_t _y_17008, kk_context_t* _ctx) { /* forall<a,e,b> (f : (a, b) -> e a, xx : list<b>, a) -> e a */ 
  return kk_std_core_foldl(xx, _y_17008, f, _ctx);
}
 
// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`


// lift anonymous function
struct kk_std_core_foldl_fun20105__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_foldl_fun20105(kk_function_t _fself, kk_box_t _y_170080, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldl_fun20105(kk_function_t f0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_foldl_fun20105__t* _self = kk_function_alloc_as(struct kk_std_core_foldl_fun20105__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foldl_fun20105, kk_context());
  _self->f0 = f0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_foldl_fun20105(kk_function_t _fself, kk_box_t _y_170080, kk_context_t* _ctx) {
  struct kk_std_core_foldl_fun20105__t* _self = kk_function_as(struct kk_std_core_foldl_fun20105__t*, _fself);
  kk_function_t f0 = _self->f0; /* (11906, 11911) -> 11908 11906 */
  kk_std_core__list xx0 = _self->xx0; /* list<11911> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  return kk_std_core__mlift17174_foldl(f0, xx0, _y_170080, _ctx);
}

kk_box_t kk_std_core_foldl(kk_std_core__list xs, kk_box_t z, kk_function_t f0, kk_context_t* _ctx) { /* forall<a,b,e> (list<a>, b, (b, a) -> e b) -> e b */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20103 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20103->head;
    kk_std_core__list xx0 = _con20103->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t x0_17441;
    kk_function_t _x20104 = kk_function_dup(f0); /*(11906, 11911) -> 11908 11906*/
    x0_17441 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20104, (_x20104, z, x, _ctx)); /*11906*/
    if (kk_yielding(kk_context())) {
      kk_box_drop(x0_17441, _ctx);
      return kk_std_core_hnd_yield_extend(kk_std_core_new_foldl_fun20105(f0, xx0, _ctx), _ctx);
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
struct kk_std_core_foldl1_fun20108__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foldl1_fun20108(kk_function_t _fself, kk_box_t _b_18384, kk_box_t _b_18385, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldl1_fun20108(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foldl1_fun20108, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foldl1_fun20108(kk_function_t _fself, kk_box_t _b_18384, kk_box_t _b_18385, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x20109 = kk_string_unbox(_b_18384); /*string*/
  kk_std_core_types__optional _x20110 = kk_std_core_types__optional_unbox(_b_18385, _ctx); /*optional<exception-info>*/
  return kk_std_core_throw(_x20109, _x20110, _ctx);
}

kk_box_t kk_std_core_foldl1(kk_std_core__list xs, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20106 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20106->head;
    kk_std_core__list xx = _con20106->tail;
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
    kk_ssize_t _b_18386_18380;
    kk_std_core_hnd__htag _x20107 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
    _b_18386_18380 = kk_std_core_hnd__evv_index(_x20107, _ctx); /*std/core/hnd/ev-index*/
    kk_box_t _x20111;
    kk_string_t _x20112;
    kk_define_string_literal(, _s20113, 33, "unexpected Nil in std/core/foldl1")
    _x20112 = kk_string_dup(_s20113); /*string*/
    _x20111 = kk_string_box(_x20112); /*5549*/
    return kk_std_core_hnd__open_at2(_b_18386_18380, kk_std_core_new_foldl1_fun20108(_ctx), _x20111, kk_std_core_types__optional_box(kk_std_core_types__new_None(_ctx), _ctx), _ctx);
  }
}
 
// lifted

kk_std_core__list kk_std_core__lift16747_reverse(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20114 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con20114->head;
    kk_std_core__list xx = _con20114->tail;
    kk_reuse_t _ru_18933 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18933 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18933 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20115;
      if (kk_likely(_ru_18933!=NULL)) {
        struct kk_std_core_Cons* _con20116 = (struct kk_std_core_Cons*)_ru_18933;
        _con20116->tail = acc;
        _x20115 = kk_std_core__base_Cons(_con20116); /*list<61>*/
      }
      else {
        _x20115 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x20115;
      ys = xx;
      goto kk__tailcall;
    }
  }
  {
    return acc;
  }
}
extern kk_box_t kk_std_core_foldr_fun20118(kk_function_t _fself, kk_box_t x, kk_box_t y, kk_context_t* _ctx) {
  struct kk_std_core_foldr_fun20118__t* _self = kk_function_as(struct kk_std_core_foldr_fun20118__t*, _fself);
  kk_function_t f = _self->f; /* (11997, 11993) -> 11995 11993 */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  return kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), f, (f, y, x, _ctx));
}


// lift anonymous function
struct kk_std_core_foldr1_fun20121__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_foldr1_fun20121(kk_function_t _fself, kk_box_t _b_18394, kk_box_t _b_18395, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foldr1_fun20121(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_foldr1_fun20121, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_foldr1_fun20121(kk_function_t _fself, kk_box_t _b_18394, kk_box_t _b_18395, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_string_t _x20122 = kk_string_unbox(_b_18394); /*string*/
  kk_std_core_types__optional _x20123 = kk_std_core_types__optional_unbox(_b_18395, _ctx); /*optional<exception-info>*/
  return kk_std_core_throw(_x20122, _x20123, _ctx);
}

kk_box_t kk_std_core_foldr1(kk_std_core__list xs, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */ 
  kk_std_core__list xs0_16649 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), xs, _ctx); /*list<12031>*/;
  if (kk_std_core__is_Cons(xs0_16649)) {
    struct kk_std_core_Cons* _con20119 = kk_std_core__as_Cons(xs0_16649);
    kk_box_t x = _con20119->head;
    kk_std_core__list xx = _con20119->tail;
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
    kk_ssize_t _b_18396_18390;
    kk_std_core_hnd__htag _x20120 = kk_std_core_hnd__htag_dup(kk_std_core__tag_exn); /*std/core/hnd/htag<.hnd-exn>*/
    _b_18396_18390 = kk_std_core_hnd__evv_index(_x20120, _ctx); /*std/core/hnd/ev-index*/
    kk_box_t _x20124;
    kk_string_t _x20125;
    kk_define_string_literal(, _s20126, 33, "unexpected Nil in std/core/foldl1")
    _x20125 = kk_string_dup(_s20126); /*string*/
    _x20124 = kk_string_box(_x20125); /*5549*/
    return kk_std_core_hnd__open_at2(_b_18396_18390, kk_std_core_new_foldr1_fun20121(_ctx), _x20124, kk_std_core_types__optional_box(kk_std_core_types__new_None(_ctx), _ctx), _ctx);
  }
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17175_op(kk_function_t action, kk_integer_t end, kk_integer_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : (int) -> e (), end : int, i : int, wild_ : ()) -> e () */ 
  kk_integer_t i0_16778 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__lift16748_for(action, end, i0_16778, _ctx); return kk_Unit;
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16748_for_fun20132__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_integer_t end0;
  kk_integer_t i0;
};
static kk_box_t kk_std_core__lift16748_for_fun20132(kk_function_t _fself, kk_box_t _b_18401, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16748_for_fun20132(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16748_for_fun20132__t* _self = kk_function_alloc_as(struct kk_std_core__lift16748_for_fun20132__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16748_for_fun20132, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16748_for_fun20132(kk_function_t _fself, kk_box_t _b_18401, kk_context_t* _ctx) {
  struct kk_std_core__lift16748_for_fun20132__t* _self = kk_function_as(struct kk_std_core__lift16748_for_fun20132__t*, _fself);
  kk_function_t action0 = _self->action0; /* (int) -> 12068 () */
  kk_integer_t end0 = _self->end0; /* int */
  kk_integer_t i0 = _self->i0; /* int */
  kk_drop_match(_self, {kk_function_dup(action0);kk_integer_dup(end0);kk_integer_dup(i0);}, {}, _ctx)
  kk_unit_t _x20133 = kk_Unit;
  kk_unit_t _x20134 = kk_Unit;
  kk_unit_unbox(_b_18401);
  kk_std_core__mlift17175_op(action0, end0, i0, _x20134, _ctx);
  return kk_unit_box(_x20133);
}

kk_unit_t kk_std_core__lift16748_for(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) { /* forall<e> (action : (int) -> e (), end : int, i : int) -> e () */ 
  kk__tailcall: ;
  bool _match_19065;
  kk_integer_t _x20127 = kk_integer_dup(i0); /*int*/
  kk_integer_t _x20128 = kk_integer_dup(end0); /*int*/
  _match_19065 = kk_integer_lte(_x20127,_x20128,kk_context()); /*bool*/
  if (_match_19065) {
    kk_unit_t x_17446 = kk_Unit;
    kk_function_t _x20130 = kk_function_dup(action0); /*(int) -> 12068 ()*/
    kk_integer_t _x20129 = kk_integer_dup(i0); /*int*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), _x20130, (_x20130, _x20129, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20131 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16748_for_fun20132(action0, end0, i0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20131); return kk_Unit;
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

kk_std_core_types__maybe kk_std_core__mlift17176_op(kk_function_t action, kk_integer_t end, kk_integer_t i, kk_std_core_types__maybe _y_17023, kk_context_t* _ctx) { /* forall<a,e> (action : (int) -> e maybe<a>, end : int, i : int, maybe<a>) -> e maybe<a> */ 
  if (kk_std_core_types__is_Nothing(_y_17023)) {
    kk_integer_t i0_16779 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
    return kk_std_core__lift16749_for_while(action, end, i0_16779, _ctx);
  }
  {
    kk_box_t x = _y_17023._cons.Just.value;
    kk_function_drop(action, _ctx);
    kk_integer_drop(end, _ctx);
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Just(x, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16749_for_while_fun20140__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_integer_t end0;
  kk_integer_t i0;
};
static kk_box_t kk_std_core__lift16749_for_while_fun20140(kk_function_t _fself, kk_box_t _b_18405, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16749_for_while_fun20140(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) {
  struct kk_std_core__lift16749_for_while_fun20140__t* _self = kk_function_alloc_as(struct kk_std_core__lift16749_for_while_fun20140__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16749_for_while_fun20140, kk_context());
  _self->action0 = action0;
  _self->end0 = end0;
  _self->i0 = i0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16749_for_while_fun20140(kk_function_t _fself, kk_box_t _b_18405, kk_context_t* _ctx) {
  struct kk_std_core__lift16749_for_while_fun20140__t* _self = kk_function_as(struct kk_std_core__lift16749_for_while_fun20140__t*, _fself);
  kk_function_t action0 = _self->action0; /* (int) -> 12119 maybe<12118> */
  kk_integer_t end0 = _self->end0; /* int */
  kk_integer_t i0 = _self->i0; /* int */
  kk_drop_match(_self, {kk_function_dup(action0);kk_integer_dup(end0);kk_integer_dup(i0);}, {}, _ctx)
  kk_std_core_types__maybe _x20141;
  kk_std_core_types__maybe _x20142 = kk_std_core_types__maybe_unbox(_b_18405, _ctx); /*maybe<12118>*/
  _x20141 = kk_std_core__mlift17176_op(action0, end0, i0, _x20142, _ctx); /*maybe<12118>*/
  return kk_std_core_types__maybe_box(_x20141, _ctx);
}

kk_std_core_types__maybe kk_std_core__lift16749_for_while(kk_function_t action0, kk_integer_t end0, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,e> (action : (int) -> e maybe<a>, end : int, i : int) -> e maybe<a> */ 
  kk__tailcall: ;
  bool _match_19063;
  kk_integer_t _x20135 = kk_integer_dup(i0); /*int*/
  kk_integer_t _x20136 = kk_integer_dup(end0); /*int*/
  _match_19063 = kk_integer_lte(_x20135,_x20136,kk_context()); /*bool*/
  if (_match_19063) {
    kk_std_core_types__maybe x0_17449;
    kk_function_t _x20138 = kk_function_dup(action0); /*(int) -> 12119 maybe<12118>*/
    kk_integer_t _x20137 = kk_integer_dup(i0); /*int*/
    x0_17449 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_integer_t, kk_context_t*), _x20138, (_x20138, _x20137, _ctx)); /*maybe<12118>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x0_17449, _ctx);
      kk_box_t _x20139 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16749_for_while_fun20140(action0, end0, i0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__maybe_unbox(_x20139, _ctx);
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
struct kk_std_core__mlift17178_foreach_indexed_fun20145__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_unit_t kk_std_core__mlift17178_foreach_indexed_fun20145(kk_function_t _fself, kk_integer_t _y_17031, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17178_foreach_indexed_fun20145(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_std_core__mlift17178_foreach_indexed_fun20145__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17178_foreach_indexed_fun20145__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17178_foreach_indexed_fun20145, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_unit_t kk_std_core__mlift17178_foreach_indexed_fun20145(kk_function_t _fself, kk_integer_t _y_17031, kk_context_t* _ctx) {
  struct kk_std_core__mlift17178_foreach_indexed_fun20145__t* _self = kk_function_as(struct kk_std_core__mlift17178_foreach_indexed_fun20145__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_integer_t _b_18417_18415 = kk_integer_add(_y_17031,(kk_integer_from_small(1)),kk_context()); /*int*/;
  return (kk_ref_set(i,(kk_integer_box(_b_18417_18415)),kk_context()));
}


// lift anonymous function
struct kk_std_core__mlift17178_foreach_indexed_fun20147__t {
  struct kk_function_s _base;
  kk_function_t next0_17453;
};
static kk_box_t kk_std_core__mlift17178_foreach_indexed_fun20147(kk_function_t _fself, kk_box_t _b_18419, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17178_foreach_indexed_fun20147(kk_function_t next0_17453, kk_context_t* _ctx) {
  struct kk_std_core__mlift17178_foreach_indexed_fun20147__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17178_foreach_indexed_fun20147__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17178_foreach_indexed_fun20147, kk_context());
  _self->next0_17453 = next0_17453;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17178_foreach_indexed_fun20147(kk_function_t _fself, kk_box_t _b_18419, kk_context_t* _ctx) {
  struct kk_std_core__mlift17178_foreach_indexed_fun20147__t* _self = kk_function_as(struct kk_std_core__mlift17178_foreach_indexed_fun20147__t*, _fself);
  kk_function_t next0_17453 = _self->next0_17453; /* (int) -> <local<12223>|12230> () */
  kk_drop_match(_self, {kk_function_dup(next0_17453);}, {}, _ctx)
  kk_unit_t _x20148 = kk_Unit;
  kk_integer_t _x20149 = kk_integer_unbox(_b_18419); /*int*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17453, (next0_17453, _x20149, _ctx));
  return kk_unit_box(_x20148);
}

kk_unit_t kk_std_core__mlift17178_foreach_indexed(kk_ref_t i, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<h,e> (i : local-var<h,int>, wild_ : ()) -> <local<h>|e> () */ 
  kk_integer_t x_17452;
  kk_box_t _x20143;
  kk_ref_t _x20144 = kk_ref_dup(i); /*local-var<12223,int>*/
  _x20143 = (kk_ref_get(_x20144,kk_context())); /*233*/
  x_17452 = kk_integer_unbox(_x20143); /*int*/
  kk_function_t next0_17453 = kk_std_core__new_mlift17178_foreach_indexed_fun20145(i, _ctx); /*(int) -> <local<12223>|12230> ()*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_17452, _ctx);
    kk_box_t _x20146 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17178_foreach_indexed_fun20147(next0_17453, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20146); return kk_Unit;
  }
  {
    kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17453, (next0_17453, x_17452, _ctx)); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17179_foreach_indexed_fun20151__t {
  struct kk_function_s _base;
  kk_ref_t i;
};
static kk_box_t kk_std_core__mlift17179_foreach_indexed_fun20151(kk_function_t _fself, kk_box_t _b_18422, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17179_foreach_indexed_fun20151(kk_ref_t i, kk_context_t* _ctx) {
  struct kk_std_core__mlift17179_foreach_indexed_fun20151__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17179_foreach_indexed_fun20151__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17179_foreach_indexed_fun20151, kk_context());
  _self->i = i;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17179_foreach_indexed_fun20151(kk_function_t _fself, kk_box_t _b_18422, kk_context_t* _ctx) {
  struct kk_std_core__mlift17179_foreach_indexed_fun20151__t* _self = kk_function_as(struct kk_std_core__mlift17179_foreach_indexed_fun20151__t*, _fself);
  kk_ref_t i = _self->i; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_ref_dup(i);}, {}, _ctx)
  kk_unit_t _x20152 = kk_Unit;
  kk_unit_t _x20153 = kk_Unit;
  kk_unit_unbox(_b_18422);
  kk_std_core__mlift17178_foreach_indexed(i, _x20153, _ctx);
  return kk_unit_box(_x20152);
}

kk_unit_t kk_std_core__mlift17179_foreach_indexed(kk_function_t action, kk_ref_t i, kk_box_t x, kk_integer_t j, kk_context_t* _ctx) { /* forall<h,a,e> (action : (int, a) -> e (), i : local-var<h,int>, x : a, j : int) -> <local<h>|e> () */ 
  kk_unit_t x0_17456 = kk_Unit;
  kk_function_call(kk_unit_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), action, (action, j, x, _ctx));
  if (kk_yielding(kk_context())) {
    kk_box_t _x20150 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17179_foreach_indexed_fun20151(i, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20150); return kk_Unit;
  }
  {
    kk_std_core__mlift17178_foreach_indexed(i, x0_17456, _ctx); return kk_Unit;
  }
}
 
// Invoke `action` for each element of a list, passing also the position of the element.


// lift anonymous function
struct kk_std_core_foreach_indexed_fun20155__t {
  struct kk_function_s _base;
  kk_function_t action;
  kk_ref_t loc;
};
static kk_unit_t kk_std_core_foreach_indexed_fun20155(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20155(kk_function_t action, kk_ref_t loc, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20155__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20155__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20155, kk_context());
  _self->action = action;
  _self->loc = loc;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_foreach_indexed_fun20159__t {
  struct kk_function_s _base;
  kk_function_t action;
  kk_ref_t loc;
  kk_box_t x;
};
static kk_box_t kk_std_core_foreach_indexed_fun20159(kk_function_t _fself, kk_box_t _b_18430, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20159(kk_function_t action, kk_ref_t loc, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20159__t* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20159__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20159, kk_context());
  _self->action = action;
  _self->loc = loc;
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_core_foreach_indexed_fun20159(kk_function_t _fself, kk_box_t _b_18430, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20159__t* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20159__t*, _fself);
  kk_function_t action = _self->action; /* (int, 12229) -> 12230 () */
  kk_ref_t loc = _self->loc; /* local-var<12223,int> */
  kk_box_t x = _self->x; /* 12229 */
  kk_drop_match(_self, {kk_function_dup(action);kk_ref_dup(loc);kk_box_dup(x);}, {}, _ctx)
  kk_unit_t _x20160 = kk_Unit;
  kk_integer_t _x20161 = kk_integer_unbox(_b_18430); /*int*/
  kk_std_core__mlift17179_foreach_indexed(action, loc, x, _x20161, _ctx);
  return kk_unit_box(_x20160);
}
static kk_unit_t kk_std_core_foreach_indexed_fun20155(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20155__t* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20155__t*, _fself);
  kk_function_t action = _self->action; /* (int, 12229) -> 12230 () */
  kk_ref_t loc = _self->loc; /* local-var<12223,int> */
  kk_drop_match(_self, {kk_function_dup(action);kk_ref_dup(loc);}, {}, _ctx)
  kk_integer_t x0_17461;
  kk_box_t _x20156;
  kk_ref_t _x20157 = kk_ref_dup(loc); /*local-var<12223,int>*/
  _x20156 = (kk_ref_get(_x20157,kk_context())); /*233*/
  x0_17461 = kk_integer_unbox(_x20156); /*int*/
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x0_17461, _ctx);
    kk_box_t _x20158 = kk_std_core_hnd_yield_extend(kk_std_core_new_foreach_indexed_fun20159(action, loc, x, _ctx), _ctx); /*3860*/
    return kk_unit_unbox(_x20158);
  }
  {
    return kk_std_core__mlift17179_foreach_indexed(action, loc, x, x0_17461, _ctx);
  }
}

kk_unit_t kk_std_core_foreach_indexed(kk_std_core__list xs, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, action : (int, a) -> e ()) -> e () */ 
  kk_ref_t loc = kk_ref_alloc((kk_integer_box(kk_integer_from_small(0))),kk_context()); /*local-var<12223,int>*/;
  kk_unit_t res = kk_Unit;
  kk_function_t _x20154;
  kk_ref_dup(loc);
  _x20154 = kk_std_core_new_foreach_indexed_fun20155(action, loc, _ctx); /*(x : 12229) -> <local<12223>|12230> ()*/
  kk_std_core_foreach(xs, _x20154, _ctx);
  kk_box_t _x20162 = kk_std_core_hnd_prompt_local_var(loc, kk_unit_box(res), _ctx); /*9897*/
  kk_unit_unbox(_x20162); return kk_Unit;
}
 
// Invoke a function `f` for each element in a vector `v`


// lift anonymous function
struct kk_std_core_foreach_indexed_fun20165__t_1 {
  struct kk_function_s _base;
  kk_function_t f;
  kk_vector_t v;
};
static kk_unit_t kk_std_core_foreach_indexed_fun20165_1(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_foreach_indexed_fun20165_1(kk_function_t f, kk_vector_t v, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20165__t_1* _self = kk_function_alloc_as(struct kk_std_core_foreach_indexed_fun20165__t_1, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_foreach_indexed_fun20165_1, kk_context());
  _self->f = f;
  _self->v = v;
  return &_self->_base;
}

static kk_unit_t kk_std_core_foreach_indexed_fun20165_1(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_foreach_indexed_fun20165__t_1* _self = kk_function_as(struct kk_std_core_foreach_indexed_fun20165__t_1*, _fself);
  kk_function_t f = _self->f; /* (12259, int) -> 12260 () */
  kk_vector_t v = _self->v; /* vector<12259> */
  kk_drop_match(_self, {kk_function_dup(f);kk_vector_dup(v);}, {}, _ctx)
  kk_box_t x_17597 = kk_vector_at(v,i,kk_context()); /*12259*/;
  kk_integer_t _x20166 = kk_integer_from_ssize_t(i,kk_context()); /*int*/
  return kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_integer_t, kk_context_t*), f, (f, x_17597, _x20166, _ctx));
}

kk_unit_t kk_std_core_foreach_indexed_1(kk_vector_t v, kk_function_t f, kk_context_t* _ctx) { /* forall<a,e> (v : vector<a>, f : (a, int) -> e ()) -> e () */ 
  kk_ssize_t start0_17465 = ((kk_ssize_t)0); /*ssize_t*/;
  kk_ssize_t end_17466;
  kk_ssize_t _x20163;
  kk_vector_t _x20164 = kk_vector_dup(v); /*vector<12259>*/
  _x20163 = kk_vector_len(_x20164,kk_context()); /*ssize_t*/
  end_17466 = kk_std_core_decr_1(_x20163, _ctx); /*ssize_t*/
  kk_std_core__lift16739_forz(kk_std_core_new_foreach_indexed_fun20165_1(f, v, _ctx), end_17466, start0_17465, _ctx); return kk_Unit;
}
 
// Return the head of list if the list is not empty.

kk_std_core_types__maybe kk_std_core_head_1(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20175 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20175->head;
    kk_std_core__list _pat0 = _con20175->tail;
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
    struct kk_std_core_Cons* _con20176 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20176->head;
    kk_std_core__list _pat0 = _con20176->tail;
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
  kk_std_core__sslice _x20177;
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_19057;
  kk_integer_t _x20178;
  kk_std_core_types__optional _match_19059 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19059)) {
    kk_box_t _box_x18437 = _match_19059._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18437);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_19059, _ctx);
    _x20178 = _n_9710; /*int*/
    goto _match20179;
  }
  {
    _x20178 = kk_integer_from_small(1); /*int*/
  }
  _match20179: ;
  _match_19057 = kk_integer_eq(_x20178,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19057) {
    _x20177 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x20181;
    kk_integer_t _x20182;
    kk_std_core_types__optional _match_19058 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19058)) {
      kk_box_t _box_x18438 = _match_19058._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18438);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_19058, _ctx);
      _x20182 = _n_97100; /*int*/
      goto _match20183;
    }
    {
      _x20182 = kk_integer_from_small(1); /*int*/
    }
    _match20183: ;
    _x20181 = kk_integer_sub(_x20182,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x20177 = kk_std_core_extend(slice0, _x20181, _ctx); /*sslice*/
  }
  return kk_std_core_string_3(_x20177, _ctx);
}
 
// Return the first character of a string (or `Nothing` for the empty string).


// lift anonymous function
struct kk_std_core_head_char_fun20189__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_core_head_char_fun20189(kk_function_t _fself, kk_char_t _b_18439, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_head_char_fun20189(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_head_char_fun20189, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_core_head_char_fun20189(kk_function_t _fself, kk_char_t _b_18439, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_types__new_Just(kk_char_box(_b_18439, _ctx), _ctx);
}

kk_std_core_types__maybe kk_std_core_head_char(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> maybe<char> */ 
  kk_std_core__sslice _x20185;
  kk_string_t _x20186 = kk_string_dup(s); /*string*/
  kk_ssize_t _x20187 = ((kk_ssize_t)0); /*ssize_t*/
  kk_ssize_t _x20188 = kk_string_len(s,kk_context()); /*ssize_t*/
  _x20185 = kk_std_core__new_Sslice(_x20186, _x20187, _x20188, _ctx); /*sslice*/
  return kk_std_core_foreach_while_1(_x20185, kk_std_core_new_head_char_fun20189(_ctx), _ctx);
}

kk_integer_t kk_std_core_index_of_acc(kk_std_core__list xs, kk_function_t pred, kk_integer_t idx, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, pred : (a) -> bool, idx : int) -> int */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_integer_drop(idx, _ctx);
    kk_function_drop(pred, _ctx);
    return kk_integer_sub((kk_integer_from_small(0)),(kk_integer_from_small(1)),kk_context());
  }
  {
    struct kk_std_core_Cons* _con20190 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20190->head;
    kk_std_core__list xx = _con20190->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    bool _match_19056;
    kk_function_t _x20191 = kk_function_dup(pred); /*(12636) -> bool*/
    _match_19056 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20191, (_x20191, x, _ctx)); /*bool*/
    if (_match_19056) {
      kk_function_drop(pred, _ctx);
      kk_std_core__list_drop(xx, _ctx);
      return idx;
    }
    { // tailcall
      kk_integer_t _x20192 = kk_integer_add(idx,(kk_integer_from_small(1)),kk_context()); /*int*/
      xs = xx;
      idx = _x20192;
      goto kk__tailcall;
    }
  }
}
 
// Return the list without its last element.
// Return an empty list for an empty list.

kk_std_core__list kk_std_core__ctail_init(kk_std_core__list xs, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20193 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20193->head;
    kk_std_core__list xx = _con20193->tail;
    if (kk_std_core__is_Cons(xx)) {
      struct kk_std_core_Cons* _con20194 = kk_std_core__as_Cons(xx);
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_reuse_t _ru_18939;
      kk_std_core__list _x20195 = kk_std_core__list_dup(xx); /*list<12673>*/
      _ru_18939 = kk_std_core__list_dropn_reuse(_x20195, ((int32_t)KI32(2)), _ctx); /*reuse*/
      kk_std_core__list _ctail_16825 = kk_std_core__list_hole(); /*list<12673>*/;
      kk_std_core__list _ctail_16826 = kk_std_core__new_Cons(_ru_18939, x, _ctail_16825, _ctx); /*list<12673>*/;
      { // tailcall
        kk_std_core_types__ctail _x20196;
        kk_box_t* _b_18450_18445 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16826)->tail)); /*cfield<list<12673>>*/;
        _x20196 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16826, _ctx)),_b_18450_18445); /*ctail<0>*/
        xs = xx;
        _acc = _x20196;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_std_core__list_drop(xs, _ctx);
    kk_box_t _x20197 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20197, _ctx);
  }
}
 
// Return the list without its last element.
// Return an empty list for an empty list.

kk_std_core__list kk_std_core_init(kk_std_core__list xs0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> list<a> */ 
  kk_std_core_types__ctail _x20198 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_init(xs0, _x20198, _ctx);
}
 
// An invalid slice

kk_std_core__sslice kk_std_core_invalid;
 
// Is the character an ASCII letter is-

bool kk_std_core_is_alpha(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> bool */ 
  bool _match_19050 = (c >= ('a')); /*bool*/;
  if (_match_19050) {
    bool _match_19052 = (c <= ('z')); /*bool*/;
    if (_match_19052) {
      return true;
    }
    {
      bool _match_19053 = (c >= ('A')); /*bool*/;
      if (_match_19053) {
        return (c <= ('Z'));
      }
      {
        return false;
      }
    }
  }
  {
    bool _match_19051 = (c >= ('A')); /*bool*/;
    if (_match_19051) {
      return (c <= ('Z'));
    }
    {
      return false;
    }
  }
}
 
// Is the character an ASCII hexa-decimal digit ?

bool kk_std_core_is_hex_digit(kk_char_t c, kk_context_t* _ctx) { /* (c : char) -> bool */ 
  bool _match_19037 = (c >= ('0')); /*bool*/;
  if (_match_19037) {
    bool _match_19042 = (c <= ('9')); /*bool*/;
    if (_match_19042) {
      return true;
    }
    {
      bool _match_19043 = (c >= ('a')); /*bool*/;
      if (_match_19043) {
        bool _match_19045 = (c <= ('f')); /*bool*/;
        if (_match_19045) {
          return true;
        }
        {
          bool _match_19046 = (c >= ('A')); /*bool*/;
          if (_match_19046) {
            return (c <= ('F'));
          }
          {
            return false;
          }
        }
      }
      {
        bool _match_19044 = (c >= ('A')); /*bool*/;
        if (_match_19044) {
          return (c <= ('F'));
        }
        {
          return false;
        }
      }
    }
  }
  {
    bool _match_19038 = (c >= ('a')); /*bool*/;
    if (_match_19038) {
      bool _match_19040 = (c <= ('f')); /*bool*/;
      if (_match_19040) {
        return true;
      }
      {
        bool _match_19041 = (c >= ('A')); /*bool*/;
        if (_match_19041) {
          return (c <= ('F'));
        }
        {
          return false;
        }
      }
    }
    {
      bool _match_19039 = (c >= ('A')); /*bool*/;
      if (_match_19039) {
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
  bool _match_19034 = (c == (' ')); /*bool*/;
  if (_match_19034) {
    return true;
  }
  {
    bool _match_19035 = (c == 0x0009); /*bool*/;
    if (_match_19035) {
      return true;
    }
    {
      bool _match_19036 = (c == 0x000A); /*bool*/;
      if (_match_19036) {
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
    kk_string_t _x20209;
    if (kk_std_core__is_Nil(xs)) {
      _x20209 = kk_string_empty(); /*string*/
    }
    else {
      struct kk_std_core_Cons* _con20211 = kk_std_core__as_Cons(xs);
      kk_box_t _box_x18456 = _con20211->head;
      kk_std_core__list xx = _con20211->tail;
      kk_string_t x = kk_string_unbox(_box_x18456);
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        kk_std_core__list_free(xs);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_string_t _x20213 = kk_string_dup(end); /*string*/
      _x20209 = kk_std_core__lift16737_joinsep(_x20213, xx, x, _ctx); /*string*/
    }
    return kk_std_core__lp__plus__plus__1_rp_(_x20209, end, _ctx);
  }
}
 
// Return the last element of a list (or `Nothing` for the empty list)

kk_std_core_types__maybe kk_std_core_last(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20214 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20214->head;
    kk_std_core__list _pat0 = _con20214->tail;
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
    struct kk_std_core_Cons* _con20215 = kk_std_core__as_Cons(xs);
    kk_box_t _pat2 = _con20215->head;
    kk_std_core__list xx = _con20215->tail;
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
    struct kk_std_core_Cons* _con20216 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20216->head;
    kk_std_core__list _pat0 = _con20216->tail;
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
    struct kk_std_core_Cons* _con20217 = kk_std_core__as_Cons(xs);
    kk_box_t _pat2 = _con20217->head;
    kk_std_core__list xx = _con20217->tail;
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
  bool _match_19033;
  kk_integer_t _x20218;
  if (kk_std_core_types__is_Optional(n)) {
    kk_box_t _box_x18457 = n._cons.Optional.value;
    kk_integer_t _n_13267 = kk_integer_unbox(_box_x18457);
    kk_integer_dup(_n_13267);
    _x20218 = _n_13267; /*int*/
    goto _match20219;
  }
  {
    _x20218 = kk_integer_from_small(1); /*int*/
  }
  _match20219: ;
  _match_19033 = kk_integer_eq(_x20218,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19033) {
    kk_std_core_types__optional_drop(n, _ctx);
    return slice0;
  }
  {
    kk_std_core__sslice _x20221;
    kk_integer_t _x20222;
    kk_integer_t _x20223;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18458 = n._cons.Optional.value;
      kk_integer_t _n_132670 = kk_integer_unbox(_box_x18458);
      kk_integer_dup(_n_132670);
      _x20223 = _n_132670; /*int*/
      goto _match20224;
    }
    {
      _x20223 = kk_integer_from_small(1); /*int*/
    }
    _match20224: ;
    _x20222 = kk_integer_sub((kk_integer_from_small(1)),_x20223,kk_context()); /*int*/
    _x20221 = kk_std_core_advance(slice0, _x20222, _ctx); /*sslice*/
    kk_integer_t _x20226;
    kk_integer_t _x20227;
    if (kk_std_core_types__is_Optional(n)) {
      kk_box_t _box_x18459 = n._cons.Optional.value;
      kk_integer_t _n_132671 = kk_integer_unbox(_box_x18459);
      kk_integer_dup(_n_132671);
      kk_std_core_types__optional_drop(n, _ctx);
      _x20227 = _n_132671; /*int*/
      goto _match20228;
    }
    {
      _x20227 = kk_integer_from_small(1); /*int*/
    }
    _match20228: ;
    _x20226 = kk_integer_sub(_x20227,(kk_integer_from_small(1)),kk_context()); /*int*/
    return kk_std_core_extend(_x20221, _x20226, _ctx);
  }
}
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)

kk_std_core__list kk_std_core__ctail_take(kk_std_core__list xs, kk_integer_t n, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20230 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20230->head;
    kk_std_core__list xx = _con20230->tail;
    kk_integer_t _x20231 = kk_integer_dup(n); /*int*/
    if (kk_integer_gt(_x20231,(kk_integer_from_small(0)),kk_context())) {
      kk_reuse_t _ru_18945 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        _ru_18945 = (kk_std_core__list_reuse(xs));
      }
      else {
        kk_box_dup(x);
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(xs, _ctx);
        _ru_18945 = kk_reuse_null;
      }
      kk_std_core__list _ctail_16827 = kk_std_core__list_hole(); /*list<13391>*/;
      kk_std_core__list _ctail_16828;
      if (kk_likely(_ru_18945!=NULL)) {
        struct kk_std_core_Cons* _con20232 = (struct kk_std_core_Cons*)_ru_18945;
        _con20232->tail = _ctail_16827;
        _ctail_16828 = kk_std_core__base_Cons(_con20232); /*list<13391>*/
      }
      else {
        _ctail_16828 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16827, _ctx); /*list<13391>*/
      }
      { // tailcall
        kk_integer_t _x20233 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
        kk_std_core_types__ctail _x20234;
        kk_box_t* _b_18470_18465 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16828)->tail)); /*cfield<list<13391>>*/;
        _x20234 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16828, _ctx)),_b_18470_18465); /*ctail<0>*/
        xs = xx;
        n = _x20233;
        _acc = _x20234;
        goto kk__tailcall;
      }
    }
  }
  {
    kk_integer_drop(n, _ctx);
    kk_std_core__list_drop(xs, _ctx);
    kk_box_t _x20235 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20235, _ctx);
  }
}
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)

kk_std_core__list kk_std_core_take(kk_std_core__list xs0, kk_integer_t n0, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> list<a> */ 
  kk_std_core_types__ctail _x20236 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_take(xs0, n0, _x20236, _ctx);
}
 
// split a list at position `n`

kk_std_core_types__tuple2_ kk_std_core_split(kk_std_core__list xs, kk_integer_t n, kk_context_t* _ctx) { /* forall<a> (xs : list<a>, n : int) -> (list<a>, list<a>) */ 
  kk_std_core__list _b_18478_18476;
  kk_std_core__list _x20237 = kk_std_core__list_dup(xs); /*list<13411>*/
  kk_integer_t _x20238 = kk_integer_dup(n); /*int*/
  _b_18478_18476 = kk_std_core_take(_x20237, _x20238, _ctx); /*list<13411>*/
  kk_std_core__list _b_18479_18477 = kk_std_core_drop(xs, n, _ctx); /*list<13411>*/;
  return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18478_18476, _ctx), kk_std_core__list_box(_b_18479_18477, _ctx), _ctx);
}
 
// Lookup the first element satisfying some predicate


// lift anonymous function
struct kk_std_core_lookup_fun20242__t {
  struct kk_function_s _base;
  kk_function_t pred;
};
static kk_std_core_types__maybe kk_std_core_lookup_fun20242(kk_function_t _fself, kk_box_t _b_18482, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_lookup_fun20242(kk_function_t pred, kk_context_t* _ctx) {
  struct kk_std_core_lookup_fun20242__t* _self = kk_function_alloc_as(struct kk_std_core_lookup_fun20242__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_lookup_fun20242, kk_context());
  _self->pred = pred;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_core_lookup_fun20242(kk_function_t _fself, kk_box_t _b_18482, kk_context_t* _ctx) {
  struct kk_std_core_lookup_fun20242__t* _self = kk_function_as(struct kk_std_core_lookup_fun20242__t*, _fself);
  kk_function_t pred = _self->pred; /* (13725) -> bool */
  kk_drop_match(_self, {kk_function_dup(pred);}, {}, _ctx)
  bool _match_19030;
  kk_box_t _x20243;
  kk_std_core_types__tuple2_ _match_19032;
  kk_box_t _x20244 = kk_box_dup(_b_18482); /*10243*/
  _match_19032 = kk_std_core_types__tuple2__unbox(_x20244, _ctx); /*(13725, 13726)*/
  {
    kk_box_t _x = _match_19032.fst;
    kk_box_dup(_x);
    kk_std_core_types__tuple2__drop(_match_19032, _ctx);
    _x20243 = _x; /*13725*/
  }
  _match_19030 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), pred, (pred, _x20243, _ctx)); /*bool*/
  if (_match_19030) {
    kk_box_t _x20245;
    kk_std_core_types__tuple2_ _match_19031 = kk_std_core_types__tuple2__unbox(_b_18482, _ctx); /*(13725, 13726)*/;
    {
      kk_box_t _x0 = _match_19031.snd;
      kk_box_dup(_x0);
      kk_std_core_types__tuple2__drop(_match_19031, _ctx);
      _x20245 = _x0; /*13726*/
    }
    return kk_std_core_types__new_Just(_x20245, _ctx);
  }
  {
    kk_box_drop(_b_18482, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_core_lookup(kk_std_core__list xs, kk_function_t pred, kk_context_t* _ctx) { /* forall<a,b> (xs : list<(a, b)>, pred : (a) -> bool) -> maybe<b> */ 
  return kk_std_core_foreach_while(xs, kk_std_core_new_lookup_fun20242(pred, _ctx), _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17180_op(kk_box_t _y_17038, kk_std_core__list _y_17039, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17038, _y_17039, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17181_op_fun20247__t {
  struct kk_function_s _base;
  kk_box_t _y_170380;
};
static kk_box_t kk_std_core__mlift17181_op_fun20247(kk_function_t _fself, kk_box_t _b_18487, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17181_op_fun20247(kk_box_t _y_170380, kk_context_t* _ctx) {
  struct kk_std_core__mlift17181_op_fun20247__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17181_op_fun20247__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17181_op_fun20247, kk_context());
  _self->_y_170380 = _y_170380;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17181_op_fun20247(kk_function_t _fself, kk_box_t _b_18487, kk_context_t* _ctx) {
  struct kk_std_core__mlift17181_op_fun20247__t* _self = kk_function_as(struct kk_std_core__mlift17181_op_fun20247__t*, _fself);
  kk_box_t _y_170380 = _self->_y_170380; /* 13781 */
  kk_drop_match(_self, {kk_box_dup(_y_170380);}, {}, _ctx)
  kk_std_core__list _x20248;
  kk_std_core__list _x20249 = kk_std_core__list_unbox(_b_18487, _ctx); /*list<13781>*/
  _x20248 = kk_std_core__mlift17180_op(_y_170380, _x20249, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20248, _ctx);
}

kk_std_core__list kk_std_core__mlift17181_op(kk_function_t f, kk_integer_t i, kk_std_core__list yy, kk_box_t _y_170380, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a) -> e b, i : int, yy : list<a>, b) -> e list<b> */ 
  kk_integer_t i0_16781 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17470 = kk_std_core__lift16750_map_indexed(f, yy, i0_16781, _ctx); /*list<13781>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17470, _ctx);
    kk_box_t _x20246 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17181_op_fun20247(_y_170380, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20246, _ctx);
  }
  {
    return kk_std_core__mlift17180_op(_y_170380, x_17470, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16750_map_indexed_fun20254__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16750_map_indexed_fun20254(kk_function_t _fself, kk_box_t _b_18491, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16750_map_indexed_fun20254(kk_function_t f0, kk_integer_t i0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20254__t* _self = kk_function_alloc_as(struct kk_std_core__lift16750_map_indexed_fun20254__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16750_map_indexed_fun20254, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16750_map_indexed_fun20254(kk_function_t _fself, kk_box_t _b_18491, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20254__t* _self = kk_function_as(struct kk_std_core__lift16750_map_indexed_fun20254__t*, _fself);
  kk_function_t f0 = _self->f0; /* (idx : int, value : 13780) -> 13782 13781 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list yy0 = _self->yy0; /* list<13780> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20255 = kk_std_core__mlift17181_op(f0, i0, yy0, _b_18491, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20255, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16750_map_indexed_fun20257__t {
  struct kk_function_s _base;
  kk_box_t x0_17472;
};
static kk_box_t kk_std_core__lift16750_map_indexed_fun20257(kk_function_t _fself, kk_box_t _b_18493, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16750_map_indexed_fun20257(kk_box_t x0_17472, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20257__t* _self = kk_function_alloc_as(struct kk_std_core__lift16750_map_indexed_fun20257__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16750_map_indexed_fun20257, kk_context());
  _self->x0_17472 = x0_17472;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16750_map_indexed_fun20257(kk_function_t _fself, kk_box_t _b_18493, kk_context_t* _ctx) {
  struct kk_std_core__lift16750_map_indexed_fun20257__t* _self = kk_function_as(struct kk_std_core__lift16750_map_indexed_fun20257__t*, _fself);
  kk_box_t x0_17472 = _self->x0_17472; /* 13781 */
  kk_drop_match(_self, {kk_box_dup(x0_17472);}, {}, _ctx)
  kk_std_core__list _x20258;
  kk_std_core__list _x20259 = kk_std_core__list_unbox(_b_18493, _ctx); /*list<13781>*/
  _x20258 = kk_std_core__mlift17180_op(x0_17472, _x20259, _ctx); /*list<13781>*/
  return kk_std_core__list_box(_x20258, _ctx);
}

kk_std_core__list kk_std_core__lift16750_map_indexed(kk_function_t f0, kk_std_core__list ys, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a) -> e b, ys : list<a>, i : int) -> e list<b> */ 
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20250 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con20250->head;
    kk_std_core__list yy0 = _con20250->tail;
    kk_reuse_t _ru_18946 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18946 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18946 = kk_reuse_null;
    }
    kk_box_t x0_17472;
    kk_function_t _x20252 = kk_function_dup(f0); /*(idx : int, value : 13780) -> 13782 13781*/
    kk_integer_t _x20251 = kk_integer_dup(i0); /*int*/
    x0_17472 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_context_t*), _x20252, (_x20252, _x20251, y, _ctx)); /*13781*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18946, _ctx);
      kk_box_drop(x0_17472, _ctx);
      kk_box_t _x20253 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16750_map_indexed_fun20254(f0, i0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20253, _ctx);
    }
    {
      kk_integer_t i0_167810 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_std_core__list x1_17475 = kk_std_core__lift16750_map_indexed(f0, yy0, i0_167810, _ctx); /*list<13781>*/;
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18946, _ctx);
        kk_std_core__list_drop(x1_17475, _ctx);
        kk_box_t _x20256 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16750_map_indexed_fun20257(x0_17472, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20256, _ctx);
      }
      {
        return kk_std_core__new_Cons(_ru_18946, x0_17472, x1_17475, _ctx);
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

kk_std_core__list kk_std_core__mlift17182_op(kk_box_t _y_17042, kk_std_core__list _y_17043, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17042, _y_17043, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17183_op_fun20261__t {
  struct kk_function_s _base;
  kk_box_t _y_170420;
};
static kk_box_t kk_std_core__mlift17183_op_fun20261(kk_function_t _fself, kk_box_t _b_18499, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17183_op_fun20261(kk_box_t _y_170420, kk_context_t* _ctx) {
  struct kk_std_core__mlift17183_op_fun20261__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17183_op_fun20261__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17183_op_fun20261, kk_context());
  _self->_y_170420 = _y_170420;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17183_op_fun20261(kk_function_t _fself, kk_box_t _b_18499, kk_context_t* _ctx) {
  struct kk_std_core__mlift17183_op_fun20261__t* _self = kk_function_as(struct kk_std_core__mlift17183_op_fun20261__t*, _fself);
  kk_box_t _y_170420 = _self->_y_170420; /* 13838 */
  kk_drop_match(_self, {kk_box_dup(_y_170420);}, {}, _ctx)
  kk_std_core__list _x20262;
  kk_std_core__list _x20263 = kk_std_core__list_unbox(_b_18499, _ctx); /*list<13838>*/
  _x20262 = kk_std_core__mlift17182_op(_y_170420, _x20263, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20262, _ctx);
}

kk_std_core__list kk_std_core__mlift17183_op(kk_function_t f, kk_integer_t i, kk_std_core__list yy, kk_box_t _y_170420, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, i : int, yy : list<a>, b) -> e list<b> */ 
  kk_integer_t i0_16783 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17478 = kk_std_core__lift16751_map_indexed_peek(f, yy, i0_16783, _ctx); /*list<13838>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17478, _ctx);
    kk_box_t _x20260 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17183_op_fun20261(_y_170420, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20260, _ctx);
  }
  {
    return kk_std_core__mlift17182_op(_y_170420, x_17478, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16751_map_indexed_peek_fun20269__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20269(kk_function_t _fself, kk_box_t _b_18503, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16751_map_indexed_peek_fun20269(kk_function_t f0, kk_integer_t i0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20269__t* _self = kk_function_alloc_as(struct kk_std_core__lift16751_map_indexed_peek_fun20269__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16751_map_indexed_peek_fun20269, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20269(kk_function_t _fself, kk_box_t _b_18503, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20269__t* _self = kk_function_as(struct kk_std_core__lift16751_map_indexed_peek_fun20269__t*, _fself);
  kk_function_t f0 = _self->f0; /* (idx : int, value : 13837, rest : list<13837>) -> 13839 13838 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list yy0 = _self->yy0; /* list<13837> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20270 = kk_std_core__mlift17183_op(f0, i0, yy0, _b_18503, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20270, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16751_map_indexed_peek_fun20272__t {
  struct kk_function_s _base;
  kk_box_t x0_17480;
};
static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20272(kk_function_t _fself, kk_box_t _b_18505, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16751_map_indexed_peek_fun20272(kk_box_t x0_17480, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20272__t* _self = kk_function_alloc_as(struct kk_std_core__lift16751_map_indexed_peek_fun20272__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16751_map_indexed_peek_fun20272, kk_context());
  _self->x0_17480 = x0_17480;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16751_map_indexed_peek_fun20272(kk_function_t _fself, kk_box_t _b_18505, kk_context_t* _ctx) {
  struct kk_std_core__lift16751_map_indexed_peek_fun20272__t* _self = kk_function_as(struct kk_std_core__lift16751_map_indexed_peek_fun20272__t*, _fself);
  kk_box_t x0_17480 = _self->x0_17480; /* 13838 */
  kk_drop_match(_self, {kk_box_dup(x0_17480);}, {}, _ctx)
  kk_std_core__list _x20273;
  kk_std_core__list _x20274 = kk_std_core__list_unbox(_b_18505, _ctx); /*list<13838>*/
  _x20273 = kk_std_core__mlift17182_op(x0_17480, _x20274, _ctx); /*list<13838>*/
  return kk_std_core__list_box(_x20273, _ctx);
}

kk_std_core__list kk_std_core__lift16751_map_indexed_peek(kk_function_t f0, kk_std_core__list ys, kk_integer_t i0, kk_context_t* _ctx) { /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, ys : list<a>, i : int) -> e list<b> */ 
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20264 = kk_std_core__as_Cons(ys);
    kk_box_t y = _con20264->head;
    kk_std_core__list yy0 = _con20264->tail;
    kk_reuse_t _ru_18947 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18947 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(y);
      kk_std_core__list_dup(yy0);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18947 = kk_reuse_null;
    }
    kk_box_t x0_17480;
    kk_function_t _x20267 = kk_function_dup(f0); /*(idx : int, value : 13837, rest : list<13837>) -> 13839 13838*/
    kk_integer_t _x20265 = kk_integer_dup(i0); /*int*/
    kk_std_core__list _x20266 = kk_std_core__list_dup(yy0); /*list<13837>*/
    x0_17480 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_std_core__list, kk_context_t*), _x20267, (_x20267, _x20265, y, _x20266, _ctx)); /*13838*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18947, _ctx);
      kk_box_drop(x0_17480, _ctx);
      kk_box_t _x20268 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16751_map_indexed_peek_fun20269(f0, i0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20268, _ctx);
    }
    {
      kk_integer_t i0_167830 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
      kk_std_core__list x1_17483 = kk_std_core__lift16751_map_indexed_peek(f0, yy0, i0_167830, _ctx); /*list<13838>*/;
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18947, _ctx);
        kk_std_core__list_drop(x1_17483, _ctx);
        kk_box_t _x20271 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16751_map_indexed_peek_fun20272(x0_17480, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20271, _ctx);
      }
      {
        return kk_std_core__new_Cons(_ru_18947, x0_17480, x1_17483, _ctx);
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

kk_std_core__list kk_std_core__mlift17184_op(kk_std_core_types__ctail _acc, kk_function_t action, kk_std_core__list xx, kk_std_core_types__maybe _y_17046, kk_context_t* _ctx) { /* forall<a,b,e> (ctail<list<b>>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17046)) {
    kk_box_t y = _y_17046._cons.Just.value;
    kk_std_core__list _ctail_16829 = kk_std_core__list_hole(); /*list<13880>*/;
    kk_std_core__list _ctail_16830 = kk_std_core__new_Cons(kk_reuse_null, y, _ctail_16829, _ctx); /*list<13880>*/;
    kk_std_core_types__ctail _x20275;
    kk_box_t* _b_18520_18515 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16830)->tail)); /*cfield<list<13880>>*/;
    _x20275 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16830, _ctx)),_b_18520_18515); /*ctail<0>*/
    return kk_std_core__ctail_map_while(xx, action, _x20275, _ctx);
  }
  {
    kk_function_drop(action, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    kk_box_t _x20276 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20276, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17185_op_fun20277__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t y0;
};
static kk_std_core__list kk_std_core__mlift17185_op_fun20277(kk_function_t _fself, kk_std_core__list _ctail_16832, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17185_op_fun20277(kk_function_t _accm, kk_box_t y0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17185_op_fun20277__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17185_op_fun20277__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17185_op_fun20277, kk_context());
  _self->_accm = _accm;
  _self->y0 = y0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17185_op_fun20277(kk_function_t _fself, kk_std_core__list _ctail_16832, kk_context_t* _ctx) {
  struct kk_std_core__mlift17185_op_fun20277__t* _self = kk_function_as(struct kk_std_core__mlift17185_op_fun20277__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<13880>) -> list<13880> */
  kk_box_t y0 = _self->y0; /* 13880 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(y0);}, {}, _ctx)
  kk_std_core__list _x20278 = kk_std_core__new_Cons(kk_reuse_null, y0, _ctail_16832, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20278, _ctx));
}

kk_std_core__list kk_std_core__mlift17185_op(kk_function_t _accm, kk_function_t action0, kk_std_core__list xx0, kk_std_core_types__maybe _y_17050, kk_context_t* _ctx) { /* forall<a,b,e> ((list<b>) -> list<b>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */ 
  if (kk_std_core_types__is_Just(_y_17050)) {
    kk_box_t y0 = _y_17050._cons.Just.value;
    return kk_std_core__ctailm_map_while(xx0, action0, kk_std_core__new_mlift17185_op_fun20277(_accm, y0, _ctx), _ctx);
  }
  {
    kk_function_drop(action0, _ctx);
    kk_std_core__list_drop(xx0, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, kk_std_core__new_Nil(_ctx), _ctx));
  }
}
 
// Invoke `action` on each element of a list while `action` returns `Just`


// lift anonymous function
struct kk_std_core__ctail_map_while_fun20283__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_map_while_fun20283(kk_function_t _fself, kk_box_t _b_18529, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_map_while_fun20283(kk_function_t action1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_while_fun20283__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_map_while_fun20283__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_map_while_fun20283, kk_context());
  _self->action1 = action1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_map_while_fun20283(kk_function_t _fself, kk_box_t _b_18529, kk_context_t* _ctx) {
  struct kk_std_core__ctail_map_while_fun20283__t* _self = kk_function_as(struct kk_std_core__ctail_map_while_fun20283__t*, _fself);
  kk_function_t action1 = _self->action1; /* (13879) -> 13881 maybe<13880> */
  kk_std_core__list xx1 = _self->xx1; /* list<13879> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<13880>> */
  kk_drop_match(_self, {kk_function_dup(action1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20284;
  kk_std_core_types__maybe _x20285 = kk_std_core_types__maybe_unbox(_b_18529, _ctx); /*maybe<13880>*/
  _x20284 = kk_std_core__mlift17184_op(_acc0, action1, xx1, _x20285, _ctx); /*list<13880>*/
  return kk_std_core__list_box(_x20284, _ctx);
}

kk_std_core__list kk_std_core__ctail_map_while(kk_std_core__list xs, kk_function_t action1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, ctail<list<b>>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(action1, _ctx);
    kk_box_t _x20279 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20279, _ctx);
  }
  {
    struct kk_std_core_Cons* _con20280 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20280->head;
    kk_std_core__list xx1 = _con20280->tail;
    kk_reuse_t _ru_18948 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18948 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18948 = kk_reuse_null;
    }
    kk_std_core_types__maybe x0_17486;
    kk_function_t _x20281 = kk_function_dup(action1); /*(13879) -> 13881 maybe<13880>*/
    x0_17486 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20281, (_x20281, x, _ctx)); /*maybe<13880>*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18948, _ctx);
      kk_std_core_types__maybe_drop(x0_17486, _ctx);
      kk_box_t _x20282 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_map_while_fun20283(action1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20282, _ctx);
    }
    if (kk_std_core_types__is_Just(x0_17486)) {
      kk_box_t y1 = x0_17486._cons.Just.value;
      kk_std_core__list _ctail_168290 = kk_std_core__list_hole(); /*list<13880>*/;
      kk_std_core__list _ctail_168300 = kk_std_core__new_Cons(_ru_18948, y1, _ctail_168290, _ctx); /*list<13880>*/;
      { // tailcall
        kk_std_core_types__ctail _x20286;
        kk_box_t* _b_18543_18535 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168300)->tail)); /*cfield<list<13880>>*/;
        _x20286 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168300, _ctx)),_b_18543_18535); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20286;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18948, _ctx);
      kk_function_drop(action1, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20287 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20287, _ctx);
    }
  }
}
 
// Invoke `action` on each element of a list while `action` returns `Just`


// lift anonymous function
struct kk_std_core__ctailm_map_while_fun20291__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t action2;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_map_while_fun20291(kk_function_t _fself, kk_box_t _b_18551, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_while_fun20291(kk_function_t _accm0, kk_function_t action2, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20291__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_while_fun20291__t, 4, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_while_fun20291, kk_context());
  _self->_accm0 = _accm0;
  _self->action2 = action2;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_map_while_fun20291(kk_function_t _fself, kk_box_t _b_18551, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20291__t* _self = kk_function_as(struct kk_std_core__ctailm_map_while_fun20291__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<13880>) -> list<13880> */
  kk_function_t action2 = _self->action2; /* (13879) -> 13881 maybe<13880> */
  kk_std_core__list xx2 = _self->xx2; /* list<13879> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(action2);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20292;
  kk_std_core_types__maybe _x20293 = kk_std_core_types__maybe_unbox(_b_18551, _ctx); /*maybe<13880>*/
  _x20292 = kk_std_core__mlift17185_op(_accm0, action2, xx2, _x20293, _ctx); /*list<13880>*/
  return kk_std_core__list_box(_x20292, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_map_while_fun20295__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t y2;
};
static kk_std_core__list kk_std_core__ctailm_map_while_fun20295(kk_function_t _fself, kk_std_core__list _ctail_168320, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_map_while_fun20295(kk_function_t _accm0, kk_box_t y2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20295__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_map_while_fun20295__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_map_while_fun20295, kk_context());
  _self->_accm0 = _accm0;
  _self->y2 = y2;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_map_while_fun20295(kk_function_t _fself, kk_std_core__list _ctail_168320, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_map_while_fun20295__t* _self = kk_function_as(struct kk_std_core__ctailm_map_while_fun20295__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<13880>) -> list<13880> */
  kk_box_t y2 = _self->y2; /* 13880 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(y2);}, {}, _ctx)
  kk_std_core__list _x20296 = kk_std_core__new_Cons(kk_reuse_null, y2, _ctail_168320, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20296, _ctx));
}

kk_std_core__list kk_std_core__ctailm_map_while(kk_std_core__list xs0, kk_function_t action2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs0)) {
    kk_function_drop(action2, _ctx);
    return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, kk_std_core__new_Nil(_ctx), _ctx));
  }
  {
    struct kk_std_core_Cons* _con20288 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20288->head;
    kk_std_core__list xx2 = _con20288->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    kk_std_core_types__maybe x2_17489;
    kk_function_t _x20289 = kk_function_dup(action2); /*(13879) -> 13881 maybe<13880>*/
    x2_17489 = kk_function_call(kk_std_core_types__maybe, (kk_function_t, kk_box_t, kk_context_t*), _x20289, (_x20289, x1, _ctx)); /*maybe<13880>*/
    if (kk_yielding(kk_context())) {
      kk_std_core_types__maybe_drop(x2_17489, _ctx);
      kk_box_t _x20290 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_map_while_fun20291(_accm0, action2, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20290, _ctx);
    }
    if (kk_std_core_types__is_Just(x2_17489)) {
      kk_box_t y2 = x2_17489._cons.Just.value;
      { // tailcall
        kk_function_t _x20294 = kk_std_core__new_ctailm_map_while_fun20295(_accm0, y2, _ctx); /*(list<13880>) -> list<13880>*/
        xs0 = xx2;
        _accm0 = _x20294;
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
struct kk_std_core_map_while_fun20298__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_map_while_fun20298(kk_function_t _fself, kk_std_core__list _ctail_16831, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_map_while_fun20298(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_map_while_fun20298, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_map_while_fun20298(kk_function_t _fself, kk_std_core__list _ctail_16831, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16831;
}

kk_std_core__list kk_std_core_map_while(kk_std_core__list xs1, kk_function_t action3, kk_context_t* _ctx) { /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e list<b> */ 
  bool _match_19021 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_19021) {
    kk_std_core_types__ctail _x20297 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_map_while(xs1, action3, _x20297, _ctx);
  }
  {
    return kk_std_core__ctailm_map_while(xs1, action3, kk_std_core_new_map_while_fun20298(_ctx), _ctx);
  }
}
 
// Return the maximum of two integers

kk_integer_t kk_std_core_max(kk_integer_t i, kk_integer_t j, kk_context_t* _ctx) { /* (i : int, j : int) -> int */ 
  bool _match_19020;
  kk_integer_t _x20299 = kk_integer_dup(i); /*int*/
  kk_integer_t _x20300 = kk_integer_dup(j); /*int*/
  _match_19020 = kk_integer_gte(_x20299,_x20300,kk_context()); /*bool*/
  if (_match_19020) {
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
struct kk_std_core_maximum_fun20305__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_maximum_fun20305(kk_function_t _fself, kk_box_t _b_18559, kk_box_t _b_18560, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_maximum_fun20305(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_maximum_fun20305, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_maximum_fun20305(kk_function_t _fself, kk_box_t _b_18559, kk_box_t _b_18560, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20306;
  kk_integer_t _x20307 = kk_integer_unbox(_b_18559); /*int*/
  kk_integer_t _x20308 = kk_integer_unbox(_b_18560); /*int*/
  _x20306 = kk_std_core_max(_x20307, _x20308, _ctx); /*int*/
  return kk_integer_box(_x20306);
}

kk_integer_t kk_std_core_maximum(kk_std_core__list xs, kk_std_core_types__optional default0, kk_context_t* _ctx) { /* (xs : list<int>, default : optional<int>) -> int */ 
  if (kk_std_core__is_Nil(xs)) {
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18554 = default0._cons.Optional.value;
      kk_integer_t _default_13929 = kk_integer_unbox(_box_x18554);
      kk_integer_dup(_default_13929);
      kk_std_core_types__optional_drop(default0, _ctx);
      return _default_13929;
    }
    {
      return kk_integer_from_small(0);
    }
  }
  {
    struct kk_std_core_Cons* _con20302 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18555 = _con20302->head;
    kk_std_core__list xx = _con20302->tail;
    kk_integer_t x = kk_integer_unbox(_box_x18555);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_integer_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__optional_drop(default0, _ctx);
    kk_box_t _x20304 = kk_std_core_foldl(xx, kk_integer_box(x), kk_std_core_new_maximum_fun20305(_ctx), _ctx); /*11906*/
    return kk_integer_unbox(_x20304);
  }
}
 
// Returns the largest element of a list of doubles (or `0` for the empty list)


// lift anonymous function
struct kk_std_core_maximum_fun20312__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_maximum_fun20312_1(kk_function_t _fself, kk_box_t _b_18568, kk_box_t _b_18569, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_maximum_fun20312_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_maximum_fun20312_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_maximum_fun20312_1(kk_function_t _fself, kk_box_t _b_18568, kk_box_t _b_18569, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  double _x20313;
  double _x20314 = kk_double_unbox(_b_18568, _ctx); /*double*/
  double _x20315 = kk_double_unbox(_b_18569, _ctx); /*double*/
  _x20313 = kk_std_core_max_1(_x20314, _x20315, _ctx); /*double*/
  return kk_double_box(_x20313, _ctx);
}

double kk_std_core_maximum_1(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<double>) -> double */ 
  if (kk_std_core__is_Nil(xs)) {
    return 0.0;
  }
  {
    struct kk_std_core_Cons* _con20309 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18564 = _con20309->head;
    kk_std_core__list xx = _con20309->tail;
    double x = kk_double_unbox(_box_x18564, NULL);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_box_x18564, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t _x20311 = kk_std_core_foldl(xx, kk_double_box(x, _ctx), kk_std_core_new_maximum_fun20312_1(_ctx), _ctx); /*11906*/
    return kk_double_unbox(_x20311, _ctx);
  }
}
 
// Convert a list to a `:maybe` type, using `Nothing` for an empty list, and otherwise `Just` on the head element.
// Note: this is just `head`.

kk_std_core_types__maybe kk_std_core_maybe_3(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> maybe<a> */ 
  if (kk_std_core__is_Nil(xs)) {
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    struct kk_std_core_Cons* _con20316 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20316->head;
    kk_std_core__list _pat1 = _con20316->tail;
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
  bool _match_19018;
  kk_integer_t _x20317 = kk_integer_dup(i); /*int*/
  _match_19018 = kk_integer_eq(_x20317,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19018) {
    kk_integer_drop(i, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
  {
    return kk_std_core_types__new_Just(kk_integer_box(i), _ctx);
  }
}
 
// Transform a string to a maybe type, using `Nothing` for an empty string

kk_std_core_types__maybe kk_std_core_maybe_6(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> maybe<string> */ 
  bool _match_19017;
  kk_string_t _x20318 = kk_string_dup(s); /*string*/
  kk_string_t _x20319 = kk_string_empty(); /*string*/
  _match_19017 = kk_string_is_eq(_x20318,_x20319,kk_context()); /*bool*/
  if (_match_19017) {
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
  bool _match_19016;
  kk_integer_t _x20321 = kk_integer_dup(i); /*int*/
  kk_integer_t _x20322 = kk_integer_dup(j); /*int*/
  _match_19016 = kk_integer_lte(_x20321,_x20322,kk_context()); /*bool*/
  if (_match_19016) {
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
struct kk_std_core_minimum_fun20327__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_minimum_fun20327(kk_function_t _fself, kk_box_t _b_18584, kk_box_t _b_18585, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_minimum_fun20327(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_minimum_fun20327, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_minimum_fun20327(kk_function_t _fself, kk_box_t _b_18584, kk_box_t _b_18585, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20328;
  kk_integer_t _x20329 = kk_integer_unbox(_b_18584); /*int*/
  kk_integer_t _x20330 = kk_integer_unbox(_b_18585); /*int*/
  _x20328 = kk_std_core_min(_x20329, _x20330, _ctx); /*int*/
  return kk_integer_box(_x20328);
}

kk_integer_t kk_std_core_minimum(kk_std_core__list xs, kk_std_core_types__optional default0, kk_context_t* _ctx) { /* (xs : list<int>, default : optional<int>) -> int */ 
  if (kk_std_core__is_Nil(xs)) {
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18579 = default0._cons.Optional.value;
      kk_integer_t _default_14408 = kk_integer_unbox(_box_x18579);
      kk_integer_dup(_default_14408);
      kk_std_core_types__optional_drop(default0, _ctx);
      return _default_14408;
    }
    {
      return kk_integer_from_small(0);
    }
  }
  {
    struct kk_std_core_Cons* _con20324 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18580 = _con20324->head;
    kk_std_core__list xx = _con20324->tail;
    kk_integer_t x = kk_integer_unbox(_box_x18580);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_integer_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_std_core_types__optional_drop(default0, _ctx);
    kk_box_t _x20326 = kk_std_core_foldl(xx, kk_integer_box(x), kk_std_core_new_minimum_fun20327(_ctx), _ctx); /*11906*/
    return kk_integer_unbox(_x20326);
  }
}
 
// Returns the smallest element of a list of doubles (or `0` for the empty list)


// lift anonymous function
struct kk_std_core_minimum_fun20334__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_minimum_fun20334_1(kk_function_t _fself, kk_box_t _b_18593, kk_box_t _b_18594, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_minimum_fun20334_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_minimum_fun20334_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_minimum_fun20334_1(kk_function_t _fself, kk_box_t _b_18593, kk_box_t _b_18594, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  double _x20335;
  double _x20336 = kk_double_unbox(_b_18593, _ctx); /*double*/
  double _x20337 = kk_double_unbox(_b_18594, _ctx); /*double*/
  _x20335 = kk_std_core_min_1(_x20336, _x20337, _ctx); /*double*/
  return kk_double_box(_x20335, _ctx);
}

double kk_std_core_minimum_1(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<double>) -> double */ 
  if (kk_std_core__is_Nil(xs)) {
    return 0.0;
  }
  {
    struct kk_std_core_Cons* _con20331 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18589 = _con20331->head;
    kk_std_core__list xx = _con20331->tail;
    double x = kk_double_unbox(_box_x18589, NULL);
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_box_drop(_box_x18589, _ctx);
      kk_std_core__list_free(xs);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    kk_box_t _x20333 = kk_std_core_foldl(xx, kk_double_box(x, _ctx), kk_std_core_new_minimum_fun20334_1(_ctx), _ctx); /*11906*/
    return kk_double_unbox(_x20333, _ctx);
  }
}
 
// Disable tracing completely.

kk_unit_t kk_std_core_notrace(kk_context_t* _ctx) { /* () -> (st<global>) () */ 
  kk_ref_t _x20338 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  kk_ref_set(_x20338,(kk_bool_box(false)),kk_context()); return kk_Unit;
}
 
// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).

kk_std_core__null kk_std_core_null(kk_std_core_types__maybe x, kk_context_t* _ctx) { /* forall<a> (x : maybe<a>) -> null<a> */ 
  return (kk_std_core_types__is_Nothing(x) ? kk_datatype_from_ptr(NULL) : kk_datatype_unbox((x)._cons.Just.value));
}
 
// Cast a integer that is zero to a null

kk_std_core__null kk_std_core_null_1(kk_integer_t i, kk_context_t* _ctx) { /* (i : int) -> null<int> */ 
  kk_std_core_types__maybe _x20339;
  bool _match_19014;
  kk_integer_t _x20340 = kk_integer_dup(i); /*int*/
  _match_19014 = kk_integer_eq(_x20340,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19014) {
    kk_integer_drop(i, _ctx);
    _x20339 = kk_std_core_types__new_Nothing(_ctx); /*forall<a> maybe<a>*/
  }
  else {
    _x20339 = kk_std_core_types__new_Just(kk_integer_box(i), _ctx); /*forall<a> maybe<a>*/
  }
  return kk_std_core_null(_x20339, _ctx);
}
 
// Cast an empty string a null

kk_std_core__null kk_std_core_null_2(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> null<string> */ 
  kk_std_core_types__maybe _x20341;
  bool _match_19013;
  kk_string_t _x20342 = kk_string_dup(s); /*string*/
  kk_string_t _x20343 = kk_string_empty(); /*string*/
  _match_19013 = kk_string_is_eq(_x20342,_x20343,kk_context()); /*bool*/
  if (_match_19013) {
    kk_string_drop(s, _ctx);
    _x20341 = kk_std_core_types__new_Nothing(_ctx); /*forall<a> maybe<a>*/
  }
  else {
    _x20341 = kk_std_core_types__new_Just(kk_string_box(s), _ctx); /*forall<a> maybe<a>*/
  }
  return kk_std_core_null(_x20341, _ctx);
}
 
// Left-align a string to width `width`  using `fill`  (default is a space) to fill on the right.

kk_string_t kk_std_core_pad_right(kk_string_t s, kk_integer_t width, kk_std_core_types__optional fill, kk_context_t* _ctx) { /* (s : string, width : int, fill : optional<char>) -> string */ 
  kk_ssize_t w = kk_std_core_ssize__t(width, _ctx); /*ssize_t*/;
  kk_ssize_t n;
  kk_string_t _x20346 = kk_string_dup(s); /*string*/
  n = kk_string_len(_x20346,kk_context()); /*ssize_t*/
  bool _match_19012 = (w <= n); /*bool*/;
  if (_match_19012) {
    kk_std_core_types__optional_drop(fill, _ctx);
    return s;
  }
  {
    kk_string_t _x20347;
    kk_string_t _x20348;
    kk_char_t _x20349;
    if (kk_std_core_types__is_Optional(fill)) {
      kk_box_t _box_x18608 = fill._cons.Optional.value;
      kk_char_t _fill_14810 = kk_char_unbox(_box_x18608, NULL);
      kk_std_core_types__optional_drop(fill, _ctx);
      _x20349 = _fill_14810; /*char*/
      goto _match20350;
    }
    {
      _x20349 = ' '; /*char*/
    }
    _match20350: ;
    _x20348 = kk_std_core_string(_x20349, _ctx); /*string*/
    kk_ssize_t _x20352 = (w - n); /*ssize_t*/
    _x20347 = kk_std_core_repeatz(_x20348, _x20352, _ctx); /*string*/
    return kk_std_core__lp__plus__plus__1_rp_(s, _x20347, _ctx);
  }
}
 
// Is `pre`  a prefix of `s`? If so, returns a slice
// of `s` following `pre` up to the end of `s`.

kk_std_core_types__maybe kk_std_core_starts_with(kk_string_t s, kk_string_t pre, kk_context_t* _ctx) { /* (s : string, pre : string) -> maybe<sslice> */ 
  bool _match_19011;
  kk_string_t _x20353 = kk_string_dup(s); /*string*/
  kk_string_t _x20354 = kk_string_dup(pre); /*string*/
  _match_19011 = kk_string_starts_with(_x20353,_x20354,kk_context()); /*bool*/
  if (_match_19011) {
    kk_std_core__sslice _b_18610_18609;
    kk_string_t _x20355 = kk_string_dup(s); /*string*/
    kk_ssize_t _x20356;
    kk_string_t _x20357 = kk_string_dup(pre); /*string*/
    _x20356 = kk_string_len(_x20357,kk_context()); /*ssize_t*/
    kk_ssize_t _x20358;
    kk_ssize_t _x20359 = kk_string_len(s,kk_context()); /*ssize_t*/
    kk_ssize_t _x20360 = kk_string_len(pre,kk_context()); /*ssize_t*/
    _x20358 = (_x20359 - _x20360); /*ssize_t*/
    _b_18610_18609 = kk_std_core__new_Sslice(_x20355, _x20356, _x20358, _ctx); /*sslice*/
    return kk_std_core_types__new_Just(kk_std_core__sslice_box(_b_18610_18609, _ctx), _ctx);
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
  bool _match_19009;
  kk_string_t _x20361 = kk_string_dup(sub); /*string*/
  kk_string_t _x20362 = kk_string_empty(); /*string*/
  _match_19009 = kk_string_is_eq(_x20361,_x20362,kk_context()); /*bool*/
  if (_match_19009) {
    kk_string_drop(sub, _ctx);
    return s;
  }
  {
    kk_std_core_types__maybe _match_19010;
    kk_string_t _x20364 = kk_string_dup(s); /*string*/
    kk_string_t _x20365 = kk_string_dup(sub); /*string*/
    _match_19010 = kk_std_core_starts_with(_x20364, _x20365, _ctx); /*maybe<sslice>*/
    if (kk_std_core_types__is_Just(_match_19010)) {
      kk_box_t _box_x18611 = _match_19010._cons.Just.value;
      kk_std_core__sslice slice0 = kk_std_core__sslice_unbox(_box_x18611, NULL);
      kk_string_drop(s, _ctx);
      { // tailcall
        kk_string_t _x20367 = kk_std_core_string_3(slice0, _ctx); /*string*/
        s = _x20367;
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
  bool _match_19007;
  kk_string_t _x20368 = kk_string_dup(sub); /*string*/
  kk_string_t _x20369 = kk_string_empty(); /*string*/
  _match_19007 = kk_string_is_eq(_x20368,_x20369,kk_context()); /*bool*/
  if (_match_19007) {
    kk_string_drop(sub, _ctx);
    return s;
  }
  {
    kk_std_core_types__maybe _match_19008;
    kk_string_t _x20371 = kk_string_dup(s); /*string*/
    kk_string_t _x20372 = kk_string_dup(sub); /*string*/
    _match_19008 = kk_std_core_ends_with(_x20371, _x20372, _ctx); /*maybe<sslice>*/
    if (kk_std_core_types__is_Just(_match_19008)) {
      kk_box_t _box_x18612 = _match_19008._cons.Just.value;
      kk_std_core__sslice slice0 = kk_std_core__sslice_unbox(_box_x18612, NULL);
      kk_string_drop(s, _ctx);
      { // tailcall
        kk_string_t _x20374 = kk_std_core_string_3(slice0, _ctx); /*string*/
        s = _x20374;
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
  bool _match_19006;
  kk_string_t _x20381 = kk_string_dup(s); /*string*/
  kk_string_t _x20382 = kk_string_empty(); /*string*/
  _match_19006 = kk_string_is_eq(_x20381,_x20382,kk_context()); /*bool*/
  if (_match_19006) {
    kk_std_core_types__optional_drop(hex, _ctx);
    kk_string_drop(s, _ctx);
    if (kk_std_core_types__is_Optional(default0)) {
      kk_box_t _box_x18614 = default0._cons.Optional.value;
      kk_integer_t _default_15092 = kk_integer_unbox(_box_x18614);
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
    kk_string_t _x20385;
    kk_string_t _x20386 = kk_string_trim_left(s,kk_context()); /*string*/
    _x20385 = kk_string_trim_right(_x20386,kk_context()); /*string*/
    bool _x20387;
    if (kk_std_core_types__is_Optional(hex)) {
      kk_box_t _box_x18615 = hex._cons.Optional.value;
      bool _hex_15096 = kk_bool_unbox(_box_x18615);
      kk_std_core_types__optional_drop(hex, _ctx);
      _x20387 = _hex_15096; /*bool*/
      goto _match20388;
    }
    {
      _x20387 = false; /*bool*/
    }
    _match20388: ;
    m_16684 = kk_std_core_xparse_int(_x20385, _x20387, _ctx); /*maybe<int>*/
    if (kk_std_core_types__is_Nothing(m_16684)) {
      if (kk_std_core_types__is_Optional(default0)) {
        kk_box_t _box_x18616 = default0._cons.Optional.value;
        kk_integer_t _default_150920 = kk_integer_unbox(_box_x18616);
        kk_integer_dup(_default_150920);
        kk_std_core_types__optional_drop(default0, _ctx);
        return _default_150920;
      }
      {
        return kk_integer_from_small(0);
      }
    }
    {
      kk_box_t _box_x18617 = m_16684._cons.Just.value;
      kk_integer_t x = kk_integer_unbox(_box_x18617);
      kk_std_core_types__optional_drop(default0, _ctx);
      return x;
    }
  }
}
 
// monadic lift

kk_std_core_types__tuple2_ kk_std_core__mlift17186_partition_acc(kk_std_core__list acc1, kk_std_core__list acc2, kk_function_t pred, kk_box_t x, kk_std_core__list xx, bool _y_17059, kk_context_t* _ctx) { /* forall<a,e> (acc1 : list<a>, acc2 : list<a>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e (list<a>, list<a>) */ 
  if (_y_17059) {
    kk_std_core__list _x20392 = kk_std_core__new_Cons(kk_reuse_null, x, acc1, _ctx); /*list<61>*/
    return kk_std_core_partition_acc(xx, pred, _x20392, acc2, _ctx);
  }
  {
    kk_std_core__list _x20393 = kk_std_core__new_Cons(kk_reuse_null, x, acc2, _ctx); /*list<61>*/
    return kk_std_core_partition_acc(xx, pred, acc1, _x20393, _ctx);
  }
}


// lift anonymous function
struct kk_std_core_partition_acc_fun20398__t {
  struct kk_function_s _base;
  kk_std_core__list acc10;
  kk_std_core__list acc20;
  kk_function_t pred0;
  kk_box_t x0;
  kk_std_core__list xx0;
};
static kk_box_t kk_std_core_partition_acc_fun20398(kk_function_t _fself, kk_box_t _b_18621, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_partition_acc_fun20398(kk_std_core__list acc10, kk_std_core__list acc20, kk_function_t pred0, kk_box_t x0, kk_std_core__list xx0, kk_context_t* _ctx) {
  struct kk_std_core_partition_acc_fun20398__t* _self = kk_function_alloc_as(struct kk_std_core_partition_acc_fun20398__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_partition_acc_fun20398, kk_context());
  _self->acc10 = acc10;
  _self->acc20 = acc20;
  _self->pred0 = pred0;
  _self->x0 = x0;
  _self->xx0 = xx0;
  return &_self->_base;
}

static kk_box_t kk_std_core_partition_acc_fun20398(kk_function_t _fself, kk_box_t _b_18621, kk_context_t* _ctx) {
  struct kk_std_core_partition_acc_fun20398__t* _self = kk_function_as(struct kk_std_core_partition_acc_fun20398__t*, _fself);
  kk_std_core__list acc10 = _self->acc10; /* list<15214> */
  kk_std_core__list acc20 = _self->acc20; /* list<15214> */
  kk_function_t pred0 = _self->pred0; /* (15214) -> 15215 bool */
  kk_box_t x0 = _self->x0; /* 15214 */
  kk_std_core__list xx0 = _self->xx0; /* list<15214> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc10);kk_std_core__list_dup(acc20);kk_function_dup(pred0);kk_box_dup(x0);kk_std_core__list_dup(xx0);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x20399;
  bool _x20400 = kk_bool_unbox(_b_18621); /*bool*/
  _x20399 = kk_std_core__mlift17186_partition_acc(acc10, acc20, pred0, x0, xx0, _x20400, _ctx); /*(list<15214>, list<15214>)*/
  return kk_std_core_types__tuple2__box(_x20399, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core_partition_acc(kk_std_core__list xs, kk_function_t pred0, kk_std_core__list acc10, kk_std_core__list acc20, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, acc1 : list<a>, acc2 : list<a>) -> e (list<a>, list<a>) */ 
  kk__tailcall: ;
  if (kk_std_core__is_Nil(xs)) {
    kk_function_drop(pred0, _ctx);
    kk_std_core__list _b_18622_18618 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc10, _ctx); /*list<15214>*/;
    kk_std_core__list _b_18623_18619 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc20, _ctx); /*list<15214>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18622_18618, _ctx), kk_std_core__list_box(_b_18623_18619, _ctx), _ctx);
  }
  {
    struct kk_std_core_Cons* _con20394 = kk_std_core__as_Cons(xs);
    kk_box_t x0 = _con20394->head;
    kk_std_core__list xx0 = _con20394->tail;
    kk_reuse_t _ru_18955 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18955 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18955 = kk_reuse_null;
    }
    bool x1_17494;
    kk_function_t _x20396 = kk_function_dup(pred0); /*(15214) -> 15215 bool*/
    kk_box_t _x20395 = kk_box_dup(x0); /*15214*/
    x1_17494 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20396, (_x20396, _x20395, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18955, _ctx);
      kk_box_t _x20397 = kk_std_core_hnd_yield_extend(kk_std_core_new_partition_acc_fun20398(acc10, acc20, pred0, x0, xx0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x20397, _ctx);
    }
    if (x1_17494) { // tailcall
                    kk_std_core__list _x20401;
                    if (kk_likely(_ru_18955!=NULL)) {
                      struct kk_std_core_Cons* _con20402 = (struct kk_std_core_Cons*)_ru_18955;
                      _con20402->tail = acc10;
                      _x20401 = kk_std_core__base_Cons(_con20402); /*list<61>*/
                    }
                    else {
                      _x20401 = kk_std_core__new_Cons(kk_reuse_null, x0, acc10, _ctx); /*list<61>*/
                    }
                    xs = xx0;
                    acc10 = _x20401;
                    goto kk__tailcall;
    }
    { // tailcall
      kk_std_core__list _x20403;
      if (kk_likely(_ru_18955!=NULL)) {
        struct kk_std_core_Cons* _con20404 = (struct kk_std_core_Cons*)_ru_18955;
        _con20404->tail = acc20;
        _x20403 = kk_std_core__base_Cons(_con20404); /*list<61>*/
      }
      else {
        _x20403 = kk_std_core__new_Cons(kk_reuse_null, x0, acc20, _ctx); /*list<61>*/
      }
      xs = xx0;
      acc20 = _x20403;
      goto kk__tailcall;
    }
  }
}
 
// redirect `print` and `println` calls to a specified function.


// lift anonymous function
struct kk_std_core_print_redirect_fun20408__t {
  struct kk_function_s _base;
  kk_function_t print0;
};
static kk_box_t kk_std_core_print_redirect_fun20408(kk_function_t _fself, kk_box_t _b_18629, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_print_redirect_fun20408(kk_function_t print0, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20408__t* _self = kk_function_alloc_as(struct kk_std_core_print_redirect_fun20408__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_print_redirect_fun20408, kk_context());
  _self->print0 = print0;
  return &_self->_base;
}

static kk_box_t kk_std_core_print_redirect_fun20408(kk_function_t _fself, kk_box_t _b_18629, kk_context_t* _ctx) {
  struct kk_std_core_print_redirect_fun20408__t* _self = kk_function_as(struct kk_std_core_print_redirect_fun20408__t*, _fself);
  kk_function_t print0 = _self->print0; /* (msg : string) -> console () */
  kk_drop_match(_self, {kk_function_dup(print0);}, {}, _ctx)
  kk_unit_t _x20409 = kk_Unit;
  kk_string_t _x20410 = kk_string_unbox(_b_18629); /*string*/
  kk_function_call(kk_unit_t, (kk_function_t, kk_string_t, kk_context_t*), print0, (print0, _x20410, _ctx));
  return kk_unit_box(_x20409);
}

kk_unit_t kk_std_core_print_redirect(kk_function_t print0, kk_context_t* _ctx) { /* (print : (msg : string) -> console ()) -> io () */ 
  kk_ref_t _x20405 = kk_ref_dup(kk_std_core_redirect); /*ref<global,maybe<(string) -> console ()>>*/
  kk_box_t _x20406;
  kk_std_core_types__maybe _x20407 = kk_std_core_types__new_Just(kk_function_box(kk_std_core_new_print_redirect_fun20408(print0, _ctx)), _ctx); /*maybe<105>*/
  _x20406 = kk_std_core_types__maybe_box(_x20407, _ctx); /*171*/
  kk_ref_set(_x20405,_x20406,kk_context()); return kk_Unit;
}
extern bool kk_std_core_remove_fun20411(kk_function_t _fself, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_remove_fun20411__t* _self = kk_function_as(struct kk_std_core_remove_fun20411__t*, _fself);
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
extern kk_unit_t kk_std_core_repeat_fun20413_1(kk_function_t _fself, kk_integer_t i, kk_context_t* _ctx) {
  struct kk_std_core_repeat_fun20413__t_1* _self = kk_function_as(struct kk_std_core_repeat_fun20413__t_1*, _fself);
  kk_function_t action = _self->action; /* () -> 15302 () */
  kk_drop_match(_self, {kk_function_dup(action);}, {}, _ctx)
  kk_integer_drop(i, _ctx);
  return kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), action, (action, _ctx));
}
 
// Create a list of `n`  repeated elementes `x`

kk_std_core__list kk_std_core__ctail_replicate(kk_box_t x, kk_integer_t n, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* forall<a> (x : a, n : int, ctail<list<a>>) -> list<a> */ 
  kk__tailcall: ;
  bool _match_19004;
  kk_integer_t _x20414 = kk_integer_dup(n); /*int*/
  _match_19004 = kk_integer_gt(_x20414,(kk_integer_from_small(0)),kk_context()); /*bool*/
  if (_match_19004) {
    kk_std_core__list _ctail_16833 = kk_std_core__list_hole(); /*list<15346>*/;
    kk_std_core__list _ctail_16834;
    kk_box_t _x20415 = kk_box_dup(x); /*15346*/
    _ctail_16834 = kk_std_core__new_Cons(kk_reuse_null, _x20415, _ctail_16833, _ctx); /*list<15346>*/
    { // tailcall
      kk_integer_t _x20416 = kk_integer_sub(n,(kk_integer_from_small(1)),kk_context()); /*int*/
      kk_std_core_types__ctail _x20417;
      kk_box_t* _b_18645_18640 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16834)->tail)); /*cfield<list<15346>>*/;
      _x20417 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16834, _ctx)),_b_18645_18640); /*ctail<0>*/
      n = _x20416;
      _acc = _x20417;
      goto kk__tailcall;
    }
  }
  {
    kk_integer_drop(n, _ctx);
    kk_box_drop(x, _ctx);
    kk_box_t _x20418 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20418, _ctx);
  }
}
 
// Create a list of `n`  repeated elementes `x`

kk_std_core__list kk_std_core_replicate(kk_box_t x0, kk_integer_t n0, kk_context_t* _ctx) { /* forall<a> (x : a, n : int) -> list<a> */ 
  kk_std_core_types__ctail _x20419 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_replicate(x0, n0, _x20419, _ctx);
}
 
// lifted

kk_std_core__list kk_std_core__lift16752_reverse_join(kk_std_core__list acc, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20420 = kk_std_core__as_Cons(ys);
    kk_box_t x = _con20420->head;
    kk_std_core__list xx = _con20420->tail;
    kk_reuse_t _ru_18956 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      _ru_18956 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18956 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20421;
      if (kk_likely(_ru_18956!=NULL)) {
        struct kk_std_core_Cons* _con20422 = (struct kk_std_core_Cons*)_ru_18956;
        _con20422->tail = acc;
        _x20421 = kk_std_core__base_Cons(_con20422); /*list<61>*/
      }
      else {
        _x20421 = kk_std_core__new_Cons(kk_reuse_null, x, acc, _ctx); /*list<61>*/
      }
      acc = _x20421;
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
    struct kk_std_core_Cons* _con20423 = kk_std_core__as_Cons(ys0);
    kk_box_t _box_x18651 = _con20423->head;
    kk_std_core__list yy = _con20423->tail;
    kk_string_t y = kk_string_unbox(_box_x18651);
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      kk_std_core__list_free(ys0);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys0, _ctx);
    }
    { // tailcall
      kk_string_t _x20425;
      kk_string_t _x20426;
      kk_string_t _x20427 = kk_string_empty(); /*string*/
      _x20426 = kk_std_core__lp__plus__plus__1_rp_(_x20427, y, _ctx); /*string*/
      _x20425 = kk_std_core__lp__plus__plus__1_rp_(acc0, _x20426, _ctx); /*string*/
      ys0 = yy;
      acc0 = _x20425;
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
    struct kk_std_core_Cons* _con20430 = kk_std_core__as_Cons(xs0_16693);
    kk_box_t _box_x18652 = _con20430->head;
    kk_std_core__list xx0 = _con20430->tail;
    kk_string_t x0 = kk_string_unbox(_box_x18652);
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
  kk_string_t _x20432;
  kk_define_string_literal(, _s20433, 1, "(")
  _x20432 = kk_string_dup(_s20433); /*string*/
  kk_string_t _x20434;
  kk_string_t _x20435;
  kk_box_t _x20436;
  {
    kk_box_t _x = x.fst;
    kk_box_dup(_x);
    _x20436 = _x; /*15666*/
  }
  _x20435 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), showfst, (showfst, _x20436, _ctx)); /*string*/
  kk_string_t _x20437;
  kk_string_t _x20438;
  kk_define_string_literal(, _s20439, 1, ",")
  _x20438 = kk_string_dup(_s20439); /*string*/
  kk_string_t _x20440;
  kk_string_t _x20441;
  kk_box_t _x20442;
  {
    kk_box_t _x0 = x.snd;
    kk_box_dup(_x0);
    kk_std_core_types__tuple2__drop(x, _ctx);
    _x20442 = _x0; /*15667*/
  }
  _x20441 = kk_function_call(kk_string_t, (kk_function_t, kk_box_t, kk_context_t*), showsnd, (showsnd, _x20442, _ctx)); /*string*/
  kk_string_t _x20443;
  kk_define_string_literal(, _s20444, 1, ")")
  _x20443 = kk_string_dup(_s20444); /*string*/
  _x20440 = kk_std_core__lp__plus__plus__1_rp_(_x20441, _x20443, _ctx); /*string*/
  _x20437 = kk_std_core__lp__plus__plus__1_rp_(_x20438, _x20440, _ctx); /*string*/
  _x20434 = kk_std_core__lp__plus__plus__1_rp_(_x20435, _x20437, _ctx); /*string*/
  return kk_std_core__lp__plus__plus__1_rp_(_x20432, _x20434, _ctx);
}
 
// monadic lift

kk_std_core_types__tuple2_ kk_std_core__mlift17187_op(kk_std_core__list acc, kk_function_t predicate, kk_box_t y, kk_std_core__list ys, kk_std_core__list yy, bool _y_17067, kk_context_t* _ctx) { /* forall<a,e> (acc : list<a>, predicate : (a) -> e bool, y : a, ys : list<a>, yy : list<a>, bool) -> e (list<a>, list<a>) */ 
  if (_y_17067) {
    kk_std_core__list_drop(ys, _ctx);
    kk_std_core__list _x20445 = kk_std_core__new_Cons(kk_reuse_null, y, acc, _ctx); /*list<61>*/
    return kk_std_core__lift16754_span(predicate, yy, _x20445, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_box_drop(y, _ctx);
    kk_std_core__list_drop(yy, _ctx);
    kk_std_core__list _b_18655_18653 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc, _ctx); /*list<15725>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18655_18653, _ctx), kk_std_core__list_box(ys, _ctx), _ctx);
  }
}
 
// lifted
// todo: implement TRMC with multiple results to avoid the reverse


// lift anonymous function
struct kk_std_core__lift16754_span_fun20451__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t predicate0;
  kk_box_t y0;
  kk_std_core__list ys0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16754_span_fun20451(kk_function_t _fself, kk_box_t _b_18658, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16754_span_fun20451(kk_std_core__list acc0, kk_function_t predicate0, kk_box_t y0, kk_std_core__list ys0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16754_span_fun20451__t* _self = kk_function_alloc_as(struct kk_std_core__lift16754_span_fun20451__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16754_span_fun20451, kk_context());
  _self->acc0 = acc0;
  _self->predicate0 = predicate0;
  _self->y0 = y0;
  _self->ys0 = ys0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16754_span_fun20451(kk_function_t _fself, kk_box_t _b_18658, kk_context_t* _ctx) {
  struct kk_std_core__lift16754_span_fun20451__t* _self = kk_function_as(struct kk_std_core__lift16754_span_fun20451__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<15725> */
  kk_function_t predicate0 = _self->predicate0; /* (15725) -> 15726 bool */
  kk_box_t y0 = _self->y0; /* 15725 */
  kk_std_core__list ys0 = _self->ys0; /* list<15725> */
  kk_std_core__list yy0 = _self->yy0; /* list<15725> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(predicate0);kk_box_dup(y0);kk_std_core__list_dup(ys0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core_types__tuple2_ _x20452;
  bool _x20453 = kk_bool_unbox(_b_18658); /*bool*/
  _x20452 = kk_std_core__mlift17187_op(acc0, predicate0, y0, ys0, yy0, _x20453, _ctx); /*(list<15725>, list<15725>)*/
  return kk_std_core_types__tuple2__box(_x20452, _ctx);
}

kk_std_core_types__tuple2_ kk_std_core__lift16754_span(kk_function_t predicate0, kk_std_core__list ys0, kk_std_core__list acc0, kk_context_t* _ctx) { /* forall<a,e> (predicate : (a) -> e bool, ys : list<a>, acc : list<a>) -> e (list<a>, list<a>) */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys0)) {
    struct kk_std_core_Cons* _con20446 = kk_std_core__as_Cons(ys0);
    kk_box_t y0 = _con20446->head;
    kk_std_core__list yy0 = _con20446->tail;
    kk_box_dup(y0);
    kk_std_core__list_dup(yy0);
    kk_reuse_t _ru_18959;
    kk_std_core__list _x20447 = kk_std_core__list_dup(ys0); /*list<15725>*/
    _ru_18959 = kk_std_core__list_dropn_reuse(_x20447, ((int32_t)KI32(2)), _ctx); /*reuse*/
    bool x_17501;
    kk_function_t _x20449 = kk_function_dup(predicate0); /*(15725) -> 15726 bool*/
    kk_box_t _x20448 = kk_box_dup(y0); /*15725*/
    x_17501 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20449, (_x20449, _x20448, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18959, _ctx);
      kk_box_t _x20450 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16754_span_fun20451(acc0, predicate0, y0, ys0, yy0, _ctx), _ctx); /*3860*/
      return kk_std_core_types__tuple2__unbox(_x20450, _ctx);
    }
    if (x_17501) {
      kk_std_core__list_dropn(ys0, ((int32_t)KI32(2)), _ctx);
      { // tailcall
        kk_std_core__list _x20454;
        if (kk_likely(_ru_18959!=NULL)) {
          struct kk_std_core_Cons* _con20455 = (struct kk_std_core_Cons*)_ru_18959;
          _con20455->tail = acc0;
          _x20454 = kk_std_core__base_Cons(_con20455); /*list<61>*/
        }
        else {
          _x20454 = kk_std_core__new_Cons(kk_reuse_null, y0, acc0, _ctx); /*list<61>*/
        }
        ys0 = yy0;
        acc0 = _x20454;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18959, _ctx);
      kk_function_drop(predicate0, _ctx);
      kk_box_drop(y0, _ctx);
      kk_std_core__list_drop(yy0, _ctx);
      kk_std_core__list _b_18664_18659 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx); /*list<15725>*/;
      return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18664_18659, _ctx), kk_std_core__list_box(ys0, _ctx), _ctx);
    }
  }
  {
    kk_function_drop(predicate0, _ctx);
    kk_std_core__list _b_18666_18661 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), acc0, _ctx); /*list<15725>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18666_18661, _ctx), kk_std_core__list_box(ys0, _ctx), _ctx);
  }
}
 
// Return the sum of a list of integers


// lift anonymous function
struct kk_std_core_sum_fun20457__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_sum_fun20457(kk_function_t _fself, kk_box_t _b_18672, kk_box_t _b_18673, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_sum_fun20457(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_sum_fun20457, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_sum_fun20457(kk_function_t _fself, kk_box_t _b_18672, kk_box_t _b_18673, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _x20458;
  kk_integer_t _x20459 = kk_integer_unbox(_b_18672); /*int*/
  kk_integer_t _x20460 = kk_integer_unbox(_b_18673); /*int*/
  _x20458 = kk_integer_add(_x20459,_x20460,kk_context()); /*int*/
  return kk_integer_box(_x20458);
}

kk_integer_t kk_std_core_sum(kk_std_core__list xs, kk_context_t* _ctx) { /* (xs : list<int>) -> int */ 
  kk_box_t _x20456 = kk_std_core_foldl(xs, kk_integer_box(kk_integer_from_small(0)), kk_std_core_new_sum_fun20457(_ctx), _ctx); /*11906*/
  return kk_integer_unbox(_x20456);
}
 
// Return the tail of list. Returns the empty list if `xs` is empty.

kk_std_core__list kk_std_core_tail_1(kk_std_core__list xs, kk_context_t* _ctx) { /* forall<a> (xs : list<a>) -> list<a> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20461 = kk_std_core__as_Cons(xs);
    kk_box_t _pat0 = _con20461->head;
    kk_std_core__list xx = _con20461->tail;
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
  kk_std_core__sslice _x20462;
  kk_std_core__sslice slice1 = kk_std_core_first1(s, _ctx); /*sslice*/;
  kk_std_core__sslice slice0_16697;
  bool _match_19000;
  kk_integer_t _x20463;
  kk_std_core_types__optional _match_19002 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_19002)) {
    kk_box_t _box_x18679 = _match_19002._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18679);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_19002, _ctx);
    _x20463 = _n_9710; /*int*/
    goto _match20464;
  }
  {
    _x20463 = kk_integer_from_small(1); /*int*/
  }
  _match20464: ;
  _match_19000 = kk_integer_eq(_x20463,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_19000) {
    slice0_16697 = slice1; /*sslice*/
  }
  else {
    kk_integer_t _x20466;
    kk_integer_t _x20467;
    kk_std_core_types__optional _match_19001 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_19001)) {
      kk_box_t _box_x18680 = _match_19001._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18680);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_19001, _ctx);
      _x20467 = _n_97100; /*int*/
      goto _match20468;
    }
    {
      _x20467 = kk_integer_from_small(1); /*int*/
    }
    _match20468: ;
    _x20466 = kk_integer_sub(_x20467,(kk_integer_from_small(1)),kk_context()); /*int*/
    slice0_16697 = kk_std_core_extend(slice1, _x20466, _ctx); /*sslice*/
  }
  {
    kk_string_t s0 = slice0_16697.str;
    kk_ssize_t start0 = slice0_16697.start;
    kk_ssize_t len0 = slice0_16697.len;
    kk_string_dup(s0);
    kk_std_core__sslice_drop(slice0_16697, _ctx);
    kk_string_t _x20470 = kk_string_dup(s0); /*string*/
    kk_ssize_t _x20471 = (start0 + len0); /*ssize_t*/
    kk_ssize_t _x20472;
    kk_ssize_t _x20473 = kk_string_len(s0,kk_context()); /*ssize_t*/
    kk_ssize_t _x20474 = (start0 + len0); /*ssize_t*/
    _x20472 = (_x20473 - _x20474); /*ssize_t*/
    _x20462 = kk_std_core__new_Sslice(_x20470, _x20471, _x20472, _ctx); /*sslice*/
  }
  return kk_std_core_string_3(_x20462, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17188_op(kk_std_core_types__ctail _acc, kk_function_t predicate, kk_box_t x, kk_std_core__list xx, bool _y_17072, kk_context_t* _ctx) { /* forall<a,e> (ctail<list<a>>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_17072) {
    kk_std_core__list _ctail_16835 = kk_std_core__list_hole(); /*list<15836>*/;
    kk_std_core__list _ctail_16836 = kk_std_core__new_Cons(kk_reuse_null, x, _ctail_16835, _ctx); /*list<15836>*/;
    kk_std_core_types__ctail _x20475;
    kk_box_t* _b_18691_18686 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16836)->tail)); /*cfield<list<15836>>*/;
    _x20475 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16836, _ctx)),_b_18691_18686); /*ctail<0>*/
    return kk_std_core__ctail_take_while(xx, predicate, _x20475, _ctx);
  }
  {
    kk_function_drop(predicate, _ctx);
    kk_box_drop(x, _ctx);
    kk_std_core__list_drop(xx, _ctx);
    kk_box_t _x20476 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20476, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17189_op_fun20477__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t x0;
};
static kk_std_core__list kk_std_core__mlift17189_op_fun20477(kk_function_t _fself, kk_std_core__list _ctail_16838, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17189_op_fun20477(kk_function_t _accm, kk_box_t x0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17189_op_fun20477__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17189_op_fun20477__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17189_op_fun20477, kk_context());
  _self->_accm = _accm;
  _self->x0 = x0;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17189_op_fun20477(kk_function_t _fself, kk_std_core__list _ctail_16838, kk_context_t* _ctx) {
  struct kk_std_core__mlift17189_op_fun20477__t* _self = kk_function_as(struct kk_std_core__mlift17189_op_fun20477__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<15836>) -> list<15836> */
  kk_box_t x0 = _self->x0; /* 15836 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(x0);}, {}, _ctx)
  kk_std_core__list _x20478 = kk_std_core__new_Cons(kk_reuse_null, x0, _ctail_16838, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20478, _ctx));
}

kk_std_core__list kk_std_core__mlift17189_op(kk_function_t _accm, kk_function_t predicate0, kk_box_t x0, kk_std_core__list xx0, bool _y_17076, kk_context_t* _ctx) { /* forall<a,e> ((list<a>) -> list<a>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */ 
  if (_y_17076) {
    return kk_std_core__ctailm_take_while(xx0, predicate0, kk_std_core__new_mlift17189_op_fun20477(_accm, x0, _ctx), _ctx);
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
struct kk_std_core__ctail_take_while_fun20483__t {
  struct kk_function_s _base;
  kk_function_t predicate1;
  kk_box_t x1;
  kk_std_core__list xx1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_take_while_fun20483(kk_function_t _fself, kk_box_t _b_18698, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_take_while_fun20483(kk_function_t predicate1, kk_box_t x1, kk_std_core__list xx1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_take_while_fun20483__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_take_while_fun20483__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_take_while_fun20483, kk_context());
  _self->predicate1 = predicate1;
  _self->x1 = x1;
  _self->xx1 = xx1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_take_while_fun20483(kk_function_t _fself, kk_box_t _b_18698, kk_context_t* _ctx) {
  struct kk_std_core__ctail_take_while_fun20483__t* _self = kk_function_as(struct kk_std_core__ctail_take_while_fun20483__t*, _fself);
  kk_function_t predicate1 = _self->predicate1; /* (15836) -> 15837 bool */
  kk_box_t x1 = _self->x1; /* 15836 */
  kk_std_core__list xx1 = _self->xx1; /* list<15836> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<15836>> */
  kk_drop_match(_self, {kk_function_dup(predicate1);kk_box_dup(x1);kk_std_core__list_dup(xx1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20484;
  bool _x20485 = kk_bool_unbox(_b_18698); /*bool*/
  _x20484 = kk_std_core__mlift17188_op(_acc0, predicate1, x1, xx1, _x20485, _ctx); /*list<15836>*/
  return kk_std_core__list_box(_x20484, _ctx);
}

kk_std_core__list kk_std_core__ctail_take_while(kk_std_core__list xs, kk_function_t predicate1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, ctail<list<a>>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20479 = kk_std_core__as_Cons(xs);
    kk_box_t x1 = _con20479->head;
    kk_std_core__list xx1 = _con20479->tail;
    kk_reuse_t _ru_18961 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_18961 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
      _ru_18961 = kk_reuse_null;
    }
    bool x2_17506;
    kk_function_t _x20481 = kk_function_dup(predicate1); /*(15836) -> 15837 bool*/
    kk_box_t _x20480 = kk_box_dup(x1); /*15836*/
    x2_17506 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20481, (_x20481, _x20480, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_reuse_drop(_ru_18961, _ctx);
      kk_box_t _x20482 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_take_while_fun20483(predicate1, x1, xx1, _acc0, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20482, _ctx);
    }
    if (x2_17506) {
      kk_std_core__list _ctail_168350 = kk_std_core__list_hole(); /*list<15836>*/;
      kk_std_core__list _ctail_168360;
      if (kk_likely(_ru_18961!=NULL)) {
        struct kk_std_core_Cons* _con20486 = (struct kk_std_core_Cons*)_ru_18961;
        _con20486->tail = _ctail_168350;
        _ctail_168360 = kk_std_core__base_Cons(_con20486); /*list<15836>*/
      }
      else {
        _ctail_168360 = kk_std_core__new_Cons(kk_reuse_null, x1, _ctail_168350, _ctx); /*list<15836>*/
      }
      { // tailcall
        kk_std_core_types__ctail _x20487;
        kk_box_t* _b_18712_18704 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168360)->tail)); /*cfield<list<15836>>*/;
        _x20487 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168360, _ctx)),_b_18712_18704); /*ctail<0>*/
        xs = xx1;
        _acc0 = _x20487;
        goto kk__tailcall;
      }
    }
    {
      kk_reuse_drop(_ru_18961, _ctx);
      kk_function_drop(predicate1, _ctx);
      kk_box_drop(x1, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20488 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20488, _ctx);
    }
  }
  {
    kk_function_drop(predicate1, _ctx);
    kk_box_t _x20489 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20489, _ctx);
  }
}
 
// Keep only those initial elements that satisfy `predicate`


// lift anonymous function
struct kk_std_core__ctailm_take_while_fun20494__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t predicate2;
  kk_box_t x3;
  kk_std_core__list xx2;
};
static kk_box_t kk_std_core__ctailm_take_while_fun20494(kk_function_t _fself, kk_box_t _b_18722, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_take_while_fun20494(kk_function_t _accm0, kk_function_t predicate2, kk_box_t x3, kk_std_core__list xx2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20494__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_take_while_fun20494__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_take_while_fun20494, kk_context());
  _self->_accm0 = _accm0;
  _self->predicate2 = predicate2;
  _self->x3 = x3;
  _self->xx2 = xx2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_take_while_fun20494(kk_function_t _fself, kk_box_t _b_18722, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20494__t* _self = kk_function_as(struct kk_std_core__ctailm_take_while_fun20494__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<15836>) -> list<15836> */
  kk_function_t predicate2 = _self->predicate2; /* (15836) -> 15837 bool */
  kk_box_t x3 = _self->x3; /* 15836 */
  kk_std_core__list xx2 = _self->xx2; /* list<15836> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(predicate2);kk_box_dup(x3);kk_std_core__list_dup(xx2);}, {}, _ctx)
  kk_std_core__list _x20495;
  bool _x20496 = kk_bool_unbox(_b_18722); /*bool*/
  _x20495 = kk_std_core__mlift17189_op(_accm0, predicate2, x3, xx2, _x20496, _ctx); /*list<15836>*/
  return kk_std_core__list_box(_x20495, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_take_while_fun20498__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x3;
};
static kk_std_core__list kk_std_core__ctailm_take_while_fun20498(kk_function_t _fself, kk_std_core__list _ctail_168380, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_take_while_fun20498(kk_function_t _accm0, kk_box_t x3, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20498__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_take_while_fun20498__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_take_while_fun20498, kk_context());
  _self->_accm0 = _accm0;
  _self->x3 = x3;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_take_while_fun20498(kk_function_t _fself, kk_std_core__list _ctail_168380, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_take_while_fun20498__t* _self = kk_function_as(struct kk_std_core__ctailm_take_while_fun20498__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<15836>) -> list<15836> */
  kk_box_t x3 = _self->x3; /* 15836 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x3);}, {}, _ctx)
  kk_std_core__list _x20499 = kk_std_core__new_Cons(kk_reuse_null, x3, _ctail_168380, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20499, _ctx));
}

kk_std_core__list kk_std_core__ctailm_take_while(kk_std_core__list xs0, kk_function_t predicate2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20490 = kk_std_core__as_Cons(xs0);
    kk_box_t x3 = _con20490->head;
    kk_std_core__list xx2 = _con20490->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x3);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    bool x4_17509;
    kk_function_t _x20492 = kk_function_dup(predicate2); /*(15836) -> 15837 bool*/
    kk_box_t _x20491 = kk_box_dup(x3); /*15836*/
    x4_17509 = kk_function_call(bool, (kk_function_t, kk_box_t, kk_context_t*), _x20492, (_x20492, _x20491, _ctx)); /*bool*/
    if (kk_yielding(kk_context())) {
      kk_box_t _x20493 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_take_while_fun20494(_accm0, predicate2, x3, xx2, _ctx), _ctx); /*3860*/
      return kk_std_core__list_unbox(_x20493, _ctx);
    }
    if (x4_17509) { // tailcall
                    kk_function_t _x20497 = kk_std_core__new_ctailm_take_while_fun20498(_accm0, x3, _ctx); /*(list<15836>) -> list<15836>*/
                    xs0 = xx2;
                    _accm0 = _x20497;
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
struct kk_std_core_take_while_fun20501__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_take_while_fun20501(kk_function_t _fself, kk_std_core__list _ctail_16837, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_take_while_fun20501(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_take_while_fun20501, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_take_while_fun20501(kk_function_t _fself, kk_std_core__list _ctail_16837, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16837;
}

kk_std_core__list kk_std_core_take_while(kk_std_core__list xs1, kk_function_t predicate3, kk_context_t* _ctx) { /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */ 
  bool _match_18997 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_18997) {
    kk_std_core_types__ctail _x20500 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_take_while(xs1, predicate3, _x20500, _ctx);
  }
  {
    return kk_std_core__ctailm_take_while(xs1, predicate3, kk_std_core_new_take_while_fun20501(_ctx), _ctx);
  }
}
 
// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.


// lift anonymous function
struct kk_std_core_trace_fun20505__t {
  struct kk_function_s _base;
  kk_string_t message0;
};
static kk_box_t kk_std_core_trace_fun20505(kk_function_t _fself, kk_box_t _b_18728, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_trace_fun20505(kk_string_t message0, kk_context_t* _ctx) {
  struct kk_std_core_trace_fun20505__t* _self = kk_function_alloc_as(struct kk_std_core_trace_fun20505__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_trace_fun20505, kk_context());
  _self->message0 = message0;
  return &_self->_base;
}

static kk_box_t kk_std_core_trace_fun20505(kk_function_t _fself, kk_box_t _b_18728, kk_context_t* _ctx) {
  struct kk_std_core_trace_fun20505__t* _self = kk_function_as(struct kk_std_core_trace_fun20505__t*, _fself);
  kk_string_t message0 = _self->message0; /* string */
  kk_drop_match(_self, {kk_string_dup(message0);}, {}, _ctx)
  kk_unit_t _x20506 = kk_Unit;
  bool _y_18730_17083 = kk_bool_unbox(_b_18728); /*bool*/;
  if (_y_18730_17083) {
    kk_std_core_xtrace(message0, _ctx);
  }
  else {
    kk_string_drop(message0, _ctx);
    kk_Unit;
  }
  return kk_unit_box(_x20506);
}

kk_unit_t kk_std_core_trace(kk_string_t message0, kk_context_t* _ctx) { /* (message : string) -> () */ 
  bool x_17512;
  kk_box_t _x20502;
  kk_ref_t _x20503 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  _x20502 = kk_ref_get(_x20503,kk_context()); /*184*/
  x_17512 = kk_bool_unbox(_x20502); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20504 = kk_std_core_hnd_yield_extend(kk_std_core_new_trace_fun20505(message0, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20504); return kk_Unit;
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
struct kk_std_core_trace_any_fun20510__t {
  struct kk_function_s _base;
  kk_string_t message0;
  kk_box_t x;
};
static kk_box_t kk_std_core_trace_any_fun20510(kk_function_t _fself, kk_box_t _b_18735, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_trace_any_fun20510(kk_string_t message0, kk_box_t x, kk_context_t* _ctx) {
  struct kk_std_core_trace_any_fun20510__t* _self = kk_function_alloc_as(struct kk_std_core_trace_any_fun20510__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_trace_any_fun20510, kk_context());
  _self->message0 = message0;
  _self->x = x;
  return &_self->_base;
}

static kk_box_t kk_std_core_trace_any_fun20510(kk_function_t _fself, kk_box_t _b_18735, kk_context_t* _ctx) {
  struct kk_std_core_trace_any_fun20510__t* _self = kk_function_as(struct kk_std_core_trace_any_fun20510__t*, _fself);
  kk_string_t message0 = _self->message0; /* string */
  kk_box_t x = _self->x; /* 15945 */
  kk_drop_match(_self, {kk_string_dup(message0);kk_box_dup(x);}, {}, _ctx)
  kk_unit_t _x20511 = kk_Unit;
  bool _y_18737_17085 = kk_bool_unbox(_b_18735); /*bool*/;
  if (_y_18737_17085) {
    kk_std_core_xtrace_any(message0, x, _ctx);
  }
  else {
    kk_string_drop(message0, _ctx);
    kk_box_drop(x, _ctx);
    kk_Unit;
  }
  return kk_unit_box(_x20511);
}

kk_unit_t kk_std_core_trace_any(kk_string_t message0, kk_box_t x, kk_context_t* _ctx) { /* forall<a> (message : string, x : a) -> () */ 
  bool x0_17516;
  kk_box_t _x20507;
  kk_ref_t _x20508 = kk_ref_dup(kk_std_core_trace_enabled); /*ref<global,bool>*/
  _x20507 = kk_ref_get(_x20508,kk_context()); /*184*/
  x0_17516 = kk_bool_unbox(_x20507); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20509 = kk_std_core_hnd_yield_extend(kk_std_core_new_trace_any_fun20510(message0, x, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20509); return kk_Unit;
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
  kk_std_core__sslice _x20512;
  kk_std_core__sslice _x20513;
  kk_std_core__sslice slice0 = kk_std_core_first1(s, _ctx); /*sslice*/;
  bool _match_18992;
  kk_integer_t _x20514;
  kk_std_core_types__optional _match_18994 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_18994)) {
    kk_box_t _box_x18739 = _match_18994._cons.Optional.value;
    kk_integer_t _n_9710 = kk_integer_unbox(_box_x18739);
    kk_integer_dup(_n_9710);
    kk_std_core_types__optional_drop(_match_18994, _ctx);
    _x20514 = _n_9710; /*int*/
    goto _match20515;
  }
  {
    _x20514 = kk_integer_from_small(1); /*int*/
  }
  _match20515: ;
  _match_18992 = kk_integer_eq(_x20514,(kk_integer_from_small(1)),kk_context()); /*bool*/
  if (_match_18992) {
    _x20513 = slice0; /*sslice*/
  }
  else {
    kk_integer_t _x20517;
    kk_integer_t _x20518;
    kk_std_core_types__optional _match_18993 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_18993)) {
      kk_box_t _box_x18740 = _match_18993._cons.Optional.value;
      kk_integer_t _n_97100 = kk_integer_unbox(_box_x18740);
      kk_integer_dup(_n_97100);
      kk_std_core_types__optional_drop(_match_18993, _ctx);
      _x20518 = _n_97100; /*int*/
      goto _match20519;
    }
    {
      _x20518 = kk_integer_from_small(1); /*int*/
    }
    _match20519: ;
    _x20517 = kk_integer_sub(_x20518,(kk_integer_from_small(1)),kk_context()); /*int*/
    _x20513 = kk_std_core_extend(slice0, _x20517, _ctx); /*sslice*/
  }
  kk_integer_t _x20521 = kk_integer_sub(count,(kk_integer_from_small(1)),kk_context()); /*int*/
  _x20512 = kk_std_core_extend(_x20513, _x20521, _ctx); /*sslice*/
  return kk_std_core_string_3(_x20512, _ctx);
}
 
// Return a default value when an exception is raised


// lift anonymous function
struct kk_std_core_try_default_fun20524__t {
  struct kk_function_s _base;
  kk_box_t value;
};
static kk_box_t kk_std_core_try_default_fun20524(kk_function_t _fself, kk_std_core_hnd__marker _b_18742, kk_std_core_hnd__ev _b_18743, kk_box_t _b_18744, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20524(kk_box_t value, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20524__t* _self = kk_function_alloc_as(struct kk_std_core_try_default_fun20524__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_default_fun20524, kk_context());
  _self->value = value;
  return &_self->_base;
}



// lift anonymous function
struct kk_std_core_try_default_fun20525__t {
  struct kk_function_s _base;
  kk_box_t value;
};
static kk_box_t kk_std_core_try_default_fun20525(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20525(kk_box_t value, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20525__t* _self = kk_function_alloc_as(struct kk_std_core_try_default_fun20525__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_try_default_fun20525, kk_context());
  _self->value = value;
  return &_self->_base;
}

static kk_box_t kk_std_core_try_default_fun20525(kk_function_t _fself, kk_function_t ___wildcard__585__45, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20525__t* _self = kk_function_as(struct kk_std_core_try_default_fun20525__t*, _fself);
  kk_box_t value = _self->value; /* 16021 */
  kk_drop_match(_self, {kk_box_dup(value);}, {}, _ctx)
  kk_function_drop(___wildcard__585__45, _ctx);
  return value;
}
static kk_box_t kk_std_core_try_default_fun20524(kk_function_t _fself, kk_std_core_hnd__marker _b_18742, kk_std_core_hnd__ev _b_18743, kk_box_t _b_18744, kk_context_t* _ctx) {
  struct kk_std_core_try_default_fun20524__t* _self = kk_function_as(struct kk_std_core_try_default_fun20524__t*, _fself);
  kk_box_t value = _self->value; /* 16021 */
  kk_drop_match(_self, {kk_box_dup(value);}, {}, _ctx)
  kk_std_core_hnd__ev_dropn(_b_18743, ((int32_t)KI32(3)), _ctx);
  kk_box_drop(_b_18744, _ctx);
  return kk_std_core_hnd_yield_to_final(_b_18742, kk_std_core_new_try_default_fun20525(value, _ctx), _ctx);
}


// lift anonymous function
struct kk_std_core_try_default_fun20526__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_core_try_default_fun20526(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_try_default_fun20526(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_try_default_fun20526, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_core_try_default_fun20526(kk_function_t _fself, kk_box_t _x, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _x;
}

kk_box_t kk_std_core_try_default(kk_box_t value, kk_function_t action, kk_context_t* _ctx) { /* forall<a,e> (value : a, action : () -> <exn|e> a) -> e a */ 
  kk_std_core__hnd_exn _x20522;
  kk_std_core_hnd__clause1 _x20523 = kk_std_core_hnd__new_Clause1(kk_std_core_new_try_default_fun20524(value, _ctx), _ctx); /*std/core/hnd/clause1<51,52,53,54,55>*/
  _x20522 = kk_std_core__new_Hnd_exn(kk_reuse_null, _x20523, _ctx); /*.hnd-exn<11,12>*/
  return kk_std_core__handle_exn(((int32_t)KI32(0)), _x20522, kk_std_core_new_try_default_fun20526(_ctx), action, _ctx);
}
 
// Returns a unique integer (modulo 32-bits).


// lift anonymous function
struct kk_std_core_unique_fun20531__t {
  struct kk_function_s _base;
};
static kk_integer_t kk_std_core_unique_fun20531(kk_function_t _fself, kk_integer_t u, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_unique_fun20531(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_unique_fun20531, _ctx)
  return kk_function_dup(_fself);
}

static kk_integer_t kk_std_core_unique_fun20531(kk_function_t _fself, kk_integer_t u, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_integer_t _b_18758_18756;
  kk_integer_t _x20532 = kk_integer_dup(u); /*int*/
  _b_18758_18756 = kk_integer_add(_x20532,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_unit_t __ = kk_Unit;
  kk_ref_t _x20533 = kk_ref_dup(kk_std_core_unique_count); /*ref<global,int>*/
  kk_ref_set(_x20533,(kk_integer_box(_b_18758_18756)),kk_context());
  return u;
}


// lift anonymous function
struct kk_std_core_unique_fun20535__t {
  struct kk_function_s _base;
  kk_function_t next0_17523;
};
static kk_box_t kk_std_core_unique_fun20535(kk_function_t _fself, kk_box_t _b_18760, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_unique_fun20535(kk_function_t next0_17523, kk_context_t* _ctx) {
  struct kk_std_core_unique_fun20535__t* _self = kk_function_alloc_as(struct kk_std_core_unique_fun20535__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_unique_fun20535, kk_context());
  _self->next0_17523 = next0_17523;
  return &_self->_base;
}

static kk_box_t kk_std_core_unique_fun20535(kk_function_t _fself, kk_box_t _b_18760, kk_context_t* _ctx) {
  struct kk_std_core_unique_fun20535__t* _self = kk_function_as(struct kk_std_core_unique_fun20535__t*, _fself);
  kk_function_t next0_17523 = _self->next0_17523; /* (int) -> <read<global>,write<global>|_16082> int */
  kk_drop_match(_self, {kk_function_dup(next0_17523);}, {}, _ctx)
  kk_integer_t _x20536;
  kk_integer_t _x20537 = kk_integer_unbox(_b_18760); /*int*/
  _x20536 = kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17523, (next0_17523, _x20537, _ctx)); /*int*/
  return kk_integer_box(_x20536);
}

kk_integer_t kk_std_core_unique(kk_context_t* _ctx) { /* () -> ndet int */ 
  kk_integer_t x_17522;
  kk_box_t _x20529;
  kk_ref_t _x20530 = kk_ref_dup(kk_std_core_unique_count); /*ref<global,int>*/
  _x20529 = kk_ref_get(_x20530,kk_context()); /*184*/
  x_17522 = kk_integer_unbox(_x20529); /*int*/
  kk_function_t next0_17523 = kk_std_core_new_unique_fun20531(_ctx); /*(int) -> <read<global>,write<global>|_16082> int*/;
  if (kk_yielding(kk_context())) {
    kk_integer_drop(x_17522, _ctx);
    kk_box_t _x20534 = kk_std_core_hnd_yield_extend(kk_std_core_new_unique_fun20535(next0_17523, _ctx), _ctx); /*3860*/
    return kk_integer_unbox(_x20534);
  }
  {
    return kk_function_call(kk_integer_t, (kk_function_t, kk_integer_t, kk_context_t*), next0_17523, (next0_17523, x_17522, _ctx));
  }
}
 
// Get the value of the `Just` constructor or raise an exception

kk_box_t kk_std_core_unjust(kk_std_core_types__maybe m, kk_context_t* _ctx) { /* forall<a> (m : maybe<a>) -> exn a */ 
  if (kk_std_core_types__is_Just(m)) {
    kk_box_t x = m._cons.Just.value;
    return x;
  }
  {
    kk_std_core_hnd__ev ev_17525;
    kk_ssize_t _x20538 = ((kk_ssize_t)0); /*ssize_t*/
    ev_17525 = kk_evv_at(_x20538,kk_context()); /*std/core/hnd/ev<.hnd-exn>*/
    {
      struct kk_std_core_hnd_Ev* _con20539 = kk_std_core_hnd__as_Ev(ev_17525);
      kk_std_core_hnd__marker m0 = _con20539->marker;
      kk_box_t _box_x18762 = _con20539->hnd;
      kk_std_core__hnd_exn h = kk_std_core__hnd_exn_unbox(_box_x18762, NULL);
      kk_std_core__hnd_exn_dup(h);
      kk_std_core_hnd__clause1 _match_18989 = kk_std_core__select_throw_exn(h, _ctx); /*std/core/hnd/clause1<exception,1933,.hnd-exn,1934,1935>*/;
      {
        kk_function_t _fun_unbox_x18766 = _match_18989.clause;
        kk_box_t _x20541;
        kk_std_core__exception _x20542;
        kk_string_t _x20543;
        kk_define_string_literal(, _s20544, 37, "unexpected Nothing in std/core/unjust")
        _x20543 = kk_string_dup(_s20544); /*string*/
        kk_std_core__exception_info _x20545;
        kk_std_core_types__optional _match_18990 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
        if (kk_std_core_types__is_Optional(_match_18990)) {
          kk_box_t _box_x18770 = _match_18990._cons.Optional.value;
          kk_std_core__exception_info _info_11284 = kk_std_core__exception_info_unbox(_box_x18770, NULL);
          kk_std_core__exception_info_dup(_info_11284);
          kk_std_core_types__optional_drop(_match_18990, _ctx);
          _x20545 = _info_11284; /*exception-info*/
          goto _match20546;
        }
        {
          _x20545 = kk_std_core__new_ExnError(_ctx); /*exception-info*/
        }
        _match20546: ;
        _x20542 = kk_std_core__new_Exception(_x20543, _x20545, _ctx); /*exception*/
        _x20541 = kk_std_core__exception_box(_x20542, _ctx); /*51*/
        return kk_function_call(kk_box_t, (kk_function_t, kk_std_core_hnd__marker, kk_std_core_hnd__ev, kk_box_t, kk_context_t*), _fun_unbox_x18766, (_fun_unbox_x18766, m0, ev_17525, _x20541, _ctx));
      }
    }
  }
}
 
// lifted

kk_string_t kk_std_core__lift16755_unlines(kk_std_core__list ys, kk_string_t acc, kk_context_t* _ctx) { /* (ys : list<string>, acc : string) -> string */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ys)) {
    struct kk_std_core_Cons* _con20548 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18771 = _con20548->head;
    kk_std_core__list yy = _con20548->tail;
    kk_string_t y = kk_string_unbox(_box_x18771);
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_std_core__list_free(ys);
    }
    else {
      kk_string_dup(y);
      kk_std_core__list_dup(yy);
      kk_std_core__list_decref(ys, _ctx);
    }
    { // tailcall
      kk_string_t _x20550;
      kk_string_t _x20551;
      kk_string_t _x20552;
      kk_define_string_literal(, _s20553, 1, "\n")
      _x20552 = kk_string_dup(_s20553); /*string*/
      _x20551 = kk_std_core__lp__plus__plus__1_rp_(_x20552, y, _ctx); /*string*/
      _x20550 = kk_std_core__lp__plus__plus__1_rp_(acc, _x20551, _ctx); /*string*/
      ys = yy;
      acc = _x20550;
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
    struct kk_std_core_Cons* _con20555 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x18772 = _con20555->head;
    kk_std_core__list xx = _con20555->tail;
    kk_string_t x = kk_string_unbox(_box_x18772);
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
    struct kk_std_core_Cons* _con20557 = kk_std_core__as_Cons(ys0);
    kk_box_t x0 = _con20557->head;
    kk_std_core__list xx0 = _con20557->tail;
    kk_reuse_t _ru_18966 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys0))) {
      _ru_18966 = (kk_std_core__list_reuse(ys0));
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(ys0, _ctx);
      _ru_18966 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20558;
      if (kk_likely(_ru_18966!=NULL)) {
        struct kk_std_core_Cons* _con20559 = (struct kk_std_core_Cons*)_ru_18966;
        _con20559->tail = acc;
        _x20558 = kk_std_core__base_Cons(_con20559); /*list<61>*/
      }
      else {
        _x20558 = kk_std_core__new_Cons(kk_reuse_null, x0, acc, _ctx); /*list<61>*/
      }
      acc = _x20558;
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
    struct kk_std_core_Cons* _con20560 = kk_std_core__as_Cons(ys1);
    kk_box_t x1 = _con20560->head;
    kk_std_core__list xx1 = _con20560->tail;
    kk_reuse_t _ru_18967 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys1))) {
      _ru_18967 = (kk_std_core__list_reuse(ys1));
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(ys1, _ctx);
      _ru_18967 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20561;
      if (kk_likely(_ru_18967!=NULL)) {
        struct kk_std_core_Cons* _con20562 = (struct kk_std_core_Cons*)_ru_18967;
        _con20562->tail = acc0;
        _x20561 = kk_std_core__base_Cons(_con20562); /*list<61>*/
      }
      else {
        _x20561 = kk_std_core__new_Cons(kk_reuse_null, x1, acc0, _ctx); /*list<61>*/
      }
      acc0 = _x20561;
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
    struct kk_std_core_Cons* _con20563 = kk_std_core__as_Cons(ys);
    kk_box_t _box_x18773 = _con20563->head;
    kk_std_core__list xx = _con20563->tail;
    kk_std_core_types__tuple2_ _pat0 = kk_std_core_types__tuple2__unbox(_box_x18773, NULL);
    kk_box_t x = _pat0.fst;
    kk_box_t y = _pat0.snd;
    kk_reuse_t _ru_18968 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ys))) {
      kk_box_dup(x);
      kk_box_dup(y);
      kk_box_drop(_box_x18773, _ctx);
      _ru_18968 = (kk_std_core__list_reuse(ys));
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_box_dup(y);
      kk_std_core__list_decref(ys, _ctx);
      _ru_18968 = kk_reuse_null;
    }
    { // tailcall
      kk_std_core__list _x20565 = kk_std_core__new_Cons(_ru_18968, x, acc1, _ctx); /*list<61>*/
      kk_std_core__list _x20566 = kk_std_core__new_Cons(kk_reuse_null, y, acc2, _ctx); /*list<61>*/
      ys = xx;
      acc1 = _x20565;
      acc2 = _x20566;
      goto kk__tailcall;
    }
  }
  {
    kk_std_core__list _b_18776_18774 = kk_std_core__lift16757_unzip(kk_std_core__new_Nil(_ctx), acc1, _ctx); /*list<16150>*/;
    kk_std_core__list _b_18777_18775 = kk_std_core__lift16758_unzip(kk_std_core__new_Nil(_ctx), acc2, _ctx); /*list<16151>*/;
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_std_core__list_box(_b_18776_18774, _ctx), kk_std_core__list_box(_b_18777_18775, _ctx), _ctx);
  }
}
 
// Convert a string to a vector of characters.

kk_vector_t kk_std_core_vector_1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> vector<char> */ 
  return kk_string_to_chars(s,kk_context());
}
extern kk_box_t kk_std_core_vector_fun20568_2(kk_function_t _fself, kk_ssize_t ___wildcard__2100__30, kk_context_t* _ctx) {
  struct kk_std_core_vector_fun20568__t_2* _self = kk_function_as(struct kk_std_core_vector_fun20568__t_2*, _fself);
  kk_box_t default0 = _self->default0; /* 16227 */
  kk_drop_match(_self, {kk_box_dup(default0);}, {}, _ctx)
  return default0;
}
extern kk_box_t kk_std_core_vector_init_fun20570(kk_function_t _fself, kk_ssize_t i, kk_context_t* _ctx) {
  struct kk_std_core_vector_init_fun20570__t* _self = kk_function_as(struct kk_std_core_vector_init_fun20570__t*, _fself);
  kk_function_t f = _self->f; /* (int) -> 16296 */
  kk_drop_match(_self, {kk_function_dup(f);}, {}, _ctx)
  kk_integer_t _x20571 = kk_integer_from_ssize_t(i,kk_context()); /*int*/
  return kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_context_t*), f, (f, _x20571, _ctx));
}
 
// monadic lift

kk_unit_t kk_std_core__mlift17193_while(kk_function_t action, kk_function_t predicate, kk_unit_t wild__, kk_context_t* _ctx) { /* forall<e> (action : () -> <div|e> (), predicate : () -> <div|e> bool, wild_ : ()) -> <div|e> () */ 
  kk_std_core_while(predicate, action, _ctx); return kk_Unit;
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17194_while_fun20574__t {
  struct kk_function_s _base;
  kk_function_t action0;
  kk_function_t predicate0;
};
static kk_box_t kk_std_core__mlift17194_while_fun20574(kk_function_t _fself, kk_box_t _b_18779, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17194_while_fun20574(kk_function_t action0, kk_function_t predicate0, kk_context_t* _ctx) {
  struct kk_std_core__mlift17194_while_fun20574__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17194_while_fun20574__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17194_while_fun20574, kk_context());
  _self->action0 = action0;
  _self->predicate0 = predicate0;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17194_while_fun20574(kk_function_t _fself, kk_box_t _b_18779, kk_context_t* _ctx) {
  struct kk_std_core__mlift17194_while_fun20574__t* _self = kk_function_as(struct kk_std_core__mlift17194_while_fun20574__t*, _fself);
  kk_function_t action0 = _self->action0; /* () -> <div|16317> () */
  kk_function_t predicate0 = _self->predicate0; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action0);kk_function_dup(predicate0);}, {}, _ctx)
  kk_unit_t _x20575 = kk_Unit;
  kk_unit_t _x20576 = kk_Unit;
  kk_unit_unbox(_b_18779);
  kk_std_core__mlift17193_while(action0, predicate0, _x20576, _ctx);
  return kk_unit_box(_x20575);
}

kk_unit_t kk_std_core__mlift17194_while(kk_function_t action0, kk_function_t predicate0, bool _y_17091, kk_context_t* _ctx) { /* forall<e> (action : () -> <div|e> (), predicate : () -> <div|e> bool, bool) -> <div|e> () */ 
  if (_y_17091) {
    kk_unit_t x_17528 = kk_Unit;
    kk_function_t _x20572 = kk_function_dup(action0); /*() -> <div|16317> ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), _x20572, (_x20572, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20573 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17194_while_fun20574(action0, predicate0, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20573); return kk_Unit;
    }
    {
      kk_std_core__mlift17193_while(action0, predicate0, x_17528, _ctx); return kk_Unit;
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
struct kk_std_core_while_fun20579__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_function_t predicate1;
};
static kk_box_t kk_std_core_while_fun20579(kk_function_t _fself, kk_box_t _b_18783, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_while_fun20579(kk_function_t action1, kk_function_t predicate1, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20579__t* _self = kk_function_alloc_as(struct kk_std_core_while_fun20579__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_while_fun20579, kk_context());
  _self->action1 = action1;
  _self->predicate1 = predicate1;
  return &_self->_base;
}

static kk_box_t kk_std_core_while_fun20579(kk_function_t _fself, kk_box_t _b_18783, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20579__t* _self = kk_function_as(struct kk_std_core_while_fun20579__t*, _fself);
  kk_function_t action1 = _self->action1; /* () -> <div|16317> () */
  kk_function_t predicate1 = _self->predicate1; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action1);kk_function_dup(predicate1);}, {}, _ctx)
  kk_unit_t _x20580 = kk_Unit;
  bool _x20581 = kk_bool_unbox(_b_18783); /*bool*/
  kk_std_core__mlift17194_while(action1, predicate1, _x20581, _ctx);
  return kk_unit_box(_x20580);
}


// lift anonymous function
struct kk_std_core_while_fun20584__t {
  struct kk_function_s _base;
  kk_function_t action1;
  kk_function_t predicate1;
};
static kk_box_t kk_std_core_while_fun20584(kk_function_t _fself, kk_box_t _b_18785, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_while_fun20584(kk_function_t action1, kk_function_t predicate1, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20584__t* _self = kk_function_alloc_as(struct kk_std_core_while_fun20584__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_while_fun20584, kk_context());
  _self->action1 = action1;
  _self->predicate1 = predicate1;
  return &_self->_base;
}

static kk_box_t kk_std_core_while_fun20584(kk_function_t _fself, kk_box_t _b_18785, kk_context_t* _ctx) {
  struct kk_std_core_while_fun20584__t* _self = kk_function_as(struct kk_std_core_while_fun20584__t*, _fself);
  kk_function_t action1 = _self->action1; /* () -> <div|16317> () */
  kk_function_t predicate1 = _self->predicate1; /* () -> <div|16317> bool */
  kk_drop_match(_self, {kk_function_dup(action1);kk_function_dup(predicate1);}, {}, _ctx)
  kk_unit_t _x20585 = kk_Unit;
  kk_unit_t _x20586 = kk_Unit;
  kk_unit_unbox(_b_18785);
  kk_std_core__mlift17193_while(action1, predicate1, _x20586, _ctx);
  return kk_unit_box(_x20585);
}

kk_unit_t kk_std_core_while(kk_function_t predicate1, kk_function_t action1, kk_context_t* _ctx) { /* forall<e> (predicate : () -> <div|e> bool, action : () -> <div|e> ()) -> <div|e> () */ 
  kk__tailcall: ;
  bool x0_17530;
  kk_function_t _x20577 = kk_function_dup(predicate1); /*() -> <div|16317> bool*/
  x0_17530 = kk_function_call(bool, (kk_function_t, kk_context_t*), _x20577, (_x20577, _ctx)); /*bool*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x20578 = kk_std_core_hnd_yield_extend(kk_std_core_new_while_fun20579(action1, predicate1, _ctx), _ctx); /*3860*/
    kk_unit_unbox(_x20578); return kk_Unit;
  }
  if (x0_17530) {
    kk_unit_t x1_17533 = kk_Unit;
    kk_function_t _x20582 = kk_function_dup(action1); /*() -> <div|16317> ()*/
    kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), _x20582, (_x20582, _ctx));
    if (kk_yielding(kk_context())) {
      kk_box_t _x20583 = kk_std_core_hnd_yield_extend(kk_std_core_new_while_fun20584(action1, predicate1, _ctx), _ctx); /*3860*/
      kk_unit_unbox(_x20583); return kk_Unit;
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
    struct kk_std_core_Cons* _con20587 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20587->head;
    kk_std_core__list xx = _con20587->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20588 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20588->head;
      kk_std_core__list yy = _con20588->tail;
      kk_reuse_t _ru_18970 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18970 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18970 = kk_reuse_null;
      }
      kk_std_core__list _ctail_16839 = kk_std_core__list_hole(); /*list<(16360, 16361)>*/;
      kk_std_core__list _ctail_16840;
      kk_box_t _x20589;
      kk_std_core_types__tuple2_ _x20590 = kk_std_core_types__new_dash__lp__comma__rp_(x, y, _ctx); /*(6, 7)*/
      _x20589 = kk_std_core_types__tuple2__box(_x20590, _ctx); /*61*/
      _ctail_16840 = kk_std_core__new_Cons(_ru_18970, _x20589, _ctail_16839, _ctx); /*list<(16360, 16361)>*/
      { // tailcall
        kk_std_core_types__ctail _x20591;
        kk_box_t* _b_18806_18799 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16840)->tail)); /*cfield<list<(16360, 16361)>>*/;
        _x20591 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16840, _ctx)),_b_18806_18799); /*ctail<0>*/
        xs = xx;
        ys = yy;
        _acc = _x20591;
        goto kk__tailcall;
      }
    }
    {
      kk_box_drop(x, _ctx);
      kk_std_core__list_drop(xx, _ctx);
      kk_box_t _x20592 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20592, _ctx);
    }
  }
  {
    kk_std_core__list_drop(ys, _ctx);
    kk_box_t _x20593 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20593, _ctx);
  }
}
 
// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.

kk_std_core__list kk_std_core_zip(kk_std_core__list xs0, kk_std_core__list ys0, kk_context_t* _ctx) { /* forall<a,b> (xs : list<a>, ys : list<b>) -> list<(a, b)> */ 
  kk_std_core_types__ctail _x20594 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_core__ctail_zip(xs0, ys0, _x20594, _ctx);
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17195_op(kk_std_core_types__ctail _acc, kk_function_t f, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _ctail_16841, kk_context_t* _ctx) { /* forall<a,b,c,e> (ctail<list<c>>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  kk_std_core__list _ctail_16842 = kk_std_core__list_hole(); /*list<16408>*/;
  kk_std_core__list _ctail_16843 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16841, _ctail_16842, _ctx); /*list<16408>*/;
  kk_std_core_types__ctail _x20595;
  kk_box_t* _b_18822_18819 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_16843)->tail)); /*cfield<list<16408>>*/;
  _x20595 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_16843, _ctx)),_b_18822_18819); /*ctail<0>*/
  return kk_std_core__ctail_zipwith(xx, yy, f, _x20595, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17196_op_fun20596__t {
  struct kk_function_s _base;
  kk_function_t _accm;
  kk_box_t _ctail_16846;
};
static kk_std_core__list kk_std_core__mlift17196_op_fun20596(kk_function_t _fself, kk_std_core__list _ctail_16845, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17196_op_fun20596(kk_function_t _accm, kk_box_t _ctail_16846, kk_context_t* _ctx) {
  struct kk_std_core__mlift17196_op_fun20596__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17196_op_fun20596__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17196_op_fun20596, kk_context());
  _self->_accm = _accm;
  _self->_ctail_16846 = _ctail_16846;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__mlift17196_op_fun20596(kk_function_t _fself, kk_std_core__list _ctail_16845, kk_context_t* _ctx) {
  struct kk_std_core__mlift17196_op_fun20596__t* _self = kk_function_as(struct kk_std_core__mlift17196_op_fun20596__t*, _fself);
  kk_function_t _accm = _self->_accm; /* (list<16408>) -> list<16408> */
  kk_box_t _ctail_16846 = _self->_ctail_16846; /* 16408 */
  kk_drop_match(_self, {kk_function_dup(_accm);kk_box_dup(_ctail_16846);}, {}, _ctx)
  kk_std_core__list _x20597 = kk_std_core__new_Cons(kk_reuse_null, _ctail_16846, _ctail_16845, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm, (_accm, _x20597, _ctx));
}

kk_std_core__list kk_std_core__mlift17196_op(kk_function_t _accm, kk_function_t f0, kk_std_core__list xx0, kk_std_core__list yy0, kk_box_t _ctail_16846, kk_context_t* _ctx) { /* forall<a,b,c,e> ((list<c>) -> list<c>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  return kk_std_core__ctailm_zipwith(xx0, yy0, f0, kk_std_core__new_mlift17196_op_fun20596(_accm, _ctail_16846, _ctx), _ctx);
}
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.


// lift anonymous function
struct kk_std_core__ctail_zipwith_fun20602__t {
  struct kk_function_s _base;
  kk_function_t f1;
  kk_std_core__list xx1;
  kk_std_core__list yy1;
  kk_std_core_types__ctail _acc0;
};
static kk_box_t kk_std_core__ctail_zipwith_fun20602(kk_function_t _fself, kk_box_t _b_18827, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctail_zipwith_fun20602(kk_function_t f1, kk_std_core__list xx1, kk_std_core__list yy1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) {
  struct kk_std_core__ctail_zipwith_fun20602__t* _self = kk_function_alloc_as(struct kk_std_core__ctail_zipwith_fun20602__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctail_zipwith_fun20602, kk_context());
  _self->f1 = f1;
  _self->xx1 = xx1;
  _self->yy1 = yy1;
  _self->_acc0 = _acc0;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctail_zipwith_fun20602(kk_function_t _fself, kk_box_t _b_18827, kk_context_t* _ctx) {
  struct kk_std_core__ctail_zipwith_fun20602__t* _self = kk_function_as(struct kk_std_core__ctail_zipwith_fun20602__t*, _fself);
  kk_function_t f1 = _self->f1; /* (16406, 16407) -> 16409 16408 */
  kk_std_core__list xx1 = _self->xx1; /* list<16406> */
  kk_std_core__list yy1 = _self->yy1; /* list<16407> */
  kk_std_core_types__ctail _acc0 = _self->_acc0; /* ctail<list<16408>> */
  kk_drop_match(_self, {kk_function_dup(f1);kk_std_core__list_dup(xx1);kk_std_core__list_dup(yy1);kk_std_core_types__ctail_dup(_acc0);}, {}, _ctx)
  kk_std_core__list _x20603 = kk_std_core__mlift17195_op(_acc0, f1, xx1, yy1, _b_18827, _ctx); /*list<16408>*/
  return kk_std_core__list_box(_x20603, _ctx);
}

kk_std_core__list kk_std_core__ctail_zipwith(kk_std_core__list xs, kk_std_core__list ys, kk_function_t f1, kk_std_core_types__ctail _acc0, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, ctail<list<c>>) -> e list<c> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20598 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20598->head;
    kk_std_core__list xx1 = _con20598->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x);
      kk_std_core__list_dup(xx1);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20599 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20599->head;
      kk_std_core__list yy1 = _con20599->tail;
      kk_reuse_t _ru_18972 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18972 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy1);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18972 = kk_reuse_null;
      }
      kk_box_t x0_17536;
      kk_function_t _x20600 = kk_function_dup(f1); /*(16406, 16407) -> 16409 16408*/
      x0_17536 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20600, (_x20600, x, y, _ctx)); /*16408*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18972, _ctx);
        kk_box_drop(x0_17536, _ctx);
        kk_box_t _x20601 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctail_zipwith_fun20602(f1, xx1, yy1, _acc0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20601, _ctx);
      }
      {
        kk_std_core__list _ctail_168420 = kk_std_core__list_hole(); /*list<16408>*/;
        kk_std_core__list _ctail_168430 = kk_std_core__new_Cons(_ru_18972, x0_17536, _ctail_168420, _ctx); /*list<16408>*/;
        { // tailcall
          kk_std_core_types__ctail _x20604;
          kk_box_t* _b_18841_18833 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_168430)->tail)); /*cfield<list<16408>>*/;
          _x20604 = kk_ctail_link(_acc0,(kk_std_core__list_box(_ctail_168430, _ctx)),_b_18841_18833); /*ctail<0>*/
          xs = xx1;
          ys = yy1;
          _acc0 = _x20604;
          goto kk__tailcall;
        }
      }
    }
    {
      kk_function_drop(f1, _ctx);
      kk_box_drop(x, _ctx);
      kk_std_core__list_drop(xx1, _ctx);
      kk_box_t _x20605 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
      return kk_std_core__list_unbox(_x20605, _ctx);
    }
  }
  {
    kk_function_drop(f1, _ctx);
    kk_std_core__list_drop(ys, _ctx);
    kk_box_t _x20606 = kk_ctail_resolve(_acc0,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x20606, _ctx);
  }
}
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.


// lift anonymous function
struct kk_std_core__ctailm_zipwith_fun20611__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_function_t f2;
  kk_std_core__list xx2;
  kk_std_core__list yy2;
};
static kk_box_t kk_std_core__ctailm_zipwith_fun20611(kk_function_t _fself, kk_box_t _b_18851, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_zipwith_fun20611(kk_function_t _accm0, kk_function_t f2, kk_std_core__list xx2, kk_std_core__list yy2, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20611__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_zipwith_fun20611__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_zipwith_fun20611, kk_context());
  _self->_accm0 = _accm0;
  _self->f2 = f2;
  _self->xx2 = xx2;
  _self->yy2 = yy2;
  return &_self->_base;
}

static kk_box_t kk_std_core__ctailm_zipwith_fun20611(kk_function_t _fself, kk_box_t _b_18851, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20611__t* _self = kk_function_as(struct kk_std_core__ctailm_zipwith_fun20611__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<16408>) -> list<16408> */
  kk_function_t f2 = _self->f2; /* (16406, 16407) -> 16409 16408 */
  kk_std_core__list xx2 = _self->xx2; /* list<16406> */
  kk_std_core__list yy2 = _self->yy2; /* list<16407> */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_function_dup(f2);kk_std_core__list_dup(xx2);kk_std_core__list_dup(yy2);}, {}, _ctx)
  kk_std_core__list _x20612 = kk_std_core__mlift17196_op(_accm0, f2, xx2, yy2, _b_18851, _ctx); /*list<16408>*/
  return kk_std_core__list_box(_x20612, _ctx);
}


// lift anonymous function
struct kk_std_core__ctailm_zipwith_fun20614__t {
  struct kk_function_s _base;
  kk_function_t _accm0;
  kk_box_t x2_17539;
};
static kk_std_core__list kk_std_core__ctailm_zipwith_fun20614(kk_function_t _fself, kk_std_core__list _ctail_168450, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_ctailm_zipwith_fun20614(kk_function_t _accm0, kk_box_t x2_17539, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20614__t* _self = kk_function_alloc_as(struct kk_std_core__ctailm_zipwith_fun20614__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__ctailm_zipwith_fun20614, kk_context());
  _self->_accm0 = _accm0;
  _self->x2_17539 = x2_17539;
  return &_self->_base;
}

static kk_std_core__list kk_std_core__ctailm_zipwith_fun20614(kk_function_t _fself, kk_std_core__list _ctail_168450, kk_context_t* _ctx) {
  struct kk_std_core__ctailm_zipwith_fun20614__t* _self = kk_function_as(struct kk_std_core__ctailm_zipwith_fun20614__t*, _fself);
  kk_function_t _accm0 = _self->_accm0; /* (list<16408>) -> list<16408> */
  kk_box_t x2_17539 = _self->x2_17539; /* 16408 */
  kk_drop_match(_self, {kk_function_dup(_accm0);kk_box_dup(x2_17539);}, {}, _ctx)
  kk_std_core__list _x20615 = kk_std_core__new_Cons(kk_reuse_null, x2_17539, _ctail_168450, _ctx); /*list<61>*/
  return kk_function_call(kk_std_core__list, (kk_function_t, kk_std_core__list, kk_context_t*), _accm0, (_accm0, _x20615, _ctx));
}

kk_std_core__list kk_std_core__ctailm_zipwith(kk_std_core__list xs0, kk_std_core__list ys0, kk_function_t f2, kk_function_t _accm0, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, (list<c>) -> list<c>) -> e list<c> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs0)) {
    struct kk_std_core_Cons* _con20607 = kk_std_core__as_Cons(xs0);
    kk_box_t x1 = _con20607->head;
    kk_std_core__list xx2 = _con20607->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs0))) {
      kk_std_core__list_free(xs0);
    }
    else {
      kk_box_dup(x1);
      kk_std_core__list_dup(xx2);
      kk_std_core__list_decref(xs0, _ctx);
    }
    if (kk_std_core__is_Cons(ys0)) {
      struct kk_std_core_Cons* _con20608 = kk_std_core__as_Cons(ys0);
      kk_box_t y0 = _con20608->head;
      kk_std_core__list yy2 = _con20608->tail;
      if (kk_likely(kk_std_core__list_is_unique(ys0))) {
        kk_std_core__list_free(ys0);
      }
      else {
        kk_box_dup(y0);
        kk_std_core__list_dup(yy2);
        kk_std_core__list_decref(ys0, _ctx);
      }
      kk_box_t x2_17539;
      kk_function_t _x20609 = kk_function_dup(f2); /*(16406, 16407) -> 16409 16408*/
      x2_17539 = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_box_t, kk_context_t*), _x20609, (_x20609, x1, y0, _ctx)); /*16408*/
      if (kk_yielding(kk_context())) {
        kk_box_drop(x2_17539, _ctx);
        kk_box_t _x20610 = kk_std_core_hnd_yield_extend(kk_std_core__new_ctailm_zipwith_fun20611(_accm0, f2, xx2, yy2, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20610, _ctx);
      }
      { // tailcall
        kk_function_t _x20613 = kk_std_core__new_ctailm_zipwith_fun20614(_accm0, x2_17539, _ctx); /*(list<16408>) -> list<16408>*/
        xs0 = xx2;
        ys0 = yy2;
        _accm0 = _x20613;
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
struct kk_std_core_zipwith_fun20617__t {
  struct kk_function_s _base;
};
static kk_std_core__list kk_std_core_zipwith_fun20617(kk_function_t _fself, kk_std_core__list _ctail_16844, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_zipwith_fun20617(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_core_zipwith_fun20617, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core__list kk_std_core_zipwith_fun20617(kk_function_t _fself, kk_std_core__list _ctail_16844, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return _ctail_16844;
}

kk_std_core__list kk_std_core_zipwith(kk_std_core__list xs1, kk_std_core__list ys1, kk_function_t f3, kk_context_t* _ctx) { /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c) -> e list<c> */ 
  bool _match_18983 = kk_std_core_hnd__evv_is_affine(_ctx); /*bool*/;
  if (_match_18983) {
    kk_std_core_types__ctail _x20616 = kk_ctail_nil(); /*ctail<0>*/
    return kk_std_core__ctail_zipwith(xs1, ys1, f3, _x20616, _ctx);
  }
  {
    return kk_std_core__ctailm_zipwith(xs1, ys1, f3, kk_std_core_new_zipwith_fun20617(_ctx), _ctx);
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17197_zipwith_acc(kk_std_core__list acc, kk_function_t f, kk_integer_t i, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _y_17106, kk_context_t* _ctx) { /* forall<e,a,b,c> (acc : list<b>, f : (int, a, c) -> e b, i : int, xx : list<a>, yy : list<c>, b) -> e list<b> */ 
  kk_integer_t _x20618 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/
  kk_std_core__list _x20619 = kk_std_core__new_Cons(kk_reuse_null, _y_17106, acc, _ctx); /*list<61>*/
  return kk_std_core_zipwith_acc(f, _x20618, _x20619, xx, yy, _ctx);
}


// lift anonymous function
struct kk_std_core_zipwith_acc_fun20625__t {
  struct kk_function_s _base;
  kk_std_core__list acc0;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list xx0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core_zipwith_acc_fun20625(kk_function_t _fself, kk_box_t _b_18855, kk_context_t* _ctx);
static kk_function_t kk_std_core_new_zipwith_acc_fun20625(kk_std_core__list acc0, kk_function_t f0, kk_integer_t i0, kk_std_core__list xx0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core_zipwith_acc_fun20625__t* _self = kk_function_alloc_as(struct kk_std_core_zipwith_acc_fun20625__t, 6, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core_zipwith_acc_fun20625, kk_context());
  _self->acc0 = acc0;
  _self->f0 = f0;
  _self->i0 = i0;
  _self->xx0 = xx0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core_zipwith_acc_fun20625(kk_function_t _fself, kk_box_t _b_18855, kk_context_t* _ctx) {
  struct kk_std_core_zipwith_acc_fun20625__t* _self = kk_function_as(struct kk_std_core_zipwith_acc_fun20625__t*, _fself);
  kk_std_core__list acc0 = _self->acc0; /* list<16421> */
  kk_function_t f0 = _self->f0; /* (int, 16419, 16429) -> 16416 16421 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list xx0 = _self->xx0; /* list<16419> */
  kk_std_core__list yy0 = _self->yy0; /* list<16429> */
  kk_drop_match(_self, {kk_std_core__list_dup(acc0);kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(xx0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20626 = kk_std_core__mlift17197_zipwith_acc(acc0, f0, i0, xx0, yy0, _b_18855, _ctx); /*list<16421>*/
  return kk_std_core__list_box(_x20626, _ctx);
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
    struct kk_std_core_Cons* _con20620 = kk_std_core__as_Cons(xs);
    kk_box_t x = _con20620->head;
    kk_std_core__list xx0 = _con20620->tail;
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
      struct kk_std_core_Cons* _con20621 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20621->head;
      kk_std_core__list yy0 = _con20621->tail;
      kk_reuse_t _ru_18976 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18976 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy0);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18976 = kk_reuse_null;
      }
      kk_box_t x0_17544;
      kk_function_t _x20623 = kk_function_dup(f0); /*(int, 16419, 16429) -> 16416 16421*/
      kk_integer_t _x20622 = kk_integer_dup(i0); /*int*/
      x0_17544 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_box_t, kk_context_t*), _x20623, (_x20623, _x20622, x, y, _ctx)); /*16421*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18976, _ctx);
        kk_box_drop(x0_17544, _ctx);
        kk_box_t _x20624 = kk_std_core_hnd_yield_extend(kk_std_core_new_zipwith_acc_fun20625(acc0, f0, i0, xx0, yy0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20624, _ctx);
      }
      { // tailcall
        kk_integer_t _x20627 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/
        kk_std_core__list _x20628 = kk_std_core__new_Cons(_ru_18976, x0_17544, acc0, _ctx); /*list<61>*/
        i0 = _x20627;
        acc0 = _x20628;
        xs = xx0;
        ys = yy0;
        goto kk__tailcall;
      }
    }
  }
}
 
// monadic lift

kk_std_core__list kk_std_core__mlift17198_op(kk_box_t _y_17110, kk_std_core__list _y_17111, kk_context_t* _ctx) { /* forall<a,e> (a, list<a>) -> e list<a> */ 
  return kk_std_core__new_Cons(kk_reuse_null, _y_17110, _y_17111, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_core__mlift17199_op_fun20630__t {
  struct kk_function_s _base;
  kk_box_t _y_171100;
};
static kk_box_t kk_std_core__mlift17199_op_fun20630(kk_function_t _fself, kk_box_t _b_18859, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_mlift17199_op_fun20630(kk_box_t _y_171100, kk_context_t* _ctx) {
  struct kk_std_core__mlift17199_op_fun20630__t* _self = kk_function_alloc_as(struct kk_std_core__mlift17199_op_fun20630__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__mlift17199_op_fun20630, kk_context());
  _self->_y_171100 = _y_171100;
  return &_self->_base;
}

static kk_box_t kk_std_core__mlift17199_op_fun20630(kk_function_t _fself, kk_box_t _b_18859, kk_context_t* _ctx) {
  struct kk_std_core__mlift17199_op_fun20630__t* _self = kk_function_as(struct kk_std_core__mlift17199_op_fun20630__t*, _fself);
  kk_box_t _y_171100 = _self->_y_171100; /* 16550 */
  kk_drop_match(_self, {kk_box_dup(_y_171100);}, {}, _ctx)
  kk_std_core__list _x20631;
  kk_std_core__list _x20632 = kk_std_core__list_unbox(_b_18859, _ctx); /*list<16550>*/
  _x20631 = kk_std_core__mlift17198_op(_y_171100, _x20632, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20631, _ctx);
}

kk_std_core__list kk_std_core__mlift17199_op(kk_function_t f, kk_integer_t i, kk_std_core__list xx, kk_std_core__list yy, kk_box_t _y_171100, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (int, a, b) -> e c, i : int, xx : list<a>, yy : list<b>, c) -> e list<c> */ 
  kk_integer_t i0_16786 = kk_integer_add(i,(kk_integer_from_small(1)),kk_context()); /*int*/;
  kk_std_core__list x_17547 = kk_std_core__lift16759_zipwith_indexed(f, i0_16786, xx, yy, _ctx); /*list<16550>*/;
  if (kk_yielding(kk_context())) {
    kk_std_core__list_drop(x_17547, _ctx);
    kk_box_t _x20629 = kk_std_core_hnd_yield_extend(kk_std_core__new_mlift17199_op_fun20630(_y_171100, _ctx), _ctx); /*3860*/
    return kk_std_core__list_unbox(_x20629, _ctx);
  }
  {
    return kk_std_core__mlift17198_op(_y_171100, x_17547, _ctx);
  }
}
 
// lifted


// lift anonymous function
struct kk_std_core__lift16759_zipwith_indexed_fun20638__t {
  struct kk_function_s _base;
  kk_function_t f0;
  kk_integer_t i0;
  kk_std_core__list xx0;
  kk_std_core__list yy0;
};
static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20638(kk_function_t _fself, kk_box_t _b_18863, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16759_zipwith_indexed_fun20638(kk_function_t f0, kk_integer_t i0, kk_std_core__list xx0, kk_std_core__list yy0, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20638__t* _self = kk_function_alloc_as(struct kk_std_core__lift16759_zipwith_indexed_fun20638__t, 5, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16759_zipwith_indexed_fun20638, kk_context());
  _self->f0 = f0;
  _self->i0 = i0;
  _self->xx0 = xx0;
  _self->yy0 = yy0;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20638(kk_function_t _fself, kk_box_t _b_18863, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20638__t* _self = kk_function_as(struct kk_std_core__lift16759_zipwith_indexed_fun20638__t*, _fself);
  kk_function_t f0 = _self->f0; /* (int, 16548, 16549) -> 16551 16550 */
  kk_integer_t i0 = _self->i0; /* int */
  kk_std_core__list xx0 = _self->xx0; /* list<16548> */
  kk_std_core__list yy0 = _self->yy0; /* list<16549> */
  kk_drop_match(_self, {kk_function_dup(f0);kk_integer_dup(i0);kk_std_core__list_dup(xx0);kk_std_core__list_dup(yy0);}, {}, _ctx)
  kk_std_core__list _x20639 = kk_std_core__mlift17199_op(f0, i0, xx0, yy0, _b_18863, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20639, _ctx);
}


// lift anonymous function
struct kk_std_core__lift16759_zipwith_indexed_fun20641__t {
  struct kk_function_s _base;
  kk_box_t x1_17549;
};
static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20641(kk_function_t _fself, kk_box_t _b_18865, kk_context_t* _ctx);
static kk_function_t kk_std_core__new_lift16759_zipwith_indexed_fun20641(kk_box_t x1_17549, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20641__t* _self = kk_function_alloc_as(struct kk_std_core__lift16759_zipwith_indexed_fun20641__t, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_core__lift16759_zipwith_indexed_fun20641, kk_context());
  _self->x1_17549 = x1_17549;
  return &_self->_base;
}

static kk_box_t kk_std_core__lift16759_zipwith_indexed_fun20641(kk_function_t _fself, kk_box_t _b_18865, kk_context_t* _ctx) {
  struct kk_std_core__lift16759_zipwith_indexed_fun20641__t* _self = kk_function_as(struct kk_std_core__lift16759_zipwith_indexed_fun20641__t*, _fself);
  kk_box_t x1_17549 = _self->x1_17549; /* 16550 */
  kk_drop_match(_self, {kk_box_dup(x1_17549);}, {}, _ctx)
  kk_std_core__list _x20642;
  kk_std_core__list _x20643 = kk_std_core__list_unbox(_b_18865, _ctx); /*list<16550>*/
  _x20642 = kk_std_core__mlift17198_op(x1_17549, _x20643, _ctx); /*list<16550>*/
  return kk_std_core__list_box(_x20642, _ctx);
}

kk_std_core__list kk_std_core__lift16759_zipwith_indexed(kk_function_t f0, kk_integer_t i0, kk_std_core__list xs, kk_std_core__list ys, kk_context_t* _ctx) { /* forall<a,b,c,e> (f : (int, a, b) -> e c, i : int, xs : list<a>, ys : list<b>) -> e list<c> */ 
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con20633 = kk_std_core__as_Cons(xs);
    kk_box_t x0 = _con20633->head;
    kk_std_core__list xx0 = _con20633->tail;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      kk_std_core__list_free(xs);
    }
    else {
      kk_box_dup(x0);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs, _ctx);
    }
    if (kk_std_core__is_Cons(ys)) {
      struct kk_std_core_Cons* _con20634 = kk_std_core__as_Cons(ys);
      kk_box_t y = _con20634->head;
      kk_std_core__list yy0 = _con20634->tail;
      kk_reuse_t _ru_18978 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(ys))) {
        _ru_18978 = (kk_std_core__list_reuse(ys));
      }
      else {
        kk_box_dup(y);
        kk_std_core__list_dup(yy0);
        kk_std_core__list_decref(ys, _ctx);
        _ru_18978 = kk_reuse_null;
      }
      kk_box_t x1_17549;
      kk_function_t _x20636 = kk_function_dup(f0); /*(int, 16548, 16549) -> 16551 16550*/
      kk_integer_t _x20635 = kk_integer_dup(i0); /*int*/
      x1_17549 = kk_function_call(kk_box_t, (kk_function_t, kk_integer_t, kk_box_t, kk_box_t, kk_context_t*), _x20636, (_x20636, _x20635, x0, y, _ctx)); /*16550*/
      if (kk_yielding(kk_context())) {
        kk_reuse_drop(_ru_18978, _ctx);
        kk_box_drop(x1_17549, _ctx);
        kk_box_t _x20637 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16759_zipwith_indexed_fun20638(f0, i0, xx0, yy0, _ctx), _ctx); /*3860*/
        return kk_std_core__list_unbox(_x20637, _ctx);
      }
      {
        kk_integer_t i0_167860 = kk_integer_add(i0,(kk_integer_from_small(1)),kk_context()); /*int*/;
        kk_std_core__list x2_17552 = kk_std_core__lift16759_zipwith_indexed(f0, i0_167860, xx0, yy0, _ctx); /*list<16550>*/;
        if (kk_yielding(kk_context())) {
          kk_reuse_drop(_ru_18978, _ctx);
          kk_std_core__list_drop(x2_17552, _ctx);
          kk_box_t _x20640 = kk_std_core_hnd_yield_extend(kk_std_core__new_lift16759_zipwith_indexed_fun20641(x1_17549, _ctx), _ctx); /*3860*/
          return kk_std_core__list_unbox(_x20640, _ctx);
        }
        {
          return kk_std_core__new_Cons(_ru_18978, x1_17549, x2_17552, _ctx);
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
    kk_string_t _x19230;
    kk_define_string_literal(, _s19231, 8, "exn.core")
    _x19230 = kk_string_dup(_s19231); /*string*/
    kk_std_core__tag_exn = kk_std_core_hnd__new_Htag(_x19230, _ctx); /*std/core/hnd/htag<.hnd-exn>*/
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
    kk_string_t _x19968 = kk_string_empty(); /*string*/
    kk_ssize_t _x19970 = ((kk_ssize_t)0); /*ssize_t*/
    kk_ssize_t _x19971 = ((kk_ssize_t)0); /*ssize_t*/
    kk_std_core_empty = kk_std_core__new_Sslice(_x19968, _x19970, _x19971, _ctx); /*sslice*/
  }
  {
    kk_string_t _x20199 = kk_string_empty(); /*string*/
    kk_ssize_t _x20201 = ((kk_ssize_t)-1); /*ssize_t*/
    kk_ssize_t _x20202 = ((kk_ssize_t)0); /*ssize_t*/
    kk_std_core_invalid = kk_std_core__new_Sslice(_x20199, _x20201, _x20202, _ctx); /*sslice*/
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
