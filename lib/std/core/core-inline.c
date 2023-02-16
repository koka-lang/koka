
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_std_core__list kk_vector_to_list(kk_vector_t v, kk_std_core__list tail, kk_context_t* ctx) {
  // todo: avoid boxed_dup if v is unique
  kk_ssize_t n;
  kk_box_t* p = kk_vector_buf_borrow(v, &n, ctx);
  if (n <= 0) {
    kk_vector_drop(v,ctx);
    return tail;
  }
  kk_std_core__list nil  = kk_std_core__new_Nil(ctx);
  struct kk_std_core_Cons* cons = NULL;
  kk_std_core__list list = kk_std_core__new_Nil(ctx);
  for( kk_ssize_t i = 0; i < n; i++ ) {
    kk_std_core__list hd = kk_std_core__new_Cons(kk_reuse_null,kk_box_dup(p[i],ctx), nil, ctx);
    if (cons==NULL) {
      list = hd;
    }
    else {
      cons->tail = hd;
    }
    cons = kk_std_core__as_Cons(hd,ctx);
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
  while (kk_std_core__is_Cons(ys,ctx)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(ys,ctx);
    len++;
    ys = cons->tail;
  }
  // alloc the vector and copy
  kk_box_t* p;
  kk_vector_t v = kk_vector_alloc_uninit(len, &p, ctx);  
  ys = xs;
  for( kk_ssize_t i = 0; i < len; i++) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(ys,ctx);
    ys = cons->tail;
    p[i] = kk_box_dup(cons->head,ctx);
  }
  kk_std_core__list_drop(xs,ctx);  // todo: drop while visiting?
  return v;
}

kk_vector_t kk_vector_init( kk_ssize_t n, kk_function_t init, kk_context_t* ctx) {
  kk_box_t* p;
  kk_vector_t v = kk_vector_alloc_uninit(n, &p, ctx);  
  for(kk_ssize_t i = 0; i < n; i++) {
    kk_function_dup(init,ctx);
    p[i] = kk_function_call(kk_box_t,(kk_function_t,kk_ssize_t,kk_context_t*),init,(init,i,ctx),ctx);
  }
  kk_function_drop(init,ctx);
  return v;
}

kk_box_t kk_main_console( kk_function_t action, kk_context_t* ctx ) {
  return kk_function_call(kk_box_t,(kk_function_t,kk_unit_t,kk_context_t*),action,(action,kk_Unit,ctx),ctx);
}


kk_std_core__list kk_string_to_list(kk_string_t s, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* p = kk_string_buf_borrow(s,&len,ctx);
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
    tl = kk_std_core__as_Cons(cons,ctx);
  }
  kk_string_drop(s,ctx);
  return list;
}

kk_string_t kk_string_from_list(kk_std_core__list cs, kk_context_t* ctx) {
  // TODO: optimize for short strings to write directly into a local buffer?
  // find total UTF8 length
  kk_ssize_t len = 0;
  kk_std_core__list xs = cs;
  while (kk_std_core__is_Cons(xs,ctx)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(xs,ctx);
    len += kk_utf8_len(kk_char_unbox(cons->head,KK_BORROWED,ctx));
    xs = cons->tail;
  }
  // allocate and copy the characters
  uint8_t* p;
  kk_string_t s = kk_unsafe_string_alloc_buf(len,&p,ctx);  // must be initialized
  xs = cs;
  while (kk_std_core__is_Cons(xs,ctx)) {
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(xs,ctx);
    kk_ssize_t count;
    kk_utf8_write( kk_char_unbox(cons->head,KK_BORROWED,ctx), p, &count );
    p += count;
    xs = cons->tail;
  }
  kk_assert_internal(*p == 0 && (p - kk_string_buf_borrow(s,NULL,ctx)) == len);
  kk_std_core__list_drop(cs,ctx);  // todo: drop while visiting?
  return s;
}

static inline void kk_sslice_start_end_borrowx( kk_std_core__sslice sslice, const uint8_t** start, const uint8_t** end, const uint8_t** sstart, const uint8_t** send, kk_context_t* ctx) {
  kk_ssize_t strlen;
  const uint8_t* s = kk_string_buf_borrow(sslice.str,&strlen,ctx);
  kk_ssize_t slstart = kk_integer_clamp_ssize_t_borrow(sslice.start,ctx);
  kk_ssize_t sllen   = kk_integer_clamp_ssize_t_borrow(sslice.len,ctx);
  *start = s + slstart;
  *end = s + slstart + sllen;
  if (sstart != NULL) *sstart = s;
  if (send != NULL) *send = s + strlen;
  kk_assert_internal(*start >= s && *start <= *end);
  kk_assert_internal(*end >= *start && *end <= s + strlen);
}

static inline void kk_sslice_start_end_borrow( kk_std_core__sslice sslice, const uint8_t** start, const uint8_t** end, kk_context_t* ctx) {
  kk_sslice_start_end_borrowx(sslice,start,end,NULL,NULL,ctx);
}

kk_integer_t kk_slice_count( kk_std_core__sslice sslice, kk_context_t* ctx ) {
  // TODO: optimize this by extending kk_string_count
  const uint8_t* start;
  const uint8_t* end;
  kk_sslice_start_end_borrow(sslice, &start, &end, ctx);
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
  kk_sslice_start_end_borrow(sslice, &start, &end, ctx);
  // is it the full string?
  if (kk_integer_is_zero_borrow(sslice.start) && 
      kk_integer_eq_borrow(sslice.len,kk_integer_from_ssize_t(kk_string_len_borrow(sslice.str,ctx),ctx),ctx)) {
    // TODO: drop sslice and dup sslice.str?
    return sslice.str;
  }
  else {
    // if not, we copy len bytes
    kk_string_t s = kk_string_alloc_dupn_valid_utf8(kk_integer_clamp_ssize_t_borrow(sslice.len,ctx), start, ctx);
    kk_std_core__sslice_drop(sslice,ctx);
    return s;
  }
}

kk_std_core__sslice kk_slice_first( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen,ctx);
  const uint8_t* next = (slen > 0 ? kk_utf8_next(s) : s);
  return kk_std_core__new_Sslice(str, kk_integer_zero, kk_integer_from_ptrdiff_t(next - s,ctx), ctx);
}

kk_std_core__sslice kk_slice_last( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen,ctx);
  const uint8_t* end = s + slen;
  const uint8_t* prev = (s==end ? s : kk_utf8_prev(end));
  return kk_std_core__new_Sslice(str, kk_integer_from_ptrdiff_t(prev - s,ctx), kk_integer_from_ptrdiff_t(end - prev,ctx), ctx);
}

kk_std_core__sslice kk_slice_between( struct kk_std_core_Sslice slice1, struct kk_std_core_Sslice slice2, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow( slice1.str, NULL, ctx );
  const uint8_t* s2 = kk_string_buf_borrow( slice2.str, NULL, ctx );
  if (s1 != s2) {
    kk_info_message("between: not equal slices: %p vs. %p\n", s1, s2);
    return kk_std_core__new_Sslice(kk_string_empty(), kk_integer_zero, kk_integer_min_one, ctx); // invalid slice
  }
  
  kk_integer_t start;
  kk_integer_t len;
  if (kk_integer_lte_borrow(slice1.start,slice2.start,ctx)) {
    start = kk_integer_dup(slice1.start,ctx);
    len   = kk_integer_sub(kk_integer_dup(slice2.start,ctx),kk_integer_dup(slice1.start,ctx),ctx);
  }
  else  {
    start = kk_integer_dup(slice2.start,ctx);
    len   = kk_integer_sub(kk_integer_dup(slice1.start,ctx),kk_integer_dup(slice2.start,ctx),ctx);
  }  
  return kk_std_core__new_Sslice(slice1.str, start, len, ctx);
}

kk_std_core_types__maybe kk_slice_next( struct kk_std_core_Sslice slice, kk_context_t* ctx ) {
  if (!kk_integer_is_pos_borrow(slice.len,ctx)) {
    kk_std_core__sslice_drop(slice,ctx);
    return kk_std_core_types__new_Nothing(ctx);
  }
  const uint8_t* start;
  const uint8_t* end;
  kk_sslice_start_end_borrow(slice, &start, &end, ctx);
  kk_ssize_t clen;
  const kk_char_t c = kk_utf8_read(start,&clen);
  kk_assert_internal(clen > 0 && clen <= kk_integer_clamp_ssize_t_borrow(slice.len,ctx));
  kk_integer_t iclen = kk_integer_min(kk_integer_from_ssize_t(clen,ctx),kk_integer_dup(slice.len,ctx),ctx);
  // TODO: specialize type to avoid boxing
  // note: don't drop slice as we take over all fields  
  kk_integer_t istart = kk_integer_add(slice.start,kk_integer_dup(iclen,ctx),ctx);
  kk_integer_t ilen   = kk_integer_sub(slice.len,iclen,ctx);
  kk_std_core__sslice snext = kk_std_core__new_Sslice(slice.str, istart, ilen, ctx);
  kk_std_core_types__tuple2_ res = kk_std_core_types__new_dash__lp__comma__rp_( kk_char_box(c,ctx), kk_std_core__sslice_box(snext,ctx), ctx);
  return kk_std_core_types__new_Just( kk_std_core_types__tuple2__box(res,ctx), ctx );
}

/* Borrow count */
struct kk_std_core_Sslice kk_slice_extend_borrow( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
  kk_ssize_t cnt = kk_integer_clamp_ssize_t_borrow(count,ctx);
  if (cnt==0 || (!kk_integer_is_pos_borrow(slice.len,ctx) && cnt<0)) return slice;
  const uint8_t* s0;
  const uint8_t* s1;
  kk_sslice_start_end_borrow(slice,&s0,&s1,ctx);
  const uint8_t* t  = s1;
  if (cnt >= 0) {
    do {
      t = kk_utf8_next(t);
      cnt--;
    } while (cnt > 0 && *t != 0);
  }
  else {  // cnt < 0
    const uint8_t* sstart = s0 - kk_integer_clamp_ssize_t_borrow(slice.start,ctx);
    do {
      t = kk_utf8_prev(t);
      cnt++;
    } while (cnt < 0 && t > sstart);
  }
  if (t == s1) return slice;  // length is unchanged
  kk_integer_drop(slice.len,ctx);
  return kk_std_core__new_Sslice(slice.str, slice.start, kk_integer_from_ptrdiff_t(t < s0 ? 0 : (t - s0),ctx), ctx);
}

/* Borrow count */
struct kk_std_core_Sslice kk_slice_advance_borrow( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
  const kk_ssize_t cnt0 = kk_integer_clamp_ssize_t_borrow(count,ctx);
  kk_ssize_t cnt = cnt0;
  if (cnt==0 || (kk_integer_is_zero_borrow(slice.start) && cnt<0)) return slice;
  const uint8_t* sstart;
  const uint8_t* s0;
  const uint8_t* s1;
  const uint8_t* send;
  kk_sslice_start_end_borrowx(slice,&s0,&s1,&sstart,&send,ctx);
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
  kk_integer_drop(slice.start,ctx);
  kk_integer_drop(slice.len,ctx);
  return kk_std_core__new_Sslice(slice.str, kk_integer_from_ptrdiff_t(t0 - sstart,ctx), 
                                            kk_integer_from_ptrdiff_t(t1 - t0,ctx), ctx);
}

/* Borrow iupto */
struct kk_std_core_Sslice kk_slice_common_prefix_borrow( kk_string_t str1, kk_string_t str2, kk_integer_t iupto, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow(str1,NULL,ctx);
  const uint8_t* s2 = kk_string_buf_borrow(str2,NULL,ctx);
  kk_ssize_t upto = kk_integer_clamp_ssize_t_borrow(iupto,ctx);
  kk_ssize_t count;
  for(count = 0; count < upto && *s1 != 0 && *s2 != 0; count++, s1++, s2++ ) {
    if (*s1 != *s2) break;
  }
  kk_string_drop(str2,ctx);
  return kk_std_core__new_Sslice(str1, kk_integer_zero, kk_integer_from_ssize_t(count,ctx), ctx);
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
  kk_fatal_error(EINVAL, "assertion failed: %s\n", kk_string_cbuf_borrow(msg,NULL,ctx));
  kk_string_drop(msg,ctx);
  return kk_Unit;
}