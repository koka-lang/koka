

/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

__std_core__list vector_to_list(vector_t v, __std_core__list tail, context_t* ctx) {
  // todo: avoid boxed_dup if v is unique
  size_t n;
  box_t* p = vector_buf(v, &n);
  if (n == 0) {
    drop_vector_t(v,ctx);
    return tail;
  }
  __std_core__list nil  = __std_core__new_Nil(ctx);
  struct __std_core_Cons* cons = NULL;
  __std_core__list list = __std_core__new_Nil(ctx);
  for( size_t i = 0; i < n; i++ ) {
    __std_core__list hd = __std_core__new_Cons(reuse_null,dup_box_t(p[i]), nil, ctx);
    if (cons==NULL) {
      list = hd;
    }
    else {
      cons->tail = hd;
    }
    cons = __std_core__as_Cons(hd);
  }
  cons->tail = tail;
  drop_vector_t(v,ctx);
  return list;
}

vector_t list_to_vector(__std_core__list xs, context_t* ctx) {
  // todo: avoid boxed_dup if xs is unique
  // find the length
  size_t len = 0;
  __std_core__list ys = xs;
  while (__std_core__is_Cons(ys)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(ys);
    len++;
    ys = cons->tail;
  }
  // alloc the vector and copy
  vector_t v = vector_alloc(len,box_null,ctx);
  box_t* p = vector_buf(v,NULL);
  ys = xs;
  for( size_t i = 0; i < len; i++) {
    struct __std_core_Cons* cons = __std_core__as_Cons(ys);
    ys = cons->tail;
    p[i] = dup_box_t(cons->head);
  }
  return v;
}

vector_t vector_init32( int32_t n, function_t init, context_t* ctx) {
  vector_t v = vector_alloc(n,box_null,ctx);
  box_t* p = vector_buf(v,NULL);
  for(int32_t i = 0; i < n; i++) {
    dup_function_t(init);
    p[i] = function_call(box_t,(function_t,int32_t,context_t*),init,(init,i,ctx));
  }
  drop_function_t(init,ctx);
  return v;
}

box_t main_console( function_t action, context_t* ctx ) {
  return function_call(box_t,(function_t,unit_t,context_t*),action,(action,Unit,ctx));
}


__std_core__list string_to_list(string_t s, context_t* ctx) {
  const uint8_t* p = string_buf_borrow(s);
  __std_core__list nil  = __std_core__new_Nil(ctx);
  __std_core__list list = nil;
  struct __std_core_Cons* tl = NULL;
  size_t count;
  char_t c;
  while( (c = utf8_read(p,&count), c != 0) ) {
    p += count;
    __std_core__list cons = __std_core__new_Cons(reuse_null,box_char_t(c,ctx), nil, ctx);
    if (tl!=NULL) {
      tl->tail = cons;
    }
    else {
      list = cons;
    }
    tl = __std_core__as_Cons(cons);
  }
  return list;
}

string_t string_from_list(__std_core__list cs, context_t* ctx) {
  // TODO: optimize for short strings to write directly into a local buffer?
  // find total UTF8 length
  size_t len = 0;
  __std_core__list xs = cs;
  while (__std_core__is_Cons(xs)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(xs);
    len += utf8_len(unbox_char_t(cons->head,ctx));
    xs = cons->tail;
  }
  // allocate and copy the characters
  string_t s = string_alloc_len(len,0,ctx);
  uint8_t* p = (uint8_t*)string_buf_borrow(s);
  xs = cs;
  while (__std_core__is_Cons(xs)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(xs);
    size_t count;
    utf8_write( unbox_char_t(cons->head,ctx), p, &count );
    p += count;
    xs = cons->tail;
  }
  assert_internal(*p == 0);
  drop___std_core__list(cs,ctx);  // todo: drop while visiting
  return s;
}

static inline void sslice_start_end_borrow( __std_core__sslice sslice, const uint8_t** start, const uint8_t** end) {
  const uint8_t* s = string_buf_borrow(sslice.str);
  *start = s + sslice.start;
  *end = s + sslice.start + sslice.len;
}

integer_t slice_count( __std_core__sslice sslice, context_t* ctx ) {
  // TODO: optimize this by extending string_count
  const uint8_t* start;
  const uint8_t* end;
  sslice_start_end_borrow(sslice, &start, &end);
  size_t count = 0;
  while( start < end && *start != 0 ) {
    const uint8_t* next = utf8_next(start);
    count++;
    start = next;
  }
  return integer_from_size_t(count,ctx);
}

string_t slice_to_string( __std_core__sslice  sslice, context_t* ctx ) {
  const uint8_t* start;
  const uint8_t* end;
  sslice_start_end_borrow(sslice, &start, &end);
  // is it the full string?
  if (sslice.start == 0 && (size_t)sslice.len == string_len_borrow(sslice.str)) {
    // TODO: drop sslice and dup sslice.str?
    return sslice.str;
  }
  else {
    // if not, we copy
    string_t s = string_alloc_len(sslice.len, (const char*)start, ctx);
    drop___std_core__sslice(sslice,ctx);
    return s;
  }
}

__std_core__sslice slice_first( string_t str, context_t* ctx ) {
  const uint8_t* s = string_buf_borrow(str);
  const uint8_t* next = utf8_next(s);
  return __std_core__new_Sslice(str, 0, (int32_t)(next - s), ctx);
}

__std_core__sslice slice_last( string_t str, context_t* ctx ) {
  const uint8_t* s = string_buf_borrow(str);
  const uint8_t* end = s + string_len_borrow(str);
  const uint8_t* prev = (s==end ? s : utf8_prev(end));
  return __std_core__new_Sslice(str, (int32_t)(end - s), (int32_t)(end - prev), ctx);
}

__std_core_types__maybe slice_next( struct __std_core_Sslice slice, context_t* ctx ) {
  if (slice.len == 0) {
    drop___std_core__sslice(slice,ctx);
    return __std_core_types__new_Nothing(ctx);
  }
  const uint8_t* s = string_buf_borrow(slice.str);
  const uint8_t* next = utf8_next(s);
  ptrdiff_t clen = (next - s);
  assert_internal(clen > 0 && clen <= slice.len);
  if (clen < 0) clen = 0;
  if (clen > slice.len) clen = slice.len;
  __std_core__sslice snext = __std_core__new_Sslice(slice.str, slice.start + (int32_t)clen, slice.len - (int32_t)clen, ctx);
  return __std_core_types__new_Just( box___std_core__sslice(snext,ctx), ctx);
}

struct __std_core_Sslice slice_extend( struct __std_core_Sslice slice, integer_t count, context_t* ctx ) {
  ptrdiff_t cnt = integer_clamp(count,ctx);
  if (cnt==0 || (slice.len == 0 && cnt<0)) return slice;
  const uint8_t* const s0 = string_buf_borrow(slice.str);  // start
  const uint8_t* const s1 = s0 + slice.start + slice.len;  // end
  const uint8_t* t  = s1;
  if (cnt >= 0) {
    do {
      t = utf8_next(t);
      cnt--;
    } while (cnt > 0 && *t != 0);
  }
  else {  // cnt < 0
    const uint8_t* sstart = s0 - slice.start;
    do {
      t = utf8_prev(t);
      cnt++;
    } while (cnt < 0 && t > sstart);
  }
  if (t == s1) return slice;  // length is unchanged
  return __std_core__new_Sslice(slice.str, slice.start, (t < s0 ? 0 : (int32_t)(t - s0)), ctx);
}

struct __std_core_Sslice slice_advance( struct __std_core_Sslice slice, integer_t count, context_t* ctx ) {
  const ptrdiff_t cnt0 = integer_clamp(count,ctx);
  ptrdiff_t cnt = cnt0;
  if (cnt==0 || (slice.start == 0 && cnt<0)) return slice;
  const uint8_t* const s0 = string_buf_borrow(slice.str);  // start
  const uint8_t* const s1 = s0 + slice.start + slice.len;  // end
  const uint8_t* const sstart = s0 - slice.start;
  assert_internal(sstart == string_buf_borrow(slice.str));
  // advance the start
  const uint8_t* t0  = s0;
  if (cnt >= 0) {
    do {
      t0 = utf8_next(t0);
      cnt--;
    } while (cnt > 0 && *t0 != 0);
  }
  else {  // cnt < 0
    do {
      t0 = utf8_prev(t0);
      cnt++;
    } while (cnt < 0 && t0 > sstart);
  }
  if (t0 == s0) return slice;  // start is unchanged
  // "t0" points to the new start, now advance the end by the same amount of codepoints
  const uint8_t* t1 = s1;
  cnt = cnt0;
  if (cnt >= 0) {
    do {
      t1 = utf8_next(t1);
      cnt--;
    } while (cnt > 0 && *t1 != 0);
  }
  else {  // cnt < 0
    do {
      t1 = utf8_prev(t1);
      cnt++;
    } while (cnt < 0 && t1 > sstart);
  }
  // t1 points to the new end
  assert_internal(t1 >= t0);
  return __std_core__new_Sslice(slice.str, (int32_t)(t0 - sstart), (int32_t)(t1 - t0), ctx);
}

struct __std_core_Sslice slice_common_prefix( string_t str1, string_t str2, integer_t iupto, context_t* ctx ) {
  const uint8_t* s1 = string_buf_borrow(str1);
  const uint8_t* s2 = string_buf_borrow(str1);
  size_t upto = integer_clamp(iupto,ctx);
  size_t count;
  for(count = 0; count < upto && *s1 != 0 && *s2 != 0; count++, s1++, s2++ ) {
    if (*s1 != *s2) break;
  }
  drop_string_t(str2,ctx);
  return __std_core__new_Sslice(str1, 0, (int32_t)(count), ctx);
}
