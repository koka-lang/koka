
/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline void kk_sslice_start_end_borrowx( kk_std_core_sslice__sslice sslice, const uint8_t** start, const uint8_t** end, const uint8_t** sstart, const uint8_t** send, kk_context_t* ctx) {
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

static inline void kk_sslice_start_end_borrow( kk_std_core_sslice__sslice sslice, const uint8_t** start, const uint8_t** end, kk_context_t* ctx) {
  kk_sslice_start_end_borrowx(sslice,start,end,NULL,NULL,ctx);
}

kk_integer_t kk_slice_count( kk_std_core_sslice__sslice sslice, kk_context_t* ctx ) {
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
  kk_std_core_sslice__sslice_drop(sslice,ctx);
  return kk_integer_from_ssize_t(count,ctx);
}

kk_string_t kk_slice_to_string( kk_std_core_sslice__sslice  sslice, kk_context_t* ctx ) {
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
    kk_std_core_sslice__sslice_drop(sslice,ctx);
    return s;
  }
}

kk_std_core_sslice__sslice kk_slice_first( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen,ctx);
  const uint8_t* next = (slen > 0 ? kk_utf8_next(s) : s);
  return kk_std_core_sslice__new_Sslice(str, kk_integer_zero, kk_integer_from_ptrdiff_t(next - s,ctx), ctx);
}

kk_std_core_sslice__sslice kk_slice_last( kk_string_t str, kk_context_t* ctx ) {
  kk_ssize_t slen;
  const uint8_t* s = kk_string_buf_borrow(str,&slen,ctx);
  const uint8_t* end = s + slen;
  const uint8_t* prev = (s==end ? s : kk_utf8_prev(end));
  return kk_std_core_sslice__new_Sslice(str, kk_integer_from_ptrdiff_t(prev - s,ctx), kk_integer_from_ptrdiff_t(end - prev,ctx), ctx);
}

kk_std_core_sslice__sslice kk_slice_between( struct kk_std_core_sslice_Sslice slice1, struct kk_std_core_sslice_Sslice slice2, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow( slice1.str, NULL, ctx );
  const uint8_t* s2 = kk_string_buf_borrow( slice2.str, NULL, ctx );
  if (s1 != s2) {
    kk_info_message("between: not equal slices: %p vs. %p\n", s1, s2);
    return kk_std_core_sslice__new_Sslice(kk_string_empty(), kk_integer_zero, kk_integer_min_one, ctx); // invalid slice
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
  return kk_std_core_sslice__new_Sslice(slice1.str, start, len, ctx);
}

kk_std_core_types__maybe kk_slice_next( struct kk_std_core_sslice_Sslice slice, kk_context_t* ctx ) {
  if (!kk_integer_is_pos_borrow(slice.len,ctx)) {
    kk_std_core_sslice__sslice_drop(slice,ctx);
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
  kk_std_core_sslice__sslice snext = kk_std_core_sslice__new_Sslice(slice.str, istart, ilen, ctx);
  kk_std_core_types__tuple2 res = kk_std_core_types__new_Tuple2( kk_char_box(c,ctx), kk_std_core_sslice__sslice_box(snext,ctx), ctx);
  return kk_std_core_types__new_Just( kk_std_core_types__tuple2_box(res,ctx), ctx );
}

/* Borrow count */
struct kk_std_core_sslice_Sslice kk_slice_extend_borrow( struct kk_std_core_sslice_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
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
  return kk_std_core_sslice__new_Sslice(slice.str, slice.start, kk_integer_from_ptrdiff_t(t < s0 ? 0 : (t - s0),ctx), ctx);
}

/* Borrow count */
struct kk_std_core_sslice_Sslice kk_slice_advance_borrow( struct kk_std_core_sslice_Sslice slice, kk_integer_t count, kk_context_t* ctx ) {
  const kk_ssize_t cnt0 = kk_integer_clamp_ssize_t_borrow(count,ctx);
  kk_ssize_t cnt = cnt0;
  if (cnt==0) return slice;
  const uint8_t* sstart;
  const uint8_t* s0;
  const uint8_t* s1;
  const uint8_t* send;
  kk_sslice_start_end_borrowx(slice,&s0,&s1,&sstart,&send,ctx);
  // advance the start
  const uint8_t* t0  = s0;
  if (cnt >= 0) {
    while (cnt > 0 && t0 < send) {
      t0 = kk_utf8_next(t0);
      cnt--;
    }
  }
  else {  // cnt < 0
    while (cnt < 0 && t0 > sstart) {
      t0 = kk_utf8_prev(t0);
      cnt++;
    }
  }
  if (t0 == s0 && cnt0 > 0) return slice;  // start is unchanged
  // "t0" points to the new start, now advance the end by the same amount of codepoints
  const uint8_t* t1 = s1;
  cnt = cnt0;
  if (cnt >= 0) {
    while (cnt > 0 && t1 < send) {
      t1 = kk_utf8_next(t1);
      cnt--;
    }
  }
  else {  // cnt < 0
    while (cnt < 0 && t1 > sstart) {
      t1 = kk_utf8_prev(t1);
      cnt++;
    }
  }
  // t1 points to the new end
  kk_assert_internal(t1 >= t0);
  const kk_ssize_t in_len = kk_integer_clamp_ssize_t_borrow(slice.len, ctx);
  kk_ssize_t new_len = (t1-t0);
  // kk_info_message("Here %d %d %d t: %d %d s: %d %d, st: %d %d\n", in_len, cnt0, new_len, t1, t0, s1, s0, sstart, send);
  kk_assert_internal(t1 <= send && t0 >= sstart);
  kk_integer_drop(slice.start,ctx);
  kk_integer_drop(slice.len,ctx);
  return kk_std_core_sslice__new_Sslice(slice.str, kk_integer_from_ptrdiff_t(t0 - sstart,ctx),
                                          kk_integer_from_ptrdiff_t(new_len, ctx), ctx);
}

/* Borrow iupto */
struct kk_std_core_sslice_Sslice kk_slice_common_prefix_borrow( kk_string_t str1, kk_string_t str2, kk_integer_t iupto, kk_context_t* ctx ) {
  const uint8_t* s1 = kk_string_buf_borrow(str1,NULL,ctx);
  const uint8_t* s2 = kk_string_buf_borrow(str2,NULL,ctx);
  kk_ssize_t upto = kk_integer_clamp_ssize_t_borrow(iupto,ctx);
  kk_ssize_t count;
  for(count = 0; count < upto && *s1 != 0 && *s2 != 0; count++, s1++, s2++ ) {
    if (*s1 != *s2) break;
  }
  kk_string_drop(str2,ctx);
  return kk_std_core_sslice__new_Sslice(str1, kk_integer_zero, kk_integer_from_ssize_t(count,ctx), ctx);
}
