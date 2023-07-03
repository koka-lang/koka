/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redibibute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this dibibution.
---------------------------------------------------------------------------*/
#include "kklib.h"

/*--------------------------------------------------------------------------------------------------
  Low level allocation of bytes
--------------------------------------------------------------------------------------------------*/


// Allocate `len` bytes.
// If (p /= NULL) then initialize with at most `min(len,plen)` bytes from `p`, which must point to at least `plen` valid bytes. 
// Adds a terminating zero at the end. Return the raw buffer pointer in `buf` if non-NULL
kk_decl_export kk_decl_noinline kk_bytes_t kk_bytes_alloc_len(kk_ssize_t len, kk_ssize_t plen, const uint8_t* p, uint8_t** buf, kk_context_t* ctx) {
  // kk_assert_internal(s == NULL || blen(s) >= len);  // s may contain embedded 0 characters
  static uint8_t empty[16] = { 0 };
  if (len <= 0) {
    if (buf != NULL) *buf = empty;
    return kk_bytes_empty();
  }
  if (plen > len) plen = len;  // limit plen <= len
  if (len <= KK_BYTES_SMALL_MAX) {
    kk_bytes_small_t b = kk_block_alloc_as(struct kk_bytes_small_s, 0, KK_TAG_BYTES_SMALL, ctx);
    b->u.buf_value = ~KK_U64(0);
    if (p != NULL && plen > 0) {
      kk_memcpy(&b->u.buf[0], p, plen);
    }
    b->u.buf[len] = 0;
    if (buf != NULL) *buf = &b->u.buf[0];
    return kk_datatype_from_base(&b->_base,ctx);
  }
  else {
    kk_bytes_normal_t b = kk_block_assert(kk_bytes_normal_t, kk_block_alloc_any(kk_ssizeof(struct kk_bytes_normal_s) - 1 /* char b[1] */ + len + 1 /* 0 terminator */, 0, KK_TAG_BYTES, ctx), KK_TAG_BYTES);
    if (p != NULL && plen > 0) {
      kk_memcpy(&b->buf[0], p, plen);
    }
    b->length = len;
    b->buf[len] = 0;
    if (buf != NULL) *buf = &b->buf[0];
    // todo: kk_assert valid utf-8 in debug mode
    return kk_datatype_from_base(&b->_base,ctx);
  }  
}


kk_bytes_t kk_bytes_adjust_length(kk_bytes_t b, kk_ssize_t newlen, kk_context_t* ctx) {
  if (newlen<=0) {
    kk_bytes_drop(b, ctx);
    return kk_bytes_empty();
  }
  kk_ssize_t len;
  const uint8_t* s = kk_bytes_buf_borrow(b,&len,ctx);
  if (len == newlen) {
    return b;
  }
  else if (len > newlen && (3*(len/4)) < newlen &&  // 0.75*len < newlen < len: update length in place if we can
           kk_datatype_ptr_is_unique(b,ctx) && kk_datatype_ptr_has_tag(b, KK_TAG_BYTES, ctx)) {
    // length in place
    kk_assert_internal(kk_datatype_has_tag(b, KK_TAG_BYTES,ctx) && kk_datatype_ptr_is_unique(b,ctx));
    kk_bytes_normal_t nb = kk_datatype_as_assert(kk_bytes_normal_t, b, KK_TAG_BYTES, ctx);
    nb->length = newlen;
    nb->buf[newlen] = 0;
    // kk_assert_internal(kk_bytes_is_valid(kk_bytes_dup(s),ctx));
    return b;
  }
  else if (newlen < len) {
    // full copy
    kk_bytes_t tb = kk_bytes_alloc_dupn(newlen, s, ctx);
    kk_bytes_drop(b, ctx);
    return tb;
  }
  else {
    // full copy
    kk_assert_internal(newlen > len);
    uint8_t* t;
    kk_bytes_t tb = kk_bytes_alloc_buf(newlen, &t, ctx);
    kk_memcpy( t, s, len );
    kk_memset(t + len, 0, newlen - len);
    kk_bytes_drop(b, ctx);
    return tb;
  }
}


/*--------------------------------------------------------------------------------------------------
  Compare
--------------------------------------------------------------------------------------------------*/

const uint8_t* kk_memmem(const uint8_t* p, kk_ssize_t plen, const uint8_t* pat, kk_ssize_t patlen) {
  // todo: optimize search algo?
  kk_assert(p != NULL && pat != NULL);
  if (plen <= 0 || patlen <= 0 || patlen > plen) return NULL;
  const uint8_t* end = p + (plen - (patlen-1));
  for (; p < end; p++) {
    if (kk_memcmp(p, pat, patlen) == 0) return p;
  }
  return NULL;
}

int kk_bytes_cmp_borrow(kk_bytes_t b1, kk_bytes_t b2, kk_context_t* ctx) {
  if (kk_bytes_ptr_eq_borrow(b1, b2)) return 0;
  kk_ssize_t len1;
  const uint8_t* s1 = kk_bytes_buf_borrow(b1,&len1,ctx);
  kk_ssize_t len2;
  const uint8_t* s2 = kk_bytes_buf_borrow(b2,&len2,ctx);
  kk_ssize_t minlen = (len1 <= len2 ? len1 : len2);
  int ord = kk_memcmp(s1, s2, minlen);
  if (ord == 0) {
    if (len1 > len2) return 1;
    else if (len1 < len2) return -1;
  }
  return ord;
}

int kk_bytes_cmp(kk_bytes_t b1, kk_bytes_t b2, kk_context_t* ctx) {
  int ord = kk_bytes_cmp_borrow(b1,b2,ctx);
  kk_bytes_drop(b1,ctx);
  kk_bytes_drop(b2,ctx);
  return ord;
}


/*--------------------------------------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------------------------------------*/

kk_ssize_t kk_decl_pure kk_bytes_count_pattern_borrow(kk_bytes_t b, kk_bytes_t pattern, kk_context_t* ctx) {
  kk_ssize_t patlen;
  const uint8_t* pat = kk_bytes_buf_borrow(pattern,&patlen,ctx);  
  kk_ssize_t len;
  const uint8_t* s   = kk_bytes_buf_borrow(b,&len,ctx);
  if (patlen <= 0)  return kk_bytes_len_borrow(b,ctx);
  if (patlen > len) return 0;
  
  //todo: optimize by doing backward Boyer-Moore? or use forward Knuth-Morris-Pratt?
  kk_ssize_t count = 0;
  const uint8_t* end = s + (len - (patlen - 1));
  for (const uint8_t* p = s; p < end; p++) {
    if (kk_memcmp(p, pat, patlen) == 0) {
      count++;
      p += (patlen - 1);
    }    
  }
  return count;
}


kk_bytes_t kk_bytes_cat(kk_bytes_t b1, kk_bytes_t b2, kk_context_t* ctx) {
  kk_ssize_t len1;
  const uint8_t* s1 = kk_bytes_buf_borrow(b1, &len1, ctx);
  kk_ssize_t len2;
  const uint8_t* s2 = kk_bytes_buf_borrow(b2, &len2, ctx);
  uint8_t* p;
  kk_bytes_t t = kk_bytes_alloc_buf(len1 + len2, &p, ctx );
  kk_memcpy(p, s1, len1);
  kk_memcpy(p+len1, s2, len2);
  kk_assert_internal(p[len1+len2] == 0);
  kk_bytes_drop(b1, ctx);
  kk_bytes_drop(b2, ctx);
  return t;
}

kk_bytes_t kk_bytes_cat_from_buf(kk_bytes_t b1, kk_ssize_t len2, const uint8_t* b2, kk_context_t* ctx) {
  if (b2 == NULL || len2 <= 0) return b1;
  kk_ssize_t len1;
  const uint8_t* s1 = kk_bytes_buf_borrow(b1,&len1,ctx);
  uint8_t* p;
  kk_bytes_t t = kk_bytes_alloc_buf(len1 + len2, &p, ctx);
  kk_memcpy(p, s1, len1);
  kk_memcpy(p+len1, b2, len2);
  kk_assert_internal(p[len1+len2] == 0);
  kk_bytes_drop(b1, ctx);
  return t;
}

kk_vector_t kk_bytes_splitv(kk_bytes_t s, kk_bytes_t sep, kk_context_t* ctx) {
  return kk_bytes_splitv_atmost(s, sep, KK_SSIZE_MAX, ctx);
}

kk_vector_t kk_bytes_splitv_atmost(kk_bytes_t b, kk_bytes_t sepb, kk_ssize_t n, kk_context_t* ctx) 
{
  if (n < 1) n = 1;
  kk_ssize_t len;
  const uint8_t* s = kk_bytes_buf_borrow(b, &len, ctx);
  const uint8_t* const end = s + len;
  kk_ssize_t seplen;
  const uint8_t* sep = kk_bytes_buf_borrow(sepb, &seplen, ctx);

  // count parts
  kk_ssize_t count = 1;
  if (seplen > 0) {    
    const uint8_t* p = s;
    while (count < n && (p = kk_memmem(p, end - p, sep, seplen)) != NULL) {
      p += seplen;
      count++;
    }
  }
  else if (n > 1) {
    count = len;
    if (count > n) count = n;
  }
  kk_assert_internal(count >= 1 && count <= n);
  
  // copy to vector
  kk_box_t* v;
  kk_vector_t vec = kk_vector_alloc_uninit(count, &v, ctx);  
  const uint8_t* p = s;
  for (kk_ssize_t i = 0; i < (count-1) && p < end; i++) {
    const uint8_t* r;
    if (seplen > 0) {
      r = kk_memmem(p, end - p,  sep, seplen);
    }
    else {
      r = p + 1;
    }
    kk_assert_internal(r != NULL && r >= p && r < end);    
    const kk_ssize_t partlen = (r - p);
    v[i] = kk_bytes_box(kk_bytes_alloc_dupn(partlen, p, ctx));
    p = r + seplen;  // advance
  }
  kk_assert_internal(p <= end);
  v[count-1] = kk_bytes_box(kk_bytes_alloc_dupn(end - p, p, ctx));  // todo: share bytes if p == s ?
  kk_bytes_drop(b,ctx);
  kk_bytes_drop(sepb, ctx);
  return vec;
}

kk_bytes_t kk_bytes_replace_all(kk_bytes_t s, kk_bytes_t pat, kk_bytes_t rep, kk_context_t* ctx) {
  return kk_bytes_replace_atmost(s, pat, rep, KK_SSIZE_MAX, ctx);
}

kk_bytes_t kk_bytes_replace_atmost(kk_bytes_t s, kk_bytes_t pat, kk_bytes_t rep, kk_ssize_t n, kk_context_t* ctx) {
  kk_bytes_t t = s;
  if (!(n<=0 || kk_bytes_is_empty_borrow(s, ctx) || kk_bytes_is_empty_borrow(pat, ctx)))
  {
    kk_ssize_t plen;
    const uint8_t* p = kk_bytes_buf_borrow(s,&plen,ctx);
    kk_ssize_t ppat_len;
    const uint8_t* ppat = kk_bytes_buf_borrow(pat,&ppat_len,ctx);
    kk_ssize_t prep_len; 
    const uint8_t* prep = kk_bytes_buf_borrow(rep, &prep_len,ctx);
    
    const uint8_t* const pend = p + plen;
    // if unique s && |rep| == |pat|, update in-place
    // TODO: if unique s & |rep| <= |pat|, maybe update in-place if not too much waste?
    if (kk_datatype_ptr_is_unique(s,ctx) && ppat_len == prep_len) {
      kk_ssize_t count = 0;
      while (count < n && p < pend) {
        const uint8_t* r = kk_memmem(p, pend - p, ppat, ppat_len);
        if (r == NULL) break;
        kk_memcpy((uint8_t*)r, prep, prep_len);
        count++;
        p = r + prep_len;
      }
    }
    else {
      // count pat occurrences so we can pre-allocate the result buffer
      kk_ssize_t count = 0;
      const uint8_t* r = p;
      while (count < n && ((r = kk_memmem(r, pend - r, ppat, ppat_len)) != NULL)) {
        count++;
        r += ppat_len;
      }
      if (count == 0) goto done; // no pattern found
      
      // allocate
      kk_ssize_t newlen = plen - (count * ppat_len) + (count * prep_len);
      uint8_t* q;
      t = kk_bytes_alloc_buf(newlen, &q, ctx);
      while (count > 0) {
        count--;
        r = kk_memmem(p, pend - p, ppat, ppat_len);
        kk_assert_internal(r != NULL);
        kk_ssize_t ofs = (r - p);
        kk_memcpy(q, p, ofs);
        kk_memcpy(q + ofs, prep, prep_len);
        q += ofs + prep_len;
        p += ofs + ppat_len;
      }
      kk_ssize_t rest = (pend - p);
      kk_memcpy(q, p, rest);
      kk_assert_internal(q + rest == kk_bytes_buf_borrow(t,NULL,ctx) + newlen);
    }
  }

done:
  kk_bytes_drop(pat, ctx);
  kk_bytes_drop(rep, ctx);
  if (!kk_datatype_eq(t, s)) kk_datatype_drop(s, ctx);
  return t;
}


kk_bytes_t kk_bytes_repeat(kk_bytes_t b, kk_ssize_t n, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_bytes_buf_borrow(b,&len,ctx);  
  if (len <= 0 || n<=0) return kk_bytes_empty();  
  uint8_t* t;
  kk_bytes_t tb = kk_bytes_alloc_buf(len*n, &t, ctx); // TODO: check overflow
  if (len == 1) {
    kk_memset(t, *s, n);
    t += n;
  }
  else {
    for (kk_ssize_t i = 0; i < n; i++) {
      kk_memcpy(t, s, len);
      t += len;
    }
  }
  kk_assert_internal(*t == 0);
  kk_bytes_drop(b,ctx);
  return tb;
}

// to avoid casting to signed, return 0 for not found, or the index+1
kk_ssize_t kk_bytes_index_of1(kk_bytes_t b, kk_bytes_t sub, kk_context_t* ctx) {
  kk_ssize_t slen;
  const uint8_t* s = kk_bytes_buf_borrow(b, &slen,ctx);
  kk_ssize_t tlen;
  const uint8_t* t = kk_bytes_buf_borrow(sub, &tlen,ctx);  
  kk_ssize_t idx;
  if (tlen <= 0) {
    idx = (slen <= 0 ? 0 : 1);
  }
  else if (tlen > slen) {
    idx = 0;
  }
  else {
    const uint8_t* p = kk_memmem(s, slen, t, tlen);
    idx = (p == NULL ? 0 : (p - s) + 1);
  }
  kk_bytes_drop(b, ctx);
  kk_bytes_drop(sub, ctx);
  return idx;
}

kk_ssize_t kk_bytes_last_index_of1(kk_bytes_t b, kk_bytes_t sub, kk_context_t* ctx) {
  kk_ssize_t slen;
  const uint8_t* s = kk_bytes_buf_borrow(b, &slen,ctx);
  kk_ssize_t tlen;
  const uint8_t* t = kk_bytes_buf_borrow(sub, &tlen,ctx);
  kk_ssize_t idx;
  if (tlen <= 0) {
    idx = slen;
  }
  else if (tlen > slen) {
    idx = 0;
  }
  else if (tlen == slen) {
    idx = (kk_bytes_cmp_borrow(b, sub,ctx) == 0 ? 1 : 0);
  }
  else {
    const uint8_t* p;
    for (p = s + slen - tlen; p >= s; p--) {  // todo: use reverse Boyer-Moore instead of one character at a time
      if (kk_memcmp(p, t, tlen) == 0) break;
    }
    idx = (p >= s ? (p - s) + 1 : 0);
  }
  kk_bytes_drop(b, ctx);
  kk_bytes_drop(sub, ctx);
  return idx;
}

bool kk_bytes_starts_with(kk_bytes_t b, kk_bytes_t pre, kk_context_t* ctx) {
  kk_ssize_t slen;
  const uint8_t* s = kk_bytes_buf_borrow(b, &slen,ctx);
  kk_ssize_t tlen;
  const uint8_t* t = kk_bytes_buf_borrow(pre, &tlen,ctx);
  bool starts;
  if (tlen <= 0) {
    starts = (slen > 0);
  }
  else if (tlen > slen) {
    starts = false;
  }
  else {
    starts = (kk_memcmp(s, t, tlen) == 0);
  }
  kk_bytes_drop(b, ctx);
  kk_bytes_drop(pre, ctx);
  return starts;
}

bool kk_bytes_ends_with(kk_bytes_t b, kk_bytes_t post, kk_context_t* ctx) {
  kk_ssize_t slen;
  const uint8_t* s = kk_bytes_buf_borrow(b, &slen, ctx);
  kk_ssize_t tlen;
  const uint8_t* t = kk_bytes_buf_borrow(post, &tlen, ctx);
  bool ends;
  if (tlen <= 0) {
    ends = (slen > 0);
  }
  else if (tlen > slen) {
    ends = false;
  }
  else {
    ends = (kk_memcmp(s + slen - tlen, t, tlen) == 0);
  }
  kk_bytes_drop(b, ctx);
  kk_bytes_drop(post, ctx);
  return ends;
}

bool kk_bytes_contains(kk_bytes_t b, kk_bytes_t sub, kk_context_t* ctx) {
  return (kk_bytes_index_of1(b, sub, ctx) > 0);
}


