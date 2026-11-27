/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Canonical Unicode normalization (NFC and NFD) as described in <https://www.unicode.org/reports/tr15>,
   using the tables in `unicode-tables.c`. The compatibility forms (NFKC and NFKD)
   are treated as their canonical counterparts.
*/

// Hangul syllables are composed algorithmically
#define KK_HANGUL_SBASE   0xAC00
#define KK_HANGUL_LBASE   0x1100
#define KK_HANGUL_VBASE   0x1161
#define KK_HANGUL_TBASE   0x11A7
#define KK_HANGUL_LCOUNT  19
#define KK_HANGUL_VCOUNT  21
#define KK_HANGUL_TCOUNT  28
#define KK_HANGUL_NCOUNT  (KK_HANGUL_VCOUNT * KK_HANGUL_TCOUNT)
#define KK_HANGUL_SCOUNT  (KK_HANGUL_LCOUNT * KK_HANGUL_NCOUNT)

#define kk_unicode_count(table)  (sizeof(table)/sizeof(table[0]))

// Binary search rows (of `stride` entries) sorted on the first column (and the second one if `pair` is true)
static const int32_t* kk_unicode_search(const int32_t* rows, size_t count, size_t stride, int32_t k0, int32_t k1, bool pair) {
  size_t lo = 0;
  size_t hi = count;
  while (lo < hi) {
    const size_t mid = (lo + hi) / 2;
    const int32_t* row = rows + (mid * stride);
    const int32_t cmp = (row[0] != k0 ? row[0] - k0 : (pair ? row[1] - k1 : 0));
    if (cmp == 0) return row;
    if (cmp < 0) { lo = mid + 1; } else { hi = mid; }
  }
  return NULL;
}

static int32_t kk_unicode_ccc_of(kk_char_t c) {
  if (c < 0x300) return 0;
  const int32_t* row = kk_unicode_search(&kk_unicode_ccc[0][0], kk_unicode_count(kk_unicode_ccc), 2, c, 0, false);
  return (row == NULL ? 0 : row[1]);
}

// Write the full canonical decomposition of `c` to `out` (at most 4 code points) and return its length
static kk_ssize_t kk_unicode_decompose(kk_char_t c, kk_char_t* out) {
  const int32_t s = c - KK_HANGUL_SBASE;
  if (s >= 0 && s < KK_HANGUL_SCOUNT) {
    out[0] = KK_HANGUL_LBASE + (s / KK_HANGUL_NCOUNT);
    out[1] = KK_HANGUL_VBASE + ((s % KK_HANGUL_NCOUNT) / KK_HANGUL_TCOUNT);
    const int32_t t = s % KK_HANGUL_TCOUNT;
    if (t == 0) return 2;
    out[2] = KK_HANGUL_TBASE + t;
    return 3;
  }
  const int32_t* row = (c < 0xC0 ? NULL : kk_unicode_search(&kk_unicode_decomp[0][0], kk_unicode_count(kk_unicode_decomp), 3, c, 0, false));
  if (row == NULL) {
    out[0] = c;
    return 1;
  }
  kk_ssize_t n = kk_unicode_decompose(row[1], out);
  if (row[2] != 0) { n += kk_unicode_decompose(row[2], out + n); }
  return n;
}

// Stable sort each run of non-starters on their combining class
static void kk_unicode_reorder(kk_char_t* buf, kk_ssize_t n) {
  for (kk_ssize_t i = 1; i < n; i++) {
    const kk_char_t c = buf[i];
    const int32_t ccc = kk_unicode_ccc_of(c);
    if (ccc == 0) continue;
    kk_ssize_t j = i;
    for (; j > 0 && kk_unicode_ccc_of(buf[j-1]) > ccc; j--) {
      buf[j] = buf[j-1];
    }
    buf[j] = c;
  }
}

// Return the primary composite of `a` and `b`, or 0 if there is none
static kk_char_t kk_unicode_compose_pair(kk_char_t a, kk_char_t b) {
  const int32_t l = a - KK_HANGUL_LBASE;
  const int32_t v = b - KK_HANGUL_VBASE;
  if (l >= 0 && l < KK_HANGUL_LCOUNT && v >= 0 && v < KK_HANGUL_VCOUNT) {
    return KK_HANGUL_SBASE + (((l * KK_HANGUL_VCOUNT) + v) * KK_HANGUL_TCOUNT);
  }
  const int32_t s = a - KK_HANGUL_SBASE;
  const int32_t t = b - KK_HANGUL_TBASE;
  if (s >= 0 && s < KK_HANGUL_SCOUNT && (s % KK_HANGUL_TCOUNT) == 0 && t > 0 && t < KK_HANGUL_TCOUNT) {
    return a + t;
  }
  const int32_t* row = kk_unicode_search(&kk_unicode_comp[0][0], kk_unicode_count(kk_unicode_comp), 3, a, b, true);
  return (row == NULL ? 0 : row[2]);
}

// Canonically compose a decomposed and reordered buffer in place and return the new length
static kk_ssize_t kk_unicode_compose(kk_char_t* buf, kk_ssize_t n) {
  if (n == 0) return 0;
  kk_ssize_t starter = 0;
  int32_t last = (kk_unicode_ccc_of(buf[0]) == 0 ? 0 : 256);  // a leading non-starter blocks composition
  kk_ssize_t m = 1;
  for (kk_ssize_t i = 1; i < n; i++) {
    const kk_char_t c = buf[i];
    const int32_t ccc = kk_unicode_ccc_of(c);
    const kk_char_t comp = kk_unicode_compose_pair(buf[starter], c);
    if (comp != 0 && (last < ccc || last == 0)) {
      buf[starter] = comp;
      continue;
    }
    if (ccc == 0) { starter = m; }
    last = ccc;
    buf[m++] = c;
  }
  return m;
}

static kk_string_t kk_unicode_normalize(kk_string_t s, kk_string_t form, kk_context_t* ctx) {
  const bool compose = (strchr(kk_string_cbuf_borrow(form, NULL, ctx), 'C') != NULL);
  kk_string_drop(form, ctx);
  kk_ssize_t len;
  const uint8_t* p = kk_string_buf_borrow(s, &len, ctx);
  // quick check: code points below U+0300 are already composed, and those below U+00C0 are also decomposed
  const uint8_t qmax = (compose ? 0xCC : 0xC3);
  kk_ssize_t i = 0;
  while (i < len && p[i] < qmax) { i++; }
  if (i == len) return s;
  // decompose: each code point takes at least one byte and decomposes to at most 4 code points
  kk_char_t* buf = (kk_char_t*)kk_malloc(4 * len * kk_ssizeof(kk_char_t), ctx);
  kk_ssize_t n = 0;
  for (const uint8_t* q = p; q < p + len; ) {
    kk_ssize_t count;
    const kk_char_t c = kk_utf8_read(q, &count);
    q += count;
    n += kk_unicode_decompose(c, buf + n);
  }
  kk_unicode_reorder(buf, n);
  if (compose) { n = kk_unicode_compose(buf, n); }
  // encode
  kk_ssize_t m = 0;
  for (i = 0; i < n; i++) { m += kk_utf8_len(buf[i]); }
  uint8_t* out;
  kk_string_t t = kk_unsafe_string_alloc_buf(m, &out, ctx);
  for (i = 0; i < n; i++) {
    kk_ssize_t count;
    kk_utf8_write(buf[i], out, &count);
    out += count;
  }
  kk_free(buf, ctx);
  kk_string_drop(s, ctx);
  return t;
}
