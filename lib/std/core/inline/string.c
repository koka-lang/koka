
/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_std_core_types__list kk_string_to_list(kk_string_t s, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* p = kk_string_buf_borrow(s,&len,ctx);
  const uint8_t* const end = p + len;
  kk_std_core_types__list nil  = kk_std_core_types__new_Nil(ctx);
  kk_std_core_types__list list = nil;
  struct kk_std_core_types_Cons* tl = NULL;
  kk_ssize_t count;
  while( p < end ) {
    kk_char_t c = kk_utf8_read(p,&count);
    p += count;
    kk_std_core_types__list cons = kk_std_core_types__new_Cons(kk_reuse_null,0,kk_char_box(c,ctx), nil, ctx);
    if (tl!=NULL) {
      tl->tail = cons;
    }
    else {
      list = cons;
    }
    tl = kk_std_core_types__as_Cons(cons,ctx);
  }
  kk_string_drop(s,ctx);
  return list;
}

kk_string_t kk_string_from_list(kk_std_core_types__list cs, kk_context_t* ctx) {
  // TODO: optimize for short strings to write directly into a local buffer?
  // find total UTF8 length
  kk_ssize_t len = 0;
  kk_std_core_types__list xs = cs;
  while (kk_std_core_types__is_Cons(xs,ctx)) {
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(xs,ctx);
    len += kk_utf8_len(kk_char_unbox(cons->head,KK_BORROWED,ctx));
    xs = cons->tail;
  }
  // allocate and copy the characters
  uint8_t* p;
  kk_string_t s = kk_unsafe_string_alloc_buf(len,&p,ctx);  // must be initialized
  xs = cs;
  while (kk_std_core_types__is_Cons(xs,ctx)) {
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(xs,ctx);
    kk_ssize_t count;
    kk_utf8_write( kk_char_unbox(cons->head,KK_BORROWED,ctx), p, &count );
    p += count;
    xs = cons->tail;
  }
  kk_assert_internal(*p == 0 && (p - kk_string_buf_borrow(s,NULL,ctx)) == len);
  kk_std_core_types__list_drop(cs,ctx);  // todo: drop while visiting?
  return s;
}
