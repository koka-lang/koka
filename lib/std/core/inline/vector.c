

/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_std_core_types__list kk_vector_to_list(kk_vector_t v, kk_std_core_types__list tail, kk_context_t* ctx) {
  // todo: avoid boxed_dup if v is unique
  kk_ssize_t n;
  kk_box_t* p = kk_vector_buf_borrow(v, &n, ctx);
  if (n <= 0) {
    kk_vector_drop(v,ctx);
    return tail;
  }
  kk_std_core_types__list nil  = kk_std_core_types__new_Nil(ctx);
  struct kk_std_core_types_Cons* cons = NULL;
  kk_std_core_types__list list = kk_std_core_types__new_Nil(ctx);
  for( kk_ssize_t i = 0; i < n; i++ ) {
    kk_std_core_types__list hd = kk_std_core_types__new_Cons(kk_reuse_null,0,kk_box_dup(p[i],ctx), nil, ctx);
    if (cons==NULL) {
      list = hd;
    }
    else {
      cons->tail = hd;
    }
    cons = kk_std_core_types__as_Cons(hd,ctx);
  }
  if (cons == NULL) { list = tail; }
               else { cons->tail = tail; }
  kk_vector_drop(v,ctx);
  return list;
}

kk_vector_t kk_list_to_vector(kk_std_core_types__list xs, kk_context_t* ctx) {
  // todo: avoid boxed_dup if xs is unique
  // find the length
  kk_ssize_t len = 0;
  kk_std_core_types__list ys = xs;
  while (kk_std_core_types__is_Cons(ys,ctx)) {
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(ys,ctx);
    len++;
    ys = cons->tail;
  }
  // alloc the vector and copy
  kk_box_t* p;
  kk_vector_t v = kk_vector_alloc_uninit(len, &p, ctx);
  ys = xs;
  for( kk_ssize_t i = 0; i < len; i++) {
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(ys,ctx);
    ys = cons->tail;
    p[i] = kk_box_dup(cons->head,ctx);
  }
  kk_std_core_types__list_drop(xs,ctx);  // todo: drop while visiting?
  return v;
}


kk_vector_t kk_vector_init_total( kk_ssize_t n, kk_function_t init, kk_context_t* ctx) {
  kk_vector_t v = kk_vector_alloc(n, kk_box_null(), ctx);
  kk_box_t* p = kk_vector_buf_borrow(v, NULL, ctx);
  for(kk_ssize_t i = 0; i < n; i++) {
    kk_function_dup(init,ctx);
    p[i] = kk_function_call(kk_box_t,(kk_function_t,kk_ssize_t,kk_context_t*),init,(init,i,ctx),ctx);
  }
  kk_function_drop(init,ctx);
  return v;
}
