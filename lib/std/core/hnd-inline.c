

/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
/*
typedef datatype_t kk_std_core_hnd__ev;
struct kk_std_core_hnd_Ev {
  kk_std_core_hnd__htag _field1;
  kk_box_t _field3;
  kk_std_core_hnd__evv _field4;
  kk_std_core_hnd__marker _field2;
};
*/

static inline struct kk_std_core_hnd_Ev* kk_ev_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_std_core_hnd__as_Ev(kk_std_core_hnd__ev_unbox(b,ctx));
}

static inline kk_box_t* kk_evv_as_vec(kk_evv_t evv, size_t* len, kk_box_t* single) {  
  if (kk_evv_is_vector(evv)) {
    return kk_vector_buf(evv,len);
  }
  else {
    // single evidence
    *single = kk_datatype_box(evv);
    *len = 1;
    return single;
  }
}

struct kk_std_core_hnd__ev_s* kk_ev_none(kk_context_t* ctx) {
  static kk_std_core_hnd__ev ev_none_singleton;
  if (ev_none_singleton==NULL) {
    ev_none_singleton = kk_std_core_hnd__new_Ev(
      kk_reuse_null,
      kk_std_core_hnd__new_Htag(kk_string_dup(kk_string_empty),ctx), // tag ""
      kk_std_core_hnd__new_Marker(-1,ctx),                       // marker -1
      kk_box_null,                                                 // no handler
      kk_vector_empty(),
      ctx
    );      
  }
  return kk_std_core_hnd__ev_dup(ev_none_singleton);
}


int32_t kk_evv_index( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx ) {
  // todo: drop htag?
  size_t len;
  kk_box_t single;
  kk_box_t* vec = kk_evv_as_vec(ctx->evv,&len,&single);
  for(size_t i = 0; i < len; i++) {
    struct kk_std_core_hnd_Ev* ev = kk_ev_unbox(vec[i],ctx);
    if (kk_string_cmp_borrow(htag._field1,ev->_field1._field1) <= 0) return (int32_t)(i); // break on insertion point
  }
  //string_t evvs = kk_evv_show(dup_datatype_as(kk_evv_t,ctx->evv),ctx);
  //fatal_error(EFAULT,"cannot find tag '%s' in: %s", string_cbuf_borrow(htag._field1), string_cbuf_borrow(evvs));
  //drop_string_t(evvs,ctx);  
  return (int32_t)len;
}

kk_std_core_hnd__ev kk_evv_lookup( struct kk_std_core_hnd_Htag htag, kk_context_t* ctx ) {
  // todo: drop htag
  int32_t idx = kk_evv_index(htag,ctx);
  return kk_evv_at(idx,ctx);
}


kk_evv_t kk_evv_insert(kk_evv_t evvd, kk_std_core_hnd__ev evd, kk_context_t* ctx) {
  struct kk_std_core_hnd_Ev* ev = kk_std_core_hnd__as_Ev(evd);
  // update ev
  int32_t marker = ev->_field2.m;
  if (marker < 0) { return evvd; } // ev-none 
  kk_evv_drop(ev->_field4,ctx);
  ev->_field4 = evvd;     // dup evvd
  if (marker==0) { return kk_evv_dup(evvd); } // zero marker means this evidence is not in the evidence vector
  // insert ev
  size_t n;
  kk_box_t single;
  const kk_box_t* evv1 = kk_evv_as_vec(evvd, &n, &single);
  const kk_vector_t vec2 = kk_vector_alloc(n+1,kk_box_null,ctx);
  kk_box_t* const evv2 = kk_vector_buf(vec2,NULL);
  size_t i;
  for(i = 0; i < n; i++) {
    kk_box_t evb1 = evv1[i];
    const struct kk_std_core_hnd_Ev* ev1 = kk_ev_unbox(evb1,ctx);
    if (kk_string_cmp_borrow(ev->_field1._field1,ev1->_field1._field1) <= 0) break;
    evv2[i] = kk_box_dup(evb1); // use dup_datatype for efficiency?
  }
  evv2[i] = kk_basetype_box(evd);
  for(; i < n; i++) {
    evv2[i+1] = kk_box_dup(evv1[i]);  // use dup_datatype for efficiency?
  }
  // drop_datatype(evvd,ctx);  // assigned to evidence already
  return vec2;
}

kk_evv_t kk_evv_delete(kk_evv_t evvd, int32_t index, bool behind, kk_context_t* ctx) {
  size_t n;
  kk_box_t single;
  const kk_box_t* evv1 = kk_evv_as_vec(evvd, &n, &single);
  if (n <= 1) {
    kk_datatype_drop(evvd,ctx);
    return kk_evv_total(ctx);
  }
  if (behind) index++;
  kk_assert_internal((size_t)index < n);  
  const kk_vector_t vec2 = kk_vector_alloc(n+1,kk_box_null,ctx);  
  kk_box_t* const evv2 = kk_vector_buf(vec2,NULL);
  size_t i;
  for(i = 0; i < (size_t)index; i++) {
    evv2[i] = kk_box_dup(evv1[i]);  // todo: use box_datatype for efficiency?
  }
  for(; i < n-1; i++) {
    evv2[i] = kk_box_dup(evv1[i+1]);
  }
  kk_datatype_drop(evvd,ctx);
  return vec2;
}

kk_string_t kk_evv_show(kk_evv_t evv, kk_context_t* ctx) {
  return kk_string_alloc_dup("(not yet implemented: kk_evv_show)",ctx);
}



/*-----------------------------------------------------------------------
  Compose continuations
-----------------------------------------------------------------------*/

struct kcompose_fun_s {
  struct kk_function_s _base;  
  kk_box_t      count;
  kk_function_t conts[1];
};

// kleisli composition of continuations
static kk_box_t kcompose( kk_function_t fself, kk_box_t x, kk_context_t* ctx) {
  struct kcompose_fun_s* self = kk_function_as(struct kcompose_fun_s*,fself);
  kk_intx_t count = kk_intx_unbox(self->count);
  kk_function_t* conts = &self->conts[0];  
  // call each continuation in order
  for(kk_intx_t i = 0; i < count; i++) {
    // todo: take uniqueness of fself into account to avoid dup_function
    kk_function_t f = kk_function_dup(conts[i]);
    x = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), f, (f, x, ctx));
    if (kk_yielding(ctx)) {
      // if yielding, `yield_next` all continuations that still need to be done
      while(++i < count) {
        // todo: if fself is unique, we could copy without dup?
        kk_yield_extend(kk_function_dup(conts[i]),ctx);        
      }
      kk_function_drop(fself,ctx);
      kk_box_drop(x,ctx);
      return kk_box_any(ctx); // return yielding
    }
  }
  kk_function_drop(fself,ctx);
  return x;
}

static kk_function_t new_kcompose( kk_function_t* conts, kk_intx_t count, kk_context_t* ctx ) {
  if (count<=0) return kk_function_id(ctx);
  if (count==1) return conts[0];
  struct kcompose_fun_s* f = kk_block_as(struct kcompose_fun_s*, 
                               kk_block_alloc(sizeof(struct kcompose_fun_s) - sizeof(kk_function_t) + (count*sizeof(kk_function_t)), 
                                 2 + count /* scan size */, KK_TAG_FUNCTION, ctx));
  f->_base.fun = kk_cptr_box(&kcompose,ctx);
  f->count = kk_intx_box(count);
  memcpy(f->conts, conts, count * sizeof(kk_function_t));
  return (&f->_base);                              
}

/*-----------------------------------------------------------------------
  Yield extension
-----------------------------------------------------------------------*/

kk_box_t kk_yield_extend( kk_function_t next, kk_context_t* ctx ) {
  kk_yield_t* yield = &ctx->yield;
  kk_assert_internal(kk_yielding(ctx));  // cannot extend if not yielding
  if (kk_unlikely(kk_yielding_final(ctx))) {
    // todo: can we optimize this so `next` is never allocated in the first place?
    kk_function_drop(next,ctx); // ignore extension if never resuming
  }
  else {
    if (kk_unlikely(yield->conts_count >= KK_YIELD_CONT_MAX)) {
      // alloc a function to compose all continuations in the array
      kk_function_t comp = new_kcompose( yield->conts, yield->conts_count, ctx );
      yield->conts[0] = comp;
      yield->conts_count = 1;
    }
    yield->conts[yield->conts_count++] = next;
  }
  return kk_box_any(ctx);
}

// cont_apply: \x -> f(cont,x) 
struct cont_apply_fun_s {
  struct kk_function_s _base;
  kk_function_t f;
  kk_function_t cont;
};

static kk_box_t cont_apply( kk_function_t fself, kk_box_t x, kk_context_t* ctx ) {
  struct cont_apply_fun_s* self = kk_function_as(struct cont_apply_fun_s*, fself);
  kk_function_t f = self->f;
  kk_function_t cont = self->cont;  
  kk_drop_match(self,{kk_function_dup(f);kk_function_dup(cont);},{},ctx);
  return kk_function_call( kk_box_t, (kk_function_t, kk_function_t, kk_box_t, kk_context_t* ctx), f, (f, cont, x, ctx));  
}

kk_function_t kk_new_cont_apply( kk_function_t f, kk_function_t cont, kk_context_t* ctx ) {
  struct cont_apply_fun_s* self = kk_function_alloc_as(struct cont_apply_fun_s, 2, ctx);
  self->_base.fun = kk_cptr_box(&cont_apply,ctx);
  self->f = f;
  self->cont = cont;
  return (&self->_base);
}

// Unlike `yield_extend`, `yield_cont` gets access to the current continuation. This is used in `yield_prompt`.
kk_box_t kk_yield_cont( kk_function_t f, kk_context_t* ctx ) {
  kk_yield_t* yield = &ctx->yield;
  kk_assert_internal(kk_yielding(ctx)); // cannot extend if not yielding
  if (kk_unlikely(kk_yielding_final(ctx))) {
    kk_function_drop(f,ctx); // ignore extension if never resuming
  }
  else {
    kk_function_t cont = new_kcompose(yield->conts, yield->conts_count, ctx);
    yield->conts_count = 1;
    yield->conts[0] = kk_new_cont_apply(f, cont, ctx);
  }
  return kk_box_any(ctx);
}

kk_function_t kk_yield_to( struct kk_std_core_hnd_Marker m, kk_function_t clause, kk_context_t* ctx ) {
  kk_yield_t* yield = &ctx->yield;
  kk_assert_internal(!kk_yielding(ctx)); // already yielding  
  ctx->yielding = KK_YIELD_NORMAL;
  yield->marker = m.m;
  yield->clause = clause;
  yield->conts_count = 0;
  return kk_basetype_unbox_as(kk_function_t,kk_box_any(ctx));
}

kk_box_t kk_yield_final( struct kk_std_core_hnd_Marker m, kk_function_t clause, kk_context_t* ctx ) {
  kk_yield_to(m,clause,ctx);
  ctx->yielding = KK_YIELD_FINAL;
  return kk_box_any(ctx);
}

kk_box_t kk_fatal_resume_final(kk_context_t* ctx) {
  kk_fatal_error(EFAULT,"trying to resume a finalized resumption");
  return kk_box_any(ctx);
}

static kk_box_t _fatal_resume_final(kk_function_t self, kk_context_t* ctx) {
  kk_function_drop(self,ctx);
  return kk_fatal_resume_final(ctx);
}
static kk_function_t fun_fatal_resume_final(kk_context_t* ctx) {
  kk_define_static_function(f,_fatal_resume_final,ctx);
  return kk_function_dup(f);
}


struct kk_std_core_hnd_yld_s kk_yield_prompt( struct kk_std_core_hnd_Marker m, kk_context_t* ctx ) {
  kk_yield_t* yield = &ctx->yield;
  if (ctx->yielding == KK_YIELD_NONE) {
    return kk_std_core_hnd__new_Pure(ctx);
  }
  else if (yield->marker != m.m) {
    return (ctx->yielding == KK_YIELD_FINAL ? kk_std_core_hnd__new_YieldingFinal(ctx) : kk_std_core_hnd__new_Yielding(ctx));
  }
  else {
    kk_function_t cont = (ctx->yielding == KK_YIELD_FINAL ? fun_fatal_resume_final(ctx) : new_kcompose(yield->conts, yield->conts_count, ctx));
    kk_function_t clause = yield->clause;
    ctx->yielding = KK_YIELD_NONE;
    #ifndef NDEBUG
    memset(yield,0,sizeof(kk_yield_t));
    #endif
    return kk_std_core_hnd__new_Yield(clause, cont, ctx);
  }
}

kk_unit_t  kk_evv_guard(kk_evv_t evv, kk_context_t* ctx) {
  if (!kk_datatype_eq(ctx->evv,evv)) {
    // todo: improve error message with diagnostics
    kk_fatal_error(EFAULT,"trying to resume outside the (handler) scope of the original handler");
  }
  return kk_Unit;
}


kk_evv_t kk_evv_create(kk_evv_t evv, kk_vector_t indices, kk_context_t* ctx) {
  kk_unsupported_external("kk_evv_create");
  return kk_vector_empty();
}

kk_evv_t kk_evv_swap_create( kk_vector_t indices, kk_context_t* ctx ) {
  size_t len;
  kk_box_t* vec = kk_vector_buf(indices,&len);
  if (len==0) {
    kk_vector_drop(indices,ctx);
    return kk_evv_swap_create0(ctx);
  }
  if (len==1) {
    int32_t i = kk_int32_unbox(vec[0],ctx);
    kk_vector_drop(indices,ctx);
    return kk_evv_swap_create1(i,ctx);
  }
  return kk_evv_swap( kk_evv_create(kk_evv_dup(ctx->evv),indices,ctx), ctx );
}

typedef struct yield_info_s {
  struct kk_std_core_hnd__yield_info_s _base;
  kk_function_t clause;          
  kk_function_t conts[KK_YIELD_CONT_MAX];
  size_t     conts_count;     
  int32_t    marker;          
  uint8_t    yielding;
}* yield_info_t;

kk_std_core_hnd__yield_info kk_yield_capture(kk_context_t* ctx) {  
  kk_assert_internal(kk_yielding(ctx));
  yield_info_t yld = kk_block_alloc_as(struct yield_info_s, 1 + KK_YIELD_CONT_MAX, (kk_tag_t)1, ctx);
  yld->clause = ctx->yield.clause; 
  size_t i = 0;
  for( ; i < ctx->yield.conts_count; i++) {
    yld->conts[i] = ctx->yield.conts[i];
  }
  for( ; i < KK_YIELD_CONT_MAX; i++) {
    yld->conts[i] = kk_function_null(ctx);
  }
  yld->conts_count = ctx->yield.conts_count;
  yld->marker = ctx->yield.marker;
  yld->yielding = ctx->yielding;
  ctx->yielding = 0;
  ctx->yield.conts_count = 0;
  return kk_datatype_from_base(&yld->_base);
}

kk_box_t kk_yield_reyield( kk_std_core_hnd__yield_info yldinfo, kk_context_t* ctx) {
  kk_assert_internal(!kk_yielding(ctx));
  yield_info_t yld = kk_datatype_as_assert(yield_info_t, yldinfo, (kk_tag_t)1);  
  ctx->yield.clause = kk_function_dup(yld->clause);
  ctx->yield.marker = yld->marker;
  ctx->yield.conts_count = yld->conts_count;
  ctx->yielding = yld->yielding;
  for(size_t i = 0; i < yld->conts_count; i++) {
    ctx->yield.conts[i] = kk_function_dup(yld->conts[i]);
  }
  kk_constructor_drop(yld,ctx);
  return kk_box_any(ctx);
}
