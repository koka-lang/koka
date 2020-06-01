/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
/*
typedef datatype_t __std_core_hnd__ev;
struct __std_core_hnd_Ev {
  __std_core_hnd__htag _field1;
  box_t _field3;
  __std_core_hnd__evv _field4;
  __std_core_hnd__marker _field2;
};
*/

static inline struct __std_core_hnd_Ev* unbox_ev(box_t b, context_t* ctx) {
  return datatype_data_as(struct __std_core_hnd_Ev, unbox___std_core_hnd__ev(b,ctx));
}

static inline box_t* evv_as_vec(datatype_t evv, size_t* len, box_t* single) {  
  if (datatype_tag(evv)==TAG_VECTOR) {
    return vector_buf(evv,len);
  }
  else {
    // single evidence
    *single = box_datatype(evv);
    *len = 1;
    return single;
  }
}

int32_t evv_index( struct __std_core_hnd_Htag htag, context_t* ctx ) {
  // todo: drop htag?
  size_t len;
  box_t single;
  box_t* vec = evv_as_vec(ctx->evv,&len,&single);
  for(size_t i = 0; i < len; i++) {
    struct __std_core_hnd_Ev* ev = unbox_ev(vec[i],ctx);
    if (htag._field1 == ev->_field1._field1) return (int32_t)(i); // compare string address for equality
  }
  string_t evvs = evv_show(datatype_dup(ctx->evv),ctx);
  fatal_error(EFAULT,"cannot find tag '%s' in: %s", string_cbuf_borrow(htag._field1), string_cbuf_borrow(evvs));
  string_drop(evvs,ctx);  
  return -1;
}

datatype_t evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx ) {
  // todo: drop htag
  int32_t idx = evv_index(htag,ctx);
  return evv_at(idx,ctx);
}


datatype_t evv_insert(datatype_t evvd, datatype_t evd, context_t* ctx) {
  const struct __std_core_hnd_Ev* ev = datatype_data_as(const struct __std_core_hnd_Ev, evd);
  size_t n;
  box_t single;
  const box_t* evv1 = evv_as_vec(evvd, &n, &single);
  const vector_t vec2 = vector_alloc(box_null,n+1,ctx);  // TODO: do not initialize
  box_t* const evv2 = vector_buf(vec2,NULL);
  size_t i;
  for(i = 0; i < n; i++) {
    box_t evb1 = evv1[i];
    const struct __std_core_hnd_Ev* ev1 = unbox_ev(evb1,ctx);
    if (string_cmp_borrow(ev->_field1._field1,ev1->_field1._field1) <= 0) break;
    evv2[i] = boxed_dup(evb1);
  }
  evv2[i] = box_datatype(evd);
  for(; i < n; i++) {
    evv2[i+1] = boxed_dup(evv1[i]);
  }
  datatype_drop(evvd,ctx);
  return vec2;
}

datatype_t evv_delete(datatype_t evvd, int32_t index, bool behind, context_t* ctx) {
  size_t n;
  box_t single;
  const box_t* evv1 = evv_as_vec(evvd, &n, &single);
  if (n <= 1) {
    datatype_drop(evvd,ctx);
    return evv_total(ctx);
  }
  if (behind) index++;
  assert_internal(index < n);  
  const vector_t vec2 = vector_alloc(box_null,n+1,ctx);  // TODO: do not initialize
  box_t* const evv2 = vector_buf(vec2,NULL);
  size_t i;
  for(i = 0; i < (size_t)index; i++) {
    evv2[i] = datatype_dup(evv1[i]);
  }
  for(; i < n-1; i++) {
    evv2[i] = datatype_dup(evv1[i+1]);
  }
  datatype_drop(evvd,ctx);
  return vec2;
}

string_t evv_show(datatype_t evv, context_t* ctx) {
  return string_alloc_dup("(not yet implemented: evv_show)",ctx);
}



/*-----------------------------------------------------------------------
  Compose continuations
-----------------------------------------------------------------------*/

struct kcompose_fun_s {
  struct function_s fun;  
  box_t      count;
  function_t conts[1];
};

// kleisli composition of continuations
static box_t kcompose( function_t fself, box_t x, context_t* ctx) {
  struct kcompose_fun_s* self = function_data_as(struct kcompose_fun_s,fself);
  bool unique = function_is_unique(fself);
  int_t count = unbox_int(self->count);
  function_t* conts = &self->conts[0];  
  // call each continuation in order
  for(int_t i = 0; i < count; i++) {
    if (!unique) function_dup(conts[i]);
    x = function_call(box_t, (function_t, box_t, context_t*), conts[i], (conts[i], x, ctx));
    if (unique) conts[i] = function_null;
    if (yielding(ctx)) {
      // if yielding, `yield_next` all continuations that still need to be done
      while(++i < count) {
        if (unique) {
          yield_extend(conts[i],ctx);
          conts[i] = function_null;
        }
        else {
          yield_extend(function_dup(conts[i]),ctx);          
        }
      }
      function_drop(fself,ctx);
      boxed_drop(x,ctx);
      return box_null; // return yielding
    }
  }
  function_drop(fself,ctx);
  return x;
}

static function_t new_kcompose( function_t* conts, int_t count, context_t* ctx ) {
  if (count<=0) return function_dup(function_id);
  if (count==1) return conts[0];
  struct kcompose_fun_s* f = ptr_data_as(struct kcompose_fun_s, 
                                         ptr_alloc(sizeof(struct kcompose_fun_s) - sizeof(function_t) + (count*sizeof(function_t)), 
                                           2 + count /* scan size */, TAG_FUNCTION, ctx));
  f->fun.fun = box_cptr(&kcompose);
  f->count = box_int(count);
  memcpy(f->conts, conts, count * sizeof(function_t));
  return function_from_data(&f->fun);                              
}

/*-----------------------------------------------------------------------
  Yield extension
-----------------------------------------------------------------------*/

box_t yield_extend( function_t next, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  assert_internal(yield->yielding);  // cannot extend if not yielding
  if (unlikely(yield->yielding==YIELD_FINAL)) {
    function_drop(next,ctx); // ignore extension if never resuming
  }
  else {
    if (unlikely(yield->conts_count >= YIELD_CONT_MAX)) {
      // alloc a function to compose all continuations in the array
      function_t comp = new_kcompose( yield->conts, yield->conts_count, ctx );
      yield->conts[0] = comp;
      yield->conts_count = 1;
    }
    yield->conts[yield->conts_count++] = next;
  }
  return box_null;
}

// cont_apply: \x -> f(cont,x) 
struct cont_apply_fun_s {
  struct function_s fun;
  function_t f;
  function_t cont;
};

static box_t cont_apply( function_t fself, box_t x, context_t* ctx ) {
  struct cont_apply_fun_s* self = function_data_as(struct cont_apply_fun_s, fself);
  function_t f = function_dup(self->f);
  function_t cont = function_dup(self->cont);
  function_drop(fself,ctx);
  return function_call( box_t, (function_t, function_t, box_t, context_t* ctx), f, (f, cont, x, ctx));  
}

function_t new_cont_apply( function_t f, function_t cont, context_t* ctx ) {
  struct cont_apply_fun_s* self = function_alloc_as(struct cont_apply_fun_s, 2, ctx);
  self->fun.fun = box_cptr(&cont_apply);
  self->f = f;
  self->cont = cont;
  return function_from_data(&self->fun);
}

// Unlike `yield_extend`, `yield_cont` gets access to the current continuation. This is used in `yield_prompt`.
box_t yield_cont( function_t f, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  assert_internal(yield->yielding); // cannot extend if not yielding
  if (unlikely(yield->yielding == YIELD_FINAL)) {
    function_drop(f,ctx); // ignore extension if never resuming
  }
  else {
    function_t cont = new_kcompose(yield->conts, yield->conts_count, ctx);
    yield->conts_count = 1;
    yield->conts[0] = new_cont_apply(f, cont, ctx);
  }
  return box_null;
}

box_t yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  assert_internal(!yield->yielding); // already yielding  
  yield->yielding = YIELD_NORMAL;
  yield->marker = m.m;
  yield->clause = clause;
  yield->conts_count = 0;
  return box_null;
}

box_t yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx ) {
  yield_to(m,clause,ctx);
  ctx->yield.yielding = YIELD_FINAL;
  return box_null;
}

box_t fatal_resume_final(context_t* ctx) {
  fatal_error(EFAULT,"trying to resume a finalized resumption");
  return box_null;
}

static box_t _fatal_resume_final(function_t self, context_t* ctx) {
  return fatal_resume_final(ctx);
}
define_static_function(fun_fatal_resume_final,_fatal_resume_final);


struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  if (yield->yielding == YIELD_NONE) {
    return __std_core_hnd__new_Pure(ctx);
  }
  else if (yield->marker != m.m) {
    return (yield->yielding == YIELD_FINAL ? __std_core_hnd__new_YieldingFinal(ctx) : __std_core_hnd__new_Yielding(ctx));
  }
  else {
    yield->yielding = 0;
    function_t cont = (yield->yielding == YIELD_FINAL ? function_dup(fun_fatal_resume_final) : new_kcompose(yield->conts, yield->conts_count, ctx));
    function_t clause = yield->clause;
    #ifndef NDEBUG
    memset(yield,0,sizeof(yield_t));
    #endif
    return __std_core_hnd__new_Yield(clause, cont, ctx);
  }
}

unit_t  evv_guard(datatype_t evv, context_t* ctx) {
  if (ctx->evv != evv) {
    // todo: improve error message with diagnostics
    fatal_error(EFAULT,"trying to resume outside the (handler) scope of the original handler");
  }
  return Unit;
}


datatype_t evv_create(datatype_t evv, datatype_t indices, context_t* ctx) {
  unsupported_external("evv_create");
  return datatype_null;
}

datatype_t evv_swap_create( vector_t indices, context_t* ctx ) {
  size_t len;
  box_t* vec = vector_buf(indices,&len);
  if (len==0) {
    vector_drop(indices,ctx);
    return evv_swap_create0(ctx);
  }
  if (len==1) {
    int_t i = unbox_int(vec[0]);
    vector_drop(indices,ctx);
    return evv_swap_create1((int32_t)i,ctx);
  }
  return evv_swap( evv_create(datatype_dup(ctx->evv),indices,ctx), ctx );
}
