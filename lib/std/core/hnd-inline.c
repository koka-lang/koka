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

static evv_t* evv_alloc(size_t ev_count, context_t* ctx) {
  evv_t* evv = ptr_data_as(evv_t,ptr_alloc(sizeof(evv_t) - sizeof(datatype_t) + (ev_count*sizeof(datatype_t)), 2, TAG_EVV, ctx));
  evv->len = box_int(ev_count);
  evv->ofs = box_int(0);
  return evv;
}

static struct __std_core_hnd_Ev* evv_ev_at(const evv_t* evv, intptr_t i) {
  return datatype_data_as(struct __std_core_hnd_Ev, evv->vec[i]);
}

datatype_t evv_total(context_t* ctx) {
  return datatype_from_data(evv_alloc(0,ctx));
}

int32_t evv_index( struct __std_core_hnd_Htag htag, context_t* ctx ) {
  const evv_t* evv = context_evv(ctx);
  const intptr_t len = unbox_int(evv->len);
  const intptr_t ofs = unbox_int(evv->ofs);
  for(intptr_t i = ofs; i < len; i++) {
    struct __std_core_hnd_Ev* ev = evv_ev_at(evv,i);
    if (htag._field1 == ev->_field1._field1) return (int32_t)(i - ofs); // compare string address for equality
  }
  // fatal_error("cannot find " + tag + " in " + _evv_show({evv:evv,ofs:ofs}));  
  return -1;
}

datatype_t evv_lookup( struct __std_core_hnd_Htag htag, context_t* ctx ) {
  int32_t idx = evv_index(htag,ctx);
  // if (idx < 0) fatal_error("cannot find " + tag + " in " + _evv_show({evv:evv,ofs:ofs}));  
  return evv_at(idx,ctx);
}

int32_t evv_count( context_t* ctx ) {
  evv_t* evv = context_evv(ctx);
  return (int32_t)unbox_int(evv->len);
}

datatype_t evv_create(datatype_t evv, datatype_t indices, context_t* ctx) {
  unsupported_external("evv_create");
  return datatype_null;
}

datatype_t evv_insert(datatype_t evvd, datatype_t evd, context_t* ctx) {
  const struct __std_core_hnd_Ev* ev = datatype_data_as(const struct __std_core_hnd_Ev, evd);
  const evv_t* evv1 = datatype_data_as(evv_t, evvd);
  const intptr_t len1 = unbox_int(evv1->len);
  const intptr_t ofs1 = unbox_int(evv1->ofs);  assert_internal((len1 == 0 && ofs1==0) || len1 > ofs1);
  const intptr_t n = len1 - ofs1;
  evv_t* evv2 = evv_alloc(n+1,ctx);
  intptr_t i;
  for(i = 0; i < n; i++) {
    datatype_t evd1 = evv1->vec[i + ofs1];
    const struct __std_core_hnd_Ev* ev1 = datatype_data_as(const struct __std_core_hnd_Ev,evd1);
    if (string_cmp_borrow(ev->_field1._field1,ev1->_field1._field1) <= 0) break;
    evv2->vec[i] = datatype_dup(evd1);
  }
  evv2->vec[i] = evd;
  for(; i < n; i++) {
    evv2->vec[i+1] = datatype_dup(evv1->vec[i + ofs1]);
  }
  datatype_drop(evvd,ctx);
  return datatype_from_data(evv2);
}

datatype_t evv_delete(datatype_t evvd, int32_t index, bool behind, context_t* ctx) {
  const evv_t* evv1 = datatype_data_as(evv_t, evvd);
  const int_t len1 = unbox_int(evv1->len);
  const int_t ofs1 = unbox_int(evv1->ofs);  assert_internal((len1 == 0 && ofs1==0) || len1 > ofs1);
  const int_t n = len1 - ofs1;
  if (n <= 1) {
    datatype_drop(evvd);
    return evv_total(ctx);
  }  
  if (behind) index++;
  assert_internal(index < n);
  evv_t* evv2 = evv_alloc(n-1,ctx);  
  int_t i;
  for(i = 0; i < index; i++) {
    evv2->vec[i] = datatype_dup(evv1->vec[i + ofs1]);
  }
  for(; i < n-1; i++) {
    evv2->vec[i] = datatype_dup(evv1->vec[i + ofs1 + 1]);
  }
  datatype_drop(evvd,ctx);
  return datatype_from_data(evv2);
}

string_t evv_show(datatype_t evv, context_t* ctx) {
  return string_alloc("not implemented: evv_show",ctx);
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
  function_t* conts = self->conts;  
  // call each continuation in order
  for(int_t i = 0; i < count; i++) {
    if (!unique) function_dup(conts[i]);
    x = function_call(box_t, (function_t, box_t, context_t*), conts[i], (conts[i], x, ctx));
    if (unique) conts[i] = function_null;
    if (yielding(ctx)) {
      // if yielding, `yield_next` all continuations that still need to be done
      while(++i < count) {
        if (unique) {
          yield_extend(conts[i]);
          conts[i] = function_null;
        }
        else {
          yield_extend(function_dup(conts[i]));          
        }
      }
      function_drop(fself);
      boxed_drop(x);
      return box_null; // return yielding
    }
  }
  function_drop(fself);
  return x;
}

static function_t new_kcompose( function_t* conts, int_t count ) {
  if (count<=0) return function_dup(function_id);
  if (count==1) return conts[0];
  struct kcompose_fun_s* f = ptr_data_as(struct kcompose_fun_s*, 
                                         ptr_alloc(sizeof(struct kcompose_fun_t) - sizeof(function_t) + (count*sizeof(function_t)), 
                                           2 + count /* scan size */, TAG_FUNCTION, ctx));
  f->fun = box_cptr(&kcompose);
  f->count = box_int(count);
  memcpy(f->conts, conts, count * sizeof(function_t));
  return function_from_data(f);                              
}

/*-----------------------------------------------------------------------
  Yield extension
-----------------------------------------------------------------------*/

box_t yield_extend( function_t next, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  assert_internal(yield->yielding);  // cannot extend if not yielding
  if (unlikely(yield->final)) {
    function_drop(next); // ignore extension if never resuming
  }
  else {
    if (unlikely(yield->cont_count >= YIELD_CONT_MAX)) {
      // alloc a function to compose all continuations in the array
      function_t comp = new_kcompose( yield->conts, yield->cont_count );
      yield->conts[0] = comp;
      yield->cont_count = 1;
    }
    yield->conts[yield->cont_count++] = next;
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
  function_drop(fself);
  return function_call( box_t, (function_t, function_t, box_t, context_t* ctx), f, f, cont, x, ctx));  
}

function_t new_cont_apply( function_t f, function_t cont, context_t* ctx ) {
  struct cont_apply_fun_s* self = function_alloc_as(struct cont_apply_fun_s, 2, ctx);
  self->fun = box_cptr(&cont_apply);
  self->f = f;
  self->cont = cont;
  return function_from_data(self);
}

// Unlike `yield_extend`, `yield_cont` gets access to the current continuation. This is used in `yield_prompt`.
box_t yield_cont( function_t f, context_t* ctx ) {
  assert_internal(yield->yielding); // cannot extend if not yielding
  if (unlikely(yield->final)) {
    function_drop(f); // ignore extension if never resuming
  }
  else {
    function_t cont = new_kcompose(yield->conts, yield->cont_count);
    yield->cont_count = 1;
    yield->conts[0] = new_cont_apply(f,cont);
  }
  return box_null;
}

box_t yield_to( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  assert_internal(!yield->yielding; // already yielding  
  yield->yielding = true;
  yield->final = false;
  yield->marker = m.field1;
  yield->clause = clause;
  yield->cont_count = 0;
  return box_null;
}

box_t yield_final( struct __std_core_hnd_Marker m, function_t clause, context_t* ctx ) {
  yield_to(m,clause,ctx);
  ctx->yield.final = true;
  return box_null;
}

box_t fatal_resume_final(context_t* ctx) {
  fatal_error("trying to resume a finalized resumption");
  return box_null;
}

static box_t _fatal_resume_final(function_t self, context_t* ctx) {
  return fatal_resume_final(ctx);
}
define_static_function(fun_fatal_resume_final,_fatal_resume_final);


struct __std_core_hnd_yld_s  yield_prompt( struct __std_core_hnd_Marker m, context_t* ctx ) {
  yield_t* yield = &ctx->yield;
  if (!yield->yielding) {
    return __std_core_hnd__new_Pure(ctx);
  }
  else if (yield->marker != m.field1) {
    return (yield->final ? __std_core_hnd__new_YieldingFinal(ctx) : __std_core_hnd__new_Yielding(ctx));
  }
  else {
    yield->yielding = false;
    function_t cont = (yield->final ? function_dup(_fun_fatal_resume_final) : new_kcompose(yield->conts, yield->cont_count));
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
    fatal_error("trying to resume outside the (handler) scope of the original handler");
  }
}
