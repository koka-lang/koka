/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"


void fatal_error(const char* msg) {
  fputs(msg, stderr);
  abort();
}

// ptr_null
static block_t ptr_null_block = { HEADER_STATIC(0,TAG_INVALID) };
ptr_t ptr_null = (ptr_t)(&ptr_null_block);

// identity function
static box_t _function_id(function_t self, box_t x, context_t* ctx) {
  function_drop(self,ctx);
  return x;
}
define_static_function(function_id, _function_id);

// empty vector
static struct { block_t block; struct vector_s vec; } _vector_empty
  = { { HEADER_STATIC(0,TAG_VECTOR) }, { 0x02 /* length = box_enum(0) */, {0} } };
vector_t vector_empty = (datatype_t)(&_vector_empty);

/*--------------------------------------------------------------------------------------------------
  Process init/done
--------------------------------------------------------------------------------------------------*/

static bool process_initialized; // = false

static void runtime_done(void) {
  if (!process_initialized) return;
  process_initialized = false;
}

static void runtime_init(void) {
  if (process_initialized) return;
  process_initialized = true;
  atexit(&runtime_done);
}

/*--------------------------------------------------------------------------------------------------
  Getting the initial context (per thread)
--------------------------------------------------------------------------------------------------*/

static decl_thread context_t* context;

context_t* runtime_context(void) {
  context_t* ctx = context;
  if (ctx!=NULL) return ctx;
  runtime_init();
  ctx = (context_t*)calloc(sizeof(context_t),1);
  ctx->evv = vector_dup(vector_empty);
  ctx->thread_id = (uint_t)(&context);
  ctx->unique = integer_from_small(1);
  return ctx;
}


/*--------------------------------------------------------------------------------------------------
  Platform specific initialization hooks
--------------------------------------------------------------------------------------------------*/

#if defined(__cplusplus)  // also used for _MSC_VER
// C++: use static initialization to detect process start
static bool process_init(void) {
  runtime_init();
  return true;
}
static bool _process_init = process_init();

#elif defined(__GNUC__) || defined(__clang__)
// GCC,Clang: use the constructor attribute
static void __attribute__((constructor)) process_init(void) {
  runtime_init();
}

#else
#pragma message("define a way to call runtime_init on your platform")
#endif
