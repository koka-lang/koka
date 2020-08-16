/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"
#include <stdarg.h>
#ifdef _WIN32
#include <Windows.h>
#endif

// identity function
static box_t _function_id(function_t self, box_t x, context_t* ctx) {
  drop_function_t(self,ctx);
  return x;
}
function_t function_id(context_t* ctx) {
  define_static_function(fun_id, _function_id, ctx)
  return dup_function_t(fun_id);
}

// null function
static box_t _function_null(function_t self, context_t* ctx) {
  drop_function_t(self, ctx);
  fatal_error(EFAULT, "null function is called");
  return box_null;
}
function_t function_null(context_t* ctx) {
  define_static_function(fun_null, _function_null, ctx)
  return dup_function_t(fun_null);
}


// null functions
void free_fun_null(void* p) {
  UNUSED(p);
}


string_t get_host(context_t* ctx) {
  UNUSED(ctx);
  define_string_literal(static, host, 5, "libc")
  return dup_string_t(host);
}

/*--------------------------------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------------------------------*/
static void _strlcpy(char* dest, const char* src, size_t dest_size) {
  dest[0] = 0;
#ifdef _MSC_VER
  strncpy_s(dest, dest_size, src, dest_size - 1);
#else
  strncpy(dest, src, dest_size - 1);
#endif
  dest[dest_size - 1] = 0;
}
/*
static void _strlcat(char* dest, const char* src, size_t dest_size) {
#pragma warning(suppress:4996)
  strncat(dest, src, dest_size - 1);
  dest[dest_size - 1] = 0;
}
*/

typedef enum log_level_e {
  LOG_FATAL,
  LOG_ERROR,
  LOG_WARNING,
  LOG_INFO,
  LOG_DEBUG,
  LOG_TRACE
} log_level_t;

static void log_message(log_level_t level, const char* msg, context_t* ctx) {
  UNUSED(ctx); UNUSED(level);
  fputs(msg,stderr); // TODO: use ctx->log
}

static void log_message_fmt(context_t* ctx, log_level_t level, const char* fmt, va_list args) {
  char buf[512];
  if (fmt==NULL) return;
  size_t prefix_len = 0;
  const char* prefix = NULL;
  if (level==LOG_FATAL) prefix = "fatal: ";
  else if (level==LOG_ERROR) prefix = "error: ";
  else if (level==LOG_WARNING) prefix = "warning: ";
  else if (level==LOG_INFO) prefix = "info: ";
  else if (level==LOG_DEBUG) prefix = "debug: ";
  else if (level==LOG_TRACE) prefix = "trace: ";
  if (prefix!=NULL) {
    prefix_len = strlen(prefix);
    _strlcpy(buf, prefix, sizeof(buf));
  }
  vsnprintf(buf + prefix_len, sizeof(buf) - 1 - prefix_len, fmt, args);
  log_message(level,buf,ctx);
}

void fatal_error(int err, const char* fmt, ...) {
  UNUSED(err);
  va_list args;
  va_start(args, fmt);
  log_message_fmt(get_context(), LOG_FATAL, fmt, args);
  va_end(args);
  abort();   // todo: call error handler
}

void warning_message(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  log_message_fmt(get_context(), LOG_WARNING, fmt, args);
  va_end(args);
}

/*--------------------------------------------------------------------------------------------------
  Process init/done
--------------------------------------------------------------------------------------------------*/
static void free_context(void);
static bool process_initialized; // = false

static void kklib_done(void) {
  if (!process_initialized) return;
  free_context();
  process_initialized = false;
}


#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
bool __has_popcnt = false;
bool __has_lzcnt = false;
#endif

static void kklib_init(void) {
  if (process_initialized) return;
  process_initialized = true;
#if defined(_WIN32) && defined(_CONSOLE)
  SetConsoleOutputCP(65001); // set the console to unicode instead of OEM page
#endif
#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
  // <https://en.wikipedia.org/wiki/SSE4#POPCNT_and_LZCNT>
  int32_t cpu_info[4];
  __cpuid(cpu_info, 1);
  __has_popcnt = ((cpu_info[2] & (I32(1)<<23)) != 0);
  __cpuid(cpu_info, 0x80000001);
  __has_lzcnt  = ((cpu_info[2] & (I32(1)<<5)) != 0);
#endif
  atexit(&kklib_done);
}

/*--------------------------------------------------------------------------------------------------
  Getting the initial context (per thread)
--------------------------------------------------------------------------------------------------*/

// The thread local context; usually passed explicitly for efficiency.
static decl_thread context_t* context;

// Get the thread local context (also initializes on demand)
context_t* get_context(void) {
  context_t* ctx = context;
  if (ctx!=NULL) return ctx;
  kklib_init();
#ifdef KK_MIMALLOC
  mi_heap_t* heap = mi_heap_get_default(); //  mi_heap_new();
  ctx = (context_t*)mi_heap_zalloc(heap, sizeof(context_t));
  ctx->heap = heap;
#else
  ctx = (context_t*)calloc(1, sizeof(context_t));
#endif
  ctx->evv = vector_empty();
  ctx->thread_id = (uintptr_t)(&context);
  ctx->unique = integer_one;
  context = ctx;
  ctx->box_any = block_alloc_as(struct box_any_s, 0, TAG_BOX_ANY, ctx);
  // todo: register a thread_done function to release the context on thread terminatation.
  return ctx;
}

static void free_context(void) {
  if (context != NULL) {
    drop_vector_t(context->evv, context);
    drop_basetype_assert(context->box_any, TAG_BOX_ANY, context);
    // TODO: process delayed_free
#ifdef KK_MIMALLOC
    // mi_heap_t* heap = context->heap;
    mi_free(context);
    // mi_heap_delete(heap);
#else
    runtime_free(context);
#endif
    context = NULL;
  }
}


/*--------------------------------------------------------------------------------------------------
  Platform specific initialization hooks
--------------------------------------------------------------------------------------------------*/

#if defined(__cplusplus)  // also used for _MSC_VER
// C++: use static initialization to detect process start
static bool process_init(void) {
  kklib_init();
  return true;
}
static bool _process_init = process_init();

#elif defined(__GNUC__) || defined(__clang__)
// GCC,Clang: use the constructor attribute
static void __attribute__((constructor)) process_init(void) {
  kklib_init();
}

#else
#pragma message("define a way to call kklib_init on your platform")
#endif
