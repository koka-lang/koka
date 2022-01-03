/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
//#define _CRT_SECURE_NO_WARNINGS
#include "kklib.h"
#include <stdarg.h>
#include <stdio.h>
#ifdef WIN32
#include <Windows.h>
#endif
#include <locale.h>

// identity function
static kk_box_t _function_id(kk_function_t self, kk_box_t x, kk_context_t* ctx) {
  kk_function_drop(self,ctx);
  return x;
}
kk_function_t kk_function_id(kk_context_t* ctx) {
  kk_define_static_function(fun_id, _function_id, ctx)
  return kk_function_dup(fun_id);
}

// null function
static kk_box_t _function_null(kk_function_t self, kk_context_t* ctx) {
  kk_function_drop(self, ctx);
  kk_fatal_error(EFAULT, "null function is called");
  return kk_box_null;
}
kk_function_t kk_function_null(kk_context_t* ctx) {
  kk_define_static_function(fun_null, _function_null, ctx)
  return kk_function_dup(fun_null);
}


// null functions
void kk_free_fun_null(void* p, kk_block_t* b, kk_context_t* ctx) {
  kk_unused(p);
  kk_unused(b);
  kk_unused(ctx);
}

// free memory
void kk_free_fun(void* p, kk_block_t* b, kk_context_t* ctx) {
  kk_unused(b);
  kk_unused(ctx);
  kk_free(p,ctx);
}


kk_string_t kk_get_host(kk_context_t* ctx) {
  kk_unused(ctx);
  kk_define_string_literal(static, host, 5, "libc")
  return kk_string_dup(host);
}

/*--------------------------------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------------------------------*/
static void _strlcpy(char* dest, const char* src, size_t kk_dest_size) {
  dest[0] = 0;
#ifdef _MSC_VER
  strncpy_s(dest, kk_dest_size, src, kk_dest_size - 1);
#else
  strncpy(dest, src, kk_dest_size - 1);
#endif
  dest[kk_dest_size - 1] = 0;
}
/*
static void _strlcat(char* dest, const char* src, size_t kk_dest_size) {
#pragma warning(suppress:4996)
  strncat(dest, src, kk_dest_size - 1);
  dest[kk_dest_size - 1] = 0;
}
*/

typedef enum kk_log_level_e {
  KK_LOG_FATAL,
  KK_LOG_ERROR,
  KK_LOG_WARNING,
  KK_LOG_INFO,
  KK_LOG_DEBUG,
  KK_LOG_TRACE
} kk_log_level_t;

static void kk_log_message(kk_log_level_t level, const char* msg, kk_context_t* ctx) {
  kk_unused(ctx); kk_unused(level);
  fputs(msg,stderr); // TODO: use ctx->log
}

static void kk_log_message_fmt(kk_context_t* ctx, kk_log_level_t level, const char* fmt, va_list args) {
  char buf[512];
  if (fmt==NULL) return;
  size_t prefix_len = 0;
  const char* prefix = NULL;
  if (level==KK_LOG_FATAL) prefix = "fatal: ";
  else if (level==KK_LOG_ERROR) prefix = "error: ";
  else if (level==KK_LOG_WARNING) prefix = "warning: ";
  else if (level==KK_LOG_INFO) prefix = "info: ";
  else if (level==KK_LOG_DEBUG) prefix = "debug: ";
  else if (level==KK_LOG_TRACE) prefix = "trace: ";
  if (prefix!=NULL) {
    prefix_len = strlen(prefix);
    _strlcpy(buf, prefix, sizeof(buf));
  }
  vsnprintf(buf + prefix_len, sizeof(buf) - 1 - prefix_len, fmt, args);
  kk_log_message(level,buf,ctx);
}

void kk_fatal_error(int err, const char* fmt, ...) {
  kk_unused(err);
  va_list args;
  va_start(args, fmt);
  kk_log_message_fmt(kk_get_context(), KK_LOG_FATAL, fmt, args);
  va_end(args);
  abort();   // todo: call error handler
}

void kk_warning_message(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  kk_log_message_fmt(kk_get_context(), KK_LOG_WARNING, fmt, args);
  va_end(args);
}

void kk_info_message(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  kk_log_message_fmt(kk_get_context(), KK_LOG_INFO, fmt, args);
  va_end(args);
}

/*--------------------------------------------------------------------------------------------------
  Process init/done
--------------------------------------------------------------------------------------------------*/
static bool process_initialized; // = false

static void kklib_done(void) {
  if (!process_initialized) return;
  kk_free_context();
  process_initialized = false;
}


#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
bool kk_has_popcnt = false;
bool kk_has_lzcnt = false;
bool kk_has_tzcnt = false;
#endif

static void kklib_init(void) {
  if (process_initialized) return;
  process_initialized = true;
  // for Koka, we need to be fully deterministic and careful when using C functionality that depends on global variables
  setlocale(LC_ALL, "C.utf8"); 
#if defined(WIN32) && (defined(_CONSOLE) || defined(__MINGW32__))
  SetConsoleOutputCP(65001);   // set the console to utf-8 instead of OEM page
#endif
  //todo: do we need to set the IEEE floating point flags?
  //fexcept_t fexn;
  //fesetexceptflag(&fexn, FE_ALL_EXCEPT);
  //_controlfp(_EM_INEXACT|_EM_OVERFLOW|_EM_UNDERFLOW, _MCW_EM);

#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
  // <https://en.wikipedia.org/wiki/CPUID>
  int32_t cpu_info[4];
  __cpuid(cpu_info, 1);
  kk_has_popcnt = ((cpu_info[2] & (KK_I32(1)<<23)) != 0);
  __cpuid(cpu_info, (int)(0x80000001));
  kk_has_lzcnt  = ((cpu_info[2] & (KK_I32(1)<<5)) != 0);   // abm: https://en.wikipedia.org/wiki/X86_Bit_manipulation_instruction_set
  __cpuid(cpu_info, 7);
  kk_has_tzcnt = ((cpu_info[1] & (KK_I32(1)<<3)) != 0);    // bmi1: https://en.wikipedia.org/wiki/X86_Bit_manipulation_instruction_set
#endif
  atexit(&kklib_done);  
}

/*--------------------------------------------------------------------------------------------------
  Getting the initial context (per thread)
--------------------------------------------------------------------------------------------------*/

// The thread local context; usually passed explicitly for efficiency.
static kk_decl_thread kk_context_t* context;


static struct { kk_block_t _block; kk_integer_t cfc; } kk_evv_empty_static = {
  { KK_HEADER_STATIC(1,KK_TAG_EVV_VECTOR) }, { ((~KK_UP(0))^0x02) /*==-1 smallint*/}
};
kk_ptr_t kk_evv_empty_singleton = &kk_evv_empty_static._block;

// Get the thread local context (also initializes on demand)
kk_context_t* kk_get_context(void) {
  kk_context_t* ctx = context;
  if (ctx!=NULL) return ctx;
  kklib_init();
#ifdef KK_MIMALLOC
  mi_heap_t* heap = mi_heap_get_default(); //  mi_heap_new();
  ctx = (kk_context_t*)mi_heap_zalloc(heap, sizeof(kk_context_t));
  ctx->heap = heap;
#else
  ctx = (kk_context_t*)kk_zalloc(sizeof(kk_context_t),NULL);
#endif
  ctx->evv = kk_block_dup(kk_evv_empty_singleton);
  ctx->thread_id = (size_t)(&context);
  ctx->unique = kk_integer_one;
  context = ctx;
  ctx->kk_box_any = kk_block_alloc_as(struct kk_box_any_s, 0, KK_TAG_BOX_ANY, ctx);  
  ctx->kk_box_any->_unused = kk_integer_zero;
  // todo: register a thread_done function to release the context on thread terminatation.
  return ctx;
}

void kk_free_context(void) {
  if (context != NULL) {
    kk_block_drop(context->evv, context);
    kk_basetype_free(context->kk_box_any,context);
    // kk_basetype_drop_assert(context->kk_box_any, KK_TAG_BOX_ANY, context);
    // TODO: process delayed_free
#ifdef KK_MIMALLOC
    // mi_heap_t* heap = context->heap;
    mi_free(context);
    // mi_heap_delete(heap);
#else
    kk_free(context,context);
#endif
    context = NULL;
  }
}

/*--------------------------------------------------------------------------------------------------
  Called from main
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_context_t* kk_main_start(int argc, char** argv) {
  kk_context_t* ctx = kk_get_context();
  // process kklib options
  if (argv != NULL && argc >= 1) {
    kk_ssize_t i;
    for (i = 1; i < argc; i++) {   // argv[0] is the program name
      const char* arg = argv[i];
      if (strcmp(arg, "--kktime")==0) {
        ctx->process_start = kk_timer_start();
      }
      else {
        break;
      }
    }
    i--;  // i == number of processed --kkxxx options
    if (i > 0) {
      argv[i] = argv[0]; // move the program name to the last processed --kkxxx option
    }
    ctx->argc = argc - i;
    ctx->argv = (const char**)(argv + i);
  }
  return ctx;
}

kk_decl_export void  kk_main_end(kk_context_t* ctx) {
  if (ctx->process_start != 0) {  // started with --kktime option
    kk_usecs_t wall_time = kk_timer_end(ctx->process_start);
    kk_msecs_t user_time;
    kk_msecs_t sys_time;
    size_t peak_rss;
    size_t page_faults;
    size_t page_reclaim;
    size_t peak_commit;
    kk_process_info(&user_time, &sys_time, &peak_rss, &page_faults, &page_reclaim, &peak_commit);
    kk_info_message("elapsed: %ld.%03lds, user: %ld.%03lds, sys: %ld.%03lds, rss: %lu%s\n", 
                    (long)(wall_time/1000000), (long)((wall_time%1000000)/1000), 
                    user_time/1000, user_time%1000, sys_time/1000, sys_time%1000, 
                    (peak_rss > 10*1024*1024 ? peak_rss/(1024*1024) : peak_rss/1024),
                    (peak_rss > 10*1024*1024 ? "mb" : "kb") );
  }
}


/*--------------------------------------------------------------------------------------------------
  Debugger
--------------------------------------------------------------------------------------------------*/

#include <signal.h>

kk_decl_export void kk_debugger_break(kk_context_t* ctx) {
  kk_unused(ctx);
#if _MSC_VER
  __debugbreak();
#elif defined(SIGTRAP)
  raise(SIGTRAP);
#else
  abort();
#endif
}

/*--------------------------------------------------------------------------------------------------
  Platform specific initialization hooks
--------------------------------------------------------------------------------------------------*/
/*
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
*/
