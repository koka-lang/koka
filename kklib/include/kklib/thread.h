#pragma once
#ifndef KK_THREAD_H
#define KK_THREAD_H
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
   Promise
--------------------------------------------------------------------------------------*/

typedef kk_box_t  kk_promise_t;

kk_decl_export kk_box_t     kk_promise_get( kk_promise_t pr, kk_context_t* ctx );

/*--------------------------------------------------------------------------------------
   Tasks
--------------------------------------------------------------------------------------*/

kk_decl_export kk_promise_t kk_task_schedule( kk_function_t fun, kk_context_t* ctx );
kk_decl_export kk_promise_t kk_task_schedule_n( kk_ssize_t count, kk_ssize_t stride, kk_function_t fun, kk_function_t combine, kk_context_t* ctx );

// kk_decl_export void kk_task_group_free( kk_task_group_t* tg, kk_context_t* ctx );

/*--------------------------------------------------------------------------------------
   Lvars
--------------------------------------------------------------------------------------*/
typedef kk_box_t kk_lvar_t;

kk_decl_export kk_lvar_t kk_lvar_alloc( kk_box_t init, kk_context_t* ctx );
kk_decl_export void      kk_lvar_put( kk_lvar_t lvar, kk_box_t val, kk_function_t monotonic_combine, kk_context_t* ctx );
kk_decl_export kk_box_t  kk_lvar_get( kk_lvar_t lvar, kk_box_t bot, kk_function_t is_gte, kk_context_t* ctx );


#endif // include guard
