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

kk_decl_export kk_promise_t kk_promise_alloc( kk_context_t* ctx );
kk_decl_export bool         kk_promise_available( kk_promise_t pr, kk_context_t* ctx );
kk_decl_export kk_box_t     kk_promise_get( kk_promise_t pr, kk_context_t* ctx );
kk_decl_export void         kk_promise_set( kk_promise_t pr, kk_box_t r, kk_context_t* ctx );

/*--------------------------------------------------------------------------------------
   Tasks
--------------------------------------------------------------------------------------*/

kk_decl_export kk_promise_t kk_task_schedule( kk_function_t fun, kk_context_t* ctx );

// kk_decl_export void kk_task_group_free( kk_task_group_t* tg, kk_context_t* ctx );

#endif // include guard
