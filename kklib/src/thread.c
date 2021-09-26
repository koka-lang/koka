/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"
#include "kklib/thread.h"
#include <sys/_types/_int32_t.h>


/*---------------------------------------------------------------------------
  for windows, add a minimal pthread emulation layer
---------------------------------------------------------------------------*/
#ifdef _WIN32
#include <windows.h>

// --------------------------------------
// Threads
typedef HANDLE pthread_t;

static void pthread_join_void(pthread_t thread) {
  WaitForSingleObject(thread, INFINITE);
}

typedef struct kk_thread_proc_arg_s {
  void* (*action)(void*);
  void* arg;
} kk_thread_proc_arg_t;

static DWORD WINAPI kk_thread_proc(LPVOID varg) {
  kk_thread_proc_arg_t arg = *((kk_thread_proc_arg_t*)varg);
  kk_free(varg);
  (arg.action)(arg.arg);
  return 0;
}

static int pthread_create(pthread_t* thread, void* attr, void* (*action)(void*), void* arg) {
  KK_UNUSED(attr);
  kk_context_t* ctx = kk_get_context();
  kk_thread_proc_arg_t* parg = kk_zalloc(kk_ssizeof(kk_thread_proc_arg_t), ctx);
  parg->action = action;
  parg->arg = arg;
  DWORD tid = 0;
  *thread = CreateThread(NULL, 0, &kk_thread_proc, parg, 0, &tid);
  return (*thread == NULL ? EINVAL : 0);
}


// --------------------------------------
// Mutex
typedef CRITICAL_SECTION   pthread_mutex_t;

static int pthread_mutex_init(pthread_mutex_t* mutex, void* attr) {
  KK_UNUSED(attr);
  InitializeCriticalSection(mutex);
  return 0;
}

static void pthread_mutex_destroy(pthread_mutex_t* mutex) {
  DeleteCriticalSection(mutex);
}

static void pthread_mutex_lock(pthread_mutex_t* mutex) {
  EnterCriticalSection(mutex);
}

static void pthread_mutex_unlock(pthread_mutex_t* mutex) {
  LeaveCriticalSection(mutex);
}

// --------------------------------------
// Conditions
typedef CONDITION_VARIABLE pthread_cond_t;

static int pthread_cond_init(pthread_cond_t* cond, void* attr) {
  KK_UNUSED(attr);
  InitializeConditionVariable(cond);
  return 0;
}

static void pthread_cond_destroy(pthread_cond_t* cond) {
  KK_UNUSED(cond);
}

static int pthread_cond_wait(pthread_cond_t* cond, pthread_mutex_t* mutex) {
  if (SleepConditionVariableCS(cond, mutex, INFINITE)) {
    return 0;
  }
  else {
    return EINVAL;
  }
}

static void pthread_cond_signal(pthread_cond_t* cond) {
  WakeConditionVariable(cond);
}

static void pthread_cond_broadcast(pthread_cond_t* cond) {
  WakeAllConditionVariable(cond);
}


// --------------------------------------
// Once
typedef INIT_ONCE          pthread_once_t;
#define PTHREAD_ONCE_INIT  INIT_ONCE_STATIC_INIT

typedef struct kk_once_arg_s {
  void (*init)(void);
} kk_once_arg_t;

static BOOL kk_init_once_cb(PINIT_ONCE once, PVOID varg, PVOID* ctx) {
  KK_UNUSED(once);
  if (ctx != NULL) *ctx = NULL;
  kk_once_arg_t* arg = (kk_once_arg_t*)varg;
  (arg->init)();
  return TRUE;
}

static int pthread_once(pthread_once_t* once, void (*init)(void)) {
  kk_once_arg_t arg;
  arg.init = init;
  InitOnceExecuteOnce(once, &kk_init_once_cb, &arg, NULL);
  return 0;
}

/*---------------------------------------------------------------------------
  Other systems use posix threads
---------------------------------------------------------------------------*/
#else
#include <pthread.h>

static void pthread_join_void(pthread_t thread) {
  pthread_join(thread, NULL);
}
#endif


/*---------------------------------------------------------------------------
  Promise
---------------------------------------------------------------------------*/

typedef struct promise_s {
  kk_box_t        result;
  pthread_mutex_t lock;
  pthread_cond_t  available;
} promise_t;


static kk_promise_t kk_promise_alloc( kk_context_t* ctx );
static void         kk_promise_set( kk_promise_t pr, kk_box_t r, kk_context_t* ctx );
// static bool         kk_promise_available( kk_promise_t pr, kk_context_t* ctx );



/*---------------------------------------------------------------------------
  cpu-bound task
---------------------------------------------------------------------------*/

typedef struct kk_task_s {
  struct kk_task_s* next;
  kk_function_t     fun;
  kk_promise_t      promise;
} kk_task_t;

static void kk_task_free( kk_task_t* task, kk_context_t* ctx ) {
  kk_function_drop(task->fun,ctx);
  kk_box_drop(task->promise,ctx);
  kk_free(task);
}

static kk_task_t* kk_task_alloc( kk_function_t fun, kk_promise_t p, kk_context_t* ctx ) {
  kk_task_t* task = kk_zalloc(kk_ssizeof(kk_task_t), ctx);
  if (task == NULL) {
    kk_function_drop(fun,ctx);
    kk_box_drop(p,ctx);
    return NULL;
  }
  task->promise = p;
  task->fun  = fun;
  task->next = NULL;
  return task;
}

static void kk_task_exec( kk_task_t* task, kk_context_t* ctx ) {
  if (task->fun != NULL) {
    kk_function_dup(task->fun);
    kk_box_t res = kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),task->fun,(task->fun,ctx));
    kk_box_dup(task->promise);
    kk_promise_set( task->promise, res, ctx );
  }
  kk_task_free(task,ctx);
}


/*---------------------------------------------------------------------------
  task group (thread pool with task queue)
---------------------------------------------------------------------------*/

typedef struct kk_task_group_s {
  bool            done;
  kk_task_t*      tasks;
  kk_task_t*      tasks_tail;
  pthread_cond_t  tasks_available;
  pthread_mutex_t tasks_lock;
  pthread_t*      threads;
  kk_ssize_t      thread_count;
} kk_task_group_t;

static bool kk_tasks_is_empty( kk_task_group_t* tg ) {
  return (tg->tasks == NULL);
}

static kk_task_t* kk_tasks_dequeue( kk_task_group_t* tg ) {
  kk_task_t* task = tg->tasks;
  if (task != NULL) {
    tg->tasks = task->next;
    if (tg->tasks == NULL) {
      kk_assert(tg->tasks_tail == task);
      tg->tasks_tail = NULL;
    }
  }
  kk_assert(task != NULL || tg->done);
  return task;
}

static void kk_tasks_enqueue_n( kk_task_group_t* tg, kk_task_t* thead, kk_task_t* ttail, kk_context_t*  ctx ) {
  KK_UNUSED(ctx);
  if (tg->tasks_tail != NULL) {
    kk_assert(tg->tasks_tail->next == NULL);
    tg->tasks_tail->next = thead;
  }
  else {
    tg->tasks = thead;
  }
  tg->tasks_tail = ttail;
}

static void kk_tasks_enqueue( kk_task_group_t* tg, kk_task_t* task, kk_context_t* ctx ) {
  kk_tasks_enqueue_n( tg, task, task, ctx );
}

static kk_promise_t kk_task_group_schedule( kk_task_group_t* tg, kk_function_t fun, kk_context_t* ctx ) {
  kk_promise_t p = kk_promise_alloc(ctx);
  kk_task_t* task = kk_task_alloc(fun, kk_box_dup(p), ctx);
  pthread_mutex_lock(&tg->tasks_lock);
  kk_tasks_enqueue(tg,task,ctx);
  pthread_mutex_unlock(&tg->tasks_lock);
  pthread_cond_signal(&tg->tasks_available);
  return p;
}

static void* kk_task_group_worker( void* vtg ) {
  kk_task_group_t* tg = (kk_task_group_t*)vtg;
  kk_context_t*    ctx = kk_get_context();
  ctx->task_group = tg;
  while(true) {
     // deqeue task
     kk_task_t* task = NULL;
     pthread_mutex_lock(&tg->tasks_lock);
     while (kk_tasks_is_empty(tg) && !tg->done) {
       pthread_cond_wait(&tg->tasks_available, &tg->tasks_lock);
     }
     task = kk_tasks_dequeue(tg);
     pthread_mutex_unlock(&tg->tasks_lock);
     if (task == NULL) {  // due to tg->done
       break;
     }
     // todo: mark as concurrent
     kk_task_exec(task,ctx);
     // todo: ensure context is cleared again?
  }
  ctx->task_group = NULL;
  kk_free_context();
  return NULL;
}


void kk_task_group_free( kk_task_group_t* tg, kk_context_t* ctx ) {
  if (tg==NULL) return;
  // set done state
  kk_task_t* task = NULL;
  tg->done = true;
  pthread_mutex_lock(&tg->tasks_lock);
  task = tg->tasks;
  tg->tasks = NULL;
  tg->tasks_tail = NULL;
  tg->done = true;
  pthread_mutex_unlock(&tg->tasks_lock);
  // free tasks
  while( task != NULL ) {
    kk_task_t* next = task->next;
    kk_task_free(task,ctx);
    task = next;
  }
  // stop threads
  pthread_cond_broadcast(&tg->tasks_available);  // pretend there are tasks to make the threads exit;
  for( kk_ssize_t i = 0; i < tg->thread_count; i++) {
    if (tg->threads[i] != 0) {
      pthread_join_void(tg->threads[i]);
    }
  }
  pthread_cond_destroy(&tg->tasks_available);
  pthread_mutex_destroy(&tg->tasks_lock);
  kk_free(tg->threads);
  kk_free(tg);
}

static kk_task_group_t* kk_task_group_alloc( kk_ssize_t thread_count, kk_context_t* ctx ) {
  const kk_ssize_t cpu_count = kk_cpu_count(ctx);
  if (thread_count <= 0) { thread_count = cpu_count; }
  if (thread_count > 8*cpu_count) { thread_count = 8*cpu_count; };
  kk_task_group_t* tg = kk_zalloc( kk_ssizeof(kk_task_group_t), ctx );
  if (tg==NULL) return NULL;
  tg->threads = kk_zalloc( (thread_count+1) * sizeof(pthread_t), ctx );
  if (tg->threads == NULL) goto err;
  tg->thread_count = thread_count;
  tg->tasks = NULL;
  tg->tasks_tail = NULL;
  if (pthread_cond_init(&tg->tasks_available, NULL) != 0) goto err;
  if (pthread_mutex_init(&tg->tasks_lock, NULL) != 0) goto err;
  for (kk_ssize_t i = 0; i < tg->thread_count; i++) {
    if (pthread_create(&tg->threads[i], NULL, &kk_task_group_worker, tg) != 0) {
      goto err_threads;
    };
  }
  return tg;

err_threads:
  tg->done = true;
  pthread_cond_broadcast(&tg->tasks_available); // makes threads exit

err:
  if (tg != NULL) {
    if (tg->threads != NULL) { kk_free(tg->threads); }
    kk_free(tg);
  }
  return NULL;
}

static pthread_once_t task_group_once = PTHREAD_ONCE_INIT;
static kk_task_group_t* task_group = NULL;

static void kk_task_group_init(void) {
  task_group = kk_task_group_alloc(0,kk_get_context());
}

kk_promise_t kk_task_schedule( kk_function_t fun, kk_context_t* ctx ) {
  pthread_once( &task_group_once, &kk_task_group_init );
  kk_assert(task_group != NULL);
  kk_block_mark_shared( &fun->_block, ctx );
  return kk_task_group_schedule( task_group, fun, ctx );
}

static void kk_task_group_yield (kk_context_t* ctx) {

  if (ctx->task_group != NULL) {
    // try to get a task
    kk_task_group_t* tg = ctx->task_group;
    kk_task_t* task = NULL;

    pthread_mutex_lock(&tg->tasks_lock);

    if (!kk_tasks_is_empty(tg) && !tg->done) {
      task = kk_tasks_dequeue(tg);
    }

    pthread_mutex_unlock(&tg->tasks_lock);

    // run task
    if (task != NULL) kk_task_exec(task, ctx);
  }
}

/*---------------------------------------------------------------------------
  blocking promise
---------------------------------------------------------------------------*/

static void kk_promise_free( void* vp, kk_block_t* b, kk_context_t* ctx ) {
  KK_UNUSED(b);
  promise_t* p = (promise_t*)(vp);
  pthread_cond_destroy(&p->available);
  pthread_mutex_destroy(&p->lock);
  kk_box_drop(p->result,ctx);
  kk_free(p);
}

static kk_promise_t kk_promise_alloc(kk_context_t* ctx) {
  promise_t* p = kk_zalloc(kk_ssizeof(promise_t),ctx);
  if (p == NULL) goto err;
  p->result = kk_box_any(ctx);
  if (pthread_mutex_init(&p->lock, NULL) != 0) goto err;
  if (pthread_cond_init(&p->available, NULL) != 0) goto err;
  kk_promise_t pr = kk_cptr_raw_box( &kk_promise_free, p, ctx );
  kk_box_mark_shared(pr,ctx);
  return pr;
err:
  kk_free(p);
  return kk_box_any(ctx);
}


static void kk_promise_set( kk_promise_t pr, kk_box_t r, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  kk_box_drop(p->result,ctx);
  // TODO: mark as thread shared
  p->result = r;
  pthread_mutex_unlock(&p->lock);
  pthread_cond_signal(&p->available);
  kk_box_drop(pr,ctx);
}

/*
static bool kk_promise_available( kk_promise_t pr, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  bool available = !kk_box_is_any(p->result);
  pthread_mutex_unlock(&p->lock);
  kk_box_drop(pr,ctx);
  return available;
}
*/

kk_box_t kk_promise_get( kk_promise_t pr, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  while (kk_box_is_any(p->result)) {
    // if part of a task group, run other tasks while waiting
    if (ctx->task_group != NULL) {
      pthread_mutex_unlock(&p->lock);
      // try to get a task
      kk_task_group_t* tg = ctx->task_group;
      kk_task_t* task = NULL;
      pthread_mutex_lock(&tg->tasks_lock);
      if (!kk_tasks_is_empty(tg) && !tg->done) {
        task = kk_tasks_dequeue(tg);
      }
      pthread_mutex_unlock(&tg->tasks_lock);
      // run task
      if (task != NULL) {
        kk_task_exec(task, ctx);
        pthread_mutex_lock(&p->lock);
      }
      else {
        pthread_mutex_lock(&p->lock);
        if (kk_box_is_any(p->result)) {
          pthread_cond_wait( &p->available, &p->lock);
        }
        /*
        // no task, block for a while
        struct timespec tm;
        clock_gettime(CLOCK_REALTIME, &tm);
        tm.tv_nsec     +=  100000000;  // 0.1s
        if (tm.tv_nsec >= 1000000000) {
          tm.tv_nsec   -= 1000000000;
          tm.tv_sec += 1;
        }
        pthread_mutex_lock(&p->lock);
        if (kk_box_is_any(p->result)) {
          if (pthread_cond_timedwait( &p->available, &p->lock, &tm) == ETIMEDOUT) {
            pthread_mutex_lock(&p->lock);
          }
        }
        */
      }
    }
    // if in the main thread do a blocking wait
    else {
      pthread_cond_wait( &p->available, &p->lock );
    }
  }
  pthread_mutex_unlock(&p->lock);
  const kk_box_t result = kk_box_dup( p->result );
  kk_box_drop(pr,ctx);
  return result;
}


/*---------------------------------------------------------------------------
   Lvar
---------------------------------------------------------------------------*/

typedef struct lvar_s {
  kk_box_t        result;
  pthread_mutex_t lock;
  pthread_cond_t  available;
  int32_t         is_frozen;
} lvar_t;

typedef kk_box_t kk_lvar_t;

kk_lvar_t kk_lvar_alloc( kk_box_t lattice_bottom, kk_context_t* ctx );
void      kk_lvar_put(kk_lvar_t lvar, kk_function_t update, kk_context_t *ctx);
kk_box_t  kk_lvar_get( kk_lvar_t lvar, kk_function_t in_threshold_set, kk_context_t* ctx );
kk_box_t  kk_lvar_freeze(kk_lvar_t lvar, kk_context_t *ctx);

// frees alloc'ed lvar
static void kk_lvar_free( void* lvar, kk_block_t* b, kk_context_t* ctx ) {
  KK_UNUSED(b);
  lvar_t* lv = (lvar_t*)(lvar);
  pthread_cond_destroy(&lv->available);
  pthread_mutex_destroy(&lv->lock);
  kk_box_drop(lv->result,ctx);
  kk_free(lv);
}

// new lvar
kk_lvar_t kk_lvar_alloc(kk_box_t lattice_bottom, kk_context_t* ctx) {
  lvar_t* lv = kk_zalloc(kk_ssizeof(lvar_t),ctx);
  if (lv == NULL) goto err;
  lv->result = lattice_bottom;
  if (pthread_mutex_init(&lv->lock, NULL) != 0) goto err;
  if (pthread_cond_init(&lv->available, NULL) != 0) goto err;
  kk_lvar_t lvar = kk_cptr_raw_box( &kk_lvar_free, lv, ctx );
  kk_box_mark_shared(lattice_bottom,ctx);
  // TODO: are functions not marked?
  kk_box_mark_shared(lvar,ctx);
  return lvar;

err:
  kk_free(lv);
  kk_box_drop(lattice_bottom,ctx);
  kk_box_mark_shared(lattice_bottom,ctx);
  return kk_box_any(ctx);
}

// add value to lvar
void kk_lvar_put(kk_lvar_t lvar, kk_function_t update, kk_context_t *ctx) {
  lvar_t *lv = (lvar_t *)kk_cptr_raw_unbox(lvar);

  if (lv->is_frozen != 0) goto err;

  pthread_mutex_lock(&lv->lock);
  lv->result =
    kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t *),
                     update, (update, lv->result, ctx));

  kk_box_mark_shared(lv->result, ctx); // TODO: can we mark outside the mutex?

  // null means that update went to top
  if (kk_box_is_null(lv->result)) goto err;

  pthread_mutex_unlock(&lv->lock);

  pthread_cond_broadcast(&lv->available);

  kk_box_drop(lvar, ctx);
  kk_function_drop(update, ctx);
  return;

err:
  // TODO: how to throw an error?
  kk_box_drop(lvar, ctx);
  kk_function_drop(update, ctx);
  return;
}

static void kk_lvar_wait (kk_lvar_t lvar, kk_context_t* ctx) {
  lvar_t* lv = (lvar_t*)kk_cptr_raw_unbox(lvar);

  if (ctx->task_group != NULL) {
    // try to get a task
    kk_task_group_yield(ctx);
    pthread_mutex_lock(&lv->lock);
  }
  else if (kk_box_is_any(lv->result)) {
    pthread_mutex_lock(&lv->lock);
    pthread_cond_wait( &lv->available, &lv->lock);
  }
  kk_box_drop(lvar,ctx); // TODO: do we need to box_drop lvar from context?
}

kk_box_t kk_lvar_get( kk_lvar_t lvar, kk_function_t in_threshold_set, kk_context_t* ctx ) {
  lvar_t* lv = (lvar_t*)kk_cptr_raw_unbox(lvar);
  kk_box_t result;

  while (true) {
    // check if current value has reached threshold and get result value on threshold
    kk_function_dup(in_threshold_set);
    result = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, int32_t, kk_context_t *),
                              in_threshold_set, (in_threshold_set, kk_box_dup(lv->result), lv->is_frozen, ctx));

    // if current value has reached threshold, return
    if (!kk_box_is_null(result)) break;

    // if threshold was not reached but lvar is frozen, error
    if(lv->is_frozen != 0) goto err;

    // otherwise wait for an update to the lvar
    kk_box_dup(lvar);
    kk_lvar_wait(lvar, ctx);
    pthread_mutex_unlock(&lv->lock);
  }
  kk_function_drop(in_threshold_set, ctx);
  kk_box_drop(lvar,ctx);
  return result;
 err:
  // get should block forever in this situation
  // but maybe we can do something better?
  pthread_mutex_lock(&lv->lock);
  pthread_cond_wait(&lv->available, &lv->lock);
  return kk_box_null;
}

kk_box_t kk_lvar_freeze(kk_lvar_t lvar, kk_context_t *ctx) {
  lvar_t* lv = (lvar_t*)kk_cptr_raw_unbox(lvar);
  kk_box_t result;

  lv->is_frozen = true;
  pthread_cond_broadcast(&lv->available);
  result = kk_box_dup(lv->result);

  kk_box_drop(lvar, ctx);
  return result;
}
