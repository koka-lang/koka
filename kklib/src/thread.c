/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"
#include "kklib/thread.h"

#ifndef _WIN32
#include <pthread.h>
#endif

typedef struct promise_s {
  kk_box_t        result;
  pthread_mutex_t lock;
  pthread_cond_t  available;
  kk_function_t   combine;
  kk_box_t        partial;
} promise_t;


static kk_promise_t kk_promise_alloc( kk_function_t combine, kk_context_t* ctx );
static bool         kk_promise_available( kk_promise_t pr, kk_context_t* ctx );
static void         kk_promise_set( kk_promise_t pr, kk_box_t r, kk_context_t* ctx );
static void kk_promise_combine( kk_promise_t pr, kk_box_t r, kk_context_t* ctx );
static void kk_promise_combine_done( kk_promise_t pr, kk_context_t* ctx );

/*---------------------------------------------------------------------------
  (cpu-bound) task queue
---------------------------------------------------------------------------*/
typedef struct kk_atomic_counter_s {
  kk_ssize_t        stride;
  _Atomic(intptr_t) count;
  _Atomic(intptr_t) refcount;
} kk_atomic_counter_t;


typedef struct kk_task_s {
  struct kk_task_s* next;
  kk_function_t     fun;
  kk_promise_t      promise;
  kk_atomic_counter_t* counter;
} kk_task_t;

static void kk_task_free( kk_task_t* task, kk_context_t* ctx ) {
  if (task->counter != NULL) {
    if (kk_atomic_dec_relaxed(&task->counter->refcount) == 1) {
      kk_promise_combine_done( kk_box_dup(task->promise), ctx );
      kk_free(task->counter);      
    }
  }
  kk_function_drop(task->fun,ctx);
  kk_box_drop(task->promise,ctx);
  kk_free(task);
}

static kk_task_t* kk_task_alloc( kk_function_t fun, kk_promise_t p, kk_context_t* ctx ) {
  kk_task_t* task = kk_zalloc(kk_ssizeof(kk_task_t), ctx);
  if (task == NULL) {
    kk_function_drop(fun,ctx);
    kk_box_drop(p,ctx);
    NULL;
  }
  task->promise = p;
  task->fun  = fun;
  task->next = NULL;
  return task;
}

static void kk_task_exec( kk_task_t* task, kk_context_t* ctx ) {
  if (task->fun != NULL) {
    if (task->counter == NULL) {
      kk_function_dup(task->fun);      
      kk_box_t res = kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),task->fun,(task->fun,ctx));
      kk_box_dup(task->promise);
      kk_promise_set( task->promise, res, ctx );
    }
    else {
      while(true) {
        kk_ssize_t hi = kk_atomic_sub_relaxed( &task->counter->count, task->counter->stride );
        if (hi <= 0) break;
        kk_ssize_t lo = hi - task->counter->stride;
        if (lo < 0) lo = 0;
        if (lo < hi) {
          promise_t* p = kk_cptr_raw_unbox(task->promise);
          kk_function_t combine = kk_function_dup(p->combine);
          kk_box_t acc;
          for( kk_ssize_t i = lo; i < hi; i++) {
            kk_function_dup(task->fun);      
            kk_box_t res = kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),task->fun,(task->fun,ctx));
            if (i == lo) { acc = res; } else {
              kk_function_dup(combine);          
              acc = kk_function_call(kk_box_t,(kk_function_t,kk_box_t,kk_box_t,kk_context_t*),combine,(combine,acc,res,ctx));            
            }
          }
          kk_function_drop(combine,ctx);
          kk_box_dup(task->promise);
          kk_promise_combine(task->promise, acc, ctx);
        }
      }
    }
  }
  kk_task_free(task,ctx);  
}



/*---------------------------------------------------------------------------
  task group
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

static void kk_tasks_enqueue_n( kk_task_group_t* tg, kk_task_t* thead, kk_task_t* ttail, kk_context_t* ctx ) {
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
  kk_promise_t p = kk_promise_alloc(NULL,ctx);
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

static void kk_task_group_free( kk_task_group_t* tg, kk_context_t* ctx ) {
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
      pthread_join(tg->threads[i], NULL);
    }
  }
  pthread_cond_destroy(&tg->tasks_available);
  pthread_mutex_destroy(&tg->tasks_lock);
  kk_free(tg->threads);
  kk_free(tg);
}

static kk_task_group_t* kk_task_group_alloc( kk_ssize_t thread_count, kk_context_t* ctx ) {
  ssize_t cpu_count = kk_cpu_count(ctx);
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



static kk_promise_t kk_task_group_schedule_n( kk_task_group_t* tg, kk_ssize_t count, kk_ssize_t stride, kk_function_t fun, kk_function_t combine, kk_context_t* ctx ) 
{
  kk_promise_t p = kk_promise_alloc(combine,ctx);  
  kk_atomic_counter_t* counter = kk_zalloc(kk_ssizeof(kk_atomic_counter_t),ctx);
  const kk_ssize_t concurrency = tg->thread_count;
  counter->count = count;
  counter->stride = stride;
  counter->refcount = concurrency;
  kk_task_t* tail = NULL;
  kk_task_t* head = NULL;
  for( kk_ssize_t i = 0; i < concurrency; i++) {
    kk_task_t* task = kk_task_alloc( kk_function_dup(fun), kk_box_dup(p), ctx );
    task->counter = counter;
    if (tail == NULL) {
      head = task;
    }
    else {
      tail->next = task; 
    }
    tail = task;
  }
  kk_function_drop(fun,ctx); 
  pthread_mutex_lock(&tg->tasks_lock);
  kk_tasks_enqueue_n(tg,head,tail,ctx);
  pthread_mutex_unlock(&tg->tasks_lock);
  //for( kk_ssize_t i = 0; i < concurrency; i++) {
  //  pthread_cond_signal(&tg->tasks_available);
  //}
  pthread_cond_broadcast(&tg->tasks_available);
  return p;
}

kk_promise_t kk_task_schedule_n( kk_ssize_t count, kk_ssize_t stride, kk_function_t fun, kk_function_t combine, kk_context_t* ctx ) {
  pthread_once( &task_group_once, &kk_task_group_init );
  kk_assert(task_group != NULL);
  kk_block_mark_shared( &fun->_block, ctx );
  kk_block_mark_shared( &combine->_block, ctx );
  return kk_task_group_schedule_n( task_group, count, stride, fun, combine, ctx );
}


/*
kk_promise_t kk_task_run_for( int32_t count, int32_t stride, kk_function_t fun, kk_context_t* ctx ) {
  pthread_once( &task_group_once, &kk_task_group_init );
  kk_assert(task_group != NULL);
  if (stride <= 0) { stride = 1; }
  if (count <= 0)  { count = 1; }
  int32_t n = count / stride;
  if (n <= 1) {
    return kk_task_group_run( task_group, fun, ctx );
  }
  else {
    if (n > task_group->thread_count) { n = task_group->thread_count; }
    for (int32_t i = 0; i < n; i++) {

    }
  }
}
*/



/*---------------------------------------------------------------------------
  blocking promise
---------------------------------------------------------------------------*/

static void kk_promise_free( void* vp, kk_block_t* b, kk_context_t* ctx ) {
  KK_UNUSED(b);
  promise_t* p = (promise_t*)(vp);
  if (p->combine != NULL) kk_function_drop(p->combine,ctx);
  pthread_cond_destroy(&p->available);
  pthread_mutex_destroy(&p->lock);  
  kk_box_drop(p->result,ctx);
  kk_box_drop(p->partial,ctx);
  kk_free(p);
}

static kk_promise_t kk_promise_alloc(kk_function_t combine, kk_context_t* ctx) {
  promise_t* p = kk_zalloc(kk_ssizeof(promise_t),ctx);
  if (p == NULL) goto err;
  p->result = kk_box_any(ctx);
  p->partial = kk_box_any(ctx);
  p->combine = combine;
  if (pthread_mutex_init(&p->lock, NULL) != 0) goto err;
  if (pthread_cond_init(&p->available, NULL) != 0) goto err;
  kk_promise_t pr = kk_cptr_raw_box( &kk_promise_free, p, ctx );
  kk_box_mark_shared(pr,ctx);
  return pr;
err:
  kk_free(p);
  if (combine != NULL) kk_function_drop(combine,ctx);
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

static void kk_promise_combine_done( kk_promise_t pr, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  kk_box_drop(p->result,ctx);
  // TODO: mark as thread shared
  p->result = p->partial;
  p->partial = kk_box_any(ctx);
  pthread_mutex_unlock(&p->lock);
  pthread_cond_signal(&p->available);
  kk_box_drop(pr,ctx);
}

static void kk_promise_combine( kk_promise_t pr, kk_box_t r, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  // TODO: mark as thread shared  
  if (kk_box_is_any(p->partial)) {
    kk_box_drop(p->partial,ctx);
    p->partial = r;
  }
  else {
    kk_function_dup(p->combine);
    p->partial = kk_function_call(kk_box_t,(kk_function_t,kk_box_t,kk_box_t,kk_context_t*),p->combine, (p->combine, p->partial, r, ctx));    
    kk_box_mark_shared(p->partial,ctx);
  }
  pthread_mutex_unlock(&p->lock);
  kk_box_drop(pr,ctx);
}


static bool kk_promise_available( kk_promise_t pr, kk_context_t* ctx ) {
  promise_t* p = (promise_t*)kk_cptr_raw_unbox(pr);
  pthread_mutex_lock(&p->lock);
  bool available = !kk_box_is_any(p->result);
  pthread_mutex_unlock(&p->lock);
  kk_box_drop(pr,ctx);
  return available;
}

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
