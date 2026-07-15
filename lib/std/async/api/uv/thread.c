/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Threading primitives for `std/async`, built on libuv.
//
// - `kk_uv_queue_work` : run a pure computation on a libuv threadpool worker
//   thread and deliver the (thread-shared) result back on the owning loop.
//
// The heap is thread-local and Perceus refcounts are non-atomic, so anything
// that crosses a thread boundary is marked thread-shared first (atomic
// refcounts) exactly as `kk_task_schedule`/`kk_promise_set` do in
// `kklib/src/thread.c`. The `work` closure crosses to the worker (marked on
// the loop thread before queueing); the `result` crosses back (marked on the
// worker thread before the loop consumes it).

// for IDE
// #include <kklib.h>
// #include "std_async_api_uv_evloop.h"

#include <uv.h>

// -------------------------------------------------------------------------
// compute: offload a pure `work` computation to a libuv threadpool thread
// -------------------------------------------------------------------------

// `kk_function_t` is a (boxed) datatype value, not a pointer, so — like the uv
// `handle->data` idiom in evloop.c — we store the underlying block pointer as a
// `void*` (NULL when consumed) and rebuild the function on use.
typedef struct kk_uv_compute_s {
  uv_work_t     req;       // must be first: we alias `kk_uv_compute_t*` <-> `uv_work_t*`
  void*         work;      // () -> box  block ptr ; marked thread-shared before queueing; NULL once consumed
  kk_box_t      result;    // filled by the worker (marked thread-shared there)
  void*         resume;    // (box) -> ioc () block ptr ; loop-thread callback; NULL once consumed/disposed
  bool          ran;       // whether `work` actually executed (vs. canceled before it started)
} kk_uv_compute_t;

// Runs on a libuv threadpool (worker) thread.
static void kk_uv_compute_work_cb(uv_work_t* req) {
  kk_uv_compute_t* w = (kk_uv_compute_t*)req;
  // libuv reuses a small fixed pool of worker threads; `kk_get_context` lazily
  // creates (and caches) a Koka context per OS thread.
  kk_context_t* ctx = kk_get_context();
  kk_function_t work = kk_datatype_from_ptr((kk_ptr_t)w->work, ctx);
  w->work = NULL;
  // call consumes (drops) the `work` reference; the reachable graph is already thread-shared
  kk_box_t res = kk_function_call(kk_box_t, (kk_function_t, kk_context_t*), work, (work, ctx), ctx);
  // the result was allocated on this worker heap and is about to cross back to
  // the loop thread: mark it thread-shared so refcounting stays atomic
  kk_box_mark_shared(res, ctx);
  w->result = res;
  w->ran = true;
}

// Runs on the owning (loop) thread after the worker completes or the request is canceled.
static void kk_uv_compute_after_cb(uv_work_t* req, int status) {
  kk_unused(status);
  kk_context_t* ctx = kk_get_context();
  kk_uv_compute_t* w = (kk_uv_compute_t*)req;
  void* resume_ptr = w->resume;
  w->resume = NULL;
  if (resume_ptr != NULL) {
    // normal delivery on the loop thread; `resume` consumes both itself and the boxed result
    kk_function_t resume = kk_datatype_from_ptr((kk_ptr_t)resume_ptr, ctx);
    kk_box_t res = (w->ran ? w->result : kk_box_any(ctx));
    kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), resume, (resume, res, ctx), ctx);
  }
  else {
    // disposed/canceled before delivery: release anything still held
    if (w->ran) { kk_box_drop(w->result, ctx); }
    if (w->work != NULL) { kk_datatype_drop(kk_datatype_from_ptr((kk_ptr_t)w->work, ctx), ctx); }
  }
  kk_free(w, ctx);
}

// Dispose (on cancelation). Runs on the loop thread (same thread as `after_cb`),
// so there is no race on `w->resume`. Prevents delivery and best-effort cancels
// the queued work; `after_cb` still runs exactly once and frees `w`.
static void kk_uv_compute_dispose(uv_handle_t* h, void* arg, kk_context_t* ctx) {
  kk_unused(arg);
  kk_uv_compute_t* w = (kk_uv_compute_t*)h;
  if (w->resume != NULL) {
    kk_datatype_drop(kk_datatype_from_ptr((kk_ptr_t)w->resume, ctx), ctx);
    w->resume = NULL;
  }
  uv_cancel((uv_req_t*)&w->req);  // if it already started/finished, after_cb still frees w
}

// Setup: queue `work` on the loop's threadpool, delivering the result via `resume`.
// `work` : () -> box , `resume` : (box) -> ioc () . Returns a dispose function.
kk_std_core_exn__error kk_uv_queue_work(kk_uv_loop_t loop, kk_function_t work, kk_function_t resume, kk_context_t* ctx) {
  kk_uv_compute_t* w = (kk_uv_compute_t*)kk_zalloc(sizeof(kk_uv_compute_t), ctx);
  if (w == NULL) {
    kk_function_drop(work, ctx);
    kk_function_drop(resume, ctx);
    return kk_error_from_uv_errno(UV_ENOMEM, ctx);
  }
  // `work` crosses to a worker thread -> mark its reachable graph thread-shared (atomic refcounts)
  kk_block_mark_shared(kk_datatype_as_ptr(work, ctx), ctx);
  w->work = (void*)kk_datatype_as_ptr(work, ctx);
  w->resume = (void*)kk_datatype_as_ptr(resume, ctx);   // stays on the loop thread; not marked
  w->ran = false;
  int err = uv_queue_work(kk_uv_loop(loop, ctx), &w->req, &kk_uv_compute_work_cb, &kk_uv_compute_after_cb);
  if (err != 0) {
    kk_function_drop(work, ctx);
    kk_function_drop(resume, ctx);
    kk_free(w, ctx);
    return kk_error_from_uv_errno(err, ctx);
  }
  return kk_result_uv_handle_dispose((uv_handle_t*)w, NULL, &kk_uv_compute_dispose, ctx);
}

// -------------------------------------------------------------------------
// xchannel: a THREAD-SAFE channel. Any thread may `xemit`; each value is marked
// thread-shared, pushed onto a mutex-protected FIFO queue, and the OWNER loop is
// woken via `uv_async_send`. The async callback (which runs on the owner loop
// thread) drains the whole queue and hands each value to `deliver` -- an
// owner-thread callback that emits into a normal loop-local Koka channel that
// receivers await. This is the "extend libuv" core the persistent-thread work
// builds on.
// -------------------------------------------------------------------------

typedef struct kk_xnode_s { struct kk_xnode_s* next; kk_box_t value; } kk_xnode_t;

typedef struct kk_xchan_s {
  uv_async_t   async;    // MUST be first: we alias `kk_xchan_t*` <-> `uv_async_t*`/`uv_handle_t*`
  uv_mutex_t   mutex;
  kk_xnode_t*  head;     // FIFO queue head/tail, guarded by `mutex`
  kk_xnode_t*  tail;
  void*        deliver;  // (box) -> ioc () block ptr; OWNER-thread ref (delivers into the Koka channel)
} kk_xchan_t;

// OWNER loop thread: drain the queue and deliver each value in FIFO order. libuv
// coalesces `uv_async_send`s, so we must drain ALL queued values per wake.
static void kk_xchan_async_cb(uv_async_t* async) {
  kk_xchan_t* c = (kk_xchan_t*)async;
  kk_context_t* ctx = kk_get_context();
  uv_mutex_lock(&c->mutex);
  kk_xnode_t* n = c->head; c->head = NULL; c->tail = NULL;
  uv_mutex_unlock(&c->mutex);
  while (n != NULL) {
    kk_xnode_t* next = n->next;
    // keep our stored `deliver` ref alive across the (consuming) call
    kk_function_t deliver = kk_datatype_from_ptr((kk_ptr_t)c->deliver, ctx);
    kk_function_dup(deliver, ctx);
    kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), deliver, (deliver, n->value, ctx), ctx);
    free(n);
    n = next;
  }
}

// after uv_close completes (owner thread): drop anything still held, then free
static void kk_xchan_close_cb(uv_handle_t* h) {
  kk_xchan_t* c = (kk_xchan_t*)h;
  kk_context_t* ctx = kk_get_context();
  uv_mutex_destroy(&c->mutex);
  kk_xnode_t* n = c->head;
  while (n != NULL) { kk_xnode_t* next = n->next; kk_box_drop(n->value, ctx); free(n); n = next; }
  if (c->deliver != NULL) kk_datatype_drop(kk_datatype_from_ptr((kk_ptr_t)c->deliver, ctx), ctx);
  free(c);
}

// noop dispose for the creation await (the channel outlives the await -- its
// lifetime is the UNREFCOUNTED cptr handed back through `resume`, closed
// explicitly via `xchannel/close`, never by a Koka drop)
static void kk_xchan_noop_dispose(uv_handle_t* h, void* arg, kk_context_t* ctx) {
  kk_unused(h); kk_unused(arg); kk_unused(ctx);
}

// Setup (owner loop thread): create the channel on `loop`, deliver its values via
// `deliver`, and hand the boxed channel pointer back through `resume`. The handle
// is boxed as an UNREFCOUNTED raw pointer (kk_cptr_box, a value for heap
// addresses): it is NOT Koka memory, and a `sender` crosses it to worker threads,
// so refcounting it would run a destructor on whatever thread drops last (a
// cross-thread free -> heap corruption). Lifetime is managed explicitly: the
// channel lives until `xchannel/close` (owner thread) or process exit.
kk_std_core_exn__error kk_xchan_create(kk_uv_loop_t loop, kk_function_t deliver, kk_function_t resume, kk_context_t* ctx) {
  kk_xchan_t* c = (kk_xchan_t*)calloc(1, sizeof(kk_xchan_t));
  if (c == NULL) { kk_function_drop(deliver, ctx); kk_function_drop(resume, ctx); return kk_error_from_uv_errno(UV_ENOMEM, ctx); }
  uv_mutex_init(&c->mutex);
  int err = uv_async_init(kk_uv_loop(loop, ctx), &c->async, kk_xchan_async_cb);
  if (err != 0) { uv_mutex_destroy(&c->mutex); free(c); kk_function_drop(deliver, ctx); kk_function_drop(resume, ctx); return kk_error_from_uv_errno(err, ctx); }
  c->head = c->tail = NULL;
  c->deliver = (void*)kk_datatype_as_ptr(deliver, ctx);   // owner-thread ref
  kk_box_t boxed = kk_cptr_box(c, ctx);                    // unrefcounted handle
  kk_function_call(kk_unit_t, (kk_function_t, kk_box_t, kk_context_t*), resume, (resume, boxed, ctx), ctx);
  return kk_result_uv_handle_dispose(NULL, NULL, &kk_xchan_noop_dispose, ctx);
}

// Close the channel from the OWNER thread (the loop that created it): uv_close the
// async handle, then `close_cb` drains any queued values, drops the deliver, and
// frees the struct. Owner-thread only, so no cross-thread free. Callers must
// ensure no other thread `xemit`s after close (senders then dangle).
kk_unit_t kk_xchan_close(kk_box_t xcbox, kk_context_t* ctx) {
  kk_xchan_t* c = (kk_xchan_t*)kk_cptr_unbox_borrowed(xcbox, ctx);
  uv_close((uv_handle_t*)&c->async, kk_xchan_close_cb);
  kk_box_drop(xcbox, ctx);
  return kk_Unit;
}

// Emit `value` into the channel from ANY thread: mark it thread-shared, enqueue
// under the mutex, and wake the owner loop.
kk_unit_t kk_xchan_emit(kk_box_t xcbox, kk_box_t value, kk_context_t* ctx) {
  kk_xchan_t* c = (kk_xchan_t*)kk_cptr_unbox_borrowed(xcbox, ctx);
  kk_box_mark_shared(value, ctx);
  kk_xnode_t* n = (kk_xnode_t*)malloc(sizeof(kk_xnode_t));
  n->next = NULL; n->value = value;   // ownership of `value` transferred to the node
  uv_mutex_lock(&c->mutex);
  if (c->tail != NULL) c->tail->next = n; else c->head = n;
  c->tail = n;
  uv_mutex_unlock(&c->mutex);
  uv_async_send(&c->async);
  kk_box_drop(xcbox, ctx);            // drop this (dup'd) channel reference
  return kk_Unit;
}

// -------------------------------------------------------------------------
// spawn-thread: a PERSISTENT worker thread running its OWN Koka async loop.
// The thread gets a fresh per-thread Koka context and calls `body`, which is a
// Koka wrapper that runs `async(...)` -- i.e. it inits its own uv_loop, installs
// the async/exn handlers, runs the user body, and `uv_run(UV_RUN_DEFAULT)`s until
// its work drains. So the worker is "just another async main" on its own thread,
// able to host the delivery callbacks of thread-safe channels correctly.
// -------------------------------------------------------------------------

static void kk_thread_entry(void* arg) {
  // `arg` is the `() -> ioc ()` body block ptr (marked thread-shared before spawn)
  kk_context_t* ctx = kk_get_context();   // fresh, lazily-created per-thread context
  kk_function_t body = kk_datatype_from_ptr((kk_ptr_t)arg, ctx);
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), body, (body, ctx), ctx);
}

// Spawn an OS thread that runs `body` (a `() -> ioc ()` that wraps `async(...)`).
kk_unit_t kk_spawn_thread(kk_function_t body, kk_context_t* ctx) {
  // `body` crosses to the new thread -> mark its reachable graph thread-shared
  kk_block_mark_shared(kk_datatype_as_ptr(body, ctx), ctx);
  void* body_ptr = (void*)kk_datatype_as_ptr(body, ctx);
  uv_thread_t tid;
  int err = uv_thread_create(&tid, kk_thread_entry, body_ptr);
  if (err != 0) {
    // reclaim the body ref on failure
    kk_datatype_drop(kk_datatype_from_ptr((kk_ptr_t)body_ptr, ctx), ctx);
    kk_warning_message("spawn-thread: uv_thread_create failed: %s\n", uv_strerror(err));
  }
  return kk_Unit;
}
