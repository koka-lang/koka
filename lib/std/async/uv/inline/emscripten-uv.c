/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// #define __EMSCRIPTEN__
// #include "core.h"
#ifdef __EMSCRIPTEN__

// ----------------------------------
// utility

int uv_replace_allocator(uv_malloc_func malloc_fun, uv_realloc_func realloc_fun, uv_calloc_func calloc_fun, uv_free_func free_fun) {
  // nothing
  return 0;
}

const char* uv_strerror(int uverr) {
  if (uverr > 0) { uverr = -uverr; }
  switch(uverr) {
    #define XX(code,msg) case UV_##code: return msg;
    UV_ERRNO_MAP(XX)
    #undef XX
    case 0 : return "success";
    default: return "unknown error";
  }
}


// ----------------------------------------------
// event loop

static void uv_loop_ref(uv_loop_t* loop) {
  if (loop==NULL) return;
  // todo: if (evloop->refcount==0) { error("refcounting after shutdown") }
  loop->refcount++;
}


static void uv_loop_delayed_unref(void* ploop) {
  uv_loop_t* loop = (uv_loop_t*)ploop;
  // todo: if (evloop->refcount==0) { error("refcounting after shutdown") }
  loop->refcount--; 
  if (loop->refcount == 0) {
    uv_stop(loop);
  }
}

static void uv_loop_unref(uv_loop_t* loop) {
  if (loop==NULL) return;
  if (loop->refcount > 1) { // optimize common case
    loop->refcount--;
  }
  else {
    // delayed deference
    emscripten_async_call(&uv_loop_delayed_unref, loop, 0);
  }
}


int uv_loop_init(uv_loop_t* loop) {
  loop->refcount = 1;
  return 0;
}

int uv_loop_close(uv_loop_t* loop) {
  return 0;
}

static void uv_loop_iteration() {
  return;
}

int uv_run(uv_loop_t* loop, uv_run_mode mode) {
  emscripten_set_main_loop(&uv_loop_iteration, 0, true);  
  // uv_loop_unref(loop);
  return 0;
}

void uv_stop(uv_loop_t* loop) {
  kk_unused(loop);
  emscripten_cancel_main_loop();  
}

void uv_walk(uv_loop_t* loop, uv_walk_cb walk_cb, void* arg) {
  kk_unused(loop); kk_unused(walk_cb); kk_unused(arg);
  // do nothing for now
  return;
}


// ----------------------------------------------
// handles

void uv_ref(uv_handle_t* h) {
  if (h!=NULL && !h->hasref) {
    h->hasref = true;
    uv_loop_ref(uv_handle_get_loop(h));
  }
}
void uv_unref(uv_handle_t* h) {
  if (h!=NULL && h->hasref) {
    h->hasref = false;
    uv_loop_unref(uv_handle_get_loop(h));
  }
}

static void uv_dispose(uv_handle_t* h) {
  uv_unref(h);
  if (h->close_cb!=NULL) {
    (h->close_cb)(h);
  }
}

static void uv_dispose_void(void* arg) {
  uv_dispose((uv_handle_t*)arg);
}

void uv_close(uv_handle_t* h, uv_close_cb close_cb) {
  if (h==NULL || h->close_cb != NULL) return;
  if (close_cb!=NULL) {
    h->close_cb = close_cb;
    emscripten_async_call(&uv_dispose_void, h, 0);
  }
  else {
    h->close_cb = &uv_dispose;
    uv_unref(h);
  }
}

static int uv_handle_init(uv_loop_t* loop, uv_handle_t* h) {
  h->loop = loop;
  return 0;
}


// ----------------------------------------------
// timer

int uv_timer_init(uv_loop_t* loop, uv_timer_t* t) {
  return uv_handle_init(loop,(uv_handle_t*)t);
}

#ifdef __cplusplus
extern "C"
#endif
EMSCRIPTEN_KEEPALIVE void uv_timer_callback(uv_timer_t* timer) {
  if (!uv_is_closing((uv_handle_t*)timer)) {
    (timer->timer_cb)(timer);
  }
}

EM_JS(int, js_set_timeout, (uv_timer_t* t, uint64_t millisecs), {
  function cb() {
    // Module.ccall("uv_timer_callback",null,["number"],[h]);
    _uv_timer_callback(t)
  }
  return setTimeout(cb, Number(millisecs));
});
EM_JS(int, js_set_interval, (uv_timer_t* h, uint64_t millisecs), {
  return setInterval(() => _uv_timer_callback(h), Number(millisecs));
});
EM_JS(void, js_clear_timeout, (int timer), {
  clearTimeout(timer);
});
EM_JS(void, js_clear_interval, (int timer), {
  clearInterval(timer);
});

int uv_timer_start(uv_timer_t* t, uv_timer_cb cb, uint64_t millisecs, uint64_t repeat) {
  if (repeat < 0) repeat = 0;
  if (millisecs < 0) millisecs = 0;
  t->timeout  = (repeat > 0 ? repeat : millisecs);
  t->repeat   = repeat;
  t->timer_cb = cb;
  t->jstimer = (t->repeat > 0 ? js_set_interval(t,t->repeat) : js_set_timeout(t,t->timeout));
  return 0;
}

int uv_timer_stop(uv_timer_t* t) {
  (t->repeat > 0 ? js_clear_interval(t->jstimer) : js_clear_timeout(t->jstimer));
  return 0;
}


#endif