#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include <emscripten/html5.h>
//////////////////////////////////////////////////////
// Event Loop for Emscripten
//////////////////////////////////////////////////////

// A global sentinel box: each wasm handle dups it on init, drops on free.
// When the last handle drops it, the free function cancels the main loop.
static kk_box_t kk_wasm_loop_sentinel = { .box = 0 };

static void kk_wasm_sentinel_free(void* p, kk_block_t* block, kk_context_t* _ctx) {
  kk_unused(p);
  kk_unused(block);
  emscripten_cancel_main_loop();
}

// Initialize the sentinel (called once from loop init)
static void kk_wasm_sentinel_init(kk_context_t* _ctx) {
  kk_wasm_loop_sentinel = kk_cptr_raw_box(&kk_wasm_sentinel_free, NULL, _ctx);
}

// Dup the sentinel to keep the loop alive (called from handle init)
void kk_wasm_loop_ref(kk_context_t* _ctx) {
  kk_box_dup(kk_wasm_loop_sentinel, _ctx);
}

// Drop the sentinel (called from handle free)
void kk_wasm_loop_unref(kk_context_t* _ctx) {
  kk_box_drop(kk_wasm_loop_sentinel, _ctx);
}

void one_iter() {
  return;
}

void kk_emscripten_loop_run(kk_context_t* _ctx){
  // Drop our own reference to the sentinel; handles hold the remaining refs.
  // When the last handle is freed, the sentinel's free function cancels this loop.
  kk_box_drop(kk_wasm_loop_sentinel, _ctx);
  emscripten_set_main_loop(one_iter, 0, true);
}
#else

//////////////////////////////////////////////////////
// Allocators 
//////////////////////////////////////////////////////
static inline void* kk_malloc_ctx(size_t size) {
  return kk_malloc(size, kk_get_context());
}

static inline void* kk_realloc_ctx(void* p, size_t size) {
  return kk_realloc(p, size, kk_get_context());
}

static inline void* kk_calloc_ctx(size_t count, size_t size) {
  void* p = kk_malloc(count*size, kk_get_context());
  kk_memset(p, 0, count*size);
  return p;
}

static inline void kk_free_ctx(void* p) {
  kk_free(p, kk_get_context());
}

static inline void kk_uv_alloc_init(kk_context_t* _ctx){
  uv_replace_allocator(kk_malloc_ctx, kk_realloc_ctx, kk_calloc_ctx, kk_free_ctx);
}

//////////////////////////////////////////////////////
// UV Event Loop
//////////////////////////////////////////////////////
static void kk_uv_loop_init(kk_context_t* _ctx) {
  uv_loop_t* loop = kk_malloc(sizeof(uv_loop_t), kk_context());
  kk_set_uv_loop(loop); // Set thread local loop
  uv_loop_init(loop);
}

void kk_uv_loop_run(kk_context_t* _ctx){
  // Run the event loop after the initial startup of the program
  int ret = uv_run(uvloop(), UV_RUN_DEFAULT);
  if (ret != 0){
    kk_warning_message("Event loop closed with status %s\n", uv_err_name(ret));
  }
}

static char* kk_uv_handle_type_str(uv_handle_t* handle) {
  switch (handle->type) {
    case UV_UNKNOWN_HANDLE: return "UNKNOWN";
    case UV_ASYNC: return "ASYNC";
    case UV_CHECK: return "CHECK";
    case UV_FS_EVENT: return "FS_EVENT";
    case UV_FS_POLL: return "FS_POLL";
    case UV_HANDLE: return "HANDLE";
    case UV_IDLE: return "IDLE";
    case UV_NAMED_PIPE: return "NAMED_PIPE";
    case UV_POLL: return "POLL";
    case UV_PREPARE: return "PREPARE";
    case UV_PROCESS: return "PROCESS";
    case UV_STREAM: return "STREAM";
    case UV_TCP: return "TCP";
    case UV_TIMER: return "TIMER";
    case UV_TTY: return "TTY";
    case UV_UDP: return "UDP";
    case UV_SIGNAL: return "SIGNAL";
    case UV_FILE: return "FILE";
    default: return "INVALID";
  }
}

static void kk_uv_loop_walk_cb(uv_handle_t* handle, void* arg) {
  const char* closing_msg = uv_is_closing(handle) ? " [CLOSING]" : "";
  const char* active_msg = uv_is_active(handle) ? " [ACTIVE]" : "";
  kk_warning_message(" - %s handle%s%s\n", kk_uv_handle_type_str(handle), active_msg, closing_msg);
}

static void kk_uv_loop_close(kk_context_t* _ctx) {
  int ret = uv_loop_close(uvloop());
  if (ret != 0) {
    if (ret == UV_EBUSY) {
      kk_warning_message("Event loop closed with open child handles:\n");
      uv_walk(uvloop(), kk_uv_loop_walk_cb, NULL);
    } else {
      kk_warning_message("Event loop close returned error: %s\n", uv_err_name(ret));
    }
  }
  kk_free(uvloop(), _ctx);
}

#endif

//////////////////////////////////////////////////////
// Generic APIs for both emscripten and UV
////////////////////////////////////////////////////// 
static inline void kk_async_alloc_init(kk_context_t* _ctx){
  #if __EMSCRIPTEN__
    return;
  #else 
    return kk_uv_alloc_init(_ctx);
  #endif
}

static void kk_async_loop_init(kk_context_t* _ctx) {
  #if __EMSCRIPTEN__
    kk_wasm_sentinel_init(_ctx);
  #else
    return kk_uv_loop_init(_ctx);
  #endif
}

void kk_async_loop_run(kk_context_t* _ctx){
  // Run the event loop after the initial startup of the program
  #if __EMSCRIPTEN__
    return kk_emscripten_loop_run(_ctx);
  #else 
    return kk_uv_loop_run(_ctx);
  #endif
  
}

static void kk_async_loop_close(kk_context_t* _ctx) {
  #if __EMSCRIPTEN__
    return;
  #else
    return kk_uv_loop_close(_ctx);
  #endif
}
