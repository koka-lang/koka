#include <uv.h>

#define uvloop() ((uv_loop_t*)(kk_context()->loop))

typedef struct kk_uv_callback_s {
  kk_function_t callback;
} kk_uv_callback_t;

static inline kk_uv_callback_t* kk_new_uv_callback(kk_function_t cb, uv_handle_t* handle, kk_context_t* ctx) {
  kk_uv_callback_t* c = kk_malloc(sizeof(kk_uv_callback_t), ctx);
  c->callback = cb;
  handle->data = c;
  return c;
}

kk_std_core__error kk_async_error_from_errno( int err, kk_context_t* ctx );

uv_buf_t* kk_bytes_list_to_uv_buffs(kk_std_core__list buffs, int* size, kk_context_t* _ctx);
uv_buf_t* kk_bytes_to_uv_buffs(kk_bytes_t bytes, kk_context_t* _ctx);
