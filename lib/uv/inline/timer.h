#ifdef __EMSCRIPTEN__
#include <emscripten.h>

//////////////////////////////////////////////////////
// Create the wrapper type
// For wasm, we need to keep extra information
//  such as repeat time and the timer id from JS
////////////////////////////////////////////////////// 
typedef struct kk_wasm_timer_s {
  kk_function_t callback;
  int64_t repeat_ms;
  int timer;
} kk_wasm_timer_t;

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info);
#else
#include <uv.h>
//////////////////////////////////////////////////////
// Create the UV wrapper type (see utils.h)
////////////////////////////////////////////////////// 
kk_uv_handle(timer);
#endif
