#ifdef __EMSCRIPTEN__
#include <emscripten.h>

typedef struct kk_wasm_timer_s {
  kk_function_t callback;
  int64_t repeat_ms;
  int timer;
} kk_wasm_timer_t;

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info);
#endif