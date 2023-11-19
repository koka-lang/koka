#include "std_time_timer.h"


kk_box_t kk_set_timeout(kk_function_t cb, int64_t time, kk_context_t* _ctx) {
  kk_std_time_timer__timer t = kk_std_time_timer_timer_init(_ctx);
  kk_std_time_timer_timer_start(t, time, 0, cb, _ctx);
  return kk_std_time_timer__timer_box(t, _ctx);
}


static kk_box_t kk_unit_closure(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_new_unit_closure(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_unit_closure, _ctx)
  return kk_function_dup(_fself,kk_context());
}

static kk_box_t kk_unit_closure(kk_function_t _fself, kk_context_t* _ctx) {
  kk_unused(_fself);
  return kk_unit_box(kk_Unit);
}


kk_unit_t kk_clear_timeout(kk_box_t t, kk_context_t* _ctx) {
  kk_std_time_timer__timer timer = kk_std_time_timer__timer_unbox(t, KK_OWNED, _ctx);
  kk_std_os_uv_close(kk_std_os_uv__new_UvHandle(timer.internal, _ctx), kk_new_unit_closure(_ctx), _ctx);
  return kk_Unit;
}



