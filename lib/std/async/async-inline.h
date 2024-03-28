#include "std_time_timer.h"

kk_box_t kk_set_timeout(kk_function_t cb, int64_t time, kk_context_t* _ctx) {
  kk_std_time_timer__timer t = kk_std_time_timer_timer_init(_ctx);
  kk_std_time_timer_timer_start(t, time, 0, cb, _ctx);
  return kk_std_time_timer__timer_box(t, _ctx);
}

kk_unit_t kk_clear_timeout(kk_box_t boxed_timer, kk_context_t* _ctx) {
  kk_std_time_timer__timer timer = kk_std_time_timer__timer_unbox(boxed_timer, KK_OWNED, _ctx);
  kk_std_time_timer_timer_stop(timer, _ctx);
  return kk_Unit;
}