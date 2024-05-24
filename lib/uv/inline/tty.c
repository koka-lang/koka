#include "uv.h"

kk_std_core_exn__error kk_uv_tty_init(int32_t fd, kk_context_t* _ctx) {
  uv_tty_t* tty = kk_malloc(sizeof(uv_tty_t), _ctx);
  int res = uv_tty_init(uvloop(), tty, fd, 0); // Last parameter is unused
  kk_uv_check_return_err_drops(res, uv_handle_to_owned_kk_handle_box(tty, kk_handle_free, tty, tty), {kk_free(tty, _ctx);})
}

uv_tty_mode_t kk_tty_mode_to_uv_tty_mode(kk_uv_tty__tty_mode mode, kk_context_t* _ctx) {
   if (kk_uv_tty__is_UV__TTY__MODE__NORMAL(mode, _ctx)) {
    return UV_TTY_MODE_NORMAL;
  } else if (kk_uv_tty__is_UV__TTY__MODE__RAW(mode, _ctx)) {
    return UV_TTY_MODE_RAW;
  } else if (kk_uv_tty__is_UV__TTY__MODE__IO(mode, _ctx)) {
    return UV_TTY_MODE_IO;
  } else {
    kk_fatal_error(-1, "Invalid enum value to uv ffi tty_mode");
  }
}

kk_uv_tty__tty_mode kk_uv_tty_mode_to_tty_mode(uv_tty_mode_t mode, kk_context_t* _ctx) {
  switch (mode) {
    case UV_TTY_MODE_NORMAL:
      return kk_uv_tty__new_UV__TTY__MODE__NORMAL(_ctx);
    case UV_TTY_MODE_RAW:
      return kk_uv_tty__new_UV__TTY__MODE__RAW(_ctx);
    case UV_TTY_MODE_IO:
      return kk_uv_tty__new_UV__TTY__MODE__IO(_ctx);
    default:
      kk_fatal_error(-1, "Invalid enum value from uv ffi tty_mode");
  }
}

uv_tty_vtermstate_t kk_tty_vtermstate_to_uv_tty_vtermstate(kk_uv_tty__tty_vtermstate state, kk_context_t* _ctx) {
  if (kk_uv_tty__is_UV__TTY__SUPPORTED(state, _ctx)) {
    return UV_TTY_SUPPORTED;
  } else if (kk_uv_tty__is_UV__TTY__UNSUPPORTED(state, _ctx)) {
    return UV_TTY_UNSUPPORTED;
  } else {
    kk_fatal_error(-1, "Invalid enum value to uv ffi tty_vtermstate");
  }
}

kk_uv_tty__tty_vtermstate kk_uv_tty_vtermstate_to_tty_vtermstate(uv_tty_vtermstate_t state, kk_context_t* _ctx) {
  switch (state) {
    case UV_TTY_SUPPORTED:
      return kk_uv_tty__new_UV__TTY__SUPPORTED(_ctx);
    case UV_TTY_UNSUPPORTED:
      return kk_uv_tty__new_UV__TTY__UNSUPPORTED(_ctx);
    default:
      kk_fatal_error(-1, "Invalid enum value from uv ffi tty_vtermstate");
  }
}

kk_std_core_exn__error kk_uv_tty_get_winsize(kk_uv_tty__uv_tty handle, kk_context_t* _ctx) {
  int32_t width, height;
  int status = uv_tty_get_winsize(kk_owned_handle_to_uv_handle(uv_tty_t, handle), &width, &height);
  kk_uv_check_return(status, kk_std_core_types__tuple2_box(kk_std_core_types__new_Tuple2(kk_int32_box(width, _ctx), kk_int32_box(height, _ctx), _ctx), _ctx))
}

kk_std_core_exn__error kk_uv_tty_get_vtermstate(kk_context_t* _ctx) {
  uv_tty_vtermstate_t uv_state;
  int status = uv_tty_get_vterm_state(&uv_state);
  kk_uv_check_return(status, kk_uv_tty__tty_vtermstate_box(kk_uv_tty_vtermstate_to_tty_vtermstate(uv_state, _ctx), _ctx))
}