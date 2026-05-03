#ifndef __EMSCRIPTEN__

// Set the thread local uv loop
void kk_set_uv_loop(uv_loop_t* loop) {
  kk_uv_loop_default = loop;
}
// Get the thread local uv loop
uv_loop_t* uvloop() {
  return kk_uv_loop_default;
}

#endif
