// free memory using default C allocator
static void kk_free_calloc(void* p, kk_block_t* b, kk_context_t* _ctx) {
  kk_unused(b);
  kk_unused(_ctx);
  free(p);
}

static kk_box_t kk_owned_with_ptr(kk_box_t owned, kk_function_t f, kk_context_t* _ctx) {
  kk_addr_t cptr = (kk_addr_t)kk_cptr_raw_unbox_borrowed(owned, kk_context());
  return kk_function_call(kk_box_t,(kk_function_t,kk_addr_t,kk_context_t*), f, (f, cptr, kk_context()), kk_context());
}

static kk_box_t kk_borrowed_with_ptr(kk_box_t borrowed, kk_function_t f, kk_context_t* _ctx) {
  kk_addr_t cptr = (kk_addr_t)kk_cptr_unbox_borrowed(borrowed, kk_context());
  return kk_function_call(kk_box_t,(kk_function_t,kk_addr_t,kk_context_t*), f, (f, cptr, kk_context()), kk_context());
}

static kk_box_t kk_borrow_ptr(kk_addr_t cptr, kk_function_t f, kk_context_t* _ctx) {
  kk_box_t ptr = kk_cptr_box((void *)cptr, kk_context());
  return kk_function_call(kk_box_t,(kk_function_t,kk_box_t,kk_context_t*), f, (f, ptr, kk_context()), kk_context());
}

static kk_box_t kk_owned_with_ptr_idx(kk_box_t owned, kk_ssize_t idx, kk_function_t f, int32_t size, kk_context_t* _ctx) {
  uint8_t* cptr = (uint8_t*)kk_cptr_raw_unbox_borrowed(owned, kk_context());
  kk_addr_t cptr_idx = (kk_addr_t)(cptr + (idx*size));
  return kk_function_call(kk_box_t,(kk_function_t,kk_addr_t,kk_context_t*), f, (f, cptr_idx, kk_context()), kk_context());
}

static kk_string_t kk_string_alloc_raw_buff(kk_ssize_t len, char* s, bool free, kk_context_t* ctx){
  s[len] = 0;
  return kk_string_alloc_raw_len(len, s, free, ctx);
}

static kk_box_t kk_with_c_string(kk_string_t s, kk_function_t f, kk_context_t* _ctx){
  kk_addr_t cptr = (kk_addr_t)kk_string_cbuf_borrow(s, NULL, kk_context());
  return kk_function_call(kk_box_t,(kk_function_t,kk_addr_t,kk_context_t*), f, (f, cptr, kk_context()), kk_context());
}