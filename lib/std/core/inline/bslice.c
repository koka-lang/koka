kk_bytes_t kk_bslice_assign( kk_bytes_t bytes, kk_ssize_t i, struct kk_std_core_bslice_Bslice bslice, kk_context_t* ctx ){
  kk_ssize_t len;
  const uint8_t* bslice_buf = kk_bytes_buf_borrow(bslice.backing_bytes, &len, ctx);
  kk_ssize_t slice_len = kk_integer_clamp_ssize_t_borrow(bslice.len, ctx);
  kk_ssize_t slice_start = kk_integer_clamp_ssize_t_borrow(bslice.start, ctx);
  if (kk_datatype_ptr_is_unique(bytes, ctx)) {
    // kk_info_message("kk_bslice_assign: unique\n");
    uint8_t* buf = (uint8_t*)kk_bytes_buf_borrow(bytes, &len, ctx);
    for (kk_ssize_t j = 0; j < slice_len; j++) {
      buf[i + j] = bslice_buf[slice_start + j];
    }
    kk_std_core_bslice__bslice_drop(bslice,ctx);
    return bytes;
  } else {
    // kk_info_message("kk_bslice_assign: not unique\n");
    kk_bytes_t bytes_new = kk_bytes_copy(bytes, ctx);
    uint8_t* buf_new = (uint8_t*)kk_bytes_buf_borrow(bytes_new, NULL, ctx);
    for (kk_ssize_t j = 0; j < slice_len; j++) {
      buf_new[i + j] = bslice_buf[slice_start + j];
    }
    kk_std_core_bslice__bslice_drop(bslice,ctx);
    return bytes_new;
  }
}

kk_bytes_t kk_bslice_bytes(struct kk_std_core_bslice_Bslice bslice, kk_context_t* ctx ){
  kk_ssize_t slice_len = kk_integer_clamp_ssize_t_borrow(bslice.len, ctx);
  kk_ssize_t slice_start = kk_integer_clamp_ssize_t_borrow(bslice.start, ctx);
  kk_ssize_t total_len;
  const uint8_t* bslice_buf = kk_bytes_buf_borrow(bslice.backing_bytes, &total_len, ctx);
  kk_bytes_t bytes = kk_bytes_alloc_dupn(slice_len, bslice_buf + slice_start, ctx);
  kk_std_core_bslice__bslice_drop(bslice, ctx); // TODO: Optimize if getting full slice, or shortening the slice from the beginning
  return bytes;
}