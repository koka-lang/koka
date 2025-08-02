kk_string_t kk_strbuf_finalize(kk_std_core_types__list buf, kk_ssize_t byte_size, kk_context_t* ctx) {
  // Finalize the string buffer by concatenating all strings in the vector
  uint8_t* cbuf;
  kk_string_t result = kk_unsafe_string_alloc_buf(byte_size, &cbuf, ctx);
  // kk_info_message("Finalizing string buffer total size: %d", byte_size);
  kk_std_core_types__list ys = buf;
  while (kk_std_core_types__is_Cons(ys,ctx)) {
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(ys,ctx);
    kk_std_core_types__list tail = cons->tail;
    kk_std_core_types__list_dup(tail, ctx);
    kk_vector_t v = kk_vector_unbox(cons->head, ctx);
    kk_ssize_t len;
    kk_box_t* cs = kk_vector_buf_borrow(v, &len, ctx);
    // kk_info_message("Adding strings from vector size: %d", len);
    for (kk_ssize_t i = 0; i < len; i++){
      kk_ssize_t slen;
      const uint8_t* sbytes = kk_string_buf_borrow(kk_string_unbox(cs[i]), &slen, ctx); 
      memcpy(cbuf, sbytes, slen);
      cbuf += slen;
    }
    kk_std_core_types__list_drop(ys, ctx);
    ys = tail;
  }
  return result;
}