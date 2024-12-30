#ifndef __EMSCRIPTEN__

void kk_handle_free(void *p, kk_block_t *block, kk_context_t *_ctx) {
    uv_handle_t *hnd = (uv_handle_t *)p;
    kk_hnd_callback_t* hndcb = (kk_hnd_callback_t*)hnd->data;
    kk_function_drop(hndcb->callback, kk_context()); // Drop the callback
    kk_free(hndcb, kk_context()); // Free the memory used for the callback and box
    hnd->data = NULL; // Clear the reference to this
    uv_close(hnd, NULL);
    // p will be freed by uv.
}

void kk_set_uv_loop(uv_loop_t* loop) {
  kk_uv_loop_default = loop;
}

uv_loop_t* uvloop() {  
  return kk_uv_loop_default;
}

static kk_uv_utils__uv_status_code kk_uv_status_to_status_code(int32_t status,
                                                       kk_context_t *_ctx) {
  switch (status) {
  case 0:
    return kk_uv_utils_UV__OK;
  case UV_E2BIG:
    return kk_uv_utils_UV__E2BIG;
  case UV_EACCES:
    return kk_uv_utils_UV__EACCES;
  case UV_EADDRINUSE:
    return kk_uv_utils_UV__EADDRINUSE;
  case UV_EADDRNOTAVAIL:
    return kk_uv_utils_UV__EADDRNOTAVAIL;
  case UV_EAFNOSUPPORT:
    return kk_uv_utils_UV__EAFNOSUPPORT;
  case UV_EAGAIN:
    return kk_uv_utils_UV__EAGAIN;
  case UV_EAI_ADDRFAMILY:
    return kk_uv_utils_UV__EAI__ADDRFAMILY;
  case UV_EAI_AGAIN:
    return kk_uv_utils_UV__EAI__AGAIN;
  case UV_EAI_BADFLAGS:
    return kk_uv_utils_UV__EAI__BADFLAGS;
  case UV_EAI_BADHINTS:
    return kk_uv_utils_UV__EAI__BADHINTS;
  case UV_EAI_CANCELED:
    return kk_uv_utils_UV__EAI__CANCELED;
  case UV_EAI_FAIL:
    return kk_uv_utils_UV__EAI__FAIL;
  case UV_EAI_FAMILY:
    return kk_uv_utils_UV__EAI__FAMILY;
  case UV_EAI_MEMORY:
    return kk_uv_utils_UV__EAI__MEMORY;
  case UV_EAI_NODATA:
    return kk_uv_utils_UV__EAI__NODATA;
  case UV_EAI_NONAME:
    return kk_uv_utils_UV__EAI__NONAME;
  case UV_EAI_OVERFLOW:
    return kk_uv_utils_UV__EAI__OVERFLOW;
  case UV_EAI_PROTOCOL:
    return kk_uv_utils_UV__EAI__PROTOCOL;
  case UV_EAI_SERVICE:
    return kk_uv_utils_UV__EAI__SERVICE;
  case UV_EAI_SOCKTYPE:
    return kk_uv_utils_UV__EAI__SOCKTYPE;
  case UV_EALREADY:
    return kk_uv_utils_UV__EALREADY;
  case UV_EBADF:
    return kk_uv_utils_UV__EBADF;
  case UV_EBUSY:
    return kk_uv_utils_UV__EBUSY;
  case UV_ECANCELED:
    return kk_uv_utils_UV__ECANCELED;
  case UV_ECHARSET:
    return kk_uv_utils_UV__ECHARSET;
  case UV_ECONNABORTED:
    return kk_uv_utils_UV__ECONNABORTED;
  case UV_ECONNREFUSED:
    return kk_uv_utils_UV__ECONNREFUSED;
  case UV_ECONNRESET:
    return kk_uv_utils_UV__ECONNRESET;
  case UV_EDESTADDRREQ:
    return kk_uv_utils_UV__EDESTADDRREQ;
  case UV_EEXIST:
    return kk_uv_utils_UV__EEXIST;
  case UV_EFAULT:
    return kk_uv_utils_UV__EFAULT;
  case UV_EFBIG:
    return kk_uv_utils_UV__EFBIG;
  case UV_EHOSTUNREACH:
    return kk_uv_utils_UV__EHOSTUNREACH;
  case UV_EINTR:
    return kk_uv_utils_UV__EINTR;
  case UV_EINVAL:
    return kk_uv_utils_UV__EINVAL;
  case UV_EIO:
    return kk_uv_utils_UV__EIO;
  case UV_EISCONN:
    return kk_uv_utils_UV__EISCONN;
  case UV_EISDIR:
    return kk_uv_utils_UV__EISDIR;
  case UV_ELOOP:
    return kk_uv_utils_UV__ELOOP;
  case UV_EMFILE:
    return kk_uv_utils_UV__EMFILE;
  case UV_EMSGSIZE:
    return kk_uv_utils_UV__EMSGSIZE;
  case UV_ENAMETOOLONG:
    return kk_uv_utils_UV__ENAMETOOLONG;
  case UV_ENETDOWN:
    return kk_uv_utils_UV__ENETDOWN;
  case UV_ENETUNREACH:
    return kk_uv_utils_UV__ENETUNREACH;
  case UV_ENFILE:
    return kk_uv_utils_UV__ENFILE;
  case UV_ENOBUFS:
    return kk_uv_utils_UV__ENOBUFS;
  case UV_ENODEV:
    return kk_uv_utils_UV__ENODEV;
  case UV_ENOENT:
    return kk_uv_utils_UV__ENOENT;
  case UV_ENOMEM:
    return kk_uv_utils_UV__ENOMEM;
  case UV_ENONET:
    return kk_uv_utils_UV__ENONET;
  case UV_ENOPROTOOPT:
    return kk_uv_utils_UV__ENOPROTOOPT;
  case UV_ENOSPC:
    return kk_uv_utils_UV__ENOSPC;
  case UV_ENOSYS:
    return kk_uv_utils_UV__ENOSYS;
  case UV_ENOTCONN:
    return kk_uv_utils_UV__ENOTCONN;
  case UV_ENOTDIR:
    return kk_uv_utils_UV__ENOTDIR;
  case UV_ENOTEMPTY:
    return kk_uv_utils_UV__ENOTEMPTY;
  case UV_ENOTSOCK:
    return kk_uv_utils_UV__ENOTSOCK;
  case UV_ENOTSUP:
    return kk_uv_utils_UV__ENOTSUP;
  case UV_EOVERFLOW:
    return kk_uv_utils_UV__EOVERFLOW;
  case UV_EPERM:
    return kk_uv_utils_UV__EPERM;
  case UV_EPIPE:
    return kk_uv_utils_UV__EPIPE;
  case UV_EPROTO:
    return kk_uv_utils_UV__EPROTO;
  case UV_EPROTONOSUPPORT:
    return kk_uv_utils_UV__EPROTONOSUPPORT;
  case UV_EPROTOTYPE:
    return kk_uv_utils_UV__EPROTOTYPE;
  case UV_ERANGE:
    return kk_uv_utils_UV__ERANGE;
  case UV_EROFS:
    return kk_uv_utils_UV__EROFS;
  case UV_ESHUTDOWN:
    return kk_uv_utils_UV__ESHUTDOWN;
  case UV_ESPIPE:
    return kk_uv_utils_UV__ESPIPE;
  case UV_ESRCH:
    return kk_uv_utils_UV__ESRCH;
  case UV_ETIMEDOUT:
    return kk_uv_utils_UV__ETIMEDOUT;
  case UV_ETXTBSY:
    return kk_uv_utils_UV__ETXTBSY;
  case UV_EXDEV:
    return kk_uv_utils_UV__EXDEV;
  case UV_UNKNOWN:
    return kk_uv_utils_UV__UNKNOWN;
  case UV_EOF:
    return kk_uv_utils_UV__EOF;
  case UV_ENXIO:
    return kk_uv_utils_UV__ENXIO;
  case UV_EMLINK:
    return kk_uv_utils_UV__EMLINK;
  case UV_ENOTTY:
    return kk_uv_utils_UV__ENOTTY;
  case UV_EFTYPE:
    return kk_uv_utils_UV__EFTYPE;
  case UV_EILSEQ:
    return kk_uv_utils_UV__EILSEQ;
  case UV_ESOCKTNOSUPPORT:
    return kk_uv_utils_UV__ESOCKTNOSUPPORT;
  default:
    return kk_uv_utils_UV__UNKNOWN;
    // case UV_EUNACH:
    // return kk_uv_utils_UV__EUNATCH;
  }
}

uv_buf_t* kk_bytes_list_to_uv_buffs(kk_std_core_types__list buffs, int* size, kk_context_t* _ctx){
  kk_std_core_types__list_dup(buffs, _ctx);
  kk_integer_t klist_len = kk_std_core_list_length(buffs, _ctx);
  int list_len = kk_integer_clamp32(klist_len, _ctx);
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t) * list_len, _ctx);
  kk_std_core_types__list list = buffs;
  for (int i = 0; i < list_len; i++){
    struct kk_std_core_types_Cons* cons = kk_std_core_types__as_Cons(list, _ctx);
    kk_bytes_t bytes = kk_bytes_unbox(cons->head);
    kk_ssize_t len;
    const char* chars = kk_bytes_cbuf_borrow(bytes, &len, _ctx);
    uv_buffs[i].base = kk_malloc(len, _ctx);
    kk_memcpy(uv_buffs[i].base, chars, len);
    uv_buffs[i].len = len;
    list = cons->tail;
  }
  *size = list_len;
  return uv_buffs;
}

uv_buf_t* kk_bytes_to_uv_buffs(kk_bytes_t bytes, kk_context_t* _ctx){
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t) * 1, _ctx);
  kk_ssize_t len;
  const char* chars = kk_bytes_cbuf_borrow(bytes, &len, _ctx);
  uv_buffs[0].base = kk_malloc(sizeof(char)*len, _ctx);
  kk_memcpy(uv_buffs[0].base, chars, len);
  uv_buffs[0].len = len;
  kk_bytes_drop(bytes, _ctx);
  return uv_buffs;
}

kk_std_core_exn__error kk_async_error_from_errno( int err, kk_context_t* _ctx ) {
  kk_uv_utils__uv_status_code code = kk_uv_status_to_status_code(err, _ctx);
  kk_string_t msg = kk_uv_utils_message(code, _ctx);
  return kk_std_core_exn__new_Error( kk_std_core_exn__new_Exception( msg, kk_uv_utils__new_AsyncExn(kk_reuse_null, 0, code, _ctx), _ctx), _ctx );
}
#endif
